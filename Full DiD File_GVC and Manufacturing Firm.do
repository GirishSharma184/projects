*Sector wide DiD
cd "C:\Users\giris\Dropbox\Masters Thesis\Data_CMIE"

use "masterdata_merged_final_7_gvcstatus",clear
summarize Year_of_Incorporation





/**************************************
   STEP 1: Clean Data & Handle Left-Truncation
**************************************/

* Keep only firms present in 2013 and non-GVC at entry
preserve
keep if year == 2013
keep firm_id GVC
rename GVC gvc2013
tempfile baseyear
save `baseyear'
restore

merge m:1 firm_id using `baseyear'
drop if _merge == 2  // Drop firms not present in 2013
drop if gvc2013 == 1 // Remove firms that were GVC in 2013
drop _merge gvc2013

* Remove firms that are GVC in their first observed year (post-2013)
bysort firm_id: gen first_year = year[1]
drop if GVC == 1 & year == first_year

/**************************************
   STEP 2: Define Treatment Variables
**************************************/

* Identify first treatment year (after first observed year)
gen entry_year = . 
bysort firm_id (year): replace entry_year = year if GVC == 1 & year > first_year
bysort firm_id: egen first_treat = min(entry_year)

* Create treatment indicators
gen treated = (year >= first_treat) & !missing(first_treat)
gen g = first_treat  // CSDID requires this exact name


/***********************Plot of Treated Trajectories****************************
********************************************************************************/

* Start from dataset with 'treated' and 'g' already defined
keep if treated == 1

* Collapse to average TFP by (entry cohort g) and calendar year
collapse (mean) omega_m, by(g year)
rename g cohort
rename omega_m avg_tfp

* Step 5: Prepare markers for each cohort
* Generate plot using a loop
levelsof cohort, local(cohorts)
local plotcmd
local i = 1

foreach c of local cohorts {
    local plotcmd `plotcmd' (connected avg_tfp year if cohort == `c', ///
        lwidth(medthick) msymbol(o))
		local labelcmd `labelcmd' label(`i' "`c'")
    local ++i
}

* Step 7: Plot the graph
twoway `plotcmd', ///
    xtitle("Year") ///
    ytitle("Log TFP") ///
    xlabel(2014(1)2023, grid) ///
    ylabel(0.2(0.1)0.7, format(%3.1f) nogrid) ///
    legend(`labelcmd' size(vsmall) row(2) ring(0) position(1)) ///
    graphregion(color(white)) ///
    bgcolor(white)
	

restore
********************************************************************************/




/**************************************
   STEP 3: Panel Setup & Diagnostics
**************************************/

xtset firm_id year

* Check treatment distribution
tab first_treat
tab year treated

save "masterdata_merged_final_DiD.dta",replace

/**************************************************
	STEP: Calculating Lagged Covariates
***************************************************/

use "masterdata_merged_final_DiD.dta",clear
drop foreign_dummy


*replace RD_ = 0 if missing(RD_)

*gen RD_int_nom = RD_/Sales_

*gen RD_int = (RD_int_nom/deflator)*100

*gen logRD = log(RD_int)

gen foreign_dummy = ForeignShares_> 0
rename size_dummy3 LargeFirm
count if LargeFirm == 1

preserve
duplicates drop firm_id, force
count 
count if SmallFirm == 1
count if MediumFirm == 1
count if LargeFirm == 1
count if ForeignShares_ > 10
count if foreign_dummy == 1
restore
summarize size, detail


* lagged productivity
gen omega_m_lag1 = L.omega_m
gen omega_m_lag2 = L2.omega_m

* Lagged R&D dummy
gen rd_dummy_lag1 = L.RD_dummy
*gen logRD_lag1 = L.logRD

* Lagged foreign firm dummy
gen foreign_dummy_lag1 = L.foreign_dummy

* Size dummies are usually stable, but if they're time-varying:
gen SmallFirm_lag1  = L.SmallFirm
gen MediumFirm_lag1 = L.MediumFirm
gen LargeFirm_lag1 = L.LargeFirm
gen size_lag1 = L.logsize
gen logage = log(age)
gen logage_lag1 = L.logage
gen agesquare = logage^2
gen agesquare_lag1 = L.agesquare

encode sector_short, gen(sectorshort_cat)

* Covariate balance test (pre-analysis)


/**************************************
   STEP 4: Covariate Balance
**************************************/

global vars omega_m_lag1 omega_m_lag2 logage_lag1 foreign_dummy_lag1 size_lag1 rd_dummy_lag1 

est clear
estpost ttest $vars, by(treated)

ereturn list
esttab 
esttab using "ttest.txt", replace ///
    cells("mu_1(fmt(1)) mu_2(fmt(1)) b(star fmt(2)) se(fmt(2) par) count(fmt(0))") ///
    collabels("NonGVC" "GVC" "Diff. (NonGVC - GVC)" "s.e." "Obs.") ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    label booktabs nonum gaps noobs compress

*because no mean diff foreign dummy, not controlling for in logit -- coeff is anyway in logit is negative because analyzing new gvc entrant amomg firms that were no gvc in 2013, so foreign firms already connected gloablly are not in your risk set

/**************************************************************
						Logit Plot (Full Specs)
***************************************************************/

logit treated ///
	omega_m_lag2 ///
	omega_m_lag1 ///
    rd_dummy_lag1 ///
	size_lag1 ///
    logage_lag1 ///
	agesquare_lag1 ///
    i.sectorshort_cat i.year, ///
	vce(cluster firm_id) 

	*coeflegend
	
estimates store model_logit

esttab model_logit, se star(* 0.10 ** 0.05 *** 0.01)

	
/*************************PLotting Logit Full Spec*********************************	

net install schemepack, from("https://raw.githubusercontent.com/asjadnaqvi/stata-schemepack/main/installation/") replace
coefplot, keep(omega_m_lag2 omega_m_lag1 rd_dummy_lag1 size_lag1 logage_lag1 agesquare_lag1) ///
    drop(_cons) xline(0, lcolor(red) lwidth(medium)) scheme(white_jet) ///
    xtitle("Effect on Log-Odds of GVC Participation") ///
    graphregion(margin(medsmall)) ///
    xsize(6.5) ysize(4.5) ///
    xlab(-1(0.5)3, glpattern(solid) glcolor(gs14)) ///
    grid(glpattern(solid) glcolor(gs14)) ///
    ylab(, labsize(*1.1)) ///
    coeflabels( ///
        omega_m_lag2 = "TFP{sub:t-2}" ///
        omega_m_lag1 = "TFP{sub:t-1}" ///
        rd_dummy_lag1 = "R&D Intensity{sub:t-1}" ///
        size_lag1 = "Firm Size{sub:t-1}" ///
        logage_lag1 = "Log Age{sub:t-1}" ///
        agesquare_lag1 = "Age Squared{sub:t-1}" ///
    ) ///
    headings( ///
        omega_m_lag2 = "{it: Productivity}" ///
        rd_dummy_lag1 = "{it: Firm Characteristics}", gap(0) ///
    ) ///
    msize(small) mcolor(%85) mlcolor(cyan) mlwidth(medium) msymbol(circle) ///
    cismooth(color(midblue) lwidth(2 30))

********************************************************************/	

/************************Logit Diff Model Specifications***************************
***********************************************************************

global controls omega_m_lag2 omega_m_lag1 rd_dummy_lag1 size_lag1 logage_lag1 agesquare_lag1 
est clear
eststo: logit treated $controls, vce(cluster firm_id)
 estadd local  FE  "No"
 estadd local  TE  "No"
 
eststo: logit treated $controls i.sectorshort_cat, vce(cluster firm_id)
 estadd local  FE  "Yes"
 estadd local  TE  "No"
 
eststo: logit treated $controls i.year, vce(cluster firm_id)
 estadd local  FE  "No"
 estadd local  TE  "Yes"
 
eststo: logit treated $controls i.sectorshort_cat i.year, vce(cluster firm_id)
 estadd local  FE  "Yes"
 estadd local  TE  "Yes"
 
esttab using "fulllogit.tex", replace   ///
 b(3) se(3) ///
 keep($controls) ///
 star(* 0.10 ** 0.05 *** 0.01) ///
 label noobs nonotes nomtitle collabels(none) compress ///
 scalars("TE Year Effects" "FE Sector effects") sfmt(3 0)
	
**************************************/


/*************************Logit Plot (Pscore Specs)********************************
						
***************************************************************/
logit treated ///
	omega_m_lag2 ///
	omega_m_lag1 ///
    rd_dummy_lag1 ///
	size_lag1 ///
    logage_lag1 ///
	agesquare_lag1 ///
    i.sectorshort_cat i.year, ///
	vce(cluster firm_id) 

	estimates store model_logit

esttab model_logit, se star(* 0.10 ** 0.05 *** 0.01)
predict pscore, pr
histogram pscore

*Distribution of Propensity Scores across Treatment and Control Groups
psgraph, treated (treated) pscore (pscore) ytitle("Density")

twoway ///
  (kdensity pscore if treated==1) ///
  (kdensity pscore if treated==0, lpattern(dash)) ///
  , legend(label(1 "GVC Firm") label(2 "Non GVC Firm")) ///
    title("Before Matching") ///
    xtitle("Propensity score") ///
	ylabel(, "Density") ///
    name(before, replace)

psmatch2 treated, outcome(omega_m) neighbor(1) pscore(pscore) caliper(0.01) 

asdoc pstest omega_m_lag2 omega_m_lag1 logage_lag1 agesquare_lag1 size_lag1 rd_dummy_lag1, ///
       treated(_treated) both graph
	   
summarize _weight

twoway (kdensity _pscore if _treated==1 [aweight=_weight]) ///
(kdensity _pscore if _treated==0 [aweight=_weight] ///
, lpattern(dash)), legend( label( 1 "GVC Firm") label( 2 "Non GVC Firm" )) ///
title("After Matching") ///
xtitle("Propensity score") ///
ytitle("Density") ///
name(after, replace)

/**************************************
   STEP 4: CSDID Estimation
**************************************/
csdid_setup firm_id year g, long


csdid omega_m omega_m_lag1 omega_m_lag2 size_lag1 logage_lag1 rd_dummy_lag1, ///
    ivar(firm_id) ///
    time(year) ///
    gvar(g) ///
    method(dripw) ///
    wboot ///
	long2 ///
    notyet  // Critical for staggered DiD!

*saverif(mainunmatched)
estat event, estore(csdid_main)

estat pretrend

estat simple

/***********************Output****************************************
Pretrend Test. H0 All Pre-treatment are equal to 0
chi2(36) =    77.5913
p-value  =     0.0001

**********************************************************************/

preserve
use mainunmatched, clear
*csdid_stats group, wboot

csdid_stats calendar

/***********************Output****************************************

---------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
        ATT |   .0132116   .0087915     1.50     -.004834    .0312572
---------------------------------------------------------------------


**********************************************************************/

csdid_stats group, wboot

/***********************Output****************************************
---------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
   GAverage |   .0210284   .0089937     2.34    -.0036372     .045694
      G2015 |   .0052089    .016351     0.32    -.0396344    .0500522
      G2016 |   .0015098   .0136061     0.11    -.0358055    .0388252
      G2017 |   .0061376   .0158541     0.39     -.037343    .0496182
      G2018 |   .0010184   .0175682     0.06     -.047163    .0491999
      G2019 |   .0113963   .0209468     0.54    -.0460514     .068844
      G2020 |   .0781386   .0214905     3.64     .0191998    .1370773
      G2021 |   .0591513   .0190109     3.11      .007013    .1112897
      G2022 |   .0159141   .0239327     0.66    -.0497224    .0815505
      G2023 |   .0544068   .0308014     1.77    -.0300673     .138881
---------------------------------------------------------------------



**********************************************************************/

/*********************Full Event Plot**********************************
***********************************************************************/
coefplot ///
    (csdid_main, keep(Tm6 Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_main, keep(Tp0 Tp1 Tp2 Tp3 Tp4 Tp5 Tp6) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
    yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
	xline(6, lp(dash) lw(0.1) lc(black)) ///
    xlabel(1 "-6" 2 "-5" 3 "-4" 4 "-3" 5 "-2" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5" 12 "6", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("ATT") ///
	ylabel(-0.15(0.05)0.15, format(%3.2f) labsize(medium)) ///
    legend(position(5)  ring(0) size(3)) ///
    graphregion(color(white)) ///
	name(event, replace)
************************************************************************

/*********************Pre Treatment Trend Plot*************************
***********************************************************************/
*Pre Treatment Coefficient

coefplot ///
    (csdid_main, keep(Tm6 Tm5 Tm4 Tm3 Tm2 Tm1) ///
        mcolor(blue) ciopts(recast(rcap) lcolor(black)) ///
        label("Pre-Treatment Estimated Coefficient")) ///
    , vertical ///
    msymbol(circle) ///
    ci ///
    addplot(scatteri 0.090 1 "Panel A", ///
        msymbol(i) mlabel(1) mlabcolor(red) mlabsize(medium)) ///
    yline(0, lpattern(dash) lw(0.3) lcolor(black)) ///
    xlabel(1 "-6" 2 "-5" 3 "-4" 4 "-3" 5 "-2" 6 "-1", angle(0) labsize(medium)) ///
    xtitle("Lead Periods to Treatment", size(5)) ///
    ytitle("Treatment Effect", size(5)) ///
    ylabel(-0.10(0.05)0.10, format(%3.2f) labsize(medium) labgap(0.5)) ///
    legend(position(5) ring(0) size(3)) ///
    graphregion(color(white)) ///
    name(pretrend, replace)

*Pretrend Power
estimates restore csdid_main

matrix list e(b)
matrix list e(V)		
matrix beta = e(b)
matrix sigma = e(V)
matrix beta  = beta[., 4..14]
matrix sigma = sigma[4..14, 4..14]

pretrends power 0.8, pre(4/8) post(9/14)
return list	

pretrends, b(beta) v(sigma) numpre(5) slope(.0076596) ytitle("Treatment Effects", size(5)) xtitle("Period Relative to Treatment", size(5)) legend(position(5) ring(0) size(3)) msymbol(circle) mcolor(blue) ciopts(recast(rcap)) text(0.090 -5 "Panel B", size(medium) color(red)) ylabel(-0.10(0.05)0.10, format(%3.2f) labsize(medium) labgap(0.5)) xlabel(-6(1)5, nogrid labsize(medium)) graphregion(color(white)) name(coef, replace)
return list


graph combine pretrend coef, ycommon
graph export "pretrendroth.png", width(3600) height(1200) replace

*********************************************************************************/
estat pretrend
estat all
csdid_plot, group(2016)
preserve
use mainunmatched, clear

* Get matrices with ATT, SE, and t-stat (no stars needed)
csdid_stats event, wboot
matrix A = r(table)'
matrix A = A[.,1..6]

csdid_stats group, wboot
matrix B = r(table)'
matrix B = B[.,1..6]

csdid_stats calendar, wboot
matrix C = r(table)'
matrix C = C[.,1..6]

csdid_stats simple, wboot
matrix D = r(table)'
matrix D = D[.,1..6]

* Combine all into one matrix
matrix ALL = A \ B \ C \ D
matrix colnames ALL = ATT SE tstat


* Step 4: Export clean table (NO stars)
esttab matrix(ALL) using att_table.txt, ///
    replace ///
    cells("ATT SE tstat") ///
    title("ATT Estimates by Aggregation Type") ///
    booktabs nonum gaps noobs compress

restore
/*****************************************************************************
							Group Averages
I further investigate dynamic treatment effects for firms that entered GVCs in 2020, a year coinciding with major macroeconomic disruptions due to COVID-19. As shown in the group-level estimates, these firms experienced a statistically significant average treatment effect on the treated (ATT) of 5 percentage points in total factor productivity (TFP), relative to a matched control group of non-GVC firms (p = 0.015). This suggests that, despite pandemic-related frictions, GVC participation was associated with a productivity gain for this cohort.

However, the event-study analysis, which decomposes treatment effects across time relative to entry, reveals wide confidence intervals for individual post-treatment years. While the point estimates are positive, especially around the second and third years post-entry, they are not statistically distinguishable from zero due to limited precision. This is consistent with lower power in dynamic specifications, where fewer observations inform each event-time estimate. Taken together, the results support the conclusion that GVC entry in 2020 was, on average, productivity-enhancing, but caution against overinterpreting year-by-year dynamics.

I do not emphasize the year-by-year evolution of treatment effects, as the dynamic specifications—while directionally consistent—are statistically imprecise, with wide confidence intervals around each event-year estimate. These limitations, likely stemming from smaller subsample sizes for each period, preclude strong conclusions about the timing or persistence of the effect. Accordingly, I interpret the cohort-level ATT as the main summary statistic of interest for this group.							
*****************************************************************************/					

csdid_plot, ///
    group(2015) ///
    title("Event Study: Cohort 2015 – GVC Entry Effect on TFP")

csdid_plot, ///
    group(2016) ///
    title("Event Study: Cohort 2016 – GVC Entry Effect on TFP")
	
csdid_plot, ///
    group(2017) ///
    title("Event Study: Cohort 2017 – GVC Entry Effect on TFP")
	
csdid_plot, ///
    group(2018) ///
    title("Event Study: Cohort 2018 – GVC Entry Effect on TFP")	
	
csdid_plot, ///
    group(2019) ///
    title("Event Study: Cohort 2019 – GVC Entry Effect on TFP")
	
csdid_plot, ///
    group(2020) ///
    title("Event Study: Cohort 2020 – GVC Entry Effect on TFP")
	
csdid_plot, ///
    group(2021) ///
    title("Event Study: Cohort 2021 – GVC Entry Effect on TFP")
	
csdid_plot, ///
    group(2022) ///
    title("Event Study: Cohort 2022 – GVC Entry Effect on TFP")

csdid_plot, ///
    group(2023) /// 
    title("Event Study: Cohort 2023 – GVC Entry Effect on TFP")




/**************************************
   STEP 5: Validation & Visualization
**************************************/

* Pre-trend test
estat pretrend  // Test first 5 pre-periods
estat simple
* Event study plot
event_plot csdid_main, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect (AE)") ///
	title("AE Event Plot") xlabel(-5(1)6)) stub_lag(Tp#) stub_lead(Tm#) together

* Reliability test for pretrends 

*Honest DiD
csdid_estat event, estore(csdid)
estimates restore csdid_main
matrix list e(b)
matrix l_vec = (1/8) \ (1/8) \ (1/8) \ (1/8) \ (1/8) \ (1/8) \ (1/8) \ (1/8)
local plotopts xtitle(Mbar, size(5)) ytitle(95% Robust CI, size(5))
honestdid, l_vec(l_vec) pre(3/8) post(9/16) mvec(0.0(0.5)2.5) ylabel(, format(%9.2f) labsize(medium) labgap(0.5) nogrid) xlabel(, grid labsize(medium)) coefplot `plotopts' name(honest,replace)	

graph combine coef honest, cols(2) imargin(0 0 0 0) graphregion(color(white))

graph export "ruthfigures.png", width(3600) height(1200) replace


/****************************
		Robustness Check
*****************************/

**Matching before estimating
**Using weights from NN matching
keep if _support == 1

csdid _omega_m size_lag1 logage rd_dummy_lag1 [pweight = _weight], ///
    ivar(firm_id) ///
    time(year) ///
    gvar(g) ///
    method(reg) ///
    wboot ///
    saverif (csdidmatched)


estat event, estore(csdid_weight)	
estat pretrend  
estat simple

/***************************Output****************************************


*************************************************************************/

preserve
use csdidmatched, clear

csdid_stats simple, wboot

/***************************Output****************************************


*************************************************************************/

csdid_stats group, wboot


/***************************Output****************************************


*************************************************************************/

 

* Event study plot
event_plot csdid_weight, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect (AE)") ///
	title("AE Event Plot") xlabel(-5(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
	
coefplot ///
    (csdid_weight, keep(Tm6 Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_main, keep(Tp0 Tp1 Tp2 Tp3 Tp4 Tp5 Tp6) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
    yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
	xline(6, lp(dash) lw(0.1) lc(black)) ///
    xlabel(1 "-6" 2 "-5" 3 "-4" 4 "-3" 5 "-2" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5" 12 "6", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("ATT") ///
	ylabel(-0.15(0.05)0.15, format(%3.2f) labsize(medium)) ///
    legend(position(5)  ring(0) size(3)) ///
    graphregion(color(white)) ///
	name(event, replace)
	
estat simple

estat all

/************************Matching****************************
Notably, the 2020 entry cohort exhibits the largest positive ATT (6.7%) among all groups under the matching-weighted IPW specification. However, this effect is not statistically significant (p = 0.293) and has a wide confidence interval (–5.8% to +19.2%). This imprecision likely reflects the reduced effective sample size and higher variance induced by the reweighting procedure. Nevertheless, the direction and magnitude of the effect remain consistent with the baseline estimates, suggesting that firms entering GVCs during the COVID-19 period may have experienced productivity improvements, although I cannot rule out the possibility of no effect.

**************************************************************/

*HonestDiD
estimates restore csdid_weight
matrix l_vec = (1/7) \ (1/7) \ (1/7) \ (1/7) \ (1/7) \ (1/7) \ (1/7)
local plotopts xtitle(M, size(5)) ytitle(95% Robust CI, size(5))
honestdid, l_vec(l_vec) pre(3/6) post(7/13) mvec(0.0(0.5)2.5) ylabel(, format(%3.2f) labsize(medium) labgap(0.5)) xlabel(, grid labsize(medium)) delta(sd) coefplot `plotopts' name(honest,replace)	

*Reliability of pretrends
estimates restore csdid_weight
matrix list e(b)
matrix list e(V)		
matrix beta = e(b)
matrix sigma = e(V)
matrix beta  = beta[., 3..13]
matrix sigma = sigma[3..13, 3..13]

pretrends power 0.8, pre(3/6) post(7/13)
return list	

pretrends, b(beta) v(sigma) numpre(6) slope(.0138055) ytitle("ATT", size(5)) xtitle("Time Relative to GVC Entry", size(5)) legend(position(5) ring(0) size(3)) ylabel(-0.15(0.05)0.15, format(%3.2f) labsize(medium) labgap(0.5)) xlabel(-6(1)4, labsize(medium))  graphregion(color(white)) name(coef, replace)
return list

save "masterdata_merged_final_2_DiD.dta", replace
/*****************Subgroup Heterogeneity Analysis RD ************************
		


**Using RD**

csdid omega_m omega_m_lag2 logsize age ///
	if rd_dummy_lag1 == 1, ///
    ivar(firm_id) time(year) gvar(g) ///
    method(dripw) wboot long2 notyet///
    
estimates store csdid_rd

estat pretrend  

estat simple
estat event

estimates restore csdid_rd
event_plot csdid_rd, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect (AE)") ///
	title("AE Event Plot") xlabel(-5(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
	
coefplot ///
    (csdid_rd, keep(Tm6 Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_main, keep(Tp0 Tp1 Tp2 Tp3 Tp4 Tp5 Tp6) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
    yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
	xline(6, lp(dash) lw(0.1) lc(black)) ///
    xlabel(1 "-6" 2 "-5" 3 "-4" 4 "-3" 5 "-2" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5" 12 "6", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("ATT") ///
	ylabel(-0.15(0.05)0.15, format(%3.2f) labsize(medium)) ///
    legend(position(5)  ring(0) size(3)) ///
    graphregion(color(white)) ///
	name(event, replace)
	
	tab g if rd_dummy_lag1 == 1
	
***^didn't work

***Trying interaction term

csdid omega_m omega_m_lag2 logsize age ///
    if rd_dummy_lag1 == 1, ///
    ivar(firm_id) time(year) gvar(g) method(dripw)
		
estimates store csdid_rnd

csdid omega_m omega_m_lag2 logsize age ///
    if rd_dummy_lag1 == 0, ///
    ivar(firm_id) time(year) gvar(g) method(dripw)

estimates store csdid_nonrnd

*Compare
estimates restore csdid_rnd
estat simple
scalar att_rnd = r(estimate)

estimates restore csdid_nonrnd
estat simple
scalar att_nonrnd = r(estimate)

display "Difference = " att_rnd - att_nonrnd



estimates restore csdid_rd
event_plot csdid_rd, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect (AE)") ///
	title("AE Event Plot") xlabel(-5(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
	
coefplot ///
    (csdid_rd, keep(Tm6 Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_main, keep(Tp0 Tp1 Tp2 Tp3 Tp4 Tp5 Tp6) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
    yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
	xline(6, lp(dash) lw(0.1) lc(black)) ///
    xlabel(1 "-6" 2 "-5" 3 "-4" 4 "-3" 5 "-2" 6 "0" 7 "1" 8 "2" 9 "3" 10 "4" 11 "5" 12 "6", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("ATT") ///
	ylabel(-0.15(0.05)0.15, format(%3.2f) labsize(medium)) ///
    legend(position(5)  ring(0) size(3)) ///
    graphregion(color(white)) ///
	name(event, replace)
	***************************************************************************************/
	
use "masterdata_merged_final_2_DiD.dta", clear

/******************Hetero analysis by size*********************************
Athey & Imbens, Wooldridge, Callaway & Sant'Anna subgroup by pretreatment covariates
***************************************************************************/

preserve
collapse (mean) logsize if inrange(year,2013,2015), by(firm_id)
rename logsize mean_logsize_pre

// 2. Compute the true firm-level median
centile mean_logsize_pre, centile(50)
scalar median_size = r(c_1)

// 3. Tag small vs. large firms here
gen small_firm = (mean_logsize_pre <= median_size)
label define sizegrp 1 "Small Firm" 0 "Large Firm"
label values small_firm sizegrp

// 4. Save this tiny lookup table
tempfile firmsize
save `firmsize'
restore

use "masterdata_merged_final_2_DiD.dta", clear
xtset firm_id year

// many:1 ensures each firm-year picks up its firm's tag exactly once
merge m:1 firm_id using `firmsize', keepusing(mean_logsize_pre small_firm) nogen

// Quick sanity checks
tab _merge
      

preserve

collapse (mean) RD_dummy omega_m age, by(small_firm)

save "smalllargedescriptive.dta",replace
restore


// DID Estimation
***************************************************
* Run csdid separately for Small Firms	
***************************************************
gen logage_lag1 = L.logage
csdid omega_m omega_m_lag2 logage_lag1 rd_dummy_lag1 if small_firm == 1, ///
    ivar(firm_id) time(year) gvar(g) ///
    method(dripw) wboot long2 notyet saverif(smallunmatched)

estat event, estore(csdid_sizeunmatchedsmall)
	
estat pretrend	
/****************Output**********************************************
Pretrend Test. H0 All Pre-treatment are equal to 0
chi2(27) =    35.7893
p-value  =     0.1200
********************************************************************/

preserve
use smallunmatched, clear

csdid_stats simple, wboot

/**********************Output*****************************************

----------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
   GAverage |     .02875   .0174949     1.64    -.0172423    .0747423
      G2016 |  -.0448325    .036187    -1.24    -.1399642    .0502993
      G2017 |    .036648   .0318157     1.15    -.0469921    .1202881
      G2018 |   .0190264   .0439927     0.43    -.0966257    .1346785
      G2019 |  -.0059847   .0322943    -0.19     -.090883    .0789135
      G2020 |   .0531286   .0478316     1.11    -.0726157    .1788728
      G2021 |     .07615   .0371622     2.05    -.0215456    .1738457
      G2022 |   .0876573    .040099     2.19    -.0177587    .1930733
      G2023 |   .0046271   .0222367     0.21    -.0538308     .063085
---------------------------------------------------------------------

*********************************************************************/

csdid_stats simple, wboot


/*********************Output*******************************************
----------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
        ATT |   .0186997   .0185814     1.01    -.0173368    .0547361
---------------------------------------------------------------------

---------------------------------------------------------------------
*************************************************************************/

coefplot ///
    (csdid_sizeunmatchedsmall, keep(Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_sizeunmatchedsmall, keep(Tp0 Tp1 Tp2 Tp3 Tp4 Tp5 Tp6) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
	    text(0.15 2 "Panel A", place(e) color(red) size(medium) just(left)) ///
    yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
    xline(5, lp(dash) lw(0.1) lc(black)) ///
    xlabel( 1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5" 11 "6", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("Treatment Effect") ///
    ylabel(-0.10(0.05)0.15, format(%3.2f) labsize(medium) nogrid) ///
    legend(position(6) ring(0) size(3)) ///
    graphregion(color(white)) ///
    name(eventsmallfirm, replace)
	
***************************************************	
***************************************************
* Run csdid separately for Large Firms	
***************************************************
***************************************************
csdid omega_m omega_m_lag2 logage_lag1 rd_dummy_lag1 if small_firm == 0, ///
    ivar(firm_id) time(year) gvar(g) ///
    method(dripw) wboot long2 notyet  saverif(largeunmatched)

estat event, estore(csdid_sizeunmatchedlarge)

estat pretrend
	
/****************Output**********************************************
Pretrend Test. H0 All Pre-treatment are equal to 0
chi2(23) =    53.3039
p-value  =     0.0003
********************************************************************/
	
preserve
use largeunmatched, clear


csdid_stats simple, wboot

/*****************Output**********************************************
---------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
        ATT |  -.0007379   .0078342    -0.09    -.0165245    .0150488
---------------------------------------------------------------------
---------------------------------------------------------------------
*********************************************************************/

csdid_stats group, wboot

/*****************Output*********************************************
Coefficient  Std.	err.	t	[95%	conf. interval]
				
GAverage    .0030134	.0071105		0.42	.0161323    .0221591
G2016     .000902	.0141281		0.06	.0371395    .0389434
G2017   -.0028922	.0151723		-0.19	.0437453    .0379608
G2018   -.0076931	.0123045		-0.63	.0408242    .0254381
G2019   -.0076034	.0215209		-0.35	.0655506    .0503439
G2020    .0321571	.0147933		2.17	.0076753    .0719895
G2021   -.0009627	.0097771		-0.10	.0272885     .025363
G2022   -.0308608	.0213404		-1.45	.0883221    .0266005
G2023    .0845044	.009127			9.26	.059929    .1090798
*********************************************************************/				

coefplot ///
    (csdid_sizeunmatchedlarge, keep(Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_sizeunmatchedlarge, keep(Tp0 Tp1 Tp2 Tp3 Tp4 Tp5 Tp6 Tp7) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
	    text(0.15 2 "Panel B", place(e) color(red) size(medium) just(left)) ///
	yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
	xline(5, lp(dash) lw(0.1) lc(black)) ///
	xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5" 11 "6", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("Treatment Effect") ///
	ylabel(-0.10(0.05)0.15, format(%3.2f) nogrid labsize(medium)) ///
    legend(position(6)  ring(0) size(3)) ///
    graphregion(color(white)) ///
	name(eventlargefirm, replace)

graph combine eventsmallfirm eventlargefirm, rows(2)


***************************************************
/*************CSDID for Matched SMALL/LArge**************************************
* Run csdid separately for Small Firms	_ Matched
***************************************************
csdid omega_m omega_m_lag2 logage rd_dummy_lag1 if small_firm == 1, ///
    ivar(firm_id) time(year) gvar(g) ///
    method(dripw) wboot long2 notyet w(_weight) saverif(smallmatched)

estat event, estore(csdid_sizesmallmatched)

estat pretrend	

/**************************Output*****************************************

Pretrend Test. H0 All Pre-treatment are equal to 0
chi2(27) =    35.8289
p-value  =     0.1191

*************************************************************************/

preserve 

use smallmatched, clear

csdid_stats event

csdid_stats simple, wboot

/**************************Output*****************************************

---------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
        ATT |   .0188005   .0180293     1.04    -.0173635    .0549645
---------------------------------------------------------------------

*************************************************************************/

csdid_stats group, wboot
/**************************Output*****************************************
---------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
   GAverage |     .02875   .0174233     1.65    -.0151223    .0726223
      G2016 |  -.0448325   .0345125    -1.30     -.131736     .042071
      G2017 |    .036648   .0352379     1.04    -.0520821    .1253781
      G2018 |   .0190264   .0441242     0.43    -.0920796    .1301325
      G2019 |  -.0059847   .0340897    -0.18    -.0918236    .0798541
      G2020 |   .0531286   .0499324     1.06    -.0726027    .1788598
      G2021 |     .07615   .0423083     1.80    -.0303834    .1826834
      G2022 |   .0876573   .0392587     2.23    -.0111973    .1865119
      G2023 |   .0046271   .0223728     0.21    -.0517082    .0609623
---------------------------------------------------------------------

*************************************************************************/

coefplot ///
    (csdid_sizesmallmatched, keep(Tm7 Tm6 Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_sizesmallmatched, keep(Tp0 Tp1 Tp2 Tp3 Tp4 Tp5 Tp6 Tp7) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
    yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
	xline(7, lp(dash) lw(0.1) lc(black)) ///
    xlabel(1 "-7" 2 "-6" 3 "-5" 4 "-4" 5 "-3" 6 "-2" 7 "0" 8 "1" 9 "2" 10 "3" 11 "4" 12 "5" 13 "6" 14 "7", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("ATT") ///
	ylabel(-0.35(0.05)0.35, format(%3.2f) labsize(medium)) ///
    legend(position(5)  ring(0) size(3)) ///
    graphregion(color(white)) ///
	name(event, replace)
***************************************************	
***************************************************
* Run csdid separately for Large Firms	_ Matched
***************************************************
***************************************************

csdid omega_m omega_m_lag2 logage rd_dummy_lag1 if small_firm == 0, ///
    ivar(firm_id) time(year) gvar(g) ///
    method(dripw) wboot long2 notyet w(_weight) saverif(largematched)

estat event, estore(csdid_sizelargematched)

estat pretrend

/******************Output*****************************
Pretrend Test. H0 All Pre-treatment are equal to 0
chi2(23) =    54.5429
p-value  =     0.0002

******************************************************/


preserve
use largematched, clear


csdid_stats simple, wboot
/******************Output*****************************
--------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
        ATT |  -.0007647   .0085494    -0.09    -.0167792    .0152498
---------------------------------------------------------------------

******************************************************/

csdid_stats group, wboot
/******************Output*****************************
---------------------------------------------------------------------
            | Coefficient  Std. err.      t      [95% conf. interval]
------------+--------------------------------------------------------
   GAverage |   .0030134   .0070392     0.43    -.0156203    .0216471
      G2016 |    .000902   .0159686     0.06     -.041369    .0431729
      G2017 |  -.0028922   .0142947    -0.20     -.040732    .0349476
      G2018 |  -.0076931   .0124057    -0.62    -.0405326    .0251465
      G2019 |  -.0076034   .0217078    -0.35    -.0650666    .0498599
      G2020 |   .0321571   .0152005     2.12    -.0080805    .0723947
      G2021 |  -.0009627   .0093256    -0.10    -.0256488    .0237233
      G2022 |  -.0308608   .0218831    -1.41    -.0887882    .0270666
      G2023 |   .0845044   .0095419     8.86     .0592458     .109763
---------------------------------------------------------------------
******************************************************/



use largeunmatched, clear
csdid_stats simple
coefplot ///
    (csdid_sizelargematched, keep(Tm8 Tm7 Tm6 Tm5 Tm4 Tm3 Tm2) ///
        mcolor(blue) ciopts(lcolor(blue)) label("Pre-Treatment")) ///
    (csdid_sizelargematched, keep(Tp0 Tp1 Tp2 Tp3 Tp4) ///
        mcolor(red) ciopts(lcolor(red)) label("Post-Treatment")), ///
    vertical ///
    msymbol(circle) ///
    ci ///
    yline(0, lpattern(dash) lw(0.1) lcolor(black)) ///
	xline(7, lp(dash) lw(0.1) lc(black)) ///
    xlabel(1 "-7" 2 "-6" 3 "-5" 4 "-4" 5 "-3" 6 "-2" 7 "0" 8 "1" 9 "2" 10 "3" 11 "4", angle(0) labsize(medium) grid) ///
    xtitle("Time Relative to GVC Entry") ///
    ytitle("ATT") ///
	ylabel(-0.15(0.05)0.15, format(%3.2f) labsize(medium)) ///
    legend(position(5)  ring(0) size(3)) ///
    graphregion(color(white)) ///
	name(event, replace)
	
	
/**************************Looping over method and covariates inclusion*****************************/

global covariates omega_m_lag2 logage rd_dummy_lag1
global covspecs none "`covariates'"

foreach m in `methods' {
    foreach covars in `covspecs' {
        csdid omega_m if small_firm == 0, ///
            ivar(firm_id) time(year) gvar(g) ///
            method(`m') w(_weight) ///
            xvar(`covars') wboot long2 notyet ///
            saverif(match_`m'_covars`covars'_)
    }
}