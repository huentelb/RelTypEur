*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in the United States
* Bettina Hünteler
* 23.02.2024
* huenteler@wiso.uni-koeln.de

*** 01 DATA CLEANING *** 

*-------------------------------------------------------------------------------


*** Set (up) working directories ***

*** folders
global WD 	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses"

global IN 	"/Users/Bettina/Documents/datasets/KINMATRIX"
global OUT 	"/Users/Bettina/Documents/datasets/KINMATRIX"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses/graphs/241014/M11_median/lc5_output/ame"

*** preparations/settings
set more off, perm
set scheme white_tableau, perm //  rainbow
set showbaselevels on


*** Sample Selection ***

use $IN/prepared_v02.dta, clear

keep if anc_cou == 10 	// USA 					-> N = 113,697 obs
keep if kin_ls == 1		// Kin alive			-> N = 90,489
drop if inlist(kin_cat, 12, 20, 10, 18) // only bio kin -> N = 84,879


* Drop if missings

keep if anc_gnd >= 0 & anc_gnd != 3		// n = 933 obs
keep if anc_eth >= 0 					// n = 4,858 obs
*keep if anc_rel1 >= 0 | anc_rel1 == -3 	// n = 834 obs
*keep if anc_rel3 >= 0 | anc_rel3 == -3	// n = 160 obs

tab1 rel_adv1 rel_clo rel_cmf1 rel_cnf rel_cnt rel_mon1 ///
						rel_tra rel_cou1, m


egen rel_mis = anycount(rel_adv1 rel_clo rel_cmf1 rel_cnf rel_cnt rel_mon1 ///
						rel_tra), ///
	v(-1 -2 -3 -4 -5)

	
tab kin_cat rel_mis, row
keep if rel_mis <= 2					// n = 4,485 obs


*** Sample selection (by respondent)

preserve
bys anc_id: gen help = _n
keep if help == 1
tab female
tab eth
restore



mvdecode _all, mv(-1=.a \ -2=.b \ -3=.c \ -4=.d \ -5=.e)

* -1 Don't know				-> .a
* -2 Prefer not to answer 	-> .b
* -3 Does not apply			-> .c
* -5 Incomplete data		-> .e



*** Racial and ethnic distribution
tab anc_eth anc_his2, m

gen eth = 1 if anc_eth == 1
replace eth = 2 if anc_eth == 2 & anc_his2 == 1
replace eth = 3 if anc_eth == 2 & anc_his2 == 2
replace eth = 4 if anc_eth == 3 & anc_his2 == 1
replace eth = 5 if anc_eth == 3 & anc_his2 == 2
replace eth = 6 if anc_his2 > 100



*** Distribution of Relationship Variables by Kin Type ***

foreach x of varlist rel_cnt {
	bys kin_cat: tab `x'
	hist `x', by(kin_cat, legend(off)) d percent normal 	///
	xlab(1 2 3 4 5 6, valuelabel angle(45)) ///
	name(hist_`x', replace)
	graph export "$GR/hist_`x'.pdf", replace
}
	
foreach x of varlist rel_tra rel_clo {
	bys kin_cat: tab `x'
	hist `x', by(kin_cat, legend(off)) d percent normal 	///
	xlab(1 2 3 4 5, valuelabel angle(45)) ///
	name(hist_`x', replace)
	graph export "$GR/hist_`x'.pdf", replace
}

foreach x of varlist rel_mon1 rel_adv1 rel_cmf1 {
	bys kin_cat: tab `x'
	hist `x', by(kin_cat, legend(off)) d percent 	///
	xlab(1 2, valuelabel angle(45)) ///
	name(hist_`x', replace) 
	graph export "$GR/hist_`x'.pdf", replace
	
	recode `x' (1=0)(2=1), gen(`x'_rec)
}

foreach x of varlist rel_cnf {
	bys kin_cat: tab `x'
	hist `x', by(kin_cat, legend(off)) d percent normal 	///
	xlab(1 2 3 4, valuelabel angle(45)) ///
	name(hist_`x', replace)
	graph export "$GR/hist_`x'.pdf", replace
}




*******************************
*** TABLE 1 
*******************************

use $WD/data/USA_lc.dta, clear




lab var anc_age "Age"
lab var eth "Race-Ethnicity"

/*lab def kin 1"Parents" 			///
			2"Siblings" 		///
			3"Grandparents" 	///
			4"Halfsiblings" 	///
			5"Uncles and Aunts"	///
			6"Cousins", replace
lab val kin_cat_ kin */

* store cluster labels in locals
global cl1 "Tight-knit"
global cl2 "Close"								
global cl3 "Ambivalent"								
global cl4 "Intimate but distant"
global cl5 "Detached"



*** some modifications for TABLES

* number of kin for each anchor by kin type
bys anc_id kin_cat_l: gen kin_n = _N
bys anc_id: gen total_kin = _N

* store number of kin type in new var for each kin type (num_1 to num_16)
foreach kin of numlist 1/16 {
	gen num_`kin' = kin_n if inlist(kin_cat_l, `kin')
	* Step 1: Sort the data by anc_id 
	sort anc_id

	* Step 2: Carry forward the value of num_1 within each anc_id
	bysort anc_id: replace num_`kin' = num_`kin'[_n-1] if missing(num_`kin')

		/* Step 3: Repeat backward fill until all missing values are filled
		* Use a loop to repeatedly apply the backward fill
		quietly {
			gen filled`kin' = 0  // A flag to track if the values have been fully filled

			while !filled`kin' {
				bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
				
				// Check if any missing values still exist within any anc_id
				by anc_id: gen missing_left`kin' = sum(missing(num_`kin'))
				replace filled`kin' = 1 if missing_left == 0
				drop missing_left`kin'
			}
		}*/
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
	*recode num_`kin'(.=0)
}

recode num_*(.=0)


order anc_id kin_cat_l kin_n num_*


tab edu, gen(edu_)
tab eth, gen(eth_)
recode female(1=0)(2=1), gen(fem)

	
global sociodemo anc_age eth_* fem edu_*
global kin num_*
global rel tra cnt clo sup mon cnf


bys anc_id: gen help = _n


* number of kin and socio-demographics for anchors TABLE 1
preserve
keep if help == 1
estpost sum $sociodemo $kin, det
	eststo tab1
restore	

	* save table
	cd $M
	esttab tab1 using des.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 

* TABLE 1 by race
preserve
keep if help == 1
foreach x of numlist 1/5 {
estpost sum $sociodemo $kin total_kin $rel if eth == `x', det
	eststo tab1eth_`x'
	}
restore	

	* save table
	cd $M
	esttab tab1eth_* using des_eth.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 

preserve
keep if help == 1
foreach x of numlist 1/5 {
estpost sum $sociodemo $kin, det
	eststo tab1clust_`x'
	}
restore	

foreach x of numlist 1/5 {
estpost sum $kin if class == `x', det
	eststo tab1clust_`x'
	}
	
	* save table
	cd $M
	esttab tab1clust_* using des_clust.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean)) sd(fmt(%12.2fc) l("(SD)") par)) ///
		mtitle("$cl1" "$cl2" "$cl3" "$cl4" "$cl5") replace label ///
		title(Description of sample by cluster) 

	* test for significant differences
	
	/* continuous covariates
	foreach var of varlist cbage_c gcbage_c pdage_c dur_* {
	    dis "`var'"
		anova `var' cluster
	}
	
	* categorical covariates
	foreach var of varlist sharec sharegc sharep {
	    dis "`var'"
		reg `var' cluster
	}
		
*/




*******************************
*** Multinomial Logit ***
*******************************



global controls "anc_age i.eth i.female"

*_l i.mig_l 




*** TABLE 1

* check kin counts by gender
tab total_kin female, row
bys female: sum total_kin, det


mlogit class i.kin_cat_small, vce(cluster anc_id)


* kin_cat_small
mlogit class i.kin_cat_small, vce(cluster anc_id)
est store miogit_small
outreg2 using $M/mlogit_small.doc, label eform stats(coef se) aster() dec(2) replace nocons


mlogit class i.kin_cat_small $controls, vce(cluster anc_id)
est store mlogit_small_c
outreg2 using $M/mlogit_small.doc, label eform stats(coef se) aster() dec(2) append nocons


mlogit class i.kin_cat_small##i.female anc_age i.eth, vce(cluster anc_id)
est store mlogit_s_fem
mlogit class i.kin_cat_small##i.eth anc_age i.female, vce(cluster anc_id)
est store mlogit_m_race


* kin_cat_med		
mlogit class i.kin_cat_med, vce(cluster anc_id)
est store miogit_med
outreg2 using $M/mlogit_med.doc, label eform stats(coef se) aster() dec(2) replace nocons


mlogit class i.kin_cat_med $controls, vce(cluster anc_id)
est store mlogit_med_c
outreg2 using $M/mlogit_med.doc, label eform stats(coef se) aster() dec(2) append nocons

mlogit class i.kin_cat_med##i.female anc_age i.eth, vce(cluster anc_id)
est store mlogit_m_fem
mlogit class i.kin_cat_med##i.eth anc_age i.female, vce(cluster anc_id)
est store mlogit_m_race

* kin_cat_l
mlogit class i.kin_cat_l, vce(cluster anc_id)
est store miogit_l
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() dec(2) replace nocons


mlogit class i.kin_cat_l $controls, vce(cluster anc_id)
est store mlogit_l_c
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() dec(2) append nocons


mlogit class i.kin_cat_l##i.female anc_age i.eth, vce(cluster anc_id)
est store mlogit_l_fem
mlogit class i.kin_cat_l##i.eth anc_age i.female, vce(cluster anc_id)
est store mlogit_l_race
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() dec(2) append nocons



*** AMEs of all variables in the model




*** GRAPH STYLE


grstyle init
grstyle set plain, grid horizontal
grstyle set color Okabe, select(2/6)
grstyle set color Okabe, select(2/6):  p#markline
*grstyle set color cblind
*grstyle set color cblind: p#markline
grstyle set symbol o t s + sh 







** kin_cat_small
est restore	mlogit_small_c
margins, dydx(i.kin_cat_small $controls) predict(outcome(1)) post
est store m1_s

est restore	mlogit_small_c
margins, dydx(i.kin_cat_small $controls) predict(outcome(2)) post
est store m2_s

est restore	mlogit_small_c
margins, dydx(i.kin_cat_small $controls) predict(outcome(3)) post
est store m3_s

est restore	mlogit_small_c
margins, dydx(i.kin_cat_small $controls) predict(outcome(4)) post
est store m4_s

est restore	mlogit_small_c
margins, dydx(i.kin_cat_small $controls) predict(outcome(5)) post
est store m5_s


coefplot 	(m1_s, label("$cl1")) 	///
			(m2_s, label("$cl2")) 	///
			(m3_s, label("$cl3")) 	///
			(m4_s, label("$cl4"))	///
			(m5_s, label("$cl5")),	///
	recast(scatter) drop(_cons) 								///
	xtitle("Effects on Probability")  							///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 		///
	///ysize(13) xsize(10) 										///
	title("Average Marginal Effects") name(covariates, replace)
	
		gr export $M/ame_cov_small.png, replace


coefplot 	(m1_s, label("$cl1")) 	///
			(m2_s, label("$cl2")) 	///
			(m3_s, label("$cl3")) 	///
			(m4_s, label("$cl4"))	///
			(m5_s, label("$cl5")),	///
	recast(scatter) drop(_cons) keep(*:) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base, replace)
		
		gr export $M/ame_cov_small_base.png, replace


coefplot 	(m1_s, label("$cl1")) 	///
			(m2_s, label("$cl2")) 	///
			(m3_s, label("$cl3")) 	///
			(m4_s, label("$cl4"))	///
			(m5_s, label("$cl5")),	///
	recast(scatter) drop(_cons *kin_cat_small) keep(*:) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_small_base_nokin.png, replace
	

** kin_cat_med
est restore	mlogit_med_c
margins, dydx(i.kin_cat_med $controls) predict(outcome(1)) post
est store m1_m

est restore	mlogit_med_c
margins, dydx(i.kin_cat_med $controls) predict(outcome(2)) post
est store m2_m

est restore	mlogit_med_c
margins, dydx(i.kin_cat_med $controls) predict(outcome(3)) post
est store m3_m

est restore	mlogit_med_c
margins, dydx(i.kin_cat_med $controls) predict(outcome(4)) post
est store m4_m

est restore	mlogit_med_c
margins, dydx(i.kin_cat_med $controls) predict(outcome(5)) post
est store m5_m

coefplot 	(m1_m, label("$cl1")) 	///
			(m2_m, label("$cl2")) 	///
			(m3_m, label("$cl3")) 	///
			(m4_m, label("$cl4"))	///
			(m5_m, label("$cl5")),	///
	recast(scatter) drop(_cons) 								///
	xtitle("Effects on Probability")  							///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 		///
	///ysize(13) xsize(10) 										///
	title("Average Marginal Effects") name(covariates, replace)
	
		gr export $M/ame_cov_med.png, replace


coefplot 	(m1_m, label("$cl1")) 	///
			(m2_m, label("$cl2")) 	///
			(m3_m, label("$cl3")) 	///
			(m4_m, label("$cl4"))	///
			(m5_m, label("$cl5")),	///
	recast(scatter) drop(_cons) keep(*:) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base, replace)
		
		gr export $M/ame_cov_med_base.png, replace
	

coefplot 	(m1_m, label("$cl1")) 	///
			(m2_m, label("$cl2")) 	///
			(m3_m, label("$cl3")) 	///
			(m4_m, label("$cl4"))	///
			(m5_m, label("$cl5")),	///
	recast(scatter) drop(_cons *kin_cat_med) keep(*:) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_med_base_nokin.png, replace
		

** kin_cat_l
est restore	mlogit_l_c
margins, dydx(i.kin_cat_l $controls) predict(outcome(1)) post
est store m1

est restore	mlogit_l_c
margins, dydx(i.kin_cat_l $controls) predict(outcome(2)) post
est store m2

est restore	mlogit_l_c
margins, dydx(i.kin_cat_l $controls) predict(outcome(3)) post
est store m3

est restore	mlogit_l_c
margins, dydx(i.kin_cat_l $controls) predict(outcome(4)) post
est store m4

est restore	mlogit_l_c
margins, dydx(i.kin_cat_l $controls) predict(outcome(5)) post
est store m5






coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) drop(_cons) 								///
	xtitle("Effects on Probability")  							///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 		///
	///ysize(13) xsize(10) 										///
	title("Average Marginal Effects") name(covariates, replace)
	
		gr export $M/ame_cov_l.png, replace
		gr save $M/ame_cov_l, replace


coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) drop(_cons) keep(*:) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base, replace)
		
		gr export $M/ame_cov_l_base.png, replace
		gr save $M/ame_cov_l_base, replace

		
		
coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) drop(_cons *kin_cat_l) keep(*:) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_l_base_nokin.png, replace
		gr save $M/ame_cov_l_base_nokin, replace
		
		
coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) drop(_cons *kin_cat_l) keep(*eth) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_l_eth.png, replace
		gr save $M/ame_cov_l_eth, replace

		
coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) drop(_cons) keep(*kin_cat_l) omitted baselevels			///
	xtitle("Effects on Probability") 							 	///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	title("Average Marginal Effects") name(covariates_base_kin, replace)
		
		gr export $M/ame_cov_l_base_kin.png, replace
		gr save $M/ame_cov_l_base_kin, replace
		

	
	
	
** Kin##Race interaction

* number of kin for each anchor by kin type
bys anc_id kin_cat_m: gen kin_m_n = _N

* store number of kin type in new var for each kin type (num_1 to num_16)
foreach kin of numlist 1/10 {
	gen num_`kin'_m = kin_m_n if inlist(kin_cat_m, `kin')
	* Step 1: Sort the data by anc_id 
	sort anc_id

	* Step 2: Carry forward the value of num_1 within each anc_id
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n-1] if missing(num_`kin'_m)

		/* Step 3: Repeat backward fill until all missing values are filled
		* Use a loop to repeatedly apply the backward fill
		quietly {
			gen filled`kin' = 0  // A flag to track if the values have been fully filled

			while !filled`kin' {
				bysort anc_id: replace num_`kin' = num_`kin'[_n+1] if missing(num_`kin')
				
				// Check if any missing values still exist within any anc_id
				by anc_id: gen missing_left`kin' = sum(missing(num_`kin'))
				replace filled`kin' = 1 if missing_left == 0
				drop missing_left`kin'
			}
		}*/
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n+1] if missing(num_`kin'_m)
	*recode num_`kin'(.=0)
}
recode num_*_m(.=0)

tab class, gen(clust)

/*foreach num of numlist 1/5 {	
logit clust`num' i.kin_cat_l##i.eth i.female, or 
est store clust`num'
}
* vermutlich über loop über kin
foreach num of numlist 1/5 {	
est restore clust`num'
margins kin_cat_l, dydx() post
est store iact`num'	
}
*/

mlogit class i.kin_cat_m##i.eth i.female, rrr
est store iact_m


foreach num of numlist 1/5 {	
*mlogit class i.kin_cat_m## i.female, rrr
*est store clust`num'_m

est restore iact_m
margins kin_cat_m#eth, predict(outcome(`num')) post
est store iact`num'_m
}

/*est restore iact_m
margins kin_cat_m#i.eth, predict(outcome(2)) post
est store iact2_`num'_m

est restore iact_m
margins kin_cat_m#i.eth, predict(outcome(3)) post
est store iact3_`num'_m

est restore iact_m
margins kin_cat_m#i.eth, predict(outcome(4)) post
est store iact4_`num'_m

est restore iact_m
margins kin_cat_m#i.eth, predict(outcome(5)) post
est store iact5_`num'_m
}
*/

global eth1 "White"
global eth2 "Hispanic"
global eth3 "Black"
global eth4 "Asian"
global eth5 "Other"



*** TIGHT-KNIT

grstyle init
grstyle set plain, grid horizontal compact
grstyle set color Okabe, select(2) : p#bar p#barline


bys eth: sum num_10_m if help == 1 // av number of cousins
est restore iact1_m
di _b[10.kin_cat_med#1.eth] * 6.471721 // White cousins
di _b[10.kin_cat_med#2.eth] * 8.928797 // Hisp cousins
di _b[10.kin_cat_med#3.eth] * 7.763158 // Black cousins 
di _b[10.kin_cat_med#4.eth] * 8.161572 // Asian cousins


bys eth: sum num_1_m if help == 1 // av number of fathers
est restore iact1_m
di _b[1.kin_cat_med#1.eth] * .8844937 // White fathers
di _b[1.kin_cat_med#2.eth] * .8253589 // Hisp fathers
di _b[1.kin_cat_med#3.eth] * .8951965 // Black fathers 
di _b[1.kin_cat_med#4.eth] * .7709924 // Asian fathers


bys eth: sum num_2_m if help == 1 // av number of mothers
est restore iact1_m
di _b[2.kin_cat_med#1.eth] * .9309347 // White mothers
di _b[2.kin_cat_med#2.eth] *  .939873 // Hisp  mothers
di _b[2.kin_cat_med#3.eth] * .9046455 // Black  mothers 
di _b[2.kin_cat_med#4.eth] * .9453925 // Asian  mothers


bys eth: sum num_4_m if help == 1 // av number of sisters
est restore iact1_m
di _b[4.kin_cat_med#1.eth] * .7144003 // White sisters
di _b[4.kin_cat_med#2.eth] * .8655063 // Hisp  sisters
di _b[4.kin_cat_med#3.eth] * .9425837 // Black sisters 
di _b[4.kin_cat_med#4.eth] * .7510917 // Asian sisters


bys eth: sum num_5_m if help == 1 // av number of grandfathers
est restore iact1_m
di _b[5.kin_cat_med#1.eth] * .4652605 // White grandfathers
di _b[5.kin_cat_med#2.eth] * .4794304 // Hisp  grandfathers
di _b[5.kin_cat_med#3.eth] * .4400978 // Black  grandfathers 
di _b[5.kin_cat_med#4.eth] * .3856655 // Asian  grandfathers


bys eth: sum num_9_m if help == 1 // av number of aunts
est restore iact1_m
di _b[9.kin_cat_med#1.eth] * 1.885029 // White  aunts
di _b[9.kin_cat_med#2.eth] * 2.473101 // Hisp   aunts
di _b[9.kin_cat_med#3.eth] * 2.334963 // Black   aunts 
di _b[9.kin_cat_med#4.eth] * 2.136519 // Asian   aunts

coefplot 	(iact1_m),							///
	recast(bar) barw(.7) drop(_cons) citop			///
	xtitle("Expected probability", size(small))  	///
	ysize(15) xsize(11)  							///
	xlabel(, labsize(vsmall)) 						///
	ylabel(, labsize(tiny))							///
	title("$cl1", size(small)) name(iact1, replace)	///
	groups(	1.kin_cat_med#?.eth = "Father" 			///
			2.kin_cat_med#?.eth = "Mother"			///
			3.kin_cat_med#?.eth = "Brothers"			///
			4.kin_cat_med#?.eth = "Sisters"			///
			5.kin_cat_med#?.eth = "Grandfathers"		///
			6.kin_cat_med#?.eth = "Grandmothers"		///
			7.kin_cat_med#?.eth = "Halfsiblings"		///
			8.kin_cat_med#?.eth = "Uncles"			///
			9.kin_cat_med#?.eth = "Aunts"				///
			10.kin_cat_med#?.eth = "Cousins")   		///
	ylabel(			1  "$eth1" 	/// Fathers
					2  "$eth2"	///
					3  "$eth3"	///
					4  "$eth4"	///
					5  "$eth5"	/// 
					7  "$eth1"	/// Mothers
					8  "$eth2"	/// 
					9  "$eth3"	/// 
					10 "$eth4"	///
					11 "$eth5"	///
					13 "$eth1"	/// Brothers
					14 "$eth2"	///
					15 "$eth3"	/// 
					16 "$eth4"	///
					17 "$eth5"	/// 
					19 "$eth1"	/// Sisters
					20 "$eth2"	///
					21 "$eth3"	///
					22 "$eth4"	/// 
					23 "$eth5"	///
					25 "$eth1"	/// Grandfathers
					26 "$eth2"	/// 
					27 "$eth3"	///
					28 "$eth4"	///
					29 "$eth5"	/// 
					31 "$eth1"	/// Grandmothers
					32 "$eth2"	///
					33 "$eth3"	/// 
					34 "$eth4"	/// 
					35 "$eth5"	/// 
					37 "$eth1"	/// Halfsiblings
					38 "$eth2"	///
					39 "$eth3"	///
					40 "$eth4"	///
					41 "$eth5"	///
					43 "$eth1"	/// Uncles
					44 "$eth2"	/// 
					45 "$eth3"	/// 
					46 "$eth4"	///
					47 "$eth5"	///
					49 "$eth1"	/// Aunts
					50 "$eth2"	///
					51 "$eth3"	/// 
					52 "$eth4"	/// 
					53 "$eth5"	/// 
					55 "$eth1"	///  Cousins
					56 "$eth2"	///
					57 "$eth3"	///
					58 "$eth4"	/// 
					59 "$eth5"	/// 
			)  	
			gr save "$M/iact_1", replace
			gr export "$M/iact_1.png", replace
	

	
*** CLOSE
grstyle init
grstyle set plain, grid horizontal compact
grstyle set color Okabe, select(3) : p#bar p#barline


bys eth: sum num_10_m if help == 1 // av number of cousins
est restore iact2_m
di _b[10.kin_cat_med#1.eth] * 6.471721 // White cousins
di _b[10.kin_cat_med#2.eth] * 8.928797 // Hisp cousins
di _b[10.kin_cat_med#3.eth] * 7.763158 // Black cousins 
di _b[10.kin_cat_med#4.eth] * 8.161572 // Asian cousins


bys eth: sum num_1_m if help == 1 // av number of fathers
est restore iact2_m
di _b[1.kin_cat_med#1.eth] * .8655914 // White fathers
di _b[1.kin_cat_med#2.eth] * .8655914 // Hisp fathers
di _b[1.kin_cat_med#3.eth] * .8288509 // Black fathers 
di _b[1.kin_cat_med#4.eth] * .883959 // Asian fathers


bys eth: sum num_2_m if help == 1 // av number of mothers
est restore iact2_m
di _b[2.kin_cat_med#1.eth] * .9309347 // White mothers
di _b[2.kin_cat_med#2.eth] *  .939873 // Hisp  mothers
di _b[2.kin_cat_med#3.eth] * .9046455 // Black  mothers 
di _b[2.kin_cat_med#4.eth] * .9453925 // Asian  mothers

bys eth: sum num_4_m if help == 1 // av number of sisters
est restore iact2_m
di _b[4.kin_cat_med#1.eth] * .7212572 // White sisters
di _b[4.kin_cat_med#2.eth] * .8623418 // Hisp  sisters
di _b[4.kin_cat_med#3.eth] * .9413203 // Black  sisters 
di _b[4.kin_cat_med#4.eth] * .7474403 // Asian  sisters


bys eth: sum num_5_m if help == 1 // av number of grandfathers
est restore iact2_m
di _b[5.kin_cat_med#1.eth] * .4652605 // White grandfathers
di _b[5.kin_cat_med#2.eth] * .4794304 // Hisp  grandfathers
di _b[5.kin_cat_med#3.eth] * .4400978 // Black  grandfathers 
di _b[5.kin_cat_med#4.eth] * .3856655 // Asian  grandfathers


bys eth: sum num_9_m if help == 1 // av number of aunts
est restore iact2_m
di _b[9.kin_cat_med#1.eth] * 1.885029 // White  aunts
di _b[9.kin_cat_med#2.eth] * 2.473101 // Hisp   aunts
di _b[9.kin_cat_med#3.eth] * 2.334963 // Black   aunts 
di _b[9.kin_cat_med#4.eth] * 2.136519 // Asian   aunts

coefplot 	(iact2_m),							///
	recast(bar) barw(.7) drop(_cons) citop			///
	xtitle("Expected probability", size(small))  	///
	ysize(15) xsize(11)  							///
	xlabel(, labsize(vsmall)) 						///
	ylabel(, labsize(tiny))							///
	title("$cl2", size(small)) name(iact2, replace)	///
	groups(	1.kin_cat_med#?.eth = "Father" 			///
			2.kin_cat_med#?.eth = "Mother"			///
			3.kin_cat_med#?.eth = "Brothers"			///
			4.kin_cat_med#?.eth = "Sisters"			///
			5.kin_cat_med#?.eth = "Grandfathers"		///
			6.kin_cat_med#?.eth = "Grandmothers"		///
			7.kin_cat_med#?.eth = "Halfsiblings"		///
			8.kin_cat_med#?.eth = "Uncles"			///
			9.kin_cat_med#?.eth = "Aunts"				///
			10.kin_cat_med#?.eth = "Cousins")   		///
	ylabel(			1  "$eth1" 	/// Fathers
					2  "$eth2"	///
					3  "$eth3"	///
					4  "$eth4"	///
					5  "$eth5"	/// 
					7  "$eth1"	/// Mothers
					8  "$eth2"	/// 
					9  "$eth3"	/// 
					10 "$eth4"	///
					11 "$eth5"	///
					13 "$eth1"	/// Brothers
					14 "$eth2"	///
					15 "$eth3"	/// 
					16 "$eth4"	///
					17 "$eth5"	/// 
					19 "$eth1"	/// Sisters
					20 "$eth2"	///
					21 "$eth3"	///
					22 "$eth4"	/// 
					23 "$eth5"	///
					25 "$eth1"	/// Grandfathers
					26 "$eth2"	/// 
					27 "$eth3"	///
					28 "$eth4"	///
					29 "$eth5"	/// 
					31 "$eth1"	/// Grandmothers
					32 "$eth2"	///
					33 "$eth3"	/// 
					34 "$eth4"	/// 
					35 "$eth5"	/// 
					37 "$eth1"	/// Halfsiblings
					38 "$eth2"	///
					39 "$eth3"	///
					40 "$eth4"	///
					41 "$eth5"	///
					43 "$eth1"	/// Uncles
					44 "$eth2"	/// 
					45 "$eth3"	/// 
					46 "$eth4"	///
					47 "$eth5"	///
					49 "$eth1"	/// Aunts
					50 "$eth2"	///
					51 "$eth3"	/// 
					52 "$eth4"	/// 
					53 "$eth5"	/// 
					55 "$eth1"	///  Cousins
					56 "$eth2"	///
					57 "$eth3"	///
					58 "$eth4"	/// 
					59 "$eth5"	/// 
			)  	
			gr save "$M/iact_2", replace
			gr export "$M/iact_2.png", replace
	
	
*** AMBIVALENT
grstyle init
grstyle set plain, grid horizontal compact
grstyle set color Okabe, select(4) : p#bar p#barline

coefplot 	(iact3_m),							///
	recast(bar) barw(.7) drop(_cons) citop			///
	xtitle("Expected probability", size(small))  	///
	ysize(15) xsize(11)  							///
	xlabel(, labsize(vsmall)) 						///
	ylabel(, labsize(tiny))							///
	title("$cl3", size(small)) name(iact3, replace)	///
	groups(	1.kin_cat_med#?.eth = "Father" 			///
			2.kin_cat_med#?.eth = "Mother"			///
			3.kin_cat_med#?.eth = "Brothers"			///
			4.kin_cat_med#?.eth = "Sisters"			///
			5.kin_cat_med#?.eth = "Grandfathers"		///
			6.kin_cat_med#?.eth = "Grandmothers"		///
			7.kin_cat_med#?.eth = "Halfsiblings"		///
			8.kin_cat_med#?.eth = "Uncles"			///
			9.kin_cat_med#?.eth = "Aunts"				///
			10.kin_cat_med#?.eth = "Cousins")   		///
	ylabel(			1  "$eth1" 	/// Fathers
					2  "$eth2"	///
					3  "$eth3"	///
					4  "$eth4"	///
					5  "$eth5"	/// 
					7  "$eth1"	/// Mothers
					8  "$eth2"	/// 
					9  "$eth3"	/// 
					10 "$eth4"	///
					11 "$eth5"	///
					13 "$eth1"	/// Brothers
					14 "$eth2"	///
					15 "$eth3"	/// 
					16 "$eth4"	///
					17 "$eth5"	/// 
					19 "$eth1"	/// Sisters
					20 "$eth2"	///
					21 "$eth3"	///
					22 "$eth4"	/// 
					23 "$eth5"	///
					25 "$eth1"	/// Grandfathers
					26 "$eth2"	/// 
					27 "$eth3"	///
					28 "$eth4"	///
					29 "$eth5"	/// 
					31 "$eth1"	/// Grandmothers
					32 "$eth2"	///
					33 "$eth3"	/// 
					34 "$eth4"	/// 
					35 "$eth5"	/// 
					37 "$eth1"	/// Halfsiblings
					38 "$eth2"	///
					39 "$eth3"	///
					40 "$eth4"	///
					41 "$eth5"	///
					43 "$eth1"	/// Uncles
					44 "$eth2"	/// 
					45 "$eth3"	/// 
					46 "$eth4"	///
					47 "$eth5"	///
					49 "$eth1"	/// Aunts
					50 "$eth2"	///
					51 "$eth3"	/// 
					52 "$eth4"	/// 
					53 "$eth5"	/// 
					55 "$eth1"	///  Cousins
					56 "$eth2"	///
					57 "$eth3"	///
					58 "$eth4"	/// 
					59 "$eth5"	/// 
			)  	
			gr save "$M/iact_3", replace
			gr export "$M/iact_3.png", replace
	

	
*** INTIMATE BUT DISTANT
grstyle init
grstyle set plain, grid horizontal compact
grstyle set color Okabe, select(5) : p#bar p#barline
grstyle set ci Okabe, select(5) opacity()


coefplot 	(iact4_m),							///
	recast(bar) barw(.7) drop(_cons) citop			///
	xtitle("Expected probability", size(small))  	///
	ysize(15) xsize(11)  							///
	xlabel(, labsize(vsmall)) 						///
	ylabel(, labsize(tiny))							///
	title("$cl4", size(small)) name(iact4, replace)	///
	groups(	1.kin_cat_med#?.eth = "Father" 			///
			2.kin_cat_med#?.eth = "Mother"			///
			3.kin_cat_med#?.eth = "Brothers"			///
			4.kin_cat_med#?.eth = "Sisters"				///
			5.kin_cat_med#?.eth = "Grandfathers"		///
			6.kin_cat_med#?.eth = "Grandmothers"		///
			7.kin_cat_med#?.eth = "Halfsiblings"		///
			8.kin_cat_med#?.eth = "Uncles"				///
			9.kin_cat_med#?.eth = "Aunts"				///
			10.kin_cat_med#?.eth = "Cousins")   		///
	ylabel(			1  "$eth1" 	/// Fathers
					2  "$eth2"	///
					3  "$eth3"	///
					4  "$eth4"	///
					5  "$eth5"	/// 
					7  "$eth1"	/// Mothers
					8  "$eth2"	/// 
					9  "$eth3"	/// 
					10 "$eth4"	///
					11 "$eth5"	///
					13 "$eth1"	/// Brothers
					14 "$eth2"	///
					15 "$eth3"	/// 
					16 "$eth4"	///
					17 "$eth5"	/// 
					19 "$eth1"	/// Sisters
					20 "$eth2"	///
					21 "$eth3"	///
					22 "$eth4"	/// 
					23 "$eth5"	///
					25 "$eth1"	/// Grandfathers
					26 "$eth2"	/// 
					27 "$eth3"	///
					28 "$eth4"	///
					29 "$eth5"	/// 
					31 "$eth1"	/// Grandmothers
					32 "$eth2"	///
					33 "$eth3"	/// 
					34 "$eth4"	/// 
					35 "$eth5"	/// 
					37 "$eth1"	/// Halfsiblings
					38 "$eth2"	///
					39 "$eth3"	///
					40 "$eth4"	///
					41 "$eth5"	///
					43 "$eth1"	/// Uncles
					44 "$eth2"	/// 
					45 "$eth3"	/// 
					46 "$eth4"	///
					47 "$eth5"	///
					49 "$eth1"	/// Aunts
					50 "$eth2"	///
					51 "$eth3"	/// 
					52 "$eth4"	/// 
					53 "$eth5"	/// 
					55 "$eth1"	///  Cousins
					56 "$eth2"	///
					57 "$eth3"	///
					58 "$eth4"	/// 
					59 "$eth5"	/// 
			)  	
			gr save "$M/iact_4", replace
			gr export "$M/iact_4.png", replace	
	
	
*** DETACHED
grstyle init
grstyle set plain, grid horizontal compact
grstyle set color Okabe, select(6) : p#bar p#barline

bys eth: sum num_10_m if help == 1 // av number of cousins
est restore iact5_m
di _b[10.kin_cat_med#1.eth] * 6.472705 // White cousins
di _b[10.kin_cat_med#2.eth] * 8.868671 // Hisp cousins
di _b[10.kin_cat_med#3.eth] * 7.838631 // Black cousins -> twice as many Tightknit cousins in Black vs White families
di _b[10.kin_cat_med#4.eth] * 7.709898 // Asian cousins



bys eth: sum num_1_m if help == 1 // av number of fathers
est restore iact5_m
di _b[1.kin_cat_med#1.eth] * .8655914 // White fathers
di _b[1.kin_cat_med#2.eth] * .8876582 // Hisp  fathers
di _b[1.kin_cat_med#3.eth] * .8288509 // Black  fathers 
di _b[1.kin_cat_med#4.eth] * .883959 // Asian  fathers



bys eth: sum num_4_m if help == 1 // av number of sisters
est restore iact5_m
di _b[4.kin_cat_med#1.eth] * .7212572 // White sisters
di _b[4.kin_cat_med#2.eth] * .8623418 // Hisp  sisters
di _b[4.kin_cat_med#3.eth] * .9413203 // Black  sisters 
di _b[4.kin_cat_med#4.eth] * .7474403 // Asian  sisters

bys eth: sum num_6_m if help == 1 // av number of grandmothers
est restore iact5_m
di _b[6.kin_cat_med#1.eth] * .7440033 // White grandmothers
di _b[6.kin_cat_med#2.eth] * .8322785 // Hisp  grandmothers
di _b[6.kin_cat_med#3.eth] * .7555012 // Black  grandmothers 
di _b[6.kin_cat_med#4.eth] * .774744 // Asian  grandmothers


bys eth: sum num_9_m if help == 1 // av number of aunts
est restore iact5_m
di _b[9.kin_cat_med#1.eth] * 1.885029 // White grandmothers
di _b[9.kin_cat_med#2.eth] * 2.473101 // Hisp  grandmothers
di _b[9.kin_cat_med#3.eth] * 2.334963 // Black  grandmothers 
di _b[9.kin_cat_med#4.eth] * 2.136519 // Asian  grandmothers


*est restore iact_m
*margins kin_cat_m, dydx(eth) predict(outcome(1)) post


coefplot 	(iact5_m),							///
	recast(bar) barw(.7) drop(_cons) citop			///
	xtitle("Expected probability", size(small))  	///
	ysize(15) xsize(11)  							///
	xlabel(, labsize(vsmall)) 						///
	ylabel(, labsize(tiny))							///
	title("$cl5", size(small)) name(iact5, replace)	///
	groups(	1.kin_cat_med#?.eth = "Father" 			///
			2.kin_cat_med#?.eth = "Mother"			///
			3.kin_cat_med#?.eth = "Brothers"			///
			4.kin_cat_med#?.eth = "Sisters"			///
			5.kin_cat_med#?.eth = "Grandfathers"		///
			6.kin_cat_med#?.eth = "Grandmothers"		///
			7.kin_cat_med#?.eth = "Halfsiblings"		///
			8.kin_cat_med#?.eth = "Uncles"			///
			9.kin_cat_med#?.eth = "Aunts"				///
			10.kin_cat_med#?.eth = "Cousins")   		///
	ylabel(			1  "$eth1" 	/// Fathers
					2  "$eth2"	///
					3  "$eth3"	///
					4  "$eth4"	///
					5  "$eth5"	/// 
					7  "$eth1"	/// Mothers
					8  "$eth2"	/// 
					9  "$eth3"	/// 
					10 "$eth4"	///
					11 "$eth5"	///
					13 "$eth1"	/// Brothers
					14 "$eth2"	///
					15 "$eth3"	/// 
					16 "$eth4"	///
					17 "$eth5"	/// 
					19 "$eth1"	/// Sisters
					20 "$eth2"	///
					21 "$eth3"	///
					22 "$eth4"	/// 
					23 "$eth5"	///
					25 "$eth1"	/// Grandfathers
					26 "$eth2"	/// 
					27 "$eth3"	///
					28 "$eth4"	///
					29 "$eth5"	/// 
					31 "$eth1"	/// Grandmothers
					32 "$eth2"	///
					33 "$eth3"	/// 
					34 "$eth4"	/// 
					35 "$eth5"	/// 
					37 "$eth1"	/// Halfsiblings
					38 "$eth2"	///
					39 "$eth3"	///
					40 "$eth4"	///
					41 "$eth5"	///
					43 "$eth1"	/// Uncles
					44 "$eth2"	/// 
					45 "$eth3"	/// 
					46 "$eth4"	///
					47 "$eth5"	///
					49 "$eth1"	/// Aunts
					50 "$eth2"	///
					51 "$eth3"	/// 
					52 "$eth4"	/// 
					53 "$eth5"	/// 
					55 "$eth1"	///  Cousins
					56 "$eth2"	///
					57 "$eth3"	///
					58 "$eth4"	/// 
					59 "$eth5"	/// 
			)  	
			gr save "$M/iact_5", replace
			gr export "$M/iact_5.png", replace	
		
	
	
	
	
	
	
	
	
	
	
	
	
			   
foreach num of numlist 1/5 {	
logit clust`num' i.kin_cat_m##i.eth, or 
est store clust`num'_logit

	foreach kin of numlist 1/10 { // s = 6, m = 10, l = 16
	est restore clust`num'_logit
	margins `kin'.kin_cat_m#eth, post
	est store kin`num'`kin'_iact
}
}


grstyle init
grstyle set plain, grid horizontal compact
grstyle set color RdYlBu, n(10) select(1/10)
grstyle set color RdYlBu, n(10) select(1/10): p#bar
*grstyle set symbol o t s + sh 

global eth1 "Asian"
global eth2 "White"
global eth3 "Hispanic"
global eth4 "Black"
global eth5 "Other"
global eth6 "Missing"

coefplot 	(kin11_iact, label("Father"))   	///
			(kin12_iact, label("Mother"))		///
			(kin13_iact, label("Brother"))		///
			(kin14_iact, label("Sister"))		///
			(kin15_iact, label("Grandfather"))	///
			(kin16_iact, label("Grandmother"))	///
			(kin17_iact, label("Halfsibling"))	///
			(kin18_iact, label("Uncle"))		///
			(kin19_iact, label("Aunt"))			///
			(kin110_iact, label("Cousin"))		///
			, drop(_cons) xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) ///
			recast(bar) barw(.7) legend(off) ///
			xtitle("Predicted probability") citop ciopts(recast(rcap)) ///
			xlabel(, labsize(small)) ///
			ylabel(, labsize(vsmall)) ///
			ysize(14) xsize(11) ///
			ylabel(	1  "$eth1" 	/// Fathers
					2  "$eth2"	///
					3  "$eth3"	///
					4  "$eth4"	///
					5  "$eth5"	///
					6  "$eth6"	///
					7  "$eth1"	/// Mothers
					8  "$eth2"	///
					9  "$eth3"	///
					10 "$eth4"	///
					11 "$eth5"	///
					12 "$eth6"	///
					13 "$eth1"	/// Brothers
					14 "$eth2"	///
					15 "$eth3"	///
					16 "$eth4"	///
					17 "$eth5"	///
					18 "$eth6"	///
					19 "$eth1"	/// Sisters
					20 "$eth2"	///
					21 "$eth3"	///
					22 "$eth4"	///
					23 "$eth5"	///
					24 "$eth6"	///
					25 "$eth1"	/// Grandfathers
					26 "$eth2"	///
					27 "$eth3"	///
					28 "$eth4"	///
					29 "$eth5"	///
					30 "$eth6"	///
					31 "$eth1"	/// Grandmothers
					32 "$eth2"	///
					33 "$eth3"	///
					34 "$eth4"	///
					35 "$eth5"	///
					36 "$eth6"	///
					37 "$eth1"	/// Halfsiblings
					38 "$eth2"	/// 
					39 "$eth3"	///
					40 "$eth4"	///
					41 "$eth5"	///
					42 "$eth6"	///
					43 "$eth1"	/// Uncles
					44 "$eth2"	/// 
					45 "$eth3"	/// 
					46 "$eth4"	/// 
					47 "$eth5"	///
					48 "$eth6"	///
					49 "$eth1"	/// Aunts
					50 "$eth2"	///
					51 "$eth3"	///
					52 "$eth4"	///
					53 "$eth5"	///
					54 "$eth6"	///
					55 "$eth1"	/// Cousins
					56 "$eth2"	///
					57 "$eth3"	///
					58 "$eth4"	///
				    59 "$eth5"	///
					60 "$eth6"	///
			)	name(bars, replace)	
			


grstyle init
grstyle set plain, grid horizontal compact
grstyle set color, opacity(50): p#bar p#barline 

			
foreach x of numlist 1/1 {	
	
	*global mean = "mean(clust`x')"
	
coefplot 	(kin`x'1_iact,  asequation("Father")   		\ 		///
			 kin`x'2_iact,  asequation("Mother")		\ 		///
			 kin`x'3_iact,  asequation("Brother")		\ 		///
			 kin`x'4_iact,  asequation("Sister")		\ 		///
			 kin`x'5_iact,  asequation("Grandfather")	\ 		///
			 kin`x'6_iact,  asequation("Grandmother")	\ 		///
			 kin`x'7_iact,  asequation("Halfsibling")	\ 		///
			 kin`x'8_iact,  asequation("Uncle")			\ 		///
			 kin`x'9_iact,  asequation("Aunt")			\ 		///
			 kin`x'10_iact, asequation("Cousin")) 			///
			, drop(_cons) recast(bar) barw(.8) legend(off) 	///
			xline(sum(_mean(clust`x')), lwidth(thin) lcolor(gs10) lpattern(solid))		///
			ylabel( 1  "$eth1" 	/// Fathers
					2  "$eth2"	///
					3  "$eth3"	///
					4  "$eth4"	///
					5  "$eth5"	///
					6  "$eth6"	///
					8  "$eth1"	/// Mothers
					9  "$eth2"	///
					10 "$eth3"	///
					11 "$eth4"	///
					12 "$eth5"	///
					13 "$eth6"	///
					15 "$eth1"	/// Brothers
					16 "$eth2"	///
					17 "$eth3"	///
					18 "$eth4"	///
					19 "$eth5"	///
					20 "$eth6"	///
					22 "$eth1"	/// Sisters
					23 "$eth2"	///
					24 "$eth3"	///
					25 "$eth4"	///
					26 "$eth5"	///
					27 "$eth6"	///
					29 "$eth1"	/// Grandfathers
					30 "$eth2"	///
					31 "$eth3"	///
					32 "$eth4"	///
					33 "$eth5"	///
					34 "$eth6"	///
					36 "$eth1"	/// Grandmothers
					37 "$eth2"	///
					38 "$eth3"	///
					39 "$eth4"	///
					40 "$eth5"	///
					41 "$eth6"	///
					43 "$eth1"	/// Halfsiblings
					44 "$eth2"	/// 
					45 "$eth3"	///
					46 "$eth4"	///
					47 "$eth5"	///
					48 "$eth6"	///
					50 "$eth1"	/// Uncles
					51 "$eth2"	/// 
					52 "$eth3"	/// 
					53 "$eth4"	/// 
					54 "$eth5"	///
					55 "$eth6"	///
					57 "$eth1"	/// Aunts
					58 "$eth2"	///
					59 "$eth3"	///
					60 "$eth4"	///
					61 "$eth5"	///
					62 "$eth6"	///
					64 "$eth1"	/// Cousins
					65 "$eth2"	///
					66 "$eth3"	///
					67 "$eth4"	///
				    68 "$eth5"	///
					69 "$eth6")	///
			xtitle("Predicted probability") citop ciopts(recast(rspike)) ///
			xlabel(, labsize(small)) ///
			ylabel(, labsize(vsmall)) ///
			title("Class `x'") 			///
			ysize(14) xsize(11) ///
			name(iact_`x', replace)	
			gr save "$M/iact_`x'", replace
			gr export "$M/iact_`x'.png", replace

}


/*			
coefplot 	kin11_iact, bylabel("Father")   	|| ///
			kin12_iact, bylabel("Mother")		|| ///
			kin13_iact, bylabel("Brother")		|| ///
			kin14_iact, bylabel("Sister")		|| ///
			kin15_iact, bylabel("Grandfather")	|| ///
			kin16_iact, bylabel("Grandmother")	|| ///
			kin17_iact, bylabel("Halfsibling")	|| ///
			kin18_iact, bylabel("Uncle")		|| ///
			kin19_iact, bylabel("Aunt")			|| ///
			kin110_iact, bylabel("Cousin")		||  ///
			, drop(_cons) xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) ///
			recast(bar) barw(.7) legend(off) ///
			xtitle("Predicted probability") citop ciopts(recast(rcap)) ///
			xlabel(, labsize(small)) ysize(13) xsize(11) ///
			name(bars, replace)	
			*/
			
			gr export $M/classbykin.png, replace
			   
			   
			   
foreach num of numlist 1/5 {	
est restore clust`num'
margins kin_cat_l, dydx() predict(outcome(1)) post
est store cla1_`num'
}


coefplot (	cla1, asequation($cl1) \ ///
			iact2, asequation($cl2) \ ///
			iact3, asequation($cl3) \ ///
			iact4, asequation($cl4) \ ///
			iact5, asequation($cl5) \ ///
			), drop(_cons) xline(0) citop ciopts(recast(rcap)) ///
			title("Kin * Race-Ethnicity") name(iact, replace) 






marginsplot, 	horizontal ///
				recast(scatter)

				
* vielleicht mit by??
coefplot (	iact1, asequation($cl1) \ ///
			iact2, asequation($cl2) \ ///
			iact3, asequation($cl3) \ ///
			iact4, asequation($cl4) \ ///
			iact5, asequation($cl5) \ ///
			), drop(_cons) xline(0) citop ciopts(recast(rcap)) ///
			title("Kin * Race-Ethnicity") name(iact, replace) 


grstyle init
grstyle set plain, grid horizontal compact
grstyle set color Paired, n(5) select(1/5) 

coefplot (	iact1, asequation($cl1) \ ///
			iact2, asequation($cl2) \ ///
			iact3, asequation($cl3) \ ///
			iact4, asequation($cl4) \ ///
			iact5, asequation($cl5) \ ///
			), drop(_cons) xline(0) citop ciopts(recast(rcap)) ///
			title("Kin * Race-Ethnicity") name(iact, replace) 
			
			gr save $M/iact, replace
			gr export $M/iact.png, replace 

est restore	mlogit_l_race
margins, dydx(i.kin_cat_l) post
est store m1_race

est restore	mlogit_l_race
margins, dydx(i.kin_cat_l $controls) predict(outcome(2)) post
est store m2_race

est restore	mlogit_l_race
margins, dydx(i.kin_cat_l $controls) predict(outcome(3)) post
est store m3_race

est restore	mlogit_l_race
margins, dydx(i.kin_cat_l $controls) predict(outcome(4)) post
est store m4_race

est restore	mlogit_l_race
margins, dydx(i.kin_cat_l $controls) predict(outcome(5)) post
est store m5_race
	
	
coefplot 	(m1_race, label("$cl1")) 	///
			(m2_race, label("$cl2")) 	///
			(m3_race, label("$cl3")) 	///
			(m4_race, label("$cl4"))	///
			(m5_race, label("$cl5")),	///
	recast(scatter) drop(_cons) 								///
	xtitle("Effects on Probability")  							///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 		///
	///ysize(13) xsize(10) 										///
	title("Average Marginal Effects") name(covariates, replace)
	
		gr export $M/ame_cov_l.png, replace	
	
	
	
*** WITH ACCOUNTING FOR CLASSIFICATION UNCERTAINTY (does not work, so far)

* Re-transpose Q to account for classification uncertainty in SEM models


local L_11=lq[1] 
local L_12=lq[2] 
local L_13=lq[3] 
local L_21=lq[4] 
local L_22=lq[5] 
local L_23=lq[6]
local L_31=lq[7]
local L_32=lq[8]
local L_33=lq[9]
local L_41=lq[10]
local L_42=lq[11]
local L_43=lq[12]

* rename class from R to class_R
rename class class_r


* run SEM (with adding uncertainty to each of the classes W)
gsem(adv_lca clo_lca cmf_lca cnf_lca cnt_lca mon_lc <- ), logit lclass(class 4)

* posterior probabilities
estat lcgof 
predict classpost*, classposteriorpr

capture noisily gsem ///
(1:1.W <- _cons@`L_11')(1:2.W <- _cons@`L_12')(1:3.W <- _cons@`L_13') 	///
(2:1.W <- _cons@`L_21')(2:2.W <- _cons@`L_22')(2:3.W <- _cons@`L_23') 	///
(3:1.W <- _cons@`L_31')(3:2.W <- _cons@`L_32')(3:3.W <- _cons@`L_33') 	///
(4:1.W <- _cons@`L_41')(4:2.W <- _cons@`L_42')(4:3.W <- _cons@`L_43') 	///
(class <- i.kin_cat_small anc_age i.female i.eth), mlogit 	///
vce(robust) lclass(class 4) nocapslatent


capture noisily gsem ///
(W <- i.kin_cat_small anc_age i.female i.eth i.lfs_l ib2.edu_l), mlogit 	///
vce(robust) lclass(class 4) nocapslatent

	
	
		
*** Code chunks not used ***

* Store separate logistic regressions
foreach c of numlist 1/5 {
	logit cl`c' i.kin_cat_small $controls, or 
est store clust`c'

}

* Pred Probs for kin type on class membership
est restore mlogit_l_c
margins kin_cat_l, post
coefplot, recast(bar) barw(.7) drop(_cons) citop ciopts(recast(rcap)) xline(0)

est restore mlogitc
margins i.kin_cat_small, pwcompare(eff)



		
			
foreach num of numlist 1/5 {	
est restore clust`num'
margins, dydx(i.female i.race) post
est store Cluster`num'	
}


*lab def fem 1"Female" 0"Male", replace
*lab val female fem

coefplot Cluster1 || Cluster2 || Cluster3 || Cluster4 || Cluster5 , ///
	drop(_cons) xline(0) xtitle("Average Marginal Effects") ///
	name(binary, replace) ciopts(recast(rcap)) bycoefs bylab( ) ///
	groups(	1 = "$cl1" ///
			2 = "$cl2" ///
			3 = "$cl3" ///
			4 = "$cl4" ///
			5 = "$cl5", wrap(23) nobreak) 
	


est restore	mlogitc
coefplot, nolabel drop(_cons) keep(*:) omitted baselevels
	
	
est restore margins
coefplot, drop(_cons) keep(*:) xline(0) name(covariates, replace)

est restore margins
mplotoffset, offset(.25) horizontal recast(scatter)
marginsplot, horizontal recast(scatter) ///
				lab("$cl1" "$cl2" "$cl3" "$cl4" "$cl5") 
	
*** END OF CODE ***
*-------------------------------------------------------------------------------
