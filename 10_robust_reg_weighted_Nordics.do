*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in Europe
* Bettina Hünteler
* 24.04.2026
* bhuenteler@diw.de

*** 10 REGRESSION ANALYSIS – ROBUST: DISAGGREGATE NORDIC COUNTRIES *** 

*-------------------------------------------------------------------------------


*** Working directories ***

global WD 	"/Users/bhuenteler/Library/CloudStorage/OneDrive-DIWBerlin/projects/Kinmatrix/RelTypEur/analyses"

global IN 	"data"
global OUT 	"data"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"$WD/graphs/251104/M11_median/lc5_output/ame_weighted"

*** stata settings
set more off, perm
set scheme white_tableau, perm 
set showbaselevels on



*******************************
*** SET UP
*******************************

* store cluster labels in locals for labelling of graphs
global cl1 "Tight-knit"
global cl2 "Connected-but-autonomous"								
global cl3 "Disharmonious-but-supportive"								
global cl4 "Intimate-but-distant"
global cl5 "Detached"

global cl1b "Tight-knit"
global cl2b "Connected-but-""autonomous"								
global cl3b "Disharmonious-""but-supportive"								
global cl4b "Intimate-""but-distant"
global cl5b "Detached"


*********************************
*** TABLE 1 - SAMPLE DESCRIPTION 
*********************************

use $WD/data/lc_analytical.dta, clear

drop cntry	// drop grouped cntry variable

* Sort countries according to 'closeness' while keeping Nordic country together
recode cntry_orig 	(4=1 "IT")	///
					(9=2 "NL")	///
					(2=3 "DE")	///
					(3=4 "PL")	///
					(1=5 "UK")	///
					(5=6 "SE")	///
					(6=7 "DK")	///
					(7=8 "FI")	///
					(8=9 "NO"), gen(cntry) l(cntry_new)
					


* store list of relevant variables as globals
global sociodemo anc_age cntry_* fem edu_*
global kin num_*
global rel tra cnt_ clo sup mon cnf


	* gender diff for rel indicators
	preserve
	*keep if help == 1
	keep if kin_cat_l <= 4
	foreach x of varlist $rel {
		tab `x' female, col
		bys female: sum `x'
	}
	restore

	
* number of kin and socio-demographics for anchors TABLE 2

* socio-demographics: anchor-level
preserve
keep if help == 1
estpost sum $sociodemo $kin total_kin [aweight = dwe], det
	eststo tab1
restore	

* relationship indicators: dyad-level
estpost sum $rel [aweight = dwe], det
	eststo tab1b

	* save table
	cd $M
	esttab tab1 using des_Nordic.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1b using des_Nordic.rtf,	///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		append label 
	
	
	
* TABLE 1 by cntry
preserve
keep if help == 1
levelsof cntry, local(cntry)
foreach x of local cntry {
estpost sum $sociodemo $kin total_kin if cntry == `x' [aweight = dwe], det
	eststo tab1cntry_`x'
	}
restore	

foreach x of local cntry {
estpost sum $rel if cntry == `x' [aweight = dwe], det
	eststo tab1bcntry_`x'
	}

	* save table
	cd $M
	esttab tab1cntry_* using des_cntry_Nordic.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1bcntry_* using des_cntry_Nordic.rtf,	///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		append label 

		
	* test for significant differences by country
	* ANCHOR level
	* continuous vars
preserve
	keep if help == 1
	foreach x of varlist anc_age num_1 num_2 num_3 num_4 num_5 num_6 ///
		num_7 num_8 num_9 num_1* total_kin {
	
	di "`x'"
	qui: reg `x' i.cntry [pweight = dwe]
	margins cntry [pweight = dwe], pwcompare(groups)
	
	}	
	
	* categorical vars
	foreach x of varlist fem edu_1 edu_2 edu_3 {
	
	di "`x'"
	qui: logit `x' i.cntry [pweight = dwe]
	margins cntry [pweight = dwe], pwcompare(groups)
	
	}
restore	

	* DYAD level
	* categorical vars
	foreach x of varlist $rel {
	
	di "`x'"
	qui: logit `x' i.cntry_orig [pweight = dwe]
	margins cntry_orig [pweight = dwe], pwcompare(groups)
	
	}
	

	
	
	
*************************
*** MULTINOMIAL LOGIT ***
*************************

global controls "i.fem"


* kin_cat_l
mlogit class i.kin_cat_l [pweight = dwe], vce(cluster anc_id)
est store mlogit_l
outreg2 using $M/mlogit_l.doc, label stats(coef ci)  ///
	adds(Pseudo R2, e(r2_p)) dec(2) replace nocons

mlogit class i.kin_cat_l##ib3.cntry $controls [pweight = dwe], vce(cluster anc_id)
est store mlogit_l_cntry
outreg2 using $M/mlogit_l.doc, label stats(coef ci)  ///
	adds(Pseudo R2, e(r2_p)) dec(2) append nocons


	
****************
*** PLOT AME ***
****************


* graph style settings
grstyle init
grstyle set plain, grid 
grstyle set lpattern solid: bygraph
grstyle set margin "5pt 15pt 50pt 5pt": graph
grstyle set color Okabe, select(2/6)
grstyle set color Okabe, select(2/6): p#markline
grstyle set symbol o t s + sh 

		
* store results for each class in separate model for easier plotting
est restore	mlogit_l_cntry
margins [pweight = dwe], dydx(i.cntry) predict(outcome(1)) post pwcompare(groups)
est store m1

est restore	mlogit_l_cntry
margins [pweight = dwe], dydx(i.cntry) predict(outcome(2)) post pwcompare(groups)
est store m2

est restore	mlogit_l_cntryi
margins [pweight = dwe], dydx(i.cntry) predict(outcome(3)) post pwcompare(groups)
est store m3

est restore	mlogit_l_cntryi
margins [pweight = dwe], dydx(i.cntry) predict(outcome(4)) post pwcompare(groups)
est store m4

est restore	mlogit_l_cntryi
margins [pweight = dwe], dydx(i.cntry) predict(outcome(5)) post pwcompare(groups)
est store m5


est restore	mlogit_l_cntry
margins i.cntry [pweight = dwe], predict(outcome(1)) post pwcompare(groups)
est store m1_cntr

est restore	mlogit_l_cntry
margins i.cntry [pweight = dwe], predict(outcome(2)) post pwcompare(groups)
est store m2_cntr

est restore	mlogit_l_cntry
margins i.cntry [pweight = dwe], predict(outcome(3)) post pwcompare(groups)
est store m3_cntr

est restore	mlogit_l_cntry
margins i.cntry [pweight = dwe], predict(outcome(4)) post pwcompare(groups)
est store m4_cntr

est restore	mlogit_l_cntry
margins i.cntry [pweight = dwe], predict(outcome(5)) post pwcompare(groups)
est store m5_cntr


		
*** Plot AME of country only (recreate main plot)

grstyle init
grstyle set plain, grid 
grstyle set lpattern solid: major_grid
grstyle set color Okabe, select(2/6)
grstyle set color Okabe, select(2/6): p#markline
grstyle set symbol o t s + sh 										


		
coefplot 	m1, bylabel("$cl1b") nokey msymbol(o) ||	///
			m2, bylabel("$cl2b") nokey msymbol(t) ||	///
			m3, bylabel("$cl3b") nokey msymbol(s) ||	///
			m4, bylabel("$cl4b") nokey msymbol(+) ||	///
			m5, bylabel("$cl5b") nokey msymbol(sh) ||,	///
	recast(scatter) norecycle 									///
	drop(_cons *kin_cat_l) keep(*cntry) omitted baselevels		///
	xtitle("Effects on Probability") 							///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 		///
	///ysize(13) xsize(10) 										///
	byopts(row(1))												///
	msize(medlarge)												///
	name(covariates_base_nokin_sep, replace)	

		gr export $M/ame_cov_l_cntry_sep.png, replace
		gr export $M/ame_cov_l_cntry_sep.pdf, replace
		gr save $M/ame_cov_l_cntry_sep, replace	
	

	
	
	

	
*** END OF CODE ***
*-------------------------------------------------------------------------------
