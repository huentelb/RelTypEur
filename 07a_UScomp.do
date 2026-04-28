*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in Europe
* Bettina Hünteler
* 24.04.2026
* bhuenteler@diw.de

*** 07a COMPARISON TO US DATA *** 

*-------------------------------------------------------------------------------


*** Working directories ***

global WD 	"/Users/bhuenteler/Library/CloudStorage/OneDrive-DIWBerlin/projects/Kinmatrix/RelTypEur/analyses"

global IN 	"data"
global OUT 	"data"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"$WD/graphs/251104/M11_median/lc5_output/ame_weighted_NORgrp"

*** stata settings
set more off, perm
set scheme white_tableau, perm 
set showbaselevels on
graph set window fontface "Times New Roman"

version 19.5


*********************************
*** TABLE 1 - SAMPLE DESCRIPTION 
*********************************

use $WD/data/lc_analytical.dta, clear



append using $WD/data/lc_analytical_US.dta
	recode cntry(.=7)
		lab def cntry2 7 "US", add

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

	
* number of kin and socio-demographics for anchors TABLE 1

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
	esttab tab1 using des_US.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1b using des_US.rtf,	///
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
	esttab tab1cntry_* using des_cntry_US.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1bcntry_* using des_cntry_US.rtf,	///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		append label 

		
		
	* test for significant differences between European total and US
	
	gen region = inrange(cntry,1,6)
	
	* ANCHOR level
	* continuous vars
preserve
	keep if help == 1
	foreach x of varlist anc_age num_1 num_2 num_3 num_4 num_5 num_6 ///
		num_7 num_8 num_9 num_1* total_kin {
	
	di "`x'"
	qui: reg `x' i.region [pweight = dwe]
	margins [pweight = dwe], dydx(region) 
	
	}	
	
	* categorical vars
	foreach x of varlist fem edu_1 edu_2 edu_3 {
	
	di "`x'"
	qui: logit `x' i.region [pweight = dwe]
	margins region [pweight = dwe], pwcompare(groups)
	
	}
restore	

	* DYAD level
	* categorical vars
	foreach x of varlist $rel {
	
	di "`x'"
	qui: logit `x' i.region [pweight = dwe]
	margins [pweight = dwe], dydx(region) 
	
	}
	