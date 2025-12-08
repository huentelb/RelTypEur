*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in the United States
* Bettina Hünteler
* 23.02.2024
* huenteler@wiso.uni-koeln.de

*** 07 REGRESSION ANALYSIS – ROBUST: MISSING RELATIONSHIP INDICATORS IMPUTED *** 

*-------------------------------------------------------------------------------


*** Set (up) working directories ***

*** folders
global WD 	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses"

global IN 	"/Users/Bettina/Documents/datasets/KINMATRIX"
global OUT 	"/Users/Bettina/Documents/datasets/KINMATRIX"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses/graphs/241114/robust_mis_items/M11_median/lc5_output/ame_weighted"

*** preparations/settings
set more off, perm
set scheme white_tableau, perm //  rainbow
set showbaselevels on



*******************************
*** TABLE 1 
*******************************

use $WD/data/USA_lc_robust_mis_items.dta, clear




lab var anc_age "Age"
lab var eth "Race-Ethnicity"

/*lab def kin 1"Parents" 			///
			2"Siblings" 		///
			3"Grandparents" 	///
			4"Halfsiblings" 	///
			5"Uncles and Aunts"	///
			6"Cousins", replace
lab val kin_cat_ kin */


* recode classes so they match labels

* store cluster labels in locals
global cl1 "Tight-knit"
global cl2 "Connected-but-autonomous"								
global cl3 "Ambivalent"								
global cl4 "Intimate-but-distant"
global cl5 "Detached"


recode class (5=1)(3=2)(2=3)(4=4)(1=5)
lab def class 	1"$cl1"	///
				2"$cl2"	///
				3"$cl3"	///
				4"$cl4"	///
				5"$cl5", replace
lab val class class

*** some modifications for TABLES


*** NUMBER OF KIN VARIABLES ***




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


* Number of kin for each anchor by kin type (med)
bys anc_id kin_cat_m: gen kin_m_n = _N

* store number of kin type in new var for each kin type (num_1 to num_10)
foreach kin of numlist 1/10 {
	gen num_`kin'_m = kin_m_n if inlist(kin_cat_m, `kin')
	* Step 1: Sort the data by anc_id 
	sort anc_id

	* Step 2: Carry forward the value of num_1 within each anc_id
	bysort anc_id: replace num_`kin'_m = num_`kin'_m[_n-1] if missing(num_`kin'_m)

		
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







*******************************
*** DESCRIPTIVE TABLES ***
*******************************

order anc_id kin_cat_l kin_n num_*


tab edu, gen(edu_)
tab eth, gen(eth_)
recode female(1=0)(2=1), gen(fem)

	
global sociodemo anc_age eth_* fem edu_*
global kin num_*
global rel tra cnt clo sup mon cnf


bys anc_id: gen help = _n


* gender diff for rel indicators
preserve
*keep if help == 1
keep if kin_cat_l <= 4
foreach x of varlist $rel {
	tab `x' female, col
}
restore

* gender diff for rel indicators
preserve
*keep if help == 1
keep if kin_cat_l <= 4
foreach x of varlist $rel {
	bys female: sum `x'
}
restore



* number of kin and socio-demographics for anchors TABLE 1
preserve
keep if help == 1
estpost sum $sociodemo $kin [aweight = dwe], det
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
foreach x of numlist 1/4 {
estpost sum $sociodemo $kin total_kin $rel if eth == `x' [aweight = dwe], det
	eststo tab1eth_`x'
	}
restore	

	* save table
	cd $M
	esttab tab1eth_* using des_eth.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 


* TABLE 1 by class
preserve
keep if help == 1
foreach x of numlist 1/4 {
estpost sum $sociodemo $kin total_kin [aweight = dwe], det
	eststo tab1clust_`x'
	}
restore	

	* save table
	cd $M
	esttab tab1clust_* using des_clust_det.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean)) sd(fmt(%12.2fc) l("(SD)") par)) ///
		mtitle("$cl1" "$cl2" "$cl3" "$cl4" "$cl5") replace label ///
		title(Description of sample by cluster) 

foreach x of numlist 1/4 {
estpost sum $kin if class == `x' [aweight = dwe], det
	eststo tab1clust_`x'
	}
	
	* save table
	cd $M
	esttab tab1clust_* using des_clust.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean)) sd(fmt(%12.2fc) l("(SD)") par)) ///
		mtitle("$cl1" "$cl2" "$cl3" "$cl4" "$cl5") replace label ///
		title(Description of sample by cluster) 
		
		
		
* TABLE 1 by gender
preserve
keep if help == 1
foreach x of numlist 1 2 {
estpost sum $sociodemo $kin total_kin if female == `x' [aweight = dwe], det
	eststo tab1gnd_`x'
	}
restore	
*	 relationship indicators over all relationships (not just first row!)
	foreach x of numlist 1 2 {
	estpost sum $rel if female == `x' [aweight = dwe], det
	eststo tab1gnd_rel_`x'
	}
	

	
	* save table
	cd $M
	esttab tab1gnd_* using des_gnd_weighted.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		mtitle("Male" "Female") replace label ///
		title(Description of sample by gender) 
		
	esttab tab1gnd_rel_* using des_gnd_weighted.rtf, append

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







*************************
*** MULTINOMIAL LOGIT ***
*************************


save $WD/data/lc_analytical_robust_mis_items.dta, replace
use $WD/data/lc_analytical_robust_mis_items.dta, clear

global controls "i.female"


* kin_cat_l
mlogit class i.kin_cat_l [pweight = dwe], vce(cluster anc_id)
est store mlogit_l
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() dec(2) replace nocons

mlogit class i.kin_cat_l##i.eth $controls [pweight = dwe], vce(cluster anc_id)
est store mlogit_l_race
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() dec(2) append nocons



****************
*** PLOT AME ***
****************

*** GRAPH STYLE


grstyle init
grstyle set plain, grid horizontal
grstyle set color Okabe, select(2/6)
grstyle set color Okabe, select(2/6):  p#markline
grstyle set symbol o t s + sh 


		
** Store results for each class in separate model for easier plotting
est restore	mlogit_l_race
margins [pweight = dwe], dydx(i.kin_cat_l i.eth $controls) predict(outcome(1)) post
est store m1

est restore	mlogit_l_race
margins [pweight = dwe], dydx(i.kin_cat_l i.eth $controls) predict(outcome(2)) post
est store m2

est restore	mlogit_l_race
margins [pweight = dwe], dydx(i.kin_cat_l  i.eth $controls) predict(outcome(3)) post
est store m3

est restore	mlogit_l_race
margins [pweight = dwe], dydx(i.kin_cat_l  i.eth $controls) predict(outcome(4)) post
est store m4

est restore	mlogit_l_race
margins [pweight = dwe], dydx(i.kin_cat_l  i.eth $controls) predict(outcome(5)) post
est store m5


* 1. Plot AME with all covariates
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


* 2. Plot AME with all covariates + base levels (e.g., female = 0)
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

		
* 3. Plot AME without kin + base levels (e.g., female = 0)		
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
		
* 4. Plot AME of race/ethnicity	only
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

* 5. Plot AME of kin category only
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
		

	
	
	

	
*** END OF CODE ***
*-------------------------------------------------------------------------------
