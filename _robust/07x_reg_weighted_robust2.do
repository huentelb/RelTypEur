*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in the United States
* Bettina Hünteler
* 03.12.2024
* huenteler@wiso.uni-koeln.de

*** 07 REGRESSION ANALYSIS *** 

*-------------------------------------------------------------------------------


*** Working directories ***

global WD 	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses"

global IN 	"/Users/Bettina/Documents/datasets/KINMATRIX"
global OUT 	"/Users/Bettina/Documents/datasets/KINMATRIX"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses/graphs/241114/robust_all_FIML/M11_median/lc4_output/ame_weighted"

*** stata settings
set more off, perm
set scheme white_tableau, perm 
set showbaselevels on



*******************************
*** SET UP
*******************************

use $WD/data/USA_lc_robust2.dta, clear



*** VARIABLE LABELS ***


* label variables
lab var anc_age "Age"
lab var eth "Race-Ethnicity"


* store cluster labels in locals for labelling of graphs
global cl1 "Tight-knit"
global cl2 "Detached"								
global cl3 "Ambivalent"								
global cl4 "Intimate-but-distant"


* recode classes so they match labels
recode class (3=1)(2=2)(4=3)(1=4)
	
	lab def class 	1"$cl1"	///
					2"$cl2"	///
					3"$cl3"	///
					4"$cl4"	, replace
	lab val class class






*** NUMBER OF KIN VARIABLES ***


* number of kin for each anchor by kin type (KIN CAT LARGE)
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

* Step 3: Replace missing entries with 0 (for correct means)
recode num_*(.=0)



* Number of kin for each anchor by kin type (KIN CAT MEDIUM)
bys anc_id kin_cat_m: gen kin_n_m = _N

* store number of kin type in new var for each kin type (num_1 to num_10)
foreach kin of numlist 1/10 {
	gen num_m_`kin' = kin_n_m if inlist(kin_cat_m, `kin')
	
	* Step 1: Sort the data by anc_id 
	sort anc_id

	* Step 2: Carry forward the value of num_m_kin within each anc_id
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n-1] if missing(num_m_`kin')

	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	bysort anc_id: replace num_m_`kin' = num_m_`kin'[_n+1] if missing(num_m_`kin')
	*recode num_`kin'(.=0)
}

* Step 3: Replace missing entries with 0 (for correct means)
recode num_m_*(.=0)


order anc_id kin_cat_l kin_n num_*



*** AUXILIARY VARIABLES ***


* generate binary indicators for categorical variables (for tables)
tab edu, gen(edu_)
tab eth, gen(eth_)
recode female(1=0)(2=1), gen(fem)


* generate help-indicator to select one (first) entry for each ego (to not display observations)
bys anc_id: gen help = _n

save $WD/data/lc_analytical_robust2.dta, replace




*********************************
*** TABLE 1 - SAMPLE DESCRIPTION 
*********************************

* store list of relevant variables as globals
global sociodemo anc_age eth_* fem edu_*
global kin num_*
global rel tra cnt clo sup mon cnf 


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
preserve
keep if help == 1
estpost sum $sociodemo $kin total_kin [aweight = dwe], det
	eststo tab1
restore	

estpost sum $rel [aweight = dwe], det
	eststo tab1b

	* save table
	cd $M
	esttab tab1 using des.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1b using des.rtf,	///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		append label 
		
* TABLE 1 by race
preserve
keep if help == 1
foreach x of numlist 1/4 {
estpost sum $sociodemo $kin total_kin if eth == `x' [aweight = dwe], det
	eststo tab1eth_`x'
	}
restore	

foreach x of numlist 1/4 {
estpost sum $rel if eth == `x' [aweight = dwe], det
	eststo tab1beth_`x'
	}

	* save table
	cd $M
	esttab tab1eth_* using des_eth.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1beth_* using des_eth.rtf,	///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		append label 

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






*************************
*** MULTINOMIAL LOGIT ***
*************************


use $WD/data/lc_analytical_robust2.dta, clear

global controls "i.female"


* kin_cat_l
mlogit class i.kin_cat_l [pweight = dwe], vce(cluster anc_id)
est store mlogit_l
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() ///
	adds(Pseudo R2, e(r2_p)) dec(2) replace nocons

mlogit class i.kin_cat_l##i.eth $controls [pweight = dwe], vce(cluster anc_id)
est store mlogit_l_race
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() ///
	adds(Pseudo R2, e(r2_p)) dec(2) append nocons



****************
*** PLOT AME ***
****************


* graph style settings
grstyle init
grstyle set plain, grid horizontal
grstyle set color Okabe, select(2/6)
grstyle set color Okabe, select(2/6):  p#markline
grstyle set symbol o t s + sh 


		
* store results for each class in separate model for easier plotting
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



*** 1. Plot AME with all covariates

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4")),	///
	recast(scatter) 										///
	drop(_cons) 											///
	xtitle("Effects on Probability")  						///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 	///
	///ysize(13) xsize(10) 									///
	legend(title("Class", size(3)))							///
	title("Average Marginal Effects (FIML?)", size(4)) 				///
	name(covariates, replace)
	
		gr export $M/ame_cov_l.png, replace
		gr export $M/ame_cov_l.pdf, replace
		gr save $M/ame_cov_l, replace


		
*** 2. Plot AME with all covariates + base levels (e.g., female = 0)

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4")),	///
	recast(scatter) 										///
	drop(_cons) keep(*:) omitted baselevels					///
	xtitle("Effects on Probability") 						///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 	///
	///ysize(13) xsize(10) 									///
	legend(title("Class", size(3)))							///
	title("Average Marginal Effects (FIML?)", size(4)) 				///
	name(covariates_base, replace)
		
		gr export $M/ame_cov_l_base.png, replace
		gr export $M/ame_cov_l_base.pdf, replace
		gr save $M/ame_cov_l_base, replace

		
*** 3. Plot AME without kin + base levels (e.g., female = 0)		

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4")),	///
	recast(scatter) 											///
	drop(_cons *kin_cat_l) keep(*:) omitted baselevels			///
	xtitle("Effects on Probability") 							///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 		///
	///ysize(13) xsize(10) 										///
	legend(title("Class", size(3)))								///
	title("Average Marginal Effects (FIML?)", size(4)) 					///
	name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_l_base_nokin.png, replace
		gr export $M/ame_cov_l_base_nokin.pdf, replace
		gr save $M/ame_cov_l_base_nokin, replace

		
*** 4. Plot AME of race/ethnicity only

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4")),	///
	recast(scatter) 										///
	drop(_cons *kin_cat_l) keep(*eth) omitted baselevels	///
	xtitle("Effects on Probability") 						///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 	///
	///ysize(13) xsize(10) 									///
	legend(title("Class", size(3)))							///
	title("Average Marginal Effects (FIML?)", size(4)) 				///
	name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_l_eth.png, replace
		gr export $M/ame_cov_l_eth.pdf, replace
		gr save $M/ame_cov_l_eth, replace

		
*** 5. Plot AME of kin category only

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4")),	///
	recast(scatter) 										///
	drop(_cons) keep(*kin_cat_l) omitted baselevels			///
	xtitle("Effects on Probability") 						///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 	///
	///ysize(13) xsize(10) 									///
	legend(title("Class", size(3)))							///
	title("Average Marginal Effects", size(4)) 				///
	name(covariates_base_kin, replace)
		
		gr export $M/ame_cov_l_base_kin.png, replace
		gr export $M/ame_cov_l_base_kin.pdf, replace
		gr save $M/ame_cov_l_base_kin, replace
		

	
	
	

	
*** END OF CODE ***
*-------------------------------------------------------------------------------
