*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in Europe
* Bettina Hünteler
* 18.11.2025
* bhuenteler@diw.de

*** 07 REGRESSION ANALYSIS – MAIN: IMPUTED MISSING REL INDICATORS *** 

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



*******************************
*** SET UP
*******************************

use $WD/data/EU_lc.dta, clear



*** VARIABLE LABELS ***


* label variables
lab var anc_age "Age"


* store cluster labels in locals for labelling of graphs
global cl1 "Tight-knit"
global cl2 "Connected-but-autonomous"								
global cl3 "Disharmonious-but-supportive"								
global cl4 "Intimate-but-distant"
global cl5 "Detached"


* recode classes so they match labels
recode class (5=1)(3=2)(2=3)(4=4)(1=5)
	
	lab def class 	1"$cl1"	///
					2"$cl2"	///
					3"$cl3"	///
					4"$cl4"	///
					5"$cl5", replace
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
tab cntry, gen(cntry_)
recode female(1=0)(2=1), gen(fem)
	lab var fem "Female"

* generate help-indicator to select one (first) entry for each ego (to not display observations)
bys anc_id: gen help = _n



* missing weights in n = 464 (0.45%) observations -> all med edu
* -> fill with cntry-sex-edu specific medium weight

egen w_1fm = mean(dwe) if cntry == 1 & fem == 1 & edu == 2	// cntry1 female
egen w_1mm = mean(dwe) if cntry == 1 & fem == 0 & edu == 2	// cntry1 male

egen w_2mm = mean(dwe) if cntry == 2 & fem == 0 & edu == 2	// cntry2 male

egen w_3fm = mean(dwe) if cntry == 3 & fem == 1 & edu == 2	// cntry3 female
egen w_3mm = mean(dwe) if cntry == 3 & fem == 0 & edu == 2	// cntry3 male

egen w_4fm = mean(dwe) if cntry == 4 & fem == 1 & edu == 2	// cntry4 female
egen w_4mm = mean(dwe) if cntry == 4 & fem == 0 & edu == 2	// cntry4 male

egen w_5fm = mean(dwe) if cntry == 5 & fem == 1 & edu == 2	// cntry5 female

egen w_6fm = mean(dwe) if cntry == 6 & fem == 1 & edu == 2	// cntry6 female
egen w_6mm = mean(dwe) if cntry == 6 & fem == 0 & edu == 2	// cntry6 male

replace dwe = w_1fm if cntry == 1 & fem == 1 & edu == 2 & dwe == .	// cntry1 female
replace dwe = w_1mm if cntry == 1 & fem == 0 & edu == 2 & dwe == .	// cntry1 male
                               
replace dwe = w_2mm if cntry == 2 & fem == 0 & edu == 2 & dwe == .	// cntry2 male
                               
replace dwe = w_3fm if cntry == 3 & fem == 1 & edu == 2 & dwe == .	// cntry3 female
replace dwe = w_3mm if cntry == 3 & fem == 0 & edu == 2 & dwe == .	// cntry3 male
                               
replace dwe = w_4fm if cntry == 4 & fem == 1 & edu == 2 & dwe == .	// cntry4 female
replace dwe = w_4mm if cntry == 4 & fem == 0 & edu == 2 & dwe == .	// cntry4 male
                               
replace dwe = w_5fm if cntry == 5 & fem == 1 & edu == 2 & dwe == .	// cntry5 female
                               
replace dwe = w_6fm if cntry == 6 & fem == 1 & edu == 2 & dwe == .	// cntry6 female
replace dwe = w_6mm if cntry == 6 & fem == 0 & edu == 2 & dwe == .	// cntry6 male




*** Group Nordic countries because of small per-country-sample size
gen cntry_orig = cntry
	lab val cntry_orig cntry

* Sort countries according to 'closeness'
recode cntry (4=1)(9=2)(2=3)(3=4)(1=5)(5/8=6)
	lab def cntry2 	1"IT"	///
					2"NL"	///
					3"DE"	///
					4"PL"	///
					5"UK"	///
					6"Nor"	, replace
	lab val cntry cntry2


save $WD/data/lc_analytical.dta, replace




*********************************
*** TABLE 2 - SAMPLE DESCRIPTION 
*********************************

use $WD/data/lc_analytical.dta, clear

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
	esttab tab1 using des.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1b using des.rtf,	///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		append label 
	
	
	
* TABLE 2 by cntry
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
	esttab tab1cntry_* using des_cntry.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean))) ///
		replace label ///
		title(Description of sample) 
	esttab tab1bcntry_* using des_cntry.rtf,	///
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
	qui: logit `x' i.cntry [pweight = dwe]
	margins cntry [pweight = dwe], pwcompare(groups)
	
	}
	
	
* TABLE 2 by class
preserve
keep if help == 1
levelsof class, local(class)
foreach x of local class {
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

foreach x of local class {
estpost sum $kin if class == `x' [aweight = dwe], det
	eststo tab1clust_`x'
	}
	
	* save table
	cd $M
	esttab tab1clust_* using des_clust.rtf, ///
		cells(mean(fmt(%12.2fc) l(Mean)) sd(fmt(%12.2fc) l("(SD)") par)) ///
		mtitle("$cl1" "$cl2" "$cl3" "$cl4" "$cl5") replace label ///
		title(Description of sample by cluster) 
		
		
		
* TABLE 2 by gender
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


use $WD/data/lc_analytical.dta, clear

global controls "i.fem"


* kin_cat_l
mlogit class i.kin_cat_l [pweight = dwe], vce(cluster anc_id)
est store mlogit_l
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() ///
	adds(Pseudo R2, e(r2_p)) dec(2) replace nocons

mlogit class i.kin_cat_l##ib3.cntry $controls [pweight = dwe], vce(cluster anc_id)
est store mlogit_l_cntry
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
est restore	mlogit_l_cntry
margins [pweight = dwe], dydx(i.kin_cat_l i.cntry $controls) predict(outcome(1)) post
est store m1

est restore	mlogit_l_cntry
margins [pweight = dwe], dydx(i.kin_cat_l i.cntry $controls) predict(outcome(2)) post
est store m2

est restore	mlogit_l_cntry
margins [pweight = dwe], dydx(i.kin_cat_l  i.cntry $controls) predict(outcome(3)) post
est store m3

est restore	mlogit_l_cntry
margins [pweight = dwe], dydx(i.kin_cat_l  i.cntry $controls) predict(outcome(4)) post
est store m4

est restore	mlogit_l_cntry
margins [pweight = dwe], dydx(i.kin_cat_l  i.cntry $controls) predict(outcome(5)) post
est store m5



*** 1. Plot AME with all covariates

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) 												///
	drop(_cons) 													///
	xtitle("Effects on Probability")  								///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	legend(title("Class", size(3)))									///
	msize(medlarge)													///
	title("Average Marginal Effects", size(4)) 						///
	note("Missing imputed with 0 - 'detached'" 						///
		"N = `e(N)' observations", size(2))							///
	name(covariates, replace)
	
		gr export $M/ame_cov_l.png, replace
		gr export $M/ame_cov_l.pdf, replace
		gr save $M/ame_cov_l, replace


		
*** 2. Plot AME with all covariates + base levels (e.g., female = 0)

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) 												///
	drop(_cons) keep(*:) omitted baselevels							///
	xtitle("Effects on Probability") 								///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	legend(title("Class", size(3)))									///
	msize(medlarge)													///
	title("Average Marginal Effects", size(4)) 						///
	note("Missing imputed with 0 - 'detached'" 						///
		"N = `e(N)' observations", size(2))							///
	name(covariates_base, replace)
		
		gr export $M/ame_cov_l_base.png, replace
		gr export $M/ame_cov_l_base.pdf, replace
		gr save $M/ame_cov_l_base, replace

		
*** 3. Plot AME without kin + base levels (e.g., female = 0)		

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) 												///
	drop(_cons *kin_cat_l) keep(*:) omitted baselevels				///
	xtitle("Effects on Probability") 								///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	legend(title("Class", size(3)))									///
	msize(medlarge)													///
	title("Average Marginal Effects", size(4)) 						///
	note("Missing imputed with 0 - 'detached'" 						///
		"N = `e(N)' observations", size(2))							///
	name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_l_base_nokin.png, replace
		gr export $M/ame_cov_l_base_nokin.pdf, replace
		gr save $M/ame_cov_l_base_nokin, replace

		
*** 4. Plot AME of country only

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) 												///
	drop(_cons *kin_cat_l) keep(*cntry) omitted baselevels			///
	xtitle("Effects on Probability") 								///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	legend(title("Class", size(3)))									///
	msize(medlarge)													///
	title("Average Marginal Effects", size(4)) 						///
	note("Missing imputed with 0 - 'detached'" 						///
		"N = `e(N)' observations", size(2))							///
	name(covariates_base_nokin, replace)
		
		gr export $M/ame_cov_l_cntry.png, replace
		gr export $M/ame_cov_l_cntry.pdf, replace
		gr save $M/ame_cov_l_cntry, replace

		
*** 5. Plot AME of kin category only

coefplot 	(m1, label("$cl1")) 	///
			(m2, label("$cl2")) 	///
			(m3, label("$cl3")) 	///
			(m4, label("$cl4"))		///
			(m5, label("$cl5")),	///
	recast(scatter) 												///
	drop(_cons) keep(*kin_cat_l) omitted baselevels					///
	xtitle("Effects on Probability") 								///
	xline(0, lwidth(thin) lcolor(gs10) lpattern(solid)) 			///
	///ysize(13) xsize(10) 											///
	legend(title("Class", size(3)))									///
	msize(medlarge)													///
	title("Average Marginal Effects", size(4)) 						///
	note("Missing imputed with 0 - 'detached'" 						///
		"N = `e(N)' observations", size(2))							///
	name(covariates_base_kin, replace)
		
		gr export $M/ame_cov_l_base_kin.png, replace
		gr export $M/ame_cov_l_base_kin.pdf, replace
		gr save $M/ame_cov_l_base_kin, replace
		

	
	
	

	
*** END OF CODE ***
*-------------------------------------------------------------------------------
