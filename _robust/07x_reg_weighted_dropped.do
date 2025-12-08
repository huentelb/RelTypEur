*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in the United States
* Bettina Hünteler
* 03.12.2024
* huenteler@wiso.uni-koeln.de

*** 07 REGRESSION ANALYSIS – MAIN: IMPUTED MISSING REL INDICATORS *** 

*-------------------------------------------------------------------------------


*** Working directories ***

global WD 	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses/graphs/241210/robust_dropped"

global IN 	"/Users/Bettina/Documents/datasets/KINMATRIX"
global OUT 	"/Users/Bettina/Documents/datasets/KINMATRIX"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses/graphs/241210/M11_median/lc5_output/ame_weighted"

*** stata settings
set more off, perm
set scheme white_tableau, perm 
set showbaselevels on



*******************************
*** SET UP
*******************************

use $WD/data/USA_robust.dta, clear



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
recode class (4=1)(5=2)(3=3)(2=4)(1=5)
	
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


* generate help-indicator to select one (first) entry for each ego (to not display observations)
bys anc_id: gen help = _n



*********************************
*** TABLE 1 - SAMPLE DESCRIPTION 
*********************************



*************************
*** MULTINOMIAL LOGIT ***
*************************


* kin_cat_l
mlogit class i.kin_cat_l i.fem, vce(cluster anc_id)
est store mlogit_l
outreg2 using $M/mlogit_l.doc, label stats(coef se) aster() ///
	adds(Pseudo R2, e(r2_p)) dec(2) replace nocons


	
	
* PREDICTED PROBABILITIES (kin cat large)

est restore mlogit_l
margins kin_cat_l 


* store results in table
matrix table = J(80,9,.)	// 80 combinations of kin (16) x class (5)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/predprobs_overall, sheet("predprobs") replace 


* cols for kincat (80x1)
matrix kincat = J(80,1,.)
matrix kincat = (	1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\	/// class1
					1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\	/// class2
					1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\	/// class3
					1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\	/// class4
					1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16)	// 	class5
					
				
* cols for class (80x1)
matrix class = J(80,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5) 	//	class5
				
* store predicted probs in pp
matrix pp = J(80,1,.)
forvalues i = 1/80 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(80,1,.)
forvalues i = 1/80 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(80,1,.)
forvalues i = 1/80 {
    matrix lb[`i', 1] = table[`i', 5]
}

* store average kin counts in kin
matrix kin = J(80,1,.)
forvalues k	= 1/16 {
    * Calculate the mean of kin == k
    sum num_`k' if help == 1
	scalar avg = r(mean)

	* Store the average in the matrix (column 1)
    matrix kin[`= 0  + `k'', 1] = avg
	matrix kin[`= 16 + `k'', 1] = avg
	matrix kin[`= 32 + `k'', 1] = avg
	matrix kin[`= 48 + `k'', 1] = avg
	matrix kin[`= 64 + `k'', 1] = avg

}

	
	
* predicted average number of kin in each class by race
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(80,1,.)
forvalues i = 1/80 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "eth.l" 					 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)


	
*** END OF CODE ***
*-------------------------------------------------------------------------------
