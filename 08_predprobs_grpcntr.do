*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in Europe
* Bettina Hünteler
* 18.11.2025
* bhuenteler@diw.de

*** 08 PREDICTED PROBABILITIES – MAIN: IMPUTED MISSING REL INDICATORS *** 
*** 

*-------------------------------------------------------------------------------


*** Working directories ***

/*
global WD 	"/Users/Bettina/Library/CloudStorage/OneDrive-DIWBerlin/projects/Kinmatrix/RelTypEur/analyses/"

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
*/



*******************************
*** SET UP
*******************************

/*
use $WD/data/lc_analytical.dta, clear
mlogit class i.kin_cat_l##i.cntry $controls [pweight = dwe], vce(cluster anc_id)
est store mlogit_l_cntry
*/


* store labels in locals for labelling of graphs

* cntry
global cntry1 "IT"
global cntry2 "NL"
global cntry3 "DE"
global cntry4 "PL"
global cntry5 "UK"
global cntry6 "Nor"



* cluster
global cl1 "Tight-knit"
global cl2 "Connected-but-autonomous"								
global cl3 "Disharmonious-but-supportive"								
global cl4 "Intimate-but-distant"
global cl5 "Detached"






************************************************
***		 OVERALL PREDICTED PROBABILITIES	 ***
************************************************

* i.e., not distinguished by cntry/cntrynicity
* base for FIGURE 3


*** KIN CAT LARGE ***

* re-run main model 
est replay mlogit_l_cntry


*mlogit class i.kin_cat_l##i.cntry i.female [pweight = dwe], vce(cluster anc_id)
*est store mlogit_l_cntry


* PAIRWISE CONTRASTS for kin comparison (kin cat large)

* run margins with pwcompare for each cluster separately 
* and store results in matrix 'table_c'
forvalues c = 1/5 {
	
	est restore mlogit_l_cntry
	margins kin_cat_l [pweight = dwe],	///
		predict(outcome(`c'))	///
		pwcompare(pv groups) post
	est store iact1_l`c'	
	
	
	matrix table`c' = r(table_vs)

	* FOR TABLE
	* Store the pvalues from table_c (row 4, col x) in a matrix on diagonals (pval_kc)
	matrix pval`c' = J(16,16,.)
    matrix pval`c'[1,2]		= table`c'[4,1]		// kin1 vs kin2
	matrix pval`c'[1,3]		= table`c'[4,2]		// kin1 vs kin3
	matrix pval`c'[1,4]		= table`c'[4,3]		// kin1 vs kin4
	matrix pval`c'[1,5]		= table`c'[4,4]		// kin1 vs kin5
	matrix pval`c'[1,6]		= table`c'[4,5]		// kin1 vs kin6
	matrix pval`c'[1,7]		= table`c'[4,6]		// kin1 vs kin7
	matrix pval`c'[1,8]		= table`c'[4,7]		// kin1 vs kin8
	matrix pval`c'[1,9]		= table`c'[4,8]		// kin1 vs kin9
	matrix pval`c'[1,10]	= table`c'[4,9]		// kin1 vs kin10
	matrix pval`c'[1,11]	= table`c'[4,10]	// kin1 vs kin11
	matrix pval`c'[1,12]	= table`c'[4,11]	// kin1 vs kin12
	matrix pval`c'[1,13]	= table`c'[4,12]	// kin1 vs kin13
	matrix pval`c'[1,14]	= table`c'[4,13]	// kin1 vs kin14
	matrix pval`c'[1,15]	= table`c'[4,14]	// kin1 vs kin15
	matrix pval`c'[1,16]	= table`c'[4,15]	// kin1 vs kin16
	
	matrix pval`c'[2,3]		= table`c'[4,16]	// kin2 vs kin3
	matrix pval`c'[2,4]		= table`c'[4,17]	// kin2 vs kin4
	matrix pval`c'[2,5]		= table`c'[4,18]	// kin2 vs kin5
	matrix pval`c'[2,6]		= table`c'[4,19]	// kin2 vs kin6
	matrix pval`c'[2,7]		= table`c'[4,20]	// kin2 vs kin7
	matrix pval`c'[2,8]		= table`c'[4,21]	// kin2 vs kin8
	matrix pval`c'[2,9]		= table`c'[4,22]	// kin2 vs kin9
	matrix pval`c'[2,10]	= table`c'[4,23]	// kin2 vs kin10
	matrix pval`c'[2,11]	= table`c'[4,24]	// kin2 vs kin11
	matrix pval`c'[2,12]	= table`c'[4,25]	// kin2 vs kin12
	matrix pval`c'[2,13]	= table`c'[4,26]	// kin2 vs kin13
	matrix pval`c'[2,14]	= table`c'[4,27]	// kin2 vs kin14
	matrix pval`c'[2,15]	= table`c'[4,28]	// kin2 vs kin15
	matrix pval`c'[2,16]	= table`c'[4,29]	// kin2 vs kin16

	matrix pval`c'[3,4]		= table`c'[4,30]	// kin3 vs kin4
	matrix pval`c'[3,5]		= table`c'[4,31]	// kin3 vs kin5
	matrix pval`c'[3,6]		= table`c'[4,32]	// kin3 vs kin6
	matrix pval`c'[3,7]		= table`c'[4,33]	// kin3 vs kin7
	matrix pval`c'[3,8]		= table`c'[4,34]	// kin3 vs kin8
	matrix pval`c'[3,9]		= table`c'[4,35]	// kin3 vs kin9
	matrix pval`c'[3,10]	= table`c'[4,36]	// kin3 vs kin10
	matrix pval`c'[3,11]	= table`c'[4,37]	// kin3 vs kin11
	matrix pval`c'[3,12]	= table`c'[4,38]	// kin3 vs kin12
	matrix pval`c'[3,13]	= table`c'[4,39]	// kin3 vs kin13
	matrix pval`c'[3,14]	= table`c'[4,40]	// kin3 vs kin14
	matrix pval`c'[3,15]	= table`c'[4,41]	// kin3 vs kin15
	matrix pval`c'[3,16]	= table`c'[4,42]	// kin3 vs kin16
	
	matrix pval`c'[4,5]		= table`c'[4,43]	// kin4 vs kin5
	matrix pval`c'[4,6]		= table`c'[4,44]	// kin4 vs kin6
	matrix pval`c'[4,7]		= table`c'[4,45]	// kin4 vs kin7
	matrix pval`c'[4,8]		= table`c'[4,46]	// kin4 vs kin8
	matrix pval`c'[4,9]		= table`c'[4,47]	// kin4 vs kin9
	matrix pval`c'[4,10]	= table`c'[4,48]	// kin4 vs kin10
	matrix pval`c'[4,11]	= table`c'[4,49]	// kin4 vs kin11
	matrix pval`c'[4,12]	= table`c'[4,50]	// kin4 vs kin12
	matrix pval`c'[4,13]	= table`c'[4,51]	// kin4 vs kin13
	matrix pval`c'[4,14]	= table`c'[4,52]	// kin4 vs kin14
	matrix pval`c'[4,15]	= table`c'[4,53]	// kin4 vs kin15
	matrix pval`c'[4,16]	= table`c'[4,54]	// kin4 vs kin16
	
	matrix pval`c'[5,6]		= table`c'[4,55]	// kin5 vs kin6
	matrix pval`c'[5,7]		= table`c'[4,56]	// kin5 vs kin7
	matrix pval`c'[5,8]		= table`c'[4,57]	// kin5 vs kin8
	matrix pval`c'[5,9]		= table`c'[4,58]	// kin5 vs kin9
	matrix pval`c'[5,10]	= table`c'[4,59]	// kin5 vs kin10
	matrix pval`c'[5,11]	= table`c'[4,60]	// kin5 vs kin11
	matrix pval`c'[5,12]	= table`c'[4,61]	// kin5 vs kin12
	matrix pval`c'[5,13]	= table`c'[4,62]	// kin5 vs kin13
	matrix pval`c'[5,14]	= table`c'[4,63]	// kin5 vs kin14
	matrix pval`c'[5,15]	= table`c'[4,64]	// kin5 vs kin15
	matrix pval`c'[5,16]	= table`c'[4,65]	// kin5 vs kin16
	
	matrix pval`c'[6,7]		= table`c'[4,57]	// kin6 vs kin7
	matrix pval`c'[6,8]		= table`c'[4,57]	// kin6 vs kin8
	matrix pval`c'[6,9]		= table`c'[4,58]	// kin6 vs kin9
	matrix pval`c'[6,10]	= table`c'[4,59]	// kin6 vs kin10
	matrix pval`c'[6,11]	= table`c'[4,60]	// kin6 vs kin11
	matrix pval`c'[6,12]	= table`c'[4,61]	// kin6 vs kin12
	matrix pval`c'[6,13]	= table`c'[4,62]	// kin6 vs kin13
	matrix pval`c'[6,14]	= table`c'[4,63]	// kin6 vs kin14
	matrix pval`c'[6,15]	= table`c'[4,64]	// kin6 vs kin15
	matrix pval`c'[6,16]	= table`c'[4,65]	// kin6 vs kin16

	matrix pval`c'[7,8]		= table`c'[4,66]	// kin7 vs kin8
	matrix pval`c'[7,9]		= table`c'[4,67]	// kin7 vs kin9
	matrix pval`c'[7,10]	= table`c'[4,68]	// kin7 vs kin10
	matrix pval`c'[7,11]	= table`c'[4,69]	// kin7 vs kin11
	matrix pval`c'[7,12]	= table`c'[4,70]	// kin7 vs kin12
	matrix pval`c'[7,13]	= table`c'[4,71]	// kin7 vs kin13
	matrix pval`c'[7,14]	= table`c'[4,72]	// kin7 vs kin14
	matrix pval`c'[7,15]	= table`c'[4,73]	// kin7 vs kin15
	matrix pval`c'[7,16]	= table`c'[4,74]	// kin7 vs kin16
	
	matrix pval`c'[8,9]		= table`c'[4,75]	// kin8 vs kin9
	matrix pval`c'[8,10]	= table`c'[4,76]	// kin8 vs kin10
	matrix pval`c'[8,11]	= table`c'[4,77]	// kin8 vs kin11
	matrix pval`c'[8,12]	= table`c'[4,78]	// kin8 vs kin12
	matrix pval`c'[8,13]	= table`c'[4,79]	// kin8 vs kin13
	matrix pval`c'[8,14]	= table`c'[4,80]	// kin8 vs kin14
	matrix pval`c'[8,15]	= table`c'[4,81]	// kin8 vs kin15
	matrix pval`c'[8,16]	= table`c'[4,82]	// kin8 vs kin16

	matrix pval`c'[9,10]	= table`c'[4,83]	// kin9 vs kin10
	matrix pval`c'[9,11]	= table`c'[4,84]	// kin9 vs kin11
	matrix pval`c'[9,12]	= table`c'[4,85]	// kin9 vs kin12
	matrix pval`c'[9,13]	= table`c'[4,86]	// kin9 vs kin13
	matrix pval`c'[9,14]	= table`c'[4,87]	// kin9 vs kin14
	matrix pval`c'[9,15]	= table`c'[4,88]	// kin9 vs kin15
	matrix pval`c'[9,16]	= table`c'[4,89]	// kin9 vs kin16
	                                     
	matrix pval`c'[10,11]	= table`c'[4,90]	// kin10 vs kin11
	matrix pval`c'[10,12]	= table`c'[4,91]	// kin10 vs kin12
	matrix pval`c'[10,13]	= table`c'[4,92]	// kin10 vs kin13
	matrix pval`c'[10,14]	= table`c'[4,93]	// kin10 vs kin14
	matrix pval`c'[10,15]	= table`c'[4,94]	// kin10 vs kin15
	matrix pval`c'[10,16]	= table`c'[4,95]	// kin10 vs kin16
	
	matrix pval`c'[11,12]	= table`c'[4,96]	// kin11 vs kin12
	matrix pval`c'[11,13]	= table`c'[4,97]	// kin11 vs kin13
	matrix pval`c'[11,14]	= table`c'[4,98]	// kin11 vs kin14
	matrix pval`c'[11,15]	= table`c'[4,99]	// kin11 vs kin15
	matrix pval`c'[11,16]	= table`c'[4,100]	// kin11 vs kin16
	
	matrix pval`c'[12,13]	= table`c'[4,101]	// kin12 vs kin13
	matrix pval`c'[12,14]	= table`c'[4,102]	// kin12 vs kin14
	matrix pval`c'[12,15]	= table`c'[4,103]	// kin12 vs kin15
	matrix pval`c'[12,16]	= table`c'[4,104]	// kin12 vs kin16
	
	matrix pval`c'[13,14]	= table`c'[4,105]	// kin13 vs kin14
	matrix pval`c'[13,15]	= table`c'[4,106]	// kin13 vs kin15
	matrix pval`c'[13,16]	= table`c'[4,107]	// kin13 vs kin16
	
	matrix pval`c'[14,15]	= table`c'[4,108]	// kin14 vs kin15
	matrix pval`c'[14,16]	= table`c'[4,109]	// kin14 vs kin16	
	
	matrix pval`c'[15,16]	= table`c'[4,110]	// kin15 vs kin16
}


* store numbering for kin as rows and columns
mat kin_lab = (1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16)
mat kin_lab_w = kin_lab'


* set up new excel sheet
putexcel set $WD/data/pval_overall, sheet("pval_overall") replace 

* setup rows and columns (labels)
putexcel 	B1 = 	"$cl1" 		B2  = 	matrix(kin_lab_w)  	A3  = 	matrix(kin_lab)	/// 
			B20 = 	"$cl2"		B21 = 	matrix(kin_lab_w)	A22 = 	matrix(kin_lab)	/// 
			B39 = 	"$cl3"		B40 = 	matrix(kin_lab_w) 	A41 = 	matrix(kin_lab)	/// 
			B58 = 	"$cl4"		B59 = 	matrix(kin_lab_w) 	A60 = 	matrix(kin_lab)	/// 
			B77 = 	"$cl5"		B78 = 	matrix(kin_lab_w) 	A79 = 	matrix(kin_lab)	/// 
			, nformat(#)

			
* merge subheader columns
putexcel	(B1 	:Q1 	)	///
			(B20	:Q20	)	///
            (B39	:Q39	)	///
			(B58	:Q58	)	///
            (B77	:Q77	)	///
            , merge hcenter vcenter
			
* fill values
putexcel	B3		= matrix(pval1)		/// Class1
			B22		= matrix(pval2)		///	Class2
			B41		= matrix(pval3)		///	Class3
			B60		= matrix(pval4)		///	Class4
			B79		= matrix(pval5)		///	Class5
			, nformat(#.00)





* PREDICTED PROBABILITIES (kin cat large)

est restore mlogit_l_cntry
margins kin_cat_l [pweight = dwe]


* store results in table
matrix table = J(80,9,.)	// 80 combinations of kin (16) x class (5)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_overall, sheet("predprobs") replace 


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

	
	
* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(80,1,.)
forvalues i = 1/80 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "cntry.l" 					 		/// empty (just set to have same columns in all outputs)
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)









******************
*** BY COUNTRY ***
******************




*** NUCLEAR KIN ***



* PREDICTED PROBABILITIES (kin cat nuclear)

* indicator nuclear kin (y/n)
gen nuclear = inrange(kin_cat_l,1,4)
* number of nuclear kin
gen nuclear_num = num_1 + num_2 + num_3 + num_4
* number of extended kin
gen ext_num = num_m_5 + num_m_6 + num_m_7 + num_m_8 + num_m_9 + num_m_10


global controls "i.fem"


mlogit class i.nuclear##i.cntry $controls [pweight = dwe],  vce(cluster anc_id)
est store mlogit_n_cntry


est restore mlogit_n_cntry
margins nuclear#cntry [pweight = dwe]
est store iact1_n

* store results in table
matrix table = J(60,9,.)	// 60 cominations of nuclear (2) x class (5) x cntry (6)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs, sheet("predprobs") replace 

* cols for cntry (1x60 -> cntry (1-6) repeated kin-times (2) in each class (5))
matrix cntry = J(60,1,.)
matrix cntry = (1\2\3\4\5\6\1\2\3\4\5\6\	/// class1
				1\2\3\4\5\6\1\2\3\4\5\6\	/// class2
				1\2\3\4\5\6\1\2\3\4\5\6\	/// class3
				1\2\3\4\5\6\1\2\3\4\5\6\	///	class4
				1\2\3\4\5\6\1\2\3\4\5\6)	// 	class5


* cols for nuclear (1x60 -> each kin (0-1) repeated cntry-times (6) in each class (5))
matrix nuclear = J(60,1,.)
matrix nuclear = (	0\0\0\0\0\0\1\1\1\1\1\1\	/// class1
					0\0\0\0\0\0\1\1\1\1\1\1\	/// class2
					0\0\0\0\0\0\1\1\1\1\1\1\	/// class3
					0\0\0\0\0\0\1\1\1\1\1\1\	/// class4
					0\0\0\0\0\0\1\1\1\1\1\1)	/// class5
				
* cols for class (1x60 -> each class (1-5) repeated cntry x kin-times (6x2))
matrix class = J(60,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5\5\5\5\5) 	//	class5
				


* store predicted probs in pp
matrix pp = J(60,1,.)
forvalues i = 1/60 {
    matrix pp[`i', 1] = table[`i', 1]	// column 1
}

* store upper bound CI in ub
matrix ub = J(60,1,.)
forvalues i = 1/60 {
    matrix ub[`i', 1] = table[`i', 6]	// column 6
}

* store lower bound CI in lb
matrix lb = J(60,1,.)
forvalues i = 1/60 {
    matrix lb[`i', 1] = table[`i', 5]	// column 5
}

* store average kin counts in kin
matrix kin = J(60,1,.)


levelsof cntry, local(cntry)
foreach i of local cntry {
	* Calculate the mean of ext_num for the subset where cntry == i
    sum ext_num if help == 1 & cntry == `i'
    scalar avg = r(mean)

	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0 + `i'', 1] = avg	//
	matrix kin[`= 12 + `i'', 1] = avg	// cntry * 2
	matrix kin[`= 24 + `i'', 1] = avg	// cntry * 4
	matrix kin[`= 36 + `i'', 1] = avg	// cntry * 6
	matrix kin[`= 48 + `i'', 1] = avg	// cntry * 8
	
	* Calculate the mean of nuclear_num for the subset where cntry == i
    sum nuclear_num if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 6 + `i'', 1] = avg	// above + cntry
	matrix kin[`= 18 + `i'', 1] = avg
	matrix kin[`= 30 + `i'', 1] = avg
	matrix kin[`= 42 + `i'', 1] = avg
	matrix kin[`= 54 + `i'', 1] = avg
	
	}

* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(60,1,.)
forvalues i = 1/60 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "cntry.l" 	A2 = matrix(cntry) 		///
			B1 = "nuclear" 	B2 = matrix(nuclear) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)
				
		

		
		
		
		
*** EXTENDED-NUCLEAR KIN (XS) ***


* indicator nuclear kin (y/n)
gen kin_cat_xs = inrange(kin_cat_med,5,7)
recode kin_cat_xs(0=2) if inrange(kin_cat_med,8,10)
	lab def kin_cat_xs 0 "Nuclear" 1 "Extended-nuclear" 2 "Extended", replace
	lab val kin_cat_xs kin_cat_xs


* number of nuclear kin
gen num_xs_0 = num_m_1 + num_m_2 + num_m_3 + num_m_4
gen num_xs_1 = num_m_5 + num_m_6 + num_m_7
gen num_xs_2 = num_m_8 + num_m_9 + num_m_10




mlogit class i.kin_cat_xs##i.cntry $controls [pweight = dwe],  vce(cluster anc_id)
est store mlogit_xs_cntry





** PAIRWISE CONTRASTS for kin comparison (kin cat XS)

* run margins with pwcompare for each cluster and kin cat separately 
* and store results in matrix 'table_ck'
forvalues k = 0/2 {
	forvalues c = 1/5 {
	
	est restore mlogit_xs_cntry
	margins kin_cat_xs#cntry if kin_cat_xs == `k' [pweight = dwe],	/// 3 kin cats
		predict(outcome(`c'))										/// 5 classes
		pwcompare(pv groups) post
	est store iact1_xs`c'`k'
	
	matrix table`c'`k' = r(table_vs)
	
	* FOR TABLE
	* Store the pvalues from table_kc (row 4, col x) in a matrix on diagonals (pval_kc)
	matrix pval`c'`k' = J(6,6,.)
    matrix pval`c'`k'[1,2] = table`c'`k'[4,1]	// UK vs DE
	matrix pval`c'`k'[1,3] = table`c'`k'[4,2]	// UK vs PL
	matrix pval`c'`k'[1,4] = table`c'`k'[4,3]	// UK vs IT
	matrix pval`c'`k'[1,5] = table`c'`k'[4,4]	// UK vs NL
	matrix pval`c'`k'[1,6] = table`c'`k'[4,5]	// UK vs N
	
	matrix pval`c'`k'[2,3] = table`c'`k'[4,6]	// DE vs PL
	matrix pval`c'`k'[2,4] = table`c'`k'[4,7]	// DE vs IT
	matrix pval`c'`k'[2,5] = table`c'`k'[4,8]	// DE vs NL
	matrix pval`c'`k'[2,6] = table`c'`k'[4,9]	// DE vs N	
	
	matrix pval`c'`k'[3,4] = table`c'`k'[4,10]	// PL vs IT
	matrix pval`c'`k'[3,5] = table`c'`k'[4,11]	// PL vs NL
	matrix pval`c'`k'[3,6] = table`c'`k'[4,12]	// PL vs N	
	
	matrix pval`c'`k'[4,5] = table`c'`k'[4,13]	// IT vs NL
	matrix pval`c'`k'[4,6] = table`c'`k'[4,14]	// IT vs N	
	
	matrix pval`c'`k'[5,6] = table`c'`k'[4,15]	// NL vs N	
	
}
}



* set up new excel sheet
putexcel set $WD/data/pvals_table, sheet("xs") replace 

* store numbering for kin as rows and columns
mat cntry_lab = (1\2\3\4\5\6)
mat cntry_lab_w = cntry_lab'

* setup rows and columns (labels)
putexcel 	B1 = 	"Nuclear" 				B2  = 	"$cl1" H2 = 	"$cl2" 	/// 
			B10 = 	"Extended-nuclear"		B11 = 	"$cl1" H11 = 	"$cl2" 	/// 
			B19 = 	"Extended"				B20 = 	"$cl1" H20 = 	"$cl2" 	/// 
			///
			N2  = 	"$cl3" T2 = 	"$cl4"	Z2 = 	"$cl5" 	/// 
			N11 = 	"$cl3" T11 = 	"$cl4"	Z11 = 	"$cl5" 	/// 
			N20 = 	"$cl3" T20 = 	"$cl4"	Z20 = 	"$cl5" 	/// 
			///
			A4 		= matrix(cntry_lab)	B3 		= matrix(cntry_lab_w)	 	///	
			A13		= matrix(cntry_lab)	B12		= matrix(cntry_lab_w)	 	///	
			A22		= matrix(cntry_lab)	B21		= matrix(cntry_lab_w)	 	///	
			///
			H3 		= matrix(cntry_lab_w)	N3 		= matrix(cntry_lab_w)	 	///	
			H12		= matrix(cntry_lab_w)	N12		= matrix(cntry_lab_w)	 	///	
			H21		= matrix(cntry_lab_w)	N21		= matrix(cntry_lab_w)	 	///	
			///
			T3 		= matrix(cntry_lab_w)	Z3 		= matrix(cntry_lab_w)	 	///	
			T12		= matrix(cntry_lab_w)	Z12		= matrix(cntry_lab_w)	 	///	
			T21		= matrix(cntry_lab_w)	Z21		= matrix(cntry_lab_w)	 	///	
			, nformat(#)

			
* merge subheader columns
putexcel	(B1 	:AE1 	)	(B2  :G2)	(H2  :M2)	(N2  :S2)	(T2  :Y2)	(Z2  :AE2)		///
			(B10	:AE10	)	(B11 :G11 )	(H11 :M11 )	(N11 :S11 )	(T11 :Y11 )	(Z11 :AE11 )	///
            (B19	:AE19	)	(B20 :G20 )	(H20 :M20 )	(N20 :S20 )	(T20 :Y20 )	(Z20 :AE20 )	///
            , merge hcenter vcenter
			
* fill values
putexcel	B4		= matrix(pval10)		H4		= matrix(pval20)	/// Classes 1 & 2
			B13		= matrix(pval11)		H13		= matrix(pval21)	///
			B22		= matrix(pval12)		H22		= matrix(pval22)	///
			///
			N4		= matrix(pval30)		T4		= matrix(pval40)	/// Classes 3 & 4
			N13		= matrix(pval31)		T13		= matrix(pval41)	///
			N22		= matrix(pval32)		T22		= matrix(pval42)	///
			///
			Z4		= matrix(pval50)		/// Class 5
			Z13		= matrix(pval51)		///
			Z22		= matrix(pval52)		///
			, nformat(#.00)


			
			

			
			
* PREDICTED PROBABILITIES (kin cat XS)


est restore mlogit_xs_cntry
margins kin_cat_xs#cntry [pweight = dwe]
est store iact1_xs

* store results in table
matrix table = J(90,9,.)	// 90 cominations of kin (3) x class (5) x cntry (6)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_xs, sheet("predprobs") replace 

* cols for cntry (1x90 -> cntry (1-6) repeated kin-times (3) in each class (5))
matrix cntry = J(90,1,.)
matrix cntry = (1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class1
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class2
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class3
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class4
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6)	/// class5


* cols for kincat (1x90 -> each kin (0-2) repeated cntry-times (6) in each class (5))
matrix kincat = J(90,1,.)
matrix kincat = (	0\0\0\0\0\0\1\1\1\1\1\1\2\2\2\2\2\2\	/// class1
					0\0\0\0\0\0\1\1\1\1\1\1\2\2\2\2\2\2\	/// class2
					0\0\0\0\0\0\1\1\1\1\1\1\2\2\2\2\2\2\	/// class3
					0\0\0\0\0\0\1\1\1\1\1\1\2\2\2\2\2\2\	/// class4
					0\0\0\0\0\0\1\1\1\1\1\1\2\2\2\2\2\2)	/// class5
				
* cols for class (1x90 -> each class (1-5) repeated cntry x kin-times (6x3))
matrix class = J(90,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5) 	//	class5
				


* store predicted probs in pp
matrix pp = J(90,1,.)
forvalues i = 1/90 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(90,1,.)
forvalues i = 1/90 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(90,1,.)
forvalues i = 1/90 {
    matrix lb[`i', 1] = table[`i', 5]
}

* store average kin counts per cntry/cntrynicity in kin
matrix kin = J(90,1,.)

levelsof cntry, local(cntry)
foreach i of local cntry {
    * Calculate the mean of num_xs_0 for the subset where cntry == i
    sum num_xs_0 if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0 + `i'', 1] = avg 	//
	matrix kin[`= 18 + `i'', 1] = avg	// cntry * 3
	matrix kin[`= 36 + `i'', 1] = avg	// cntry * 6
	matrix kin[`= 54 + `i'', 1] = avg	// cntry * 9
	matrix kin[`= 72 + `i'', 1] = avg	// cntry * 12
	
	* Calculate the mean of num_xs_1 for the subset where cntry == i
    sum num_xs_1 if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 6 + `i'', 1] = avg	// above + cntry
	matrix kin[`= 24 + `i'', 1] = avg
	matrix kin[`= 42 + `i'', 1] = avg
	matrix kin[`= 60 + `i'', 1] = avg
	matrix kin[`= 78 + `i'', 1] = avg
	
	* Calculate the mean of num_xs_2 for the subset where cntry == i
    sum num_xs_2 if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 12 + `i'', 1] = avg	// above + cntry
	matrix kin[`= 30 + `i'', 1] = avg
	matrix kin[`= 48 + `i'', 1] = avg
	matrix kin[`= 66 + `i'', 1] = avg
	matrix kin[`= 84 + `i'', 1] = avg
	
	}

* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(90,1,.)
forvalues i = 1/90 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "cntry.l" 	A2 = matrix(cntry) 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)


			
			

			
			
			
			
			
			
********************************************
********************************************
********************************************			
*** KIN CAT SMALL - CONTINUE HERE ****
/*


mlogit class i.kin_cat_small##i.cntry $control [pweight = dwe], vce(cluster anc_id)
est store mlogit_s_cntry


est restore mlogit_s_cntry
margins kin_cat_small#cntry [pweight = dwe]
est store iact1_s

* store results in table
matrix table = J(180,9,.)	// 120 cominations of kin (6) x class (5) x cntry (6)
matrix table = r(table)'

* set up new excel sheet
putexcel set $WD/data/predprobs_s, sheet("predprobs") replace 

* cols for cntry (1x120 -> cntry/cntryn (1-4) repeated kin-times (6) in each class (5))
matrix cntry = J(120,1,.)
matrix cntry = (	1\2\3\4\1\2\3\4\1\2\3\4\	/// class1
				1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class2
				1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class3
				1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class4
				1\2\3\4\1\2\3\4\1\2\3\4\	///	
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class5
				1\2\3\4\1\2\3\4\1\2\3\4)	// 	
			
				
* cols for kincat (1x120 -> each kin (0-6) repeated cntry-times (4) in each class (5))
matrix kincat = J(120,1,.)
matrix kincat = (	1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class1
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class2
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class3
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class4
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6)	/// class5
				
* cols for class (1x120 -> each class (1-5) repeated cntry x kin-times (4x10))
matrix class = J(120,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5) 	//	class5
				


* store predicted probs in pp
matrix pp = J(120,1,.)
forvalues i = 1/120 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(120,1,.)
forvalues i = 1/120 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(120,1,.)
forvalues i = 1/120 {
    matrix lb[`i', 1] = table[`i', 5]
}

* store average kin counts per cntry/cntrynicity in kin
cap drop num_s_*
gen num_s_1 = num_m_1 + num_m_2
gen num_s_2 = num_m_3 + num_m_4
gen num_s_3 = num_m_5 + num_m_6
gen num_s_4 = num_m_7
gen num_s_5 = num_m_8 + num_m_9
gen num_s_6 = num_m_10

matrix kin = J(120,1,.)

	
forvalues i = 1/4 {		// run through cntry/cntrynicity
	forvalues e = 0/5 {	// run through kin - 1
	    
	
	* Calculate the mean of each kin for the subset where cntry == i
    global x = `e' + 1 // to get real kin numbering
	sum num_s_$x if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0  + `e' * 4 + `i'', 1] = avg	// class 1
	matrix kin[`= 24 + `e' * 4 + `i'', 1] = avg	// class 2
	matrix kin[`= 48 + `e' * 4 + `i'', 1] = avg	// class 3
	matrix kin[`= 72 + `e' * 4 + `i'', 1] = avg	// class 4
	matrix kin[`= 96 + `e' * 4 + `i'', 1] = avg	// class 5
	}
	}
	

* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(120,1,.)
forvalues i = 1/120 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "cntry.l" 	A2 = matrix(cntry) 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)



			

			
			
			
			
			
			
*** KIN CAT MEDIUM



mlogit class i.kin_cat_med##i.cntry i.female [pweight = dwe], vce(cluster anc_id)
est store mlogit_m_cntry


est restore mlogit_m_cntry
margins kin_cat_med#cntry if cntry != 5 [pweight = dwe]
est store iact1_m

* store results in table
matrix table = J(200,9,.)	// 200 cominations of kin (10) x class (5) x cntry (4)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_m, sheet("predprobs") replace 

* cols for cntry (1x200 -> cntry/cntryn (1-4) repeated kin-times (10) in each class (5))
matrix cntry = J(200,1,.)
matrix cntry = (	1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class1
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class2
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class3
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class4
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	///	
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class5
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4)	// 	
			
				
* cols for kincat (1x200 -> each kin (0-10) repeated cntry-times (4) in each class (5))
matrix kincat = J(200,1,.)
matrix kincat = (	1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class1 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class2
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class3 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class4 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class5 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10)	//
					
				
* cols for class (200 -> each class (1-5) repeated cntry x kin-times (4x10))
matrix class = J(200,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	///
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	///
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	///
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	///
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\ 	///	class5
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5)


* store predicted probs in pp 
matrix pp = J(200,1,.)
forvalues i = 1/200 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(200,1,.)
forvalues i = 1/200 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(200,1,.)
forvalues i = 1/200 {
    matrix lb[`i', 1] = table[`i', 5]
}


matrix kin = J(200,1,.)

forvalues i = 1/4 {	
	forvalues e = 0/9 {
	
	* Calculate the mean of each kin for the subset where cntry == i
    global x = `e' + 1
	
	sum num_m_$x if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + `e' * 4 + `i'', 1] = avg	// class 1
	matrix kin[`= 40  + `e' * 4 + `i'', 1] = avg	// class 2
	matrix kin[`= 80  + `e' * 4 + `i'', 1] = avg	// class 3
	matrix kin[`= 120 + `e' * 4 + `i'', 1] = avg	// class 4
	matrix kin[`= 160 + `e' * 4 + `i'', 1] = avg	// class 5
}
}


* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(200,1,.)
forvalues i = 1/200 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "cntry.l" 	A2 = matrix(cntry) 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)

*/
			
			
			
			
			
			
*** KIN CAT LARGE 

* PREDICTED PROBABILITIES (kin cat large)

est restore mlogit_l_cntry
margins kin_cat_l#cntry [pweight = dwe]
est store iact1_l

est restore iact1_l
* store results in table
matrix table = J(480,9,.)	// 480 cominations of kin (16) x class (5) x cntry (6)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_l, sheet("predprobs") replace 

* cols for cntry (480 -> cntry (1-6) repeated kin-times (16) in each class (5))
matrix cntry = J(480,1,.)
matrix cntry = (1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class1
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class2
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class3
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class4
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	///	
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// class5
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\	/// 
				1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6\1\2\3\4\5\6)	// 	
			
				
* cols for kincat (1x480 -> each kin (1-16) repeated cntry-times (6) in each class (5))
matrix kincat = J(480,1,.)
matrix kincat = (	1\1\1\1\1\1\2\2\2\2\2\2\3\3\3\3\3\3\4\4\4\4\4\4\		/// class1 
					5\5\5\5\5\5\6\6\6\6\6\6\7\7\7\7\7\7\8\8\8\8\8\8\		///
					9\9\9\9\9\9\10\10\10\10\10\10\11\11\11\11\11\11\		///
					12\12\12\12\12\12\13\13\13\13\13\13\14\14\14\14\14\14\	///
					15\15\15\15\15\15\16\16\16\16\16\16\					///
					1\1\1\1\1\1\2\2\2\2\2\2\3\3\3\3\3\3\4\4\4\4\4\4\		/// class2
					5\5\5\5\5\5\6\6\6\6\6\6\7\7\7\7\7\7\8\8\8\8\8\8\		///
					9\9\9\9\9\9\10\10\10\10\10\10\11\11\11\11\11\11\		///
					12\12\12\12\12\12\13\13\13\13\13\13\14\14\14\14\14\14\	///
					15\15\15\15\15\15\16\16\16\16\16\16\					///
					1\1\1\1\1\1\2\2\2\2\2\2\3\3\3\3\3\3\4\4\4\4\4\4\		/// class3
					5\5\5\5\5\5\6\6\6\6\6\6\7\7\7\7\7\7\8\8\8\8\8\8\		///
					9\9\9\9\9\9\10\10\10\10\10\10\11\11\11\11\11\11\		///
					12\12\12\12\12\12\13\13\13\13\13\13\14\14\14\14\14\14\	///
					15\15\15\15\15\15\16\16\16\16\16\16\					///
					1\1\1\1\1\1\2\2\2\2\2\2\3\3\3\3\3\3\4\4\4\4\4\4\		/// class4
					5\5\5\5\5\5\6\6\6\6\6\6\7\7\7\7\7\7\8\8\8\8\8\8\		///
					9\9\9\9\9\9\10\10\10\10\10\10\11\11\11\11\11\11\		///
					12\12\12\12\12\12\13\13\13\13\13\13\14\14\14\14\14\14\	///
					15\15\15\15\15\15\16\16\16\16\16\16\					///
					1\1\1\1\1\1\2\2\2\2\2\2\3\3\3\3\3\3\4\4\4\4\4\4\		/// class5 
					5\5\5\5\5\5\6\6\6\6\6\6\7\7\7\7\7\7\8\8\8\8\8\8\		///
					9\9\9\9\9\9\10\10\10\10\10\10\11\11\11\11\11\11\		///
					12\12\12\12\12\12\13\13\13\13\13\13\14\14\14\14\14\14\	///
					15\15\15\15\15\15\16\16\16\16\16\16)			//


				
* cols for class (1x480 -> each class (1-5) repeated cntry x kin-times (6x16)
matrix class = J(480,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	///
				1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	///
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	///
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	///
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	///
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	///
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	///
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	///
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\ 	///	class5
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\	///
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5)


* store predicted probs in pp 
matrix pp = J(480,1,.)
forvalues i = 1/480 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(480,1,.)
forvalues i = 1/480 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(480,1,.)
forvalues i = 1/480 {
    matrix lb[`i', 1] = table[`i', 5]
}


matrix kin = J(480,1,.)

levelsof cntry, local(cntry)
foreach i of local cntry {	
	forvalues e = 0/15 {
	
	* Calculate the mean of each kin for the subset where cntry == i
    global x = `e' + 1
	sum num_$x if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
	matrix kin[`= 0   + `e' * 6 + `i'', 1] = avg	// class 1 (0*cntry*kin)
	matrix kin[`= 96  + `e' * 6 + `i'', 1] = avg	// class 2 (1*cntry*kin)
	matrix kin[`= 192 + `e' * 6 + `i'', 1] = avg	// class 3 (2*cntry*kin)
	matrix kin[`= 288 + `e' * 6 + `i'', 1] = avg	// class 4 (3*cntry*kin)
	matrix kin[`= 384 + `e' * 6 + `i'', 1] = avg	// class 5 (4*cntry*kin)
}
}


* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(480,1,.)
forvalues i = 1/480 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "cntry.l" 	A2 = matrix(cntry) 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)



** PAIRWISE CONTRASTS PVALS for kin comparison (kin cat large)

forvalues k = 1/16 {
	forvalues c = 1/5 {
	
	est restore mlogit_l_cntry
	margins kin_cat_l#cntry if kin_cat_l == `k' [pweight = dwe],	///
		predict(outcome(`c'))	///
		pwcompare(pv groups) post
	est store iact1_l`c'`k'
	
	matrix table`c'`k' = r(table_vs)
	
	* FOR TABLE
	* Store the pvalues from table_kc (row 4, col x) in a matrix on diagonals (pval_kc)
	matrix pval`c'`k' = J(6,6,.)
    matrix pval`c'`k'[1,2] = table`c'`k'[4,1]	// UK vs DE
	matrix pval`c'`k'[1,3] = table`c'`k'[4,2]	// UK vs PL
	matrix pval`c'`k'[1,4] = table`c'`k'[4,3]	// UK vs IT
	matrix pval`c'`k'[1,5] = table`c'`k'[4,4]	// UK vs NL
	matrix pval`c'`k'[1,6] = table`c'`k'[4,5]	// UK vs N
	
	matrix pval`c'`k'[2,3] = table`c'`k'[4,6]	// DE vs PL
	matrix pval`c'`k'[2,4] = table`c'`k'[4,7]	// DE vs IT
	matrix pval`c'`k'[2,5] = table`c'`k'[4,8]	// DE vs NL
	matrix pval`c'`k'[2,6] = table`c'`k'[4,9]	// DE vs N	
	
	matrix pval`c'`k'[3,4] = table`c'`k'[4,10]	// PL vs IT
	matrix pval`c'`k'[3,5] = table`c'`k'[4,11]	// PL vs NL
	matrix pval`c'`k'[3,6] = table`c'`k'[4,12]	// PL vs N	
	
	matrix pval`c'`k'[4,5] = table`c'`k'[4,13]	// IT vs NL
	matrix pval`c'`k'[4,6] = table`c'`k'[4,14]	// IT vs N	
	
	matrix pval`c'`k'[5,6] = table`c'`k'[4,15]	// NL vs N	


}
}



* set up new excel sheet
putexcel set $WD/data/pvals_table, sheet("large") modify 

mat cntry_lab = (1\2\3\4\5\6)
mat cntry_lab_w = cntry_lab'


* setup rows and columns A B H N T Z
putexcel 	B1 = 	"Father" 				B2  = 	"$cl1" H2 = 	"$cl2" 	/// 
			B10 = 	"Mother"				B11 = 	"$cl1" H11 = 	"$cl2" 	/// 
			B19 = 	"Brother"				B20 = 	"$cl1" H20 = 	"$cl2" 	/// 
			B28 = 	"Sister"				B29 = 	"$cl1" H29 = 	"$cl2" 	/// 
			B37 = 	"Paternal grandfather"	B38 = 	"$cl1" H38 = 	"$cl2" 	/// 
			B46 = 	"Maternal grandfather"	B47 = 	"$cl1" H47 = 	"$cl2" 	/// 
			B55 = 	"Paternal grandmother"	B56 = 	"$cl1" H56 = 	"$cl2" 	/// 
			B64 = 	"Maternal grandmother"	B65 = 	"$cl1" H65 = 	"$cl2" 	/// 
			B73 = 	"Paternal halfsibling"	B74 = 	"$cl1" H74 = 	"$cl2" 	/// 
			B82 = 	"Maternal halfsibling"	B83 = 	"$cl1" H83 = 	"$cl2" 	/// 
			B91 = 	"Paternal uncle"		B92 = 	"$cl1" H92 = 	"$cl2" 	/// 
			B100 =	"Maternal uncle"		B101 = 	"$cl1" H101 = 	"$cl2" 	/// 
			B109 =	"Paternal aunt"			B110 = 	"$cl1" H110 = 	"$cl2" 	/// 
			B118 =	"Maternal aunt"			B119 = 	"$cl1" H119 = 	"$cl2" 	/// 
			B127 =	"Paternal cousin"		B128 = 	"$cl1" H128 = 	"$cl2" 	/// 
			B136 = 	"Maternal cousin"		B137 = 	"$cl1" H137 = 	"$cl2" 	/// 
			///
			N2  = 	"$cl3" T2 = 	"$cl4"	Z2 = 	"$cl5" 	/// 
			N11 = 	"$cl3" T11 = 	"$cl4"	Z11 = 	"$cl5" 	/// 
			N20 = 	"$cl3" T20 = 	"$cl4"	Z20 = 	"$cl5" 	/// 
			N29 = 	"$cl3" T29 = 	"$cl4"	Z29 = 	"$cl5" 	/// 
			N38 = 	"$cl3" T38 = 	"$cl4"	Z38 = 	"$cl5" 	/// 
			N47 = 	"$cl3" T47 = 	"$cl4"	Z47 = 	"$cl5" 	/// 
			N56 = 	"$cl3" T56 = 	"$cl4"	Z56 = 	"$cl5" 	/// 
			N65 = 	"$cl3" T65 = 	"$cl4"	Z65 = 	"$cl5" 	/// 
			N74 = 	"$cl3" T74 = 	"$cl4"	Z74 = 	"$cl5" 	/// 
			N83 = 	"$cl3" T83 = 	"$cl4"	Z83 = 	"$cl5" 	/// 
			N92 = 	"$cl3" T92 = 	"$cl4"	Z92 = 	"$cl5" 	/// 
			N101 = 	"$cl3" T101 = 	"$cl4"	Z101 = 	"$cl5" 	/// 
			N110 = 	"$cl3" T110 = 	"$cl4"	Z110 = 	"$cl5" 	/// 
			N119 = 	"$cl3" T119 = 	"$cl4"	Z119 = 	"$cl5" 	/// 
			N128 = 	"$cl3" T128 = 	"$cl4"	Z128 = 	"$cl5" 	/// 
			N137 = 	"$cl3" T137 = 	"$cl4"	Z137 = 	"$cl5" 	/// 
			///
			A4 		= matrix(cntry_lab)	B3 		= matrix(cntry_lab_w)	 	///	
			A13		= matrix(cntry_lab)	B12		= matrix(cntry_lab_w)	 	///	
			A22		= matrix(cntry_lab)	B21		= matrix(cntry_lab_w)	 	///	
			A31		= matrix(cntry_lab)	B30		= matrix(cntry_lab_w)	 	///	
			A40		= matrix(cntry_lab)	B39		= matrix(cntry_lab_w)	 	///	
			A49		= matrix(cntry_lab)	B48		= matrix(cntry_lab_w)	 	///	
			A58		= matrix(cntry_lab)	B57		= matrix(cntry_lab_w)	 	///	
			A67		= matrix(cntry_lab)	B66		= matrix(cntry_lab_w)	 	///	
			A76		= matrix(cntry_lab)	B75		= matrix(cntry_lab_w)	 	///	
			A85		= matrix(cntry_lab)	B84		= matrix(cntry_lab_w)	 	///	
			A94		= matrix(cntry_lab)	B93		= matrix(cntry_lab_w)	 	///	
			A103	= matrix(cntry_lab)	B102	= matrix(cntry_lab_w) 	///
			A112	= matrix(cntry_lab)	B111	= matrix(cntry_lab_w) 	///
			A121	= matrix(cntry_lab)	B120	= matrix(cntry_lab_w) 	///
			A130	= matrix(cntry_lab)	B129	= matrix(cntry_lab_w) 	///
			A139	= matrix(cntry_lab)	B138	= matrix(cntry_lab_w) 	///	
			///
			H3 		= matrix(cntry_lab_w)	N3 		= matrix(cntry_lab_w)	 	///	
			H12		= matrix(cntry_lab_w)	N12		= matrix(cntry_lab_w)	 	///	
			H21		= matrix(cntry_lab_w)	N21		= matrix(cntry_lab_w)	 	///	
			H30		= matrix(cntry_lab_w)	N30		= matrix(cntry_lab_w)	 	///	
			H39		= matrix(cntry_lab_w)	N39		= matrix(cntry_lab_w)	 	///	
			H48		= matrix(cntry_lab_w)	N48		= matrix(cntry_lab_w)	 	///	
			H57		= matrix(cntry_lab_w)	N57		= matrix(cntry_lab_w)	 	///	
			H66		= matrix(cntry_lab_w)	N66		= matrix(cntry_lab_w)	 	///	
			H75		= matrix(cntry_lab_w)	N75		= matrix(cntry_lab_w)	 	///	
			H84		= matrix(cntry_lab_w)	N84		= matrix(cntry_lab_w)	 	///	
			H93		= matrix(cntry_lab_w)	N93		= matrix(cntry_lab_w)	 	///	
			H102	= matrix(cntry_lab_w)	N102	= matrix(cntry_lab_w) 	///
			H111	= matrix(cntry_lab_w)	N111	= matrix(cntry_lab_w) 	///
			H120	= matrix(cntry_lab_w)	N120	= matrix(cntry_lab_w) 	///
			H129	= matrix(cntry_lab_w)	N129	= matrix(cntry_lab_w) 	///
			H138	= matrix(cntry_lab_w)	N138	= matrix(cntry_lab_w) 	///	
			///
			T3 		= matrix(cntry_lab_w)	Z3 		= matrix(cntry_lab_w)	 	///	
			T12		= matrix(cntry_lab_w)	Z12		= matrix(cntry_lab_w)	 	///	
			T21		= matrix(cntry_lab_w)	Z21		= matrix(cntry_lab_w)	 	///	
			T30		= matrix(cntry_lab_w)	Z30		= matrix(cntry_lab_w)	 	///	
			T39		= matrix(cntry_lab_w)	Z39		= matrix(cntry_lab_w)	 	///	
			T48		= matrix(cntry_lab_w)	Z48		= matrix(cntry_lab_w)	 	///	
			T57		= matrix(cntry_lab_w)	Z57		= matrix(cntry_lab_w)	 	///	
			T66		= matrix(cntry_lab_w)	Z66		= matrix(cntry_lab_w)	 	///	
			T75		= matrix(cntry_lab_w)	Z75		= matrix(cntry_lab_w)	 	///	
			T84		= matrix(cntry_lab_w)	Z84		= matrix(cntry_lab_w)	 	///	
			T93		= matrix(cntry_lab_w)	Z93		= matrix(cntry_lab_w)	 	///	
			T102	= matrix(cntry_lab_w)	Z102	= matrix(cntry_lab_w) 	///
			T111	= matrix(cntry_lab_w)	Z111	= matrix(cntry_lab_w) 	///
			T120	= matrix(cntry_lab_w)	Z120	= matrix(cntry_lab_w) 	///
			T129	= matrix(cntry_lab_w)	Z129	= matrix(cntry_lab_w) 	///
			T138	= matrix(cntry_lab_w)	Z138	= matrix(cntry_lab_w) 	///	
			, nformat(#)

			
* merge subheader columns 	
putexcel	(B1 	:AE1 	)	(B2  :G2)	(H2  :M2)	(N2  :S2)	(T2  :X2)	(Z2  :AE2)		///
			(B10	:AE10	)	(B11 :G11 )	(H11 :M11 )	(N11 :S11 )	(T11 :X11 )	(Z11 :AE11 )	///
            (B19	:AE19	)	(B20 :G20 )	(H20 :M20 )	(N20 :S20 )	(T20 :X20 )	(Z20 :AE20 )	///
            (B28	:AE28	)	(B29 :G29 )	(H29 :M29 )	(N29 :S29 )	(T29 :X29 )	(Z29 :AE29 )	///
            (B37	:AE37	)	(B38 :G38 )	(H38 :M38 )	(N38 :S38 )	(T38 :X38 )	(Z38 :AE38 )	///
            (B46	:AE46	)	(B47 :G47 )	(H47 :M47 )	(N47 :S47 )	(T47 :X47 )	(Z47 :AE47 )	///
            (B55	:AE55	)	(B56 :G56 )	(H56 :M56 )	(N56 :S56 )	(T56 :X56 )	(Z56 :AE56 )	///
            (B64	:AE64	)	(B65 :G65 )	(H65 :M65 )	(N65 :S65 )	(T65 :X65 )	(Z65 :AE65 )	///
            (B73	:AE73	)	(B74 :G74 )	(H74 :M74 )	(N74 :S74 )	(T74 :X74 )	(Z74 :AE74 )	///
            (B82	:AE82	)	(B83 :G83 )	(H83 :M83 )	(N83 :S83 )	(T83 :X83 )	(Z83 :AE83 )	///
            (B91	:AE91	)	(B92 :G92 )	(H92 :M92 )	(N92 :S92 )	(T92 :X92 )	(Z92 :AE92 )	///
            (B100	:AE100	)	(B101:G101)	(H101:M101)	(N101:S101)	(T101:X101)	(Z101:AE101)	///
            (B109	:AE109	)	(B110:G110)	(H110:M110)	(N110:S110)	(T110:X110)	(Z110:AE110)	///
            (B118	:AE118	)	(B119:G119)	(H119:M119)	(N119:S119)	(T119:X119)	(Z119:AE119)	///
            (B127	:AE127	)	(B128:G128)	(H128:M128)	(N128:S128)	(T128:X128)	(Z128:AE128)	///
            (B136	:AE136	)	(B137:G137)	(H137:M137)	(N137:S137)	(T137:X137)	(Z137:AE137)	///
			, merge hcenter vcenter
			
* fill values
putexcel	B4		= matrix(pval11)		H4		= matrix(pval21)	/// Classes 1 & 2
			B13		= matrix(pval12)		H13		= matrix(pval22)	///
			B22		= matrix(pval13)		H22		= matrix(pval23)	///
			B31		= matrix(pval14)		H31		= matrix(pval24)	///
			B40		= matrix(pval15)		H40		= matrix(pval25)	///
			B49		= matrix(pval16)		H49		= matrix(pval26)	///
			B58		= matrix(pval17)		H58		= matrix(pval27)	///
			B67		= matrix(pval18)		H67		= matrix(pval28)	///
			B76		= matrix(pval19)		H76		= matrix(pval29)	///
			B85		= matrix(pval110)		H85		= matrix(pval210)	///
			B94		= matrix(pval111)		H94		= matrix(pval211)	///
			B103	= matrix(pval112)		H103	= matrix(pval212)	///
			B112	= matrix(pval113)		H112	= matrix(pval213)	///
			B121	= matrix(pval114)		H121	= matrix(pval214)	///
			B130	= matrix(pval115)		H130	= matrix(pval215)	///
			B139	= matrix(pval116)		H139	= matrix(pval216)	///
			///
			N4		= matrix(pval31)		T4		= matrix(pval41)	/// Classes 3 & 4
			N13		= matrix(pval32)		T13		= matrix(pval42)	///
			N22		= matrix(pval33)		T22		= matrix(pval43)	///
			N31		= matrix(pval34)		T31		= matrix(pval44)	///
			N40		= matrix(pval35)		T40		= matrix(pval45)	///
			N49		= matrix(pval36)		T49		= matrix(pval46)	///
			N58		= matrix(pval37)		T58		= matrix(pval47)	///
			N67		= matrix(pval38)		T67		= matrix(pval48)	///
			N76		= matrix(pval39)		T76		= matrix(pval49)	///
			N85		= matrix(pval310)		T85		= matrix(pval410)	///
			N94		= matrix(pval311)		T94		= matrix(pval411)	///
			N103	= matrix(pval312)		T103	= matrix(pval412)	///
			N112	= matrix(pval313)		T112	= matrix(pval413)	///
			N121	= matrix(pval314)		T121	= matrix(pval414)	///
			N130	= matrix(pval315)		T130	= matrix(pval415)	///
			N139	= matrix(pval316)		T139	= matrix(pval416)	///
			///
			Z4		= matrix(pval51)		/// Class 5
			Z13		= matrix(pval52)		///
			Z22		= matrix(pval53)		///
			Z31		= matrix(pval54)		///
			Z40		= matrix(pval55)		///
			Z49		= matrix(pval56)		///
			Z58		= matrix(pval57)		///
			Z67		= matrix(pval58)		///
			Z76		= matrix(pval59)		///
			Z85		= matrix(pval510)		///
			Z94		= matrix(pval511)		///
			Z103	= matrix(pval512)		///
			Z112	= matrix(pval513)		///
			Z121	= matrix(pval514)		///
			Z130	= matrix(pval515)		///
			Z139	= matrix(pval516)		///	
			, nformat(#.00)

			
			
			
			
* KIN CAT LARGE (APPENDIX TABLE)


* store pp for each kincat * cntry-combination
forvalues i = 1/16 {
	forvalues e = 1/6 {
		
	est restore mlogit_l_cntry
	margins `i'b.kin_cat_l#cntry if cntry == `e' [pweight = dwe]
	est store iact_`e'`i'

	matrix pp`e'`i' = r(b)'
	}
}

* store avg_kin for each kincat * cntry-combination

* Calculate the mean of FATHER for the subset where cntry == i
forvalues i = 1/16 {
	forvalues e = 1/6 {
	
	* store mean of kin_cat for each cntrynicity
	sum num_`i' if help == 1 & cntry == `e'
	scalar avg = r(mean)
	
	* ...and multiply with predicted probability
	matrix pn`e'`i' = pp`e'`i' * r(mean)
	}
}


* set up new excel sheet
putexcel set $WD/data/predprobs_l_app, sheet("predprobs") replace 

global pp_lab "Predicted Probability"
global avg_lab "Predicted Average Number of Kin"

mat class_lab = (1\2\3\4\5)

* setup rows and columns
putexcel 	B1 = 	"Father" 				B2  = 	"$pp_lab" H2 = 		"$avg_lab" 	/// 
			B10 = 	"Mother"				B11 = 	"$pp_lab" H11 = 	"$avg_lab" 	/// 
			B19 = 	"Brother"				B20 = 	"$pp_lab" H20 = 	"$avg_lab" 	/// 
			B28 = 	"Sister"				B29 = 	"$pp_lab" H29 = 	"$avg_lab" 	/// 
			B37 = 	"Paternal grandfather"	B38 = 	"$pp_lab" H38 = 	"$avg_lab" 	/// 
			B46 = 	"Maternal grandfather"	B47 = 	"$pp_lab" H47 = 	"$avg_lab" 	/// 
			B55 = 	"Paternal grandmother"	B56 = 	"$pp_lab" H56 = 	"$avg_lab" 	/// 
			B64 = 	"Maternal grandmother"	B65 = 	"$pp_lab" H65 = 	"$avg_lab" 	/// 
			B73 = 	"Paternal halfsibling"	B74 = 	"$pp_lab" H74 = 	"$avg_lab" 	/// 
			B82 = 	"Maternal halfsibling"	B83 = 	"$pp_lab" H83 = 	"$avg_lab" 	/// 
			B91 = 	"Paternal uncle"		B92 = 	"$pp_lab" H92 = 	"$avg_lab" 	/// 
			B100 =	"Maternal uncle"		B101 = 	"$pp_lab" H101 = 	"$avg_lab" 	/// 
			B109 =	"Paternal aunt"			B110 = 	"$pp_lab" H110 = 	"$avg_lab" 	/// 
			B118 =	"Maternal aunt"			B119 = 	"$pp_lab" H119 = 	"$avg_lab" 	/// 
			B127 =	"Paternal cousin"		B128 = 	"$pp_lab" H128 = 	"$avg_lab" 	/// 
			B136 = 	"Maternal cousin"		B137 = 	"$pp_lab" H137 = 	"$avg_lab" 	/// 
			B3 		= matrix(cntry_lab_w)	H3 		= matrix(cntry_lab_w)	///		
			B12		= matrix(cntry_lab_w)	H12		= matrix(cntry_lab_w)	///		
			B21		= matrix(cntry_lab_w)	H21		= matrix(cntry_lab_w)	///		
			B30		= matrix(cntry_lab_w)	H30		= matrix(cntry_lab_w)	///		
			B39		= matrix(cntry_lab_w)	H39		= matrix(cntry_lab_w)	///		
			B48		= matrix(cntry_lab_w)	H48		= matrix(cntry_lab_w)	///		
			B57		= matrix(cntry_lab_w)	H57		= matrix(cntry_lab_w)	///		
			B66		= matrix(cntry_lab_w)	H66		= matrix(cntry_lab_w)	///		
			B75		= matrix(cntry_lab_w)	H75		= matrix(cntry_lab_w)	///		
			B84		= matrix(cntry_lab_w)	H84		= matrix(cntry_lab_w)	///		
			B93		= matrix(cntry_lab_w)	H93		= matrix(cntry_lab_w)	///		
			B102	= matrix(cntry_lab_w) 	H102	= matrix(cntry_lab_w)	///	
			B111	= matrix(cntry_lab_w) 	H111	= matrix(cntry_lab_w)	///	
			B120	= matrix(cntry_lab_w) 	H120	= matrix(cntry_lab_w)	///	
			B129	= matrix(cntry_lab_w) 	H129	= matrix(cntry_lab_w)	///	
			B138	= matrix(cntry_lab_w) 	H138	= matrix(cntry_lab_w)	///	
			A4 		= matrix(class_lab) 	///
			A13		= matrix(class_lab) 	///
			A22		= matrix(class_lab) 	///
			A31		= matrix(class_lab) 	///
			A40		= matrix(class_lab) 	///
			A49		= matrix(class_lab) 	///
			A58		= matrix(class_lab) 	///
			A67		= matrix(class_lab) 	///
			A76		= matrix(class_lab) 	///
			A85		= matrix(class_lab) 	///
			A94		= matrix(class_lab) 	///
			A103	= matrix(class_lab)		///
			A112	= matrix(class_lab)		///
			A121	= matrix(class_lab)		///
			A130	= matrix(class_lab)		///
			A139	= matrix(class_lab)		///
			, nformat(#) 

* merge subheader columns
putexcel	(B1 	:M1 	)	(B2  :G2)	(H2	 :M2)		///
			(B10	:M10	)	(B11 :G11 )	(H11 :M11 )	///
            (B19	:M19	)	(B20 :G20 )	(H20 :M20 )	///
            (B28	:M28	)	(B29 :G29 )	(H29 :M29 )	///
            (B37	:M37	)	(B38 :G38 )	(H38 :M38 )	///
            (B46	:M46	)	(B47 :G47 )	(H47 :M47 )	///
            (B55	:M55	)	(B56 :G56 )	(H56 :M56 )	///
            (B64	:M64	)	(B65 :G65 )	(H65 :M65 )	///
            (B73	:M73	)	(B74 :G74 )	(H74 :M74 )	///
            (B82	:M82	)	(B83 :G83 )	(H83 :M83 )	///
            (B91	:M91	)	(B92 :G92 )	(H92 :M92 )	///
            (B100	:M100	)	(B101:G101)	(H101:M101)	///
            (B109	:M109	)	(B110:G110)	(H110:M110)	///
            (B118	:M118	)	(B119:G119)	(H119:M119)	///
            (B127	:M127	)	(B128:G128)	(H128:M128)	///
            (B136	:M136	)	(B137:G137)	(H137:M137)	, merge hcenter vcenter
	

* fill values (pp*cntry*kin)
putexcel	B4		= matrix(pp11)		C4		= matrix(pp21)	/// PP
			B13		= matrix(pp12)		C13		= matrix(pp22)	///
			B22		= matrix(pp13)		C22		= matrix(pp23)	///
			B31		= matrix(pp14)		C31		= matrix(pp24)	///
			B40		= matrix(pp15)		C40		= matrix(pp25)	///
			B49		= matrix(pp16)		C49		= matrix(pp26)	///
			B58		= matrix(pp17)		C58		= matrix(pp27)	///
			B67		= matrix(pp18)		C67		= matrix(pp28)	///
			B76		= matrix(pp19)		C76		= matrix(pp29)	///
			B85		= matrix(pp110)		C85		= matrix(pp210)	///
			B94		= matrix(pp111)		C94		= matrix(pp211)	///
			B103	= matrix(pp112)		C103	= matrix(pp212)	///
			B112	= matrix(pp113)		C112	= matrix(pp213)	///
			B121	= matrix(pp114)		C121	= matrix(pp214)	///
			B130	= matrix(pp115)		C130	= matrix(pp215)	///
			B139	= matrix(pp116)		C139	= matrix(pp216)	///
			///
			D4		= matrix(pp31)		E4		= matrix(pp41)	/// PP
			D13		= matrix(pp32)		E13		= matrix(pp42)	///
			D22		= matrix(pp33)		E22		= matrix(pp43)	///
			D31		= matrix(pp34)		E31		= matrix(pp44)	///
			D40		= matrix(pp35)		E40		= matrix(pp45)	///
			D49		= matrix(pp36)		E49		= matrix(pp46)	///
			D58		= matrix(pp37)		E58		= matrix(pp47)	///
			D67		= matrix(pp38)		E67		= matrix(pp48)	///
			D76		= matrix(pp39)		E76		= matrix(pp49)	///
			D85		= matrix(pp310)		E85		= matrix(pp410)	///
			D94		= matrix(pp311)		E94		= matrix(pp411)	///
			D103	= matrix(pp312)		E103	= matrix(pp412)	///
			D112	= matrix(pp313)		E112	= matrix(pp413)	///
			D121	= matrix(pp314)		E121	= matrix(pp414)	///
			D130	= matrix(pp315)		E130	= matrix(pp415)	///
			D139	= matrix(pp316)		E139	= matrix(pp416)	///
			///
			F4		= matrix(pp51)		G4		= matrix(pp61)	/// PP
			F13		= matrix(pp52)		G13		= matrix(pp62)	///
			F22		= matrix(pp53)		G22		= matrix(pp63)	///
			F31		= matrix(pp54)		G31		= matrix(pp64)	///
			F40		= matrix(pp55)		G40		= matrix(pp65)	///
			F49		= matrix(pp56)		G49		= matrix(pp66)	///
			F58		= matrix(pp57)		G58		= matrix(pp67)	///
			F67		= matrix(pp58)		G67		= matrix(pp68)	///
			F76		= matrix(pp59)		G76		= matrix(pp69)	///
			F85		= matrix(pp510)		G85		= matrix(pp610)	///
			F94		= matrix(pp511)		G94		= matrix(pp611)	///
			F103	= matrix(pp512)		G103	= matrix(pp612)	///
			F112	= matrix(pp513)		G112	= matrix(pp613)	///
			F121	= matrix(pp514)		G121	= matrix(pp614)	///
			F130	= matrix(pp515)		G130	= matrix(pp615)	///
			F139	= matrix(pp516)		G139	= matrix(pp616)	///
			///
			H4		= matrix(pn11)		I4		= matrix(pn21)	/// AVG_KIN
			H13		= matrix(pn12)		I13		= matrix(pn22)	///
			H22		= matrix(pn13)		I22		= matrix(pn23)	///
			H31		= matrix(pn14)		I31		= matrix(pn24)	///
			H40		= matrix(pn15)		I40		= matrix(pn25)	///
			H49		= matrix(pn16)		I49		= matrix(pn26)	///
			H58		= matrix(pn17)		I58		= matrix(pn27)	///
			H67		= matrix(pn18)		I67		= matrix(pn28)	///
			H76		= matrix(pn19)		I76		= matrix(pn29)	///
			H85		= matrix(pn110)		I85		= matrix(pn210)	///
			H94		= matrix(pn111)		I94		= matrix(pn211)	///
			H103	= matrix(pn112)		I103	= matrix(pn212)	///
			H112	= matrix(pn113)		I112	= matrix(pn213)	///
			H121	= matrix(pn114)		I121	= matrix(pn214)	///
			H130	= matrix(pn115)		I130	= matrix(pn215)	///
			H139	= matrix(pn116)		I139	= matrix(pn216)	///
			///
			J4		= matrix(pn31)		K4		= matrix(pn41)	/// AVG_KIN
			J13		= matrix(pn32)		K13		= matrix(pn42)	///
			J22		= matrix(pn33)		K22		= matrix(pn43)	///
			J31		= matrix(pn34)		K31		= matrix(pn44)	///
			J40		= matrix(pn35)		K40		= matrix(pn45)	///
			J49		= matrix(pn36)		K49		= matrix(pn46)	///
			J58		= matrix(pn37)		K58		= matrix(pn47)	///
			J67		= matrix(pn38)		K67		= matrix(pn48)	///
			J76		= matrix(pn39)		K76		= matrix(pn49)	///
			J85		= matrix(pn310)		K85		= matrix(pn410)	///
			J94		= matrix(pn311)		K94		= matrix(pn411)	///
			J103	= matrix(pn312)		K103	= matrix(pn412)	///
			J112	= matrix(pn313)		K112	= matrix(pn413)	///
			J121	= matrix(pn314)		K121	= matrix(pn414)	///
			J130	= matrix(pn315)		K130	= matrix(pn415)	///
			J139	= matrix(pn316)		K139	= matrix(pn416)	///
			///
			L4		= matrix(pn51)		M4		= matrix(pn61)	/// AVG_KIN
			L13		= matrix(pn52)		M13		= matrix(pn62)	///
			L22		= matrix(pn53)		M22		= matrix(pn63)	///
			L31		= matrix(pn54)		M31		= matrix(pn64)	///
			L40		= matrix(pn55)		M40		= matrix(pn65)	///
			L49		= matrix(pn56)		M49		= matrix(pn66)	///
			L58		= matrix(pn57)		M58		= matrix(pn67)	///
			L67		= matrix(pn58)		M67		= matrix(pn68)	///
			L76		= matrix(pn59)		M76		= matrix(pn69)	///
			L85		= matrix(pn510)		M85		= matrix(pn610)	///
			L94		= matrix(pn511)		M94		= matrix(pn611)	///
			L103	= matrix(pn512)		M103	= matrix(pn612)	///
			L112	= matrix(pn513)		M112	= matrix(pn613)	///
			L121	= matrix(pn514)		M121	= matrix(pn614)	///
			L130	= matrix(pn515)		M130	= matrix(pn615)	///
			L139	= matrix(pn516)		M139	= matrix(pn616)	///
			, nformat(#.00) 

			
			
*** last line ***
