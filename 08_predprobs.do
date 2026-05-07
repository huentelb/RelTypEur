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

global M	"$WD/graphs/251104/M11_median/lc5_output/ame_weighted"

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

* cntry/cntrynicity
global cntry1 "IT"
global cntry2 "NL"
global cntry3 "DE"
global cntry4 "PL"
global cntry5 "UK"
global cntry6 "SE"
global cntry7 "DK"
global cntry8 "FI"
global cntry9 "NO"


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
matrix table = J(90,9,.)	// 90 cominations of nuclear (2) x class (5) x cntry (9)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs, sheet("predprobs") replace 

* cols for cntry (1x90 -> cntry (1-9) repeated kin-times (2) in each class (5))
matrix cntry = J(90,1,.)
matrix cntry = (1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	/// class1
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	/// class2
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	/// class3
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	///	class4
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9)	// 	class5


* cols for nuclear (1x90 -> each kin (0-1) repeated cntry-times (9) in each class (5))
matrix nuclear = J(90,1,.)
matrix nuclear = (	0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\	/// class1
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\	/// class2
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\	/// class3
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\	/// class4
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1)	/// class5
				
* cols for class (1x90 -> each class (1-5) repeated cntry x kin-times (9x2))
matrix class = J(90,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5) 	//	class5
				


* store predicted probs in pp
matrix pp = J(90,1,.)
forvalues i = 1/90 {
    matrix pp[`i', 1] = table[`i', 1]	// column 1
}

* store upper bound CI in ub
matrix ub = J(90,1,.)
forvalues i = 1/90 {
    matrix ub[`i', 1] = table[`i', 6]	// column 6
}

* store lower bound CI in lb
matrix lb = J(90,1,.)
forvalues i = 1/90 {
    matrix lb[`i', 1] = table[`i', 5]	// column 5
}

* store average kin counts in kin
matrix kin = J(90,1,.)


levelsof cntry, local(cntry)
foreach i of local cntry {
	* Calculate the mean of ext_num for the subset where cntry == i
    sum ext_num if help == 1 & cntry == `i'
    scalar avg = r(mean)

	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0 	+ `i'', 1] = avg	//
	matrix kin[`= (9 * 2) + `i'', 1] = avg	// cntry * 2
	matrix kin[`= (9 * 4) + `i'', 1] = avg	// cntry * 4
	matrix kin[`= (9 * 6) + `i'', 1] = avg	// cntry * 6
	matrix kin[`= (9 * 8) + `i'', 1] = avg	// cntry * 8
	
	* Calculate the mean of nuclear_num for the subset where cntry == i
    sum nuclear_num if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 9 		  + `i'', 1] = avg	// above + cntry
	matrix kin[`= 9 + (9 * 2) + `i'', 1] = avg
	matrix kin[`= 9 + (9 * 4) + `i'', 1] = avg
	matrix kin[`= 9 + (9 * 6) + `i'', 1] = avg
	matrix kin[`= 9 + (9 * 8) + `i'', 1] = avg
	
	}

* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(90,1,.)
forvalues i = 1/90 {
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
	matrix pval`c'`k' = J(9,9,.)				// cntry, cntry
    matrix pval`c'`k'[1,2] = table`c'`k'[4,1]	// IT vs NL
	matrix pval`c'`k'[1,3] = table`c'`k'[4,2]	// IT vs DE
	matrix pval`c'`k'[1,4] = table`c'`k'[4,3]	// IT vs PL
	matrix pval`c'`k'[1,5] = table`c'`k'[4,4]	// IT vs UK
	matrix pval`c'`k'[1,6] = table`c'`k'[4,5]	// IT vs SE
	matrix pval`c'`k'[1,7] = table`c'`k'[4,6]	// IT vs DK
	matrix pval`c'`k'[1,8] = table`c'`k'[4,7]	// IT vs FI
	matrix pval`c'`k'[1,9] = table`c'`k'[4,8]	// IT vs NO
	
	matrix pval`c'`k'[2,3] = table`c'`k'[4,9]	// NL vs DE
	matrix pval`c'`k'[2,4] = table`c'`k'[4,10]	// NL vs PL
	matrix pval`c'`k'[2,5] = table`c'`k'[4,11]	// NL vs UK
	matrix pval`c'`k'[2,6] = table`c'`k'[4,12]	// NL vs SE	
	matrix pval`c'`k'[2,7] = table`c'`k'[4,13]	// NL vs DK	
	matrix pval`c'`k'[2,8] = table`c'`k'[4,14]	// NL vs FI	
	matrix pval`c'`k'[2,9] = table`c'`k'[4,15]	// NL vs NO	
	
	matrix pval`c'`k'[3,4] = table`c'`k'[4,16]	// DE vs PL
	matrix pval`c'`k'[3,5] = table`c'`k'[4,17]	// DE vs UK
	matrix pval`c'`k'[3,6] = table`c'`k'[4,18]	// DE vs SE	
	matrix pval`c'`k'[3,7] = table`c'`k'[4,19]	// DE vs DK	
	matrix pval`c'`k'[3,8] = table`c'`k'[4,20]	// DE vs FI	
	matrix pval`c'`k'[3,9] = table`c'`k'[4,21]	// DE vs NO	
	
	matrix pval`c'`k'[4,5] = table`c'`k'[4,22]	// PL vs UK
	matrix pval`c'`k'[4,6] = table`c'`k'[4,23]	// PL vs SE	
	matrix pval`c'`k'[4,7] = table`c'`k'[4,24]	// PL vs DK	
	matrix pval`c'`k'[4,8] = table`c'`k'[4,25]	// PL vs FI	
	matrix pval`c'`k'[4,9] = table`c'`k'[4,26]	// PL vs NO	
	
	matrix pval`c'`k'[5,6] = table`c'`k'[4,27]	// UK vs SE
	matrix pval`c'`k'[5,7] = table`c'`k'[4,28]	// UK vs DK
	matrix pval`c'`k'[5,8] = table`c'`k'[4,29]	// UK vs FI
	matrix pval`c'`k'[5,9] = table`c'`k'[4,30]	// UK vs NO
	
	matrix pval`c'`k'[6,7] = table`c'`k'[4,31]	// SE vs DK	
	matrix pval`c'`k'[6,8] = table`c'`k'[4,32]	// SE vs FI	
	matrix pval`c'`k'[6,9] = table`c'`k'[4,33]	// SE vs NO	

	matrix pval`c'`k'[7,8] = table`c'`k'[4,34]	// DK vs FI	
	matrix pval`c'`k'[7,9] = table`c'`k'[4,35]	// DK vs NO	

	matrix pval`c'`k'[8,9] = table`c'`k'[4,36]	// FI vs NO	
}
}



* set up new excel sheet
putexcel set $WD/data/pvals_table, sheet("xs") replace 

* store numbering for kin as rows and columns
mat cntry_lab = (1\2\3\4\5\6\7\8\9)
mat cntry_lab_w = cntry_lab'

* setup rows and columns (labels)
putexcel 	B1 = 	"Nuclear" 				B2  = 	"$cl1" K2 = 	"$cl2" 	/// 
			B13 = 	"Extended-nuclear"		B14 = 	"$cl1" K14 = 	"$cl2" 	/// 
			B25 = 	"Extended"				B26 = 	"$cl1" K26 = 	"$cl2" 	/// 
			///
			T2  = 	"$cl3" AC2 = 	"$cl4"	AL2 = 	"$cl5" 	/// 
			T14 = 	"$cl3" AC14 = 	"$cl4"	AL14 = 	"$cl5" 	/// 
			T26 = 	"$cl3" AC26 = 	"$cl4"	AL26 = 	"$cl5" 	/// 
			///
			A4 		= matrix(cntry_lab)	B3 		= matrix(cntry_lab_w)	 	///	
			A16		= matrix(cntry_lab)	B15		= matrix(cntry_lab_w)	 	///	
			A28		= matrix(cntry_lab)	B27		= matrix(cntry_lab_w)	 	///	
			///
			K3 		= matrix(cntry_lab_w)	T3 		= matrix(cntry_lab_w)	 	///	
			K15		= matrix(cntry_lab_w)	T15		= matrix(cntry_lab_w)	 	///	
			K27		= matrix(cntry_lab_w)	T27		= matrix(cntry_lab_w)	 	///	
			///
			AC3 		= matrix(cntry_lab_w)	AL3 		= matrix(cntry_lab_w)	 	///	
			AC15		= matrix(cntry_lab_w)	AL15		= matrix(cntry_lab_w)	 	///	
			AC27		= matrix(cntry_lab_w)	AL27		= matrix(cntry_lab_w)	 	///	
			, nformat(#)

			
* merge subheader columns
putexcel	(B1 	:AE1 	)	(B2  :J2)	(K2  :S2)	(T2  :AB2)		(AC2  :AK2)		(AL2  :AT2)		///
			(B13	:AE13	)	(B14 :J14 )	(K14 :S14 )	(T14 :AB14 )	(AC14 :AK14 )	(AL14 :AT14 )	///
            (B25	:AE25	)	(B26 :J26 )	(K26 :S26 )	(T26 :AB26 )	(AC26 :AK26 )	(AL26 :AT26 )	///
            , merge hcenter vcenter
			
* fill values
putexcel	B4		= matrix(pval10)		K4		= matrix(pval20)	/// Classes 1 & 2
			B16		= matrix(pval11)		K16		= matrix(pval21)	///
			B28		= matrix(pval12)		K28		= matrix(pval22)	///
			///
			T4		= matrix(pval30)		AC4		= matrix(pval40)	/// Classes 3 & 4
			T16		= matrix(pval31)		AC16	= matrix(pval41)	///
			T28		= matrix(pval32)		AC28	= matrix(pval42)	///
			///
			AL4		= matrix(pval50)		/// Class 5
			AL16	= matrix(pval51)		///
			AL28	= matrix(pval52)		///
			, nformat(#.00)


			
			

			
			
* PREDICTED PROBABILITIES (kin cat XS)


est restore mlogit_xs_cntry
margins kin_cat_xs#cntry [pweight = dwe]
est store iact1_xs

* store results in table
matrix table = J(135,9,.)	// 135 cominations of kin (3) x class (5) x cntry (9)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_xs, sheet("predprobs") replace 

* cols for cntry (1x135 -> cntry (1-9) repeated kin-times (3) in each class (5))
matrix cntry = J(135,1,.)
matrix cntry = (1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	/// class1
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	/// class2
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	/// class3
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\	/// class4
				1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9\1\2\3\4\5\6\7\8\9)	/// class5


* cols for kincat (1x135 -> each kin (0-2) repeated cntry-times (9) in each class (5))
matrix kincat = J(135,1,.)
matrix kincat = (	0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\2\2\2\2\2\2\2\2\2\	/// class1
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\2\2\2\2\2\2\2\2\2\	/// class2
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\2\2\2\2\2\2\2\2\2\	/// class3
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\2\2\2\2\2\2\2\2\2\	/// class4
					0\0\0\0\0\0\0\0\0\1\1\1\1\1\1\1\1\1\2\2\2\2\2\2\2\2\2)	/// class5
				
* cols for class (1x135 -> each class (1-5) repeated cntry x kin-times (9x3))
matrix class = J(135,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5) 	//	class5
				


* store predicted probs in pp
matrix pp = J(135,1,.)
forvalues i = 1/135 {
    matrix pp[`i', 1] = table[`i', 1]	// column 1
}

* store upper bound CI in ub
matrix ub = J(135,1,.)
forvalues i = 1/135 {
    matrix ub[`i', 1] = table[`i', 6]	// column 6
}

* store lower bound CI in lb
matrix lb = J(135,1,.)
forvalues i = 1/135 {
    matrix lb[`i', 1] = table[`i', 5]	// column 5
}

* store average kin counts per cntry/cntrynicity in kin
matrix kin = J(135,1,.)

levelsof cntry, local(cntry)
foreach i of local cntry {
    * Calculate the mean of num_xs_0 for the subset where cntry == i
    sum num_xs_0 if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0 	+ `i'', 1] = avg	//
	matrix kin[`= (9 *  3) + `i'', 1] = avg	// cntry * 3
	matrix kin[`= (9 *  6) + `i'', 1] = avg	// cntry * 6
	matrix kin[`= (9 *  9) + `i'', 1] = avg	// cntry * 9
	matrix kin[`= (9 * 12) + `i'', 1] = avg	// cntry * 12
	
	* Calculate the mean of num_xs_1 for the subset where cntry == i
    sum num_xs_1 if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 9 		   + `i'', 1] = avg	// above + cntry
	matrix kin[`= 9 + (9 *  3) + `i'', 1] = avg
	matrix kin[`= 9 + (9 *  6) + `i'', 1] = avg
	matrix kin[`= 9 + (9 *  9) + `i'', 1] = avg
	matrix kin[`= 9 + (9 * 12) + `i'', 1] = avg	
	
	* Calculate the mean of num_xs_2 for the subset where cntry == i
    sum num_xs_2 if help == 1 & cntry == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 9*2 		   + `i'', 1] 	= avg		// above + cntry
	matrix kin[`= 9*2 + (9 *  3) + `i'', 1] = avg
	matrix kin[`= 9*2 + (9 *  6) + `i'', 1] = avg
	matrix kin[`= 9*2 + (9 *  9) + `i'', 1] = avg
	matrix kin[`= 9*2 + (9 * 12) + `i'', 1] = avg	
	
	}

* predicted average number of kin in each class by cntry
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(135,1,.)
forvalues i = 1/135 {
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
		
			
*** last line ***
