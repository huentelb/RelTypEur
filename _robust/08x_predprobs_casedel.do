*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in the United States
* Bettina Hünteler
* 03.12.2024
* huenteler@wiso.uni-koeln.de

*** 08 PREDICTED PROBABILITIES *** 

*-------------------------------------------------------------------------------


*** Working directories ***

/*
global WD 	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses"

global IN 	"/Users/Bettina/Documents/datasets/KINMATRIX"
global OUT 	"/Users/Bettina/Documents/datasets/KINMATRIX"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses/graphs/241114/M11_median/lc5_output/ame_weighted"

*** stata settings
set more off, perm
set scheme white_tableau, perm 
set showbaselevels on
*/



*******************************
*** SET UP
*******************************

use $WD/data/lc_analytical.dta, clear



* store labels in locals for labelling of graphs

* race/ethnicity
global eth1 "White"
global eth2 "Hispanic"
global eth3 "Black"
global eth4 "Asian"
*global eth5 "Other"


* cluster
global cl1 "Tight-knit"
global cl2 "Connected-but-autnonomous"								
global cl3 "Ambivalent"								
global cl4 "Intimate-but-distant"
global cl5 "Detached"






************************************************
***		 OVERALL PREDICTED PROBABILITIES	 ***
************************************************

* i.e., not distinguished by race/ethnicity
* base for FIGURE 3


*** KIN CAT LARGE ***

* re-run main model 
est replay mlogit_l_race


*mlogit class i.kin_cat_l##i.eth i.female [pweight = dwe], vce(cluster anc_id)
*est store mlogit_l_race


* PAIRWISE CONTRASTS for kin comparison (kin cat large)

* run margins with pwcompare for each cluster separately 
* and store results in matrix 'table_c'
forvalues c = 1/5 {
	
	est restore mlogit_l_race
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

est restore mlogit_l_race
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









*************************
*** BY RACE-ETHNICITY ***
*************************




*** NUCLEAR KIN ***



* PREDICTED PROBABILITIES (kin cat nuclear)

* indicator nuclear kin (y/n)
gen nuclear = inrange(kin_cat_l,1,4)
* number of nuclear kin
gen nuclear_num = num_1 + num_2 + num_3 + num_4
* number of extended kin
gen ext_num = num_m_5 + num_m_6 + num_m_7 + num_m_8 + num_m_9 + num_m_10



mlogit class i.nuclear##i.eth i.female [pweight = dwe],  vce(cluster anc_id)
est store mlogit_n_race


est restore mlogit_n_race
margins nuclear#eth if eth != 5 [pweight = dwe]
est store iact1_n

* store results in table
matrix table = J(40,9,.)	// 40 cominations of nuclear (2) x class (5) x race (4)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs, sheet("predprobs") replace 

* cols for race (1x40 -> race/ethn (1-4) repeated kin-times (2) in each class (5))
matrix race = J(40,1,.)
matrix race = (	1\2\3\4\1\2\3\4\	/// class1
				1\2\3\4\1\2\3\4\	/// class2
				1\2\3\4\1\2\3\4\	/// class3
				1\2\3\4\1\2\3\4\	///	class4
				1\2\3\4\1\2\3\4)	// 	class5


* cols for nuclear (1x40 -> each kin (0-1) repeated race-times (4) in each class (5))
matrix nuclear = J(40,1,.)
matrix nuclear = (	0\0\0\0\1\1\1\1\	/// class1
					0\0\0\0\1\1\1\1\	/// class2
					0\0\0\0\1\1\1\1\	/// class3
					0\0\0\0\1\1\1\1\	/// class4
					0\0\0\0\1\1\1\1)	/// class5
				
* cols for class (1x40 -> each class (1-5) repeated race x kin-times (4x2))
matrix class = J(40,1,.)
matrix class = (1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5) 	//	class5
				


* store predicted probs in pp
matrix pp = J(40,1,.)
forvalues i = 1/40 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(40,1,.)
forvalues i = 1/40 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(40,1,.)
forvalues i = 1/40 {
    matrix lb[`i', 1] = table[`i', 5]
}

* store average kin counts in kin
matrix kin = J(40,1,.)

forvalues i = 1/4 {	
    * Calculate the mean of nuclear_num for the subset where eth == i
    sum ext_num if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0 + `i'', 1] = avg
	matrix kin[`= 8 + `i'', 1] = avg
	matrix kin[`= 16 + `i'', 1] = avg
	matrix kin[`= 24 + `i'', 1] = avg
	matrix kin[`= 32 + `i'', 1] = avg
	
	* Calculate the mean of ext_num for the subset where eth == i
    sum nuclear_num if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 4 + `i'', 1] = avg
	matrix kin[`= 12 + `i'', 1] = avg
	matrix kin[`= 20 + `i'', 1] = avg
	matrix kin[`= 28 + `i'', 1] = avg
	matrix kin[`= 36 + `i'', 1] = avg
	
	}

* predicted average number of kin in each class by race
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(40,1,.)
forvalues i = 1/40 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "eth.l" 	A2 = matrix(race) 		///
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




mlogit class i.kin_cat_xs##i.eth i.female [pweight = dwe],  vce(cluster anc_id)
est store mlogit_xs_race





** PAIRWISE CONTRASTS for kin comparison (kin cat XS)

* run margins with pwcompare for each cluster and kin cat separately 
* and store results in matrix 'table_ck'
forvalues k = 0/2 {
	forvalues c = 1/5 {
	
	est restore mlogit_xs_race
	margins kin_cat_xs#eth if kin_cat_xs == `k' [pweight = dwe],	///
		predict(outcome(`c'))										///
		pwcompare(pv groups) post
	est store iact1_xs`c'`k'
	
	matrix table`c'`k' = r(table_vs)
	
	* FOR TABLE
	* Store the pvalues from table_kc (row 4, col x) in a matrix on diagonals (pval_kc)
	matrix pval`c'`k' = J(4,4,.)
    matrix pval`c'`k'[1,2] = table`c'`k'[4,1]	// W vs H
	matrix pval`c'`k'[1,3] = table`c'`k'[4,2]	// W vs B
	matrix pval`c'`k'[1,4] = table`c'`k'[4,3]	// W vs A
	matrix pval`c'`k'[2,3] = table`c'`k'[4,4]	// H vs B
	matrix pval`c'`k'[2,4] = table`c'`k'[4,5]	// H vs A
	matrix pval`c'`k'[3,4] = table`c'`k'[4,6]	// B vs A

}
}



* set up new excel sheet
putexcel set $WD/data/pvals_table, sheet("xs") replace 

* store numbering for kin as rows and columns
mat eth_lab = (1\2\3\4)
mat eth_lab_w = eth_lab'

* setup rows and columns (labels)
putexcel 	B1 = 	"Nuclear" 				B2  = 	"$cl1" F2 = 	"$cl2" 	/// 
			B10 = 	"Extended-nuclear"		B11 = 	"$cl1" F11 = 	"$cl2" 	/// 
			B19 = 	"Extended"				B20 = 	"$cl1" F20 = 	"$cl2" 	/// 
			///
			J2  = 	"$cl3" N2 = 	"$cl4"	R2 = 	"$cl5" 	/// 
			J11 = 	"$cl3" N11 = 	"$cl4"	R11 = 	"$cl5" 	/// 
			J20 = 	"$cl3" N20 = 	"$cl4"	R20 = 	"$cl5" 	/// 
			///
			A4 		= matrix(eth_lab)	B3 		= matrix(eth_lab_w)	 	///	
			A13		= matrix(eth_lab)	B12		= matrix(eth_lab_w)	 	///	
			A22		= matrix(eth_lab)	B21		= matrix(eth_lab_w)	 	///	
			///
			F3 		= matrix(eth_lab_w)	J3 		= matrix(eth_lab_w)	 	///	
			F12		= matrix(eth_lab_w)	J12		= matrix(eth_lab_w)	 	///	
			F21		= matrix(eth_lab_w)	J21		= matrix(eth_lab_w)	 	///	
			///
			N3 		= matrix(eth_lab_w)	R3 		= matrix(eth_lab_w)	 	///	
			N12		= matrix(eth_lab_w)	R12		= matrix(eth_lab_w)	 	///	
			N21		= matrix(eth_lab_w)	R21		= matrix(eth_lab_w)	 	///	
			, nformat(#)

			
* merge subheader columns
putexcel	(B1 	:U1 	)	(B2:E2)		(F2:I2)		(J2:M2)		(N2:Q2)		(R2:U2)		///
			(B10	:U10	)	(B11 :E11 )	(F11 :I11 )	(J11 :M11 )	(N11 :Q11 )	(R11 :U11 )	///
            (B19	:U19	)	(B20 :E20 )	(F20 :I20 )	(J20 :M20 )	(N20 :Q20 )	(R20 :U20 )	///
            , merge hcenter vcenter
			
* fill values
putexcel	B4		= matrix(pval10)		F4		= matrix(pval20)	/// Classes 1 & 2
			B13		= matrix(pval11)		F13		= matrix(pval21)	///
			B22		= matrix(pval12)		F22		= matrix(pval22)	///
			///
			J4		= matrix(pval30)		N4		= matrix(pval40)	/// Classes 3 & 4
			J13		= matrix(pval31)		N13		= matrix(pval41)	///
			J22		= matrix(pval32)		N22		= matrix(pval42)	///
			///
			R4		= matrix(pval50)		/// Class 5
			R13		= matrix(pval51)		///
			R22		= matrix(pval52)		///
			, nformat(#.00)


			
			

			
			
* PREDICTED PROBABILITIES (kin cat XS)


est restore mlogit_xs_race
margins kin_cat_xs#eth if eth != 5 [pweight = dwe]
est store iact1_xs

* store results in table
matrix table = J(60,9,.)	// 60 cominations of kin (3) x class (5) x race (4)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_xs, sheet("predprobs") replace 

* cols for race (1x60 -> race/ethn (1-4) repeated kin-times (3) in each class (5))
matrix race = J(60,1,.)
matrix race = (	1\2\3\4\1\2\3\4\1\2\3\4\	/// class1
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class2
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class3
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class4
				1\2\3\4\1\2\3\4\1\2\3\4)	/// class5


* cols for kincat (1x60 -> each kin (0-2) repeated race-times (4) in each class (5))
matrix kincat = J(60,1,.)
matrix kincat = (	0\0\0\0\1\1\1\1\2\2\2\2\	/// class1
					0\0\0\0\1\1\1\1\2\2\2\2\	/// class2
					0\0\0\0\1\1\1\1\2\2\2\2\	/// class3
					0\0\0\0\1\1\1\1\2\2\2\2\	/// class4
					0\0\0\0\1\1\1\1\2\2\2\2)	/// class5
				
* cols for class (1x60 -> each class (1-5) repeated race x kin-times (4x3))
matrix class = J(60,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				5\5\5\5\5\5\5\5\5\5\5\5) 	//	class5
				


* store predicted probs in pp
matrix pp = J(60,1,.)
forvalues i = 1/60 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(60,1,.)
forvalues i = 1/60 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(60,1,.)
forvalues i = 1/60 {
    matrix lb[`i', 1] = table[`i', 5]
}

* store average kin counts per race/ethnicity in kin
matrix kin = J(60,1,.)

forvalues i = 1/4 {	
    * Calculate the mean of nuclear_num for the subset where eth == i
    sum num_xs_0 if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0 + `i'', 1] = avg
	matrix kin[`= 12 + `i'', 1] = avg
	matrix kin[`= 24 + `i'', 1] = avg
	matrix kin[`= 36 + `i'', 1] = avg
	matrix kin[`= 48 + `i'', 1] = avg
	
	* Calculate the mean of ext_num for the subset where eth == i
    sum num_xs_1 if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 4 + `i'', 1] = avg
	matrix kin[`= 16 + `i'', 1] = avg
	matrix kin[`= 28 + `i'', 1] = avg
	matrix kin[`= 40 + `i'', 1] = avg
	matrix kin[`= 52 + `i'', 1] = avg
	
	* Calculate the mean of ext_num for the subset where eth == i
    sum num_xs_2 if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 8 + `i'', 1] = avg
	matrix kin[`= 20 + `i'', 1] = avg
	matrix kin[`= 32 + `i'', 1] = avg
	matrix kin[`= 44 + `i'', 1] = avg
	matrix kin[`= 56 + `i'', 1] = avg
	
	}

* predicted average number of kin in each class by race
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(60,1,.)
forvalues i = 1/60 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "eth.l" 	A2 = matrix(race) 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)


			
			
			
			
			
*** KIN CAT SMALL



mlogit class i.kin_cat_small##i.eth i.female [pweight = dwe], vce(cluster anc_id)
est store mlogit_s_race


est restore mlogit_s_race
margins kin_cat_small#eth if eth != 5 [pweight = dwe]
est store iact1_s

* store results in table
matrix table = J(120,9,.)	// 120 cominations of kin (6) x class (5) x race (4)
matrix table = r(table)'

* set up new excel sheet
putexcel set $WD/data/predprobs_s, sheet("predprobs") replace 

* cols for race (1x120 -> race/ethn (1-4) repeated kin-times (6) in each class (5))
matrix race = J(120,1,.)
matrix race = (	1\2\3\4\1\2\3\4\1\2\3\4\	/// class1
				1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class2
				1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class3
				1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class4
				1\2\3\4\1\2\3\4\1\2\3\4\	///	
				1\2\3\4\1\2\3\4\1\2\3\4\	/// class5
				1\2\3\4\1\2\3\4\1\2\3\4)	// 	
			
				
* cols for kincat (1x120 -> each kin (0-6) repeated race-times (4) in each class (5))
matrix kincat = J(120,1,.)
matrix kincat = (	1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class1
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class2
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class3
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6\	/// class4
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\6\6\6\6)	/// class5
				
* cols for class (1x120 -> each class (1-5) repeated race x kin-times (4x10))
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

* store average kin counts per race/ethnicity in kin
cap drop num_s_*
gen num_s_1 = num_m_1 + num_m_2
gen num_s_2 = num_m_3 + num_m_4
gen num_s_3 = num_m_5 + num_m_6
gen num_s_4 = num_m_7
gen num_s_5 = num_m_8 + num_m_9
gen num_s_6 = num_m_10

matrix kin = J(120,1,.)

	
forvalues i = 1/4 {		// run through race/ethnicity
	forvalues e = 0/5 {	// run through kin - 1
	    
	
	* Calculate the mean of each kin for the subset where eth == i
    global x = `e' + 1 // to get real kin numbering
	sum num_s_$x if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0  + `e' * 4 + `i'', 1] = avg	// class 1
	matrix kin[`= 24 + `e' * 4 + `i'', 1] = avg	// class 2
	matrix kin[`= 48 + `e' * 4 + `i'', 1] = avg	// class 3
	matrix kin[`= 72 + `e' * 4 + `i'', 1] = avg	// class 4
	matrix kin[`= 96 + `e' * 4 + `i'', 1] = avg	// class 5
	}
	}
	

* predicted average number of kin in each class by race
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(120,1,.)
forvalues i = 1/120 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "eth.l" 	A2 = matrix(race) 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)



			

			
			
			
			
			
			
*** KIN CAT MEDIUM



mlogit class i.kin_cat_med##i.eth i.female [pweight = dwe], vce(cluster anc_id)
est store mlogit_m_race


est restore mlogit_m_race
margins kin_cat_med#eth if eth != 5 [pweight = dwe]
est store iact1_m

* store results in table
matrix table = J(200,9,.)	// 200 cominations of kin (10) x class (5) x race (4)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_m, sheet("predprobs") replace 

* cols for race (1x200 -> race/ethn (1-4) repeated kin-times (10) in each class (5))
matrix race = J(200,1,.)
matrix race = (	1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class1
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class2
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class3
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class4
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	///	
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class5
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4)	// 	
			
				
* cols for kincat (1x200 -> each kin (0-10) repeated race-times (4) in each class (5))
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
					
				
* cols for class (200 -> each class (1-5) repeated race x kin-times (4x10))
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
	
	* Calculate the mean of each kin for the subset where eth == i
    global x = `e' + 1
	
	sum num_m_$x if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + `e' * 4 + `i'', 1] = avg	// class 1
	matrix kin[`= 40  + `e' * 4 + `i'', 1] = avg	// class 2
	matrix kin[`= 80  + `e' * 4 + `i'', 1] = avg	// class 3
	matrix kin[`= 120 + `e' * 4 + `i'', 1] = avg	// class 4
	matrix kin[`= 160 + `e' * 4 + `i'', 1] = avg	// class 5
}
}


* predicted average number of kin in each class by race
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(200,1,.)
forvalues i = 1/200 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "eth.l" 	A2 = matrix(race) 		///
			B1 = "kincat" 	B2 = matrix(kincat) 	/// 
			C1 = "class" 	C2 = matrix(class) 		///
			D1 = "pp"		D2 = matrix(pp) 		///
			E1 = "avg_kin"	E2 = matrix(kin)		///
			F1 = "pred_num"	F2 = matrix(pn)			///
			G1 = "lb"		G2 = matrix(lb)			///
			H1 = "ub"		H2 = matrix(ub)			///
			, nformat(#.00)


			
			
			
			
			
			
*** KIN CAT LARGE 

* PREDICTED PROBABILITIES (kin cat large)

est restore mlogit_l_race
margins kin_cat_l#eth if eth != 5 [pweight = dwe]
est store iact1_l

* store results in table
matrix table = J(320,9,.)	// 320 cominations of kin (16) x class (5) x race (4)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_l, sheet("predprobs") replace 

* cols for race (1x320 -> race/ethn (1-4) repeated kin-times (16) in each class (5))
matrix race = J(320,1,.)
matrix race = (	1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class1
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class2
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class3
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// 
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class4
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	///	
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\	/// class5
				1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4\1\2\3\4)	// 	
			
				
* cols for kincat (1x320 -> each kin (0-16) repeated race-times (4) in each class (5))
matrix kincat = J(320,1,.)
matrix kincat = (	1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class1 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					11\11\11\11\12\12\12\12\13\13\13\13\			///
					14\14\14\14\15\15\15\15\16\16\16\16\			///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class2
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					11\11\11\11\12\12\12\12\13\13\13\13\			///
					14\14\14\14\15\15\15\15\16\16\16\16\			///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class3 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					11\11\11\11\12\12\12\12\13\13\13\13\			///
					14\14\14\14\15\15\15\15\16\16\16\16\			///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class4 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					11\11\11\11\12\12\12\12\13\13\13\13\			///
					14\14\14\14\15\15\15\15\16\16\16\16\			///
					1\1\1\1\2\2\2\2\3\3\3\3\4\4\4\4\5\5\5\5\		/// class5 
					6\6\6\6\7\7\7\7\8\8\8\8\9\9\9\9\10\10\10\10\	///
					11\11\11\11\12\12\12\12\13\13\13\13\			///
					14\14\14\14\15\15\15\15\16\16\16\16)			///


				
* cols for class (1x320 -> each class (1-5) repeated race x kin-times (4x16)
matrix class = J(320,1,.)
matrix class = (1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	/// class1
				1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\ 	///
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	/// class2
				2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\	///
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	/// class3
				3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\	///
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	/// class4
				4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\4\	///
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\ 	///	class5
				5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5\5)


* store predicted probs in pp 
matrix pp = J(320,1,.)
forvalues i = 1/320 {
    matrix pp[`i', 1] = table[`i', 1]
}

* store upper bound CI in ub
matrix ub = J(320,1,.)
forvalues i = 1/320 {
    matrix ub[`i', 1] = table[`i', 6]
}

* store lower bound CI in lb
matrix lb = J(320,1,.)
forvalues i = 1/320 {
    matrix lb[`i', 1] = table[`i', 5]
}


matrix kin = J(320,1,.)

forvalues i = 1/4 {	
	forvalues e = 0/15 {
	
	* Calculate the mean of each kin for the subset where eth == i
    global x = `e' + 1
	sum num_$x if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + `e' * 4 + `i'', 1] = avg	// class 1
	matrix kin[`= 64  + `e' * 4 + `i'', 1] = avg	// class 2
	matrix kin[`= 128 + `e' * 4 + `i'', 1] = avg	// class 3
	matrix kin[`= 192 + `e' * 4 + `i'', 1] = avg	// class 4
	matrix kin[`= 256 + `e' * 4 + `i'', 1] = avg	// class 5
}
}


* predicted average number of kin in each class by race
* multiply predicted probs with average number of kin (element-wise)
matrix pn = J(320,1,.)
forvalues i = 1/320 {
    matrix pn[`i', 1] = pp[`i', 1] * kin[`i', 1]
}


* store results in excel

putexcel 	A1 = "eth.l" 	A2 = matrix(race) 		///
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
	
	est restore mlogit_l_race
	margins kin_cat_l#eth if kin_cat_l == `k' [pweight = dwe],	///
		predict(outcome(`c'))	///
		pwcompare(pv groups) post
	est store iact1_l`c'`k'
	
	matrix table`c'`k' = r(table_vs)
	
	* FOR TABLE
	* Store the pvalues from table_kc (row 4, col x) in a matrix on diagonals (pval_kc)
	matrix pval`c'`k' = J(4,4,.)
    matrix pval`c'`k'[1,2] = table`c'`k'[4,1]	// W vs H
	matrix pval`c'`k'[1,3] = table`c'`k'[4,2]	// W vs B
	matrix pval`c'`k'[1,4] = table`c'`k'[4,3]	// W vs A
	matrix pval`c'`k'[2,3] = table`c'`k'[4,4]	// H vs B
	matrix pval`c'`k'[2,4] = table`c'`k'[4,5]	// H vs A
	matrix pval`c'`k'[3,4] = table`c'`k'[4,6]	// B vs A


}
}



* set up new excel sheet
putexcel set $WD/data/pvals_table, sheet("large") modify 

mat eth_lab = (1\2\3\4)
mat eth_lab_w = eth_lab'

* setup rows and columns
putexcel 	B1 = 	"Father" 				B2  = 	"$cl1" F2 = 	"$cl2" 	/// 
			B10 = 	"Mother"				B11 = 	"$cl1" F11 = 	"$cl2" 	/// 
			B19 = 	"Brother"				B20 = 	"$cl1" F20 = 	"$cl2" 	/// 
			B28 = 	"Sister"				B29 = 	"$cl1" F29 = 	"$cl2" 	/// 
			B37 = 	"Paternal grandfather"	B38 = 	"$cl1" F38 = 	"$cl2" 	/// 
			B46 = 	"Maternal grandfather"	B47 = 	"$cl1" F47 = 	"$cl2" 	/// 
			B55 = 	"Paternal grandmother"	B56 = 	"$cl1" F56 = 	"$cl2" 	/// 
			B64 = 	"Maternal grandmother"	B65 = 	"$cl1" F65 = 	"$cl2" 	/// 
			B73 = 	"Paternal halfsibling"	B74 = 	"$cl1" F74 = 	"$cl2" 	/// 
			B82 = 	"Maternal halfsibling"	B83 = 	"$cl1" F83 = 	"$cl2" 	/// 
			B91 = 	"Paternal uncle"		B92 = 	"$cl1" F92 = 	"$cl2" 	/// 
			B100 =	"Maternal uncle"		B101 = 	"$cl1" F101 = 	"$cl2" 	/// 
			B109 =	"Paternal aunt"			B110 = 	"$cl1" F110 = 	"$cl2" 	/// 
			B118 =	"Maternal aunt"			B119 = 	"$cl1" F119 = 	"$cl2" 	/// 
			B127 =	"Paternal cousin"		B128 = 	"$cl1" F128 = 	"$cl2" 	/// 
			B136 = 	"Maternal cousin"		B137 = 	"$cl1" F137 = 	"$cl2" 	/// 
			///
			J2  = 	"$cl3" N2 = 	"$cl4"	R2 = 	"$cl5" 	/// 
			J11 = 	"$cl3" N11 = 	"$cl4"	R11 = 	"$cl5" 	/// 
			J20 = 	"$cl3" N20 = 	"$cl4"	R20 = 	"$cl5" 	/// 
			J29 = 	"$cl3" N29 = 	"$cl4"	R29 = 	"$cl5" 	/// 
			J38 = 	"$cl3" N38 = 	"$cl4"	R38 = 	"$cl5" 	/// 
			J47 = 	"$cl3" N47 = 	"$cl4"	R47 = 	"$cl5" 	/// 
			J56 = 	"$cl3" N56 = 	"$cl4"	R56 = 	"$cl5" 	/// 
			J65 = 	"$cl3" N65 = 	"$cl4"	R65 = 	"$cl5" 	/// 
			J74 = 	"$cl3" N74 = 	"$cl4"	R74 = 	"$cl5" 	/// 
			J83 = 	"$cl3" N83 = 	"$cl4"	R83 = 	"$cl5" 	/// 
			J92 = 	"$cl3" N92 = 	"$cl4"	R92 = 	"$cl5" 	/// 
			J101 = 	"$cl3" N101 = 	"$cl4"	R101 = 	"$cl5" 	/// 
			J110 = 	"$cl3" N110 = 	"$cl4"	R110 = 	"$cl5" 	/// 
			J119 = 	"$cl3" N119 = 	"$cl4"	R119 = 	"$cl5" 	/// 
			J128 = 	"$cl3" N128 = 	"$cl4"	R128 = 	"$cl5" 	/// 
			J137 = 	"$cl3" N137 = 	"$cl4"	R137 = 	"$cl5" 	/// 
			///
			A4 		= matrix(eth_lab)	B3 		= matrix(eth_lab_w)	 	///	
			A13		= matrix(eth_lab)	B12		= matrix(eth_lab_w)	 	///	
			A22		= matrix(eth_lab)	B21		= matrix(eth_lab_w)	 	///	
			A31		= matrix(eth_lab)	B30		= matrix(eth_lab_w)	 	///	
			A40		= matrix(eth_lab)	B39		= matrix(eth_lab_w)	 	///	
			A49		= matrix(eth_lab)	B48		= matrix(eth_lab_w)	 	///	
			A58		= matrix(eth_lab)	B57		= matrix(eth_lab_w)	 	///	
			A67		= matrix(eth_lab)	B66		= matrix(eth_lab_w)	 	///	
			A76		= matrix(eth_lab)	B75		= matrix(eth_lab_w)	 	///	
			A85		= matrix(eth_lab)	B84		= matrix(eth_lab_w)	 	///	
			A94		= matrix(eth_lab)	B93		= matrix(eth_lab_w)	 	///	
			A103	= matrix(eth_lab)	B102	= matrix(eth_lab_w) 	///
			A112	= matrix(eth_lab)	B111	= matrix(eth_lab_w) 	///
			A121	= matrix(eth_lab)	B120	= matrix(eth_lab_w) 	///
			A130	= matrix(eth_lab)	B129	= matrix(eth_lab_w) 	///
			A139	= matrix(eth_lab)	B138	= matrix(eth_lab_w) 	///	
			///
			F3 		= matrix(eth_lab_w)	J3 		= matrix(eth_lab_w)	 	///	
			F12		= matrix(eth_lab_w)	J12		= matrix(eth_lab_w)	 	///	
			F21		= matrix(eth_lab_w)	J21		= matrix(eth_lab_w)	 	///	
			F30		= matrix(eth_lab_w)	J30		= matrix(eth_lab_w)	 	///	
			F39		= matrix(eth_lab_w)	J39		= matrix(eth_lab_w)	 	///	
			F48		= matrix(eth_lab_w)	J48		= matrix(eth_lab_w)	 	///	
			F57		= matrix(eth_lab_w)	J57		= matrix(eth_lab_w)	 	///	
			F66		= matrix(eth_lab_w)	J66		= matrix(eth_lab_w)	 	///	
			F75		= matrix(eth_lab_w)	J75		= matrix(eth_lab_w)	 	///	
			F84		= matrix(eth_lab_w)	J84		= matrix(eth_lab_w)	 	///	
			F93		= matrix(eth_lab_w)	J93		= matrix(eth_lab_w)	 	///	
			F102	= matrix(eth_lab_w)	J102	= matrix(eth_lab_w) 	///
			F111	= matrix(eth_lab_w)	J111	= matrix(eth_lab_w) 	///
			F120	= matrix(eth_lab_w)	J120	= matrix(eth_lab_w) 	///
			F129	= matrix(eth_lab_w)	J129	= matrix(eth_lab_w) 	///
			F138	= matrix(eth_lab_w)	J138	= matrix(eth_lab_w) 	///	
			///
			N3 		= matrix(eth_lab_w)	R3 		= matrix(eth_lab_w)	 	///	
			N12		= matrix(eth_lab_w)	R12		= matrix(eth_lab_w)	 	///	
			N21		= matrix(eth_lab_w)	R21		= matrix(eth_lab_w)	 	///	
			N30		= matrix(eth_lab_w)	R30		= matrix(eth_lab_w)	 	///	
			N39		= matrix(eth_lab_w)	R39		= matrix(eth_lab_w)	 	///	
			N48		= matrix(eth_lab_w)	R48		= matrix(eth_lab_w)	 	///	
			N57		= matrix(eth_lab_w)	R57		= matrix(eth_lab_w)	 	///	
			N66		= matrix(eth_lab_w)	R66		= matrix(eth_lab_w)	 	///	
			N75		= matrix(eth_lab_w)	R75		= matrix(eth_lab_w)	 	///	
			N84		= matrix(eth_lab_w)	R84		= matrix(eth_lab_w)	 	///	
			N93		= matrix(eth_lab_w)	R93		= matrix(eth_lab_w)	 	///	
			N102	= matrix(eth_lab_w)	R102	= matrix(eth_lab_w) 	///
			N111	= matrix(eth_lab_w)	R111	= matrix(eth_lab_w) 	///
			N120	= matrix(eth_lab_w)	R120	= matrix(eth_lab_w) 	///
			N129	= matrix(eth_lab_w)	R129	= matrix(eth_lab_w) 	///
			N138	= matrix(eth_lab_w)	R138	= matrix(eth_lab_w) 	///	
			, nformat(#)

			
* merge subheader columns
putexcel	(B1 	:U1 	)	(B2:E2)		(F2:I2)		(J2:M2)		(N2:Q2)		(R2:U2)		///
			(B10	:U10	)	(B11 :E11 )	(F11 :I11 )	(J11 :M11 )	(N11 :Q11 )	(R11 :U11 )	///
            (B19	:U19	)	(B20 :E20 )	(F20 :I20 )	(J20 :M20 )	(N20 :Q20 )	(R20 :U20 )	///
            (B28	:U28	)	(B29 :E29 )	(F29 :I29 )	(J29 :M29 )	(N29 :Q29 )	(R29 :U29 )	///
            (B37	:U37	)	(B38 :E38 )	(F38 :I38 )	(J38 :M38 )	(N38 :Q38 )	(R38 :U38 )	///
            (B46	:U46	)	(B47 :E47 )	(F47 :I47 )	(J47 :M47 )	(N47 :Q47 )	(R47 :U47 )	///
            (B55	:U55	)	(B56 :E56 )	(F56 :I56 )	(J56 :M56 )	(N56 :Q56 )	(R56 :U56 )	///
            (B64	:U64	)	(B65 :E65 )	(F65 :I65 )	(J65 :M65 )	(N65 :Q65 )	(R65 :U65 )	///
            (B73	:U73	)	(B74 :E74 )	(F74 :I74 )	(J74 :M74 )	(N74 :Q74 )	(R74 :U74 )	///
            (B82	:U82	)	(B83 :E83 )	(F83 :I83 )	(J83 :M83 )	(N83 :Q83 )	(R83 :U83 )	///
            (B91	:U91	)	(B92 :E92 )	(F92 :I92 )	(J92 :M92 )	(N92 :Q92 )	(R92 :U92 )	///
            (B100	:U100	)	(B101:E101)	(F101:I101)	(J101:M101)	(N101:Q101)	(R101:U101)	///
            (B109	:U109	)	(B110:E110)	(F110:I110)	(J110:M110)	(N110:Q110)	(R110:U110)	///
            (B118	:U118	)	(B119:E119)	(F119:I119)	(J119:M119)	(N119:Q119)	(R119:U119)	///
            (B127	:U127	)	(B128:E128)	(F128:I128)	(J128:M128)	(N128:Q128)	(R128:U128)	///
            (B136	:U136	)	(B137:E137)	(F137:I137)	(J137:M137)	(N137:Q137)	(R137:U137)	///
			, merge hcenter vcenter
			
* fill values
putexcel	B4		= matrix(pval11)		F4		= matrix(pval21)	/// Classes 1 & 2
			B13		= matrix(pval12)		F13		= matrix(pval22)	///
			B22		= matrix(pval13)		F22		= matrix(pval23)	///
			B31		= matrix(pval14)		F31		= matrix(pval24)	///
			B40		= matrix(pval15)		F40		= matrix(pval25)	///
			B49		= matrix(pval16)		F49		= matrix(pval26)	///
			B58		= matrix(pval17)		F58		= matrix(pval27)	///
			B67		= matrix(pval18)		F67		= matrix(pval28)	///
			B76		= matrix(pval19)		F76		= matrix(pval29)	///
			B85		= matrix(pval110)		F85		= matrix(pval210)	///
			B94		= matrix(pval111)		F94		= matrix(pval211)	///
			B103	= matrix(pval112)		F103	= matrix(pval212)	///
			B112	= matrix(pval113)		F112	= matrix(pval213)	///
			B121	= matrix(pval114)		F121	= matrix(pval214)	///
			B130	= matrix(pval115)		F130	= matrix(pval215)	///
			B139	= matrix(pval116)		F139	= matrix(pval216)	///
			///
			J4		= matrix(pval31)		N4		= matrix(pval41)	/// Classes 3 & 4
			J13		= matrix(pval32)		N13		= matrix(pval42)	///
			J22		= matrix(pval33)		N22		= matrix(pval43)	///
			J31		= matrix(pval34)		N31		= matrix(pval44)	///
			J40		= matrix(pval35)		N40		= matrix(pval45)	///
			J49		= matrix(pval36)		N49		= matrix(pval46)	///
			J58		= matrix(pval37)		N58		= matrix(pval47)	///
			J67		= matrix(pval38)		N67		= matrix(pval48)	///
			J76		= matrix(pval39)		N76		= matrix(pval49)	///
			J85		= matrix(pval310)		N85		= matrix(pval410)	///
			J94		= matrix(pval311)		N94		= matrix(pval411)	///
			J103	= matrix(pval312)		N103	= matrix(pval412)	///
			J112	= matrix(pval313)		N112	= matrix(pval413)	///
			J121	= matrix(pval314)		N121	= matrix(pval414)	///
			J130	= matrix(pval315)		N130	= matrix(pval415)	///
			J139	= matrix(pval316)		N139	= matrix(pval416)	///
			///
			R4		= matrix(pval51)		/// Class 5
			R13		= matrix(pval52)		///
			R22		= matrix(pval53)		///
			R31		= matrix(pval54)		///
			R40		= matrix(pval55)		///
			R49		= matrix(pval56)		///
			R58		= matrix(pval57)		///
			R67		= matrix(pval58)		///
			R76		= matrix(pval59)		///
			R85		= matrix(pval510)		///
			R94		= matrix(pval511)		///
			R103	= matrix(pval512)		///
			R112	= matrix(pval513)		///
			R121	= matrix(pval514)		///
			R130	= matrix(pval515)		///
			R139	= matrix(pval516)		///	
			, nformat(#.00)

			
			
			
			
* KIN CAT LARGE (APPENDIX TABLE)


* store pp for each kincat * eth-combination
forvalues i = 1/16 {
	forvalues e = 1/4 {
		
	est restore mlogit_l_race
	margins `i'b.kin_cat_l#eth if eth == `e' [pweight = dwe]
	est store iact_`e'`i'

	matrix pp`e'`i' = r(b)'
	}
}

* store avg_kin for each kincat * eth-combination

* Calculate the mean of FATHER for the subset where eth == i
forvalues i = 1/16 {
	forvalues e = 1/4 {
	
	* store mean of kin_cat for each ethnicity
	sum num_`i' if help == 1 & eth == `e'
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
putexcel 	B1 = 	"Father" 				B2  = 	"$pp_lab" F2 = 		"$avg_lab" 	/// 
			B10 = 	"Mother"				B11 = 	"$pp_lab" F11 = 	"$avg_lab" 	/// 
			B19 = 	"Brother"				B20 = 	"$pp_lab" F20 = 	"$avg_lab" 	/// 
			B28 = 	"Sister"				B29 = 	"$pp_lab" F29 = 	"$avg_lab" 	/// 
			B37 = 	"Paternal grandfather"	B38 = 	"$pp_lab" F38 = 	"$avg_lab" 	/// 
			B46 = 	"Maternal grandfather"	B47 = 	"$pp_lab" F47 = 	"$avg_lab" 	/// 
			B55 = 	"Paternal grandmother"	B56 = 	"$pp_lab" F56 = 	"$avg_lab" 	/// 
			B64 = 	"Maternal grandmother"	B65 = 	"$pp_lab" F65 = 	"$avg_lab" 	/// 
			B73 = 	"Paternal halfsibling"	B74 = 	"$pp_lab" F74 = 	"$avg_lab" 	/// 
			B82 = 	"Maternal halfsibling"	B83 = 	"$pp_lab" F83 = 	"$avg_lab" 	/// 
			B91 = 	"Paternal uncle"		B92 = 	"$pp_lab" F92 = 	"$avg_lab" 	/// 
			B100 =	"Maternal uncle"		B101 = 	"$pp_lab" F101 = 	"$avg_lab" 	/// 
			B109 =	"Paternal aunt"			B110 = 	"$pp_lab" F110 = 	"$avg_lab" 	/// 
			B118 =	"Maternal aunt"			B119 = 	"$pp_lab" F119 = 	"$avg_lab" 	/// 
			B127 =	"Paternal cousin"		B128 = 	"$pp_lab" F128 = 	"$avg_lab" 	/// 
			B136 = 	"Maternal cousin"		B137 = 	"$pp_lab" F137 = 	"$avg_lab" 	/// 
			B3 		= matrix(eth_lab_w)	F3 		= matrix(eth_lab_w)	///		
			B12		= matrix(eth_lab_w)	F12		= matrix(eth_lab_w)	///		
			B21		= matrix(eth_lab_w)	F21		= matrix(eth_lab_w)	///		
			B30		= matrix(eth_lab_w)	F30		= matrix(eth_lab_w)	///		
			B39		= matrix(eth_lab_w)	F39		= matrix(eth_lab_w)	///		
			B48		= matrix(eth_lab_w)	F48		= matrix(eth_lab_w)	///		
			B57		= matrix(eth_lab_w)	F57		= matrix(eth_lab_w)	///		
			B66		= matrix(eth_lab_w)	F66		= matrix(eth_lab_w)	///		
			B75		= matrix(eth_lab_w)	F75		= matrix(eth_lab_w)	///		
			B84		= matrix(eth_lab_w)	F84		= matrix(eth_lab_w)	///		
			B93		= matrix(eth_lab_w)	F93		= matrix(eth_lab_w)	///		
			B102	= matrix(eth_lab_w) F102	= matrix(eth_lab_w)	///	
			B111	= matrix(eth_lab_w) F111	= matrix(eth_lab_w)	///	
			B120	= matrix(eth_lab_w) F120	= matrix(eth_lab_w)	///	
			B129	= matrix(eth_lab_w) F129	= matrix(eth_lab_w)	///	
			B138	= matrix(eth_lab_w) F138	= matrix(eth_lab_w)	///	
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
putexcel	(B1 	:I1 	)	(B2:E2)		(F2:I2)		///
			(B10	:I10	)	(B11 :E11 )	(F11 :I11 )	///
            (B19	:I19	)	(B20 :E20 )	(F20 :I20 )	///
            (B28	:I28	)	(B29 :E29 )	(F29 :I29 )	///
            (B37	:I37	)	(B38 :E38 )	(F38 :I38 )	///
            (B46	:I46	)	(B47 :E47 )	(F47 :I47 )	///
            (B55	:I55	)	(B56 :E56 )	(F56 :I56 )	///
            (B64	:I64	)	(B65 :E65 )	(F65 :I65 )	///
            (B73	:I73	)	(B74 :E74 )	(F74 :I74 )	///
            (B82	:I82	)	(B83 :E83 )	(F83 :I83 )	///
            (B91	:I91	)	(B92 :E92 )	(F92 :I92 )	///
            (B100	:I100	)	(B101:E101)	(F101:I101)	///
            (B109	:I109	)	(B110:E110)	(F110:I110)	///
            (B118	:I118	)	(B119:E119)	(F119:I119)	///
            (B127	:I127	)	(B128:E128)	(F128:I128)	///
            (B136	:I136	)	(B137:E137)	(F137:I137)	, merge hcenter vcenter
	

* fill values (pp*eth*kin)
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
			F4		= matrix(pn11)		G4		= matrix(pn21)	/// AVG_KIN
			F13		= matrix(pn12)		G13		= matrix(pn22)	///
			F22		= matrix(pn13)		G22		= matrix(pn23)	///
			F31		= matrix(pn14)		G31		= matrix(pn24)	///
			F40		= matrix(pn15)		G40		= matrix(pn25)	///
			F49		= matrix(pn16)		G49		= matrix(pn26)	///
			F58		= matrix(pn17)		G58		= matrix(pn27)	///
			F67		= matrix(pn18)		G67		= matrix(pn28)	///
			F76		= matrix(pn19)		G76		= matrix(pn29)	///
			F85		= matrix(pn110)		G85		= matrix(pn210)	///
			F94		= matrix(pn111)		G94		= matrix(pn211)	///
			F103	= matrix(pn112)		G103	= matrix(pn212)	///
			F112	= matrix(pn113)		G112	= matrix(pn213)	///
			F121	= matrix(pn114)		G121	= matrix(pn214)	///
			F130	= matrix(pn115)		G130	= matrix(pn215)	///
			F139	= matrix(pn116)		G139	= matrix(pn216)	///
			///
			H4		= matrix(pn31)		I4		= matrix(pn41)	/// AVG_KIN
			H13		= matrix(pn32)		I13		= matrix(pn42)	///
			H22		= matrix(pn33)		I22		= matrix(pn43)	///
			H31		= matrix(pn34)		I31		= matrix(pn44)	///
			H40		= matrix(pn35)		I40		= matrix(pn45)	///
			H49		= matrix(pn36)		I49		= matrix(pn46)	///
			H58		= matrix(pn37)		I58		= matrix(pn47)	///
			H67		= matrix(pn38)		I67		= matrix(pn48)	///
			H76		= matrix(pn39)		I76		= matrix(pn49)	///
			H85		= matrix(pn310)		I85		= matrix(pn410)	///
			H94		= matrix(pn311)		I94		= matrix(pn411)	///
			H103	= matrix(pn312)		I103	= matrix(pn412)	///
			H112	= matrix(pn313)		I112	= matrix(pn413)	///
			H121	= matrix(pn314)		I121	= matrix(pn414)	///
			H130	= matrix(pn315)		I130	= matrix(pn415)	///
			H139	= matrix(pn316)		I139	= matrix(pn416)	///
			, nformat(#.00) 

			
			
*** last line ***
