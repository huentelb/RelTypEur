*-------------------------------------------------------------------------------

* A typology of nuclear and extended family relations in the United States
* Bettina Hünteler
* 23.02.2024
* huenteler@wiso.uni-koeln.de

*** 09 PREDICTED PROBABILITIES *** 

*-------------------------------------------------------------------------------


*** Set (up) working directories ***

/*** folders
global WD 	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses"

global IN 	"/Users/Bettina/Documents/datasets/KINMATRIX"
global OUT 	"/Users/Bettina/Documents/datasets/KINMATRIX"

global DO 	"$WD/code"
global LOG	"$WD/log"
global GR	"$WD/graphs"

global M	"/Users/Bettina/sciebo/projects/Kinmatrix/analyses/graphs/241014/M11_median/lc5_output/ame_weighted"

*** preparations/settings
set more off, perm
set scheme white_tableau, perm //  rainbow
set showbaselevels on
*/





use $WD/data/lc_analytical.dta, clear


global eth1 "White"
global eth2 "Hispanic"
global eth3 "Black"
global eth4 "Asian"
global eth5 "Other"


*tab class, gen(clust)



************************
***		 OVERALL	 ***
************************




* KIN CAT LARGE


mlogit class i.kin_cat_l##i.eth i.female [pweight = dwe], rrr
est store iact_l

est restore iact_l
margins kin_cat_l [pweight = dwe]


* store results in table
matrix table = J(80,9,.)
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

    * Calculate the mean of FATHER
    sum num_1 if help == 1
	scalar avg = r(mean)

	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 1, 1] = avg
	matrix kin[17, 1] = avg
	matrix kin[33, 1] = avg
	matrix kin[49, 1] = avg
	matrix kin[65, 1] = avg

	
	* Calculate the mean of MOTHER 
    sum num_2 if help == 1
    scalar avg = r(mean)
	
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 2, 1] = avg
	matrix kin[18, 1] = avg
	matrix kin[34, 1] = avg
	matrix kin[50, 1] = avg
	matrix kin[66, 1] = avg
	
	* Calculate the mean of BROTHER 
    sum num_3 if help == 1
    scalar avg = r(mean)
	
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 3, 1] = avg
	matrix kin[19, 1] = avg
	matrix kin[35, 1] = avg
	matrix kin[51, 1] = avg
	matrix kin[67, 1] = avg

	
	* Calculate the mean of SISTER 
    sum num_4 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 4, 1] = avg
	matrix kin[20, 1] = avg
	matrix kin[36, 1] = avg
	matrix kin[52, 1] = avg
	matrix kin[68, 1] = avg

	
	* Calculate the mean of PATERNAL GRANDFATHER 
    sum num_5 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 5, 1] = avg
	matrix kin[21, 1] = avg
	matrix kin[37, 1] = avg
	matrix kin[53, 1] = avg
	matrix kin[69, 1] = avg
	
	
	* Calculate the mean of MATERNAL GRANDFATHER 
    sum num_6 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 6, 1] = avg
	matrix kin[22, 1] = avg
	matrix kin[38, 1] = avg
	matrix kin[54, 1] = avg
	matrix kin[70, 1] = avg

	
	* Calculate the mean of MATERNAL GRANDMOTHER 
    sum num_7 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 7, 1] = avg
	matrix kin[23, 1] = avg
	matrix kin[39, 1] = avg
	matrix kin[55, 1] = avg
	matrix kin[71, 1] = avg


	* Calculate the mean of PATERNAL GRANDMOTHER 
    sum num_8 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 8, 1] = avg
	matrix kin[24, 1] = avg
	matrix kin[40, 1] = avg
	matrix kin[56, 1] = avg
	matrix kin[72, 1] = avg

	
	* Calculate the mean of PATERNAL HALFSIBLING 
    sum num_9 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[ 9, 1] = avg
	matrix kin[25, 1] = avg
	matrix kin[41, 1] = avg
	matrix kin[57, 1] = avg
	matrix kin[73, 1] = avg
	
	* Calculate the mean of MATERNAL HALFSIBLING 
    sum num_10 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[10, 1] = avg
	matrix kin[26, 1] = avg
	matrix kin[42, 1] = avg
	matrix kin[58, 1] = avg
	matrix kin[74, 1] = avg
	
	* Calculate the mean of PATERNAL UNCLE 
    sum num_11 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[11, 1] = avg
	matrix kin[27, 1] = avg
	matrix kin[43, 1] = avg
	matrix kin[59, 1] = avg
	matrix kin[75, 1] = avg


	* Calculate the mean of MATERNAL UNCLE 
    sum num_12 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[12, 1] = avg
	matrix kin[28, 1] = avg
	matrix kin[44, 1] = avg
	matrix kin[60, 1] = avg
	matrix kin[76, 1] = avg
	
	* Calculate the mean of PATERNAL AUNT 
    sum num_13 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[13, 1] = avg
	matrix kin[29, 1] = avg
	matrix kin[45, 1] = avg
	matrix kin[61, 1] = avg
	matrix kin[77, 1] = avg

	
	* Calculate the mean of MATERNAL AUNT 
    sum num_14 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[14, 1] = avg
	matrix kin[30, 1] = avg
	matrix kin[46, 1] = avg
	matrix kin[62, 1] = avg
	matrix kin[78, 1] = avg
	
	
	* Calculate the mean of PATERNAL COUSIN 
    sum num_15 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[15, 1] = avg
	matrix kin[31, 1] = avg
	matrix kin[47, 1] = avg
	matrix kin[63, 1] = avg
	matrix kin[79, 1] = avg	
	
	
	* Calculate the mean of MATERNAL COUSIN 
    sum num_16 if help == 1
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[16, 1] = avg
	matrix kin[32, 1] = avg
	matrix kin[48, 1] = avg
	matrix kin[64, 1] = avg
	matrix kin[80, 1] = avg	
	
	
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




* NUCLEAR KIN


* indicator nuclear kin (y/n)
gen nuclear = inrange(kin_cat_l,1,4)
* number of nuclear kin
gen nuclear_num = num_1 + num_2 + num_3 + num_4
* number of extended kin
gen ext_num = num_5_m + num_6_m + num_7_m + num_8_m + num_9_m + num_10_m



mlogit class i.nuclear##i.eth i.female [pweight = dwe], rrr
est store iact_n


est restore iact_n
margins nuclear#eth if eth != 5 [pweight = dwe]
est store iact1_n

* store results in table
matrix table = J(40,9,.)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs, sheet("predprobs") replace 

* cols for race (1x40)
matrix race = J(40,1,.)
matrix race = (	1\2\3\4\	/// class1
				1\2\3\4\	/// class1
				1\2\3\4\	/// class2
				1\2\3\4\	/// class2
				1\2\3\4\	/// class3
				1\2\3\4\	/// class3
				1\2\3\4\	/// class4
				1\2\3\4\	///	class4
				1\2\3\4\	/// class5
				1\2\3\4)	// 	class5


* cols for nuclear (1x40)
matrix nuclear = J(40,1,.)
matrix nuclear = (	0\0\0\0\	/// class1
					1\1\1\1\	/// class1
					0\0\0\0\	/// class2
					1\1\1\1\	/// class2
					0\0\0\0\	/// class3
					1\1\1\1\	/// class3
					0\0\0\0\	/// class4
					1\1\1\1\	///	class4
					0\0\0\0\	/// class5
					1\1\1\1)	// 	class5
				
* cols for class (1x40)
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


			
* pairwise comparison
* for each class: nuclear*eth
* for each class: 0*1 vs 0*2 vs 0*3 vs 0*4 & 1*1 vs 1*2 vs 1*3 vs 1*4
est restore iact1_n	
pwcompare nuclear#eth, ateq pv // ateq to perform comparison for each class (ex. ref)
			
			
			
			
* KIN CAT SMALL



mlogit class i.kin_cat_small##i.eth i.female [pweight = dwe], rrr
est store iact_s


est restore iact_s
margins kin_cat_small#eth if eth != 5 [pweight = dwe]
est store iact1_s

* store results in table
matrix table = J(120,9,.)
matrix table = r(table)'

* set up new excel sheet
putexcel set $WD/data/predprobs_s, sheet("predprobs") replace 

* cols for race (1x40)
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
			
				
* cols for kincat (1x120)
matrix kincat = J(120,1,.)
matrix kincat = (	1\1\1\1\	/// class1
					2\2\2\2\	/// 
					3\3\3\3\	/// 
					4\4\4\4\	/// 
					5\5\5\5\	/// 
					6\6\6\6\	/// 
					1\1\1\1\	/// class2
					2\2\2\2\	/// 
					3\3\3\3\	/// 
					4\4\4\4\	/// 
					5\5\5\5\	/// 
					6\6\6\6\	/// 
					1\1\1\1\	/// class3
					2\2\2\2\	/// 
					3\3\3\3\	/// 
					4\4\4\4\	/// 
					5\5\5\5\	/// 
					6\6\6\6\	/// 
					1\1\1\1\	/// class4
					2\2\2\2\	/// 
					3\3\3\3\	/// 
					4\4\4\4\	/// 
					5\5\5\5\	/// 
					6\6\6\6\	/// 
					1\1\1\1\	/// class5
					2\2\2\2\	/// 
					3\3\3\3\	/// 
					4\4\4\4\	/// 
					5\5\5\5\	/// 
					6\6\6\6)	// 
				
* cols for class (1x120)
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

* store average kin counts in kin
cap drop num_*_s
gen num_1_s = num_1_m + num_2_m
gen num_2_s = num_3_m + num_4_m
gen num_3_s = num_5_m + num_6_m
gen num_4_s = num_7_m
gen num_5_s = num_8_m + num_9_m
gen num_6_s = num_10_m

matrix kin = J(120,1,.)

forvalues i = 1/4 {	
    * Calculate the mean of parents for the subset where eth == i
    sum num_1_s if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0 + `i'', 1] = avg
	matrix kin[`= 24 + `i'', 1] = avg
	matrix kin[`= 48 + `i'', 1] = avg
	matrix kin[`= 72 + `i'', 1] = avg
	matrix kin[`= 96 + `i'', 1] = avg
	
	* Calculate the mean of siblings for the subset where eth == i
    sum num_2_s if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 4 + `i'', 1] = avg
	matrix kin[`= 28 + `i'', 1] = avg
	matrix kin[`= 52 + `i'', 1] = avg
	matrix kin[`= 76 + `i'', 1] = avg
	matrix kin[`= 100 + `i'', 1] = avg
	
	* Calculate the mean of grandparents for the subset where eth == i
    sum num_3_s if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 8 + `i'', 1] = avg
	matrix kin[`= 32 + `i'', 1] = avg
	matrix kin[`= 56 + `i'', 1] = avg
	matrix kin[`= 80 + `i'', 1] = avg
	matrix kin[`= 104 + `i'', 1] = avg
	
	
	* Calculate the mean of halfsiblings for the subset where eth == i
    sum num_4_s if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
	matrix kin[`= 12 + `i'', 1] = avg
	matrix kin[`= 36 + `i'', 1] = avg
	matrix kin[`= 60 + `i'', 1] = avg
	matrix kin[`= 84 + `i'', 1] = avg
	matrix kin[`= 108 + `i'', 1] = avg
	
	
	* Calculate the mean of uncles & aunts for the subset where eth == i
    sum num_5_s if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
 	matrix kin[`= 16 + `i'', 1] = avg
	matrix kin[`= 40 + `i'', 1] = avg
	matrix kin[`= 64 + `i'', 1] = avg
	matrix kin[`= 88 + `i'', 1] = avg
	matrix kin[`= 112 + `i'', 1] = avg
	
	
	* Calculate the mean of cousins for the subset where eth == i
    sum num_6_s if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
	matrix kin[`= 20 + `i'', 1] = avg
	matrix kin[`= 44 + `i'', 1] = avg
	matrix kin[`= 68 + `i'', 1] = avg
	matrix kin[`= 92 + `i'', 1] = avg
	matrix kin[`= 116 + `i'', 1] = avg
	
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



			

			
			
			
			
			
			
* KIN CAT MEDIUM



mlogit class i.kin_cat_med##i.eth i.female [pweight = dwe], rrr
est store iact_m


est restore iact_m
margins kin_cat_med#eth if eth != 5 [pweight = dwe]
est store iact1_m

* store results in table
matrix table = J(200,9,.)
matrix table = r(table)'


* set up new excel sheet
putexcel set $WD/data/predprobs_m, sheet("predprobs") replace 

* cols for race (1x200)
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
			
				
* cols for kincat (1x200)
matrix kincat = J(200,1,.)
matrix kincat = (	1\1\1\1\		/// class1
					2\2\2\2\		/// 
					3\3\3\3\		/// 
					4\4\4\4\		/// 
					5\5\5\5\		/// 
					6\6\6\6\		///
					7\7\7\7\		///
					8\8\8\8\		///
					9\9\9\9\		///
					10\10\10\10\	///
					1\1\1\1\		/// class2
					2\2\2\2\		/// 
					3\3\3\3\		/// 
					4\4\4\4\		/// 
					5\5\5\5\		/// 
					6\6\6\6\		/// 
					7\7\7\7\		///
					8\8\8\8\		///
					9\9\9\9\		///
					10\10\10\10\	///
					1\1\1\1\		/// class3
					2\2\2\2\		/// 
					3\3\3\3\		/// 
					4\4\4\4\		/// 
					5\5\5\5\		/// 
					6\6\6\6\		/// 
					7\7\7\7\		///
					8\8\8\8\		///
					9\9\9\9\		///
					10\10\10\10\	///
					1\1\1\1\		/// class4
					2\2\2\2\		/// 
					3\3\3\3\		/// 
					4\4\4\4\		/// 
					5\5\5\5\		/// 
					6\6\6\6\		/// 
					7\7\7\7\		///
					8\8\8\8\		///
					9\9\9\9\		///
					10\10\10\10\	///
					1\1\1\1\		/// class5
					2\2\2\2\		/// 
					3\3\3\3\		/// 
					4\4\4\4\		/// 
					5\5\5\5\		/// 
					6\6\6\6\		/// 
					7\7\7\7\		///
					8\8\8\8\		///
					9\9\9\9\		///
					10\10\10\10)	
				
* cols for class (200)
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
    * Calculate the mean of FATHER for the subset where eth == i
    sum num_1_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + `i'', 1] = avg
	matrix kin[`= 40  + `i'', 1] = avg
	matrix kin[`= 80  + `i'', 1] = avg
	matrix kin[`= 120 + `i'', 1] = avg
	matrix kin[`= 160 + `i'', 1] = avg
	
	* Calculate the mean of MOTHER for the subset where eth == i
    sum num_2_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 4 + `i'', 1] = avg
	matrix kin[`= 40  + 4 + `i'', 1] = avg
	matrix kin[`= 80  + 4 + `i'', 1] = avg
	matrix kin[`= 120 + 4 + `i'', 1] = avg
	matrix kin[`= 160 + 4 + `i'', 1] = avg
	
	* Calculate the mean of BROTHER for the subset where eth == i
    sum num_3_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 2*4 + `i'', 1] = avg
	matrix kin[`= 40  + 2*4 + `i'', 1] = avg
	matrix kin[`= 80  + 2*4 + `i'', 1] = avg
	matrix kin[`= 120 + 2*4 + `i'', 1] = avg
	matrix kin[`= 160 + 2*4 + `i'', 1] = avg

	
	* Calculate the mean of SISTER for the subset where eth == i
    sum num_4_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 3*4 + `i'', 1] = avg
	matrix kin[`= 40  + 3*4 + `i'', 1] = avg
	matrix kin[`= 80  + 3*4 + `i'', 1] = avg
	matrix kin[`= 120 + 3*4 + `i'', 1] = avg
	matrix kin[`= 160 + 3*4 + `i'', 1] = avg

	
	
	* Calculate the mean of GRANDFATHER for the subset where eth == i
    sum num_5_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 4*4 + `i'', 1] = avg
	matrix kin[`= 40  + 4*4 + `i'', 1] = avg
	matrix kin[`= 80  + 4*4 + `i'', 1] = avg
	matrix kin[`= 120 + 4*4 + `i'', 1] = avg
	matrix kin[`= 160 + 4*4 + `i'', 1] = avg
	
	
	* Calculate the mean of GRANDMOTHER for the subset where eth == i
    sum num_6_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 5*4 + `i'', 1] = avg
	matrix kin[`= 40  + 5*4 + `i'', 1] = avg
	matrix kin[`= 80  + 5*4 + `i'', 1] = avg
	matrix kin[`= 120 + 5*4 + `i'', 1] = avg
	matrix kin[`= 160 + 5*4 + `i'', 1] = avg

	
	* Calculate the mean of HALFSIBLING for the subset where eth == i
    sum num_7_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 6*4 + `i'', 1] = avg
	matrix kin[`= 40  + 6*4 + `i'', 1] = avg
	matrix kin[`= 80  + 6*4 + `i'', 1] = avg
	matrix kin[`= 120 + 6*4 + `i'', 1] = avg
	matrix kin[`= 160 + 6*4 + `i'', 1] = avg


	* Calculate the mean of UNCLE for the subset where eth == i
    sum num_8_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 7*4 + `i'', 1] = avg
	matrix kin[`= 40  + 7*4 + `i'', 1] = avg
	matrix kin[`= 80  + 7*4 + `i'', 1] = avg
	matrix kin[`= 120 + 7*4 + `i'', 1] = avg
	matrix kin[`= 160 + 7*4 + `i'', 1] = avg

	
	* Calculate the mean of AUNT for the subset where eth == i
    sum num_9_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 8*4 + `i'', 1] = avg
	matrix kin[`= 40  + 8*4 + `i'', 1] = avg
	matrix kin[`= 80  + 8*4 + `i'', 1] = avg
	matrix kin[`= 120 + 8*4 + `i'', 1] = avg
	matrix kin[`= 160 + 8*4 + `i'', 1] = avg
	
	* Calculate the mean of COUSIN for the subset where eth == i
    sum num_10_m if help == 1 & eth == `i'
    scalar avg = r(mean)
	
	* Store the average in the matrix (row `i`, column 1)
    matrix kin[`= 0   + 9*4 + `i'', 1] = avg
	matrix kin[`= 40  + 9*4 + `i'', 1] = avg
	matrix kin[`= 80  + 9*4 + `i'', 1] = avg
	matrix kin[`= 120 + 9*4 + `i'', 1] = avg
	matrix kin[`= 160 + 9*4 + `i'', 1] = avg

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

