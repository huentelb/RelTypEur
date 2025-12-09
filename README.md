# Younger adults’ nuclear and extended family relationships in Europe: A latent class analysis


The repository contains all necessary code to reproduce the results for the manuscript **"Younger adults’ nuclear and extended family relationships in Europe: A latent class analysis"** by Hünteler, Hank, Alburez-Gutierrez & Leopold. 

The objective is to answer these three **research questions**:
1. Which classes of relationship qualities can be identified in younger adults’ nuclear and extended family relations in Europe? 
2. How does the distribution of these relationship types vary across nuclear, nuclear-extend, and distant-extended kin ties?
3. How do these distributions differ across countries?


We answer these questions through the following **analytical steps** and their resulting **outputs**:
1. Run Latent Class Analysis (LCA) to create typology (using *R's* `poLCA`)
   - Answer RQ 1
     - Conditional item response probabilities for relationship indicators in optimal clusters solution (Figure 1 in the manuscript)
3. Conduct regression analysis (using *Stata's* `mlogit`, `margins`, and `coefplot`)
   - Answer RQ 2
     - Distribution of relationship patterns by kin category (predicted probabilities and average absolute kin counts; Figure 2)
   - Answer RQ 3
     - Average marginal effects of country on probability for class membership (Figure 3)
     - Average predicted number of kin by relationship type across coutnries (Figure S2)

The analyses are based on the **KINMATRIX** data release "perpared_v03.dta" from Oct 2024. Find more information on the data [here](https://kinmatrix.eu). To reproduce the results, run the code in the order indicated by the suffixes (01_ to 09_). The analyses were conducted using *R studio, Version 2023.06.0+421*, and *Stata 18.5*. 

For questions regarding the code, contact Bettina Hünteler (*bhuenteler@diw.de*). 

## Explanation of the code files for the main analysis
### [01_setup.R](01_setup.R) 
1. sets up the working directory
2. creates the necessary upper-level folders and
3. loads the data,
4. selects the sample, and
5. imputes missing values for all variables, except the relevant relationship indicators.
### [02_univbarplots.R](02_univbarplots.R)
The relationship indicators of interest are plotted using the code in [02_univbarplots.R](02_univbarplots.R). The graphs are automatically stored in the working directory. 
### [03_lcaprep.R](03_lcaprep.R)
In [03_lcaprep.R](03_lcaprep.R), we prepare the latent class analysis. This entails
- dichotomization of the continuous relationship indicators,
- imputing the remaining missing values for them, and
- plotting the relationship indicators with the indication of the cut-offs (**Figure S1** in the manuscript). 
### [04_lca.R](04_lca.R) 
This code conducts the latent class analysis using `poLCA`, generating solutions for 1 to 7 classes (stored as RData-files for faster re-load, if necessary). Also, we calculate the classification criteria as found in **Table S1**. 
### [05_lcaout_5.R](05_lcaout_5.R) 
This code selects the optimal cluster solution and  
- produces **Table S2** containing the average posterior class probability
- sets the class labels
- produces different graphs showing the conditional item response probabilities (**Figure 2**)
### [06_prep_reg.R](06_prep_reg.R) 
This code prepares the data for analysis in *Stata*, which entails
1. modification of relevant covariates (country, gender, and kin categories)
2. exporting the data to .dta-format
### [07_reg_weighted.do](07_reg_weighted.do)
In *Stata*, we
1. generate the kin count variables for different categories of kin
2. produce **Table 1** (sample description by country)
3. run the multinomial logistic regression models using `mlogit` (shown in **Table S3**), and
4. plot the average marginal effect (AME) of country on class membership (**Figure 3**) using `margins` and `coefplot`
### [08_predprobs.do](08_predprobs.do)
1. calculates the estimated average number of kin in each class by country, based on the multinomial logistic regressions using `margins` and
2. estimates the p-values for the group-wise comparisons using the `pwcompare`-option (available on request).
3. Predicted kin counts are stored in different excel-files which serve as the basis for **Figure S2**. 
### [09_predprobs_graphs.R](09_predprobs_graphs.R) 
This code produces **Figure S2** as well as different variants of these figures incl. predicted probabilities across kin types and countries, alternating the amount of detail in the kin categories (e.g., parents vs fathers/mothers) and the organization of their display (e.g., dodged vs stacked bar plots). 
