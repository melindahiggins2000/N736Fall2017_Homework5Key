* Encoding: UTF-8.
* ============================================.
* N736 Homework 5: Analysis of Covariance (ANCOVA)
* and Moderation (interaction effects)
*
* by Melinda Higgins, PhD
* dated December 1, 2018
* ============================================.

* ============================================.
* For this Homework 5 we will be working with the HELP dataset
* load helpmkh.sav
* ============================================.

* ============================================.
* We will be working with these variables from the HELP dataset
*      mcs, age and homeless
*
* Run descriptive statistics on these variables
* ============================================.

FREQUENCIES VARIABLES=mcs age homeless
  /NTILES=4
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /HISTOGRAM
  /ORDER=ANALYSIS.

* ============================================.
* Let's also look at the correlation matrix
* ============================================.

CORRELATIONS
  /VARIABLES=mcs age homeless
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
NONPAR CORR
  /VARIABLES=mcs age homeless
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

* ============================================.
* We will also be exploring the affect of the contrast comparison
* tests run for homeless and the default handling of 0,1 coding
* so, let's also code a variable "nothomeless" which is the reverse coding
* of "homeless".
* ============================================.

compute nothomeless = homeless=0.
compute age_x_homeless = age * homeless.
compute age_x_nothomeless = age * nothomeless.
execute.

* ============================================.
* We can use the Blocks or Step Approach of SPSS
* to first get the 
*   model1 for mcs = homeless
* then to get 
*   model2 for mcs = homeless + age
* and finally to get 
*   model3 for mcs = homeless + age + age_x_homeless
* which tests the interaction term - homogenity of slopes test
* ============================================.

* ============================================.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mcs
  /METHOD=ENTER homeless
  /METHOD=ENTER age
  /METHOD=ENTER age_x_homeless
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).

* ============================================.
* run again using nothomeless
* and compare the p-values for the age effect
* ============================================.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mcs
  /METHOD=ENTER nothomeless
  /METHOD=ENTER age
  /METHOD=ENTER age_x_nothomeless
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).

* ============================================.
* Make a plot of the potential interaction between 
* age and homeless
* ============================================.

GRAPH
  /SCATTERPLOT(BIVAR)=mcs WITH age
  /PANEL COLVAR=homeless COLOP=CROSS
  /MISSING=LISTWISE.

* ============================================.
* notice that the slope of the lines are different for the 
* homelessness - the slope is slightly negative for 
* nothomeless and more strongly positive for homeless
* this is WHY the test for the age effect
* is DIFFERENT in the 2 regression models
* homeless versus nothomeless
* ============================================.

