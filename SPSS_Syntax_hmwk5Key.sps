* Encoding: UTF-8.
* ============================================.
* N736 Homework 5: Analysis of Covariance (ANCOVA)
*
* by Melinda Higgins, PhD
* dated November 13, 2017
* ============================================.

* ============================================.
* For this Homework 5 we will be working with the HELP dataset
* load helpmkh.sav
* ============================================.

* ============================================.
* We will be working with these variables from the HELP dataset
*      indtot, mcs, pss_fr and female
*
* Run descriptive statistics on these variables
* ============================================.

FREQUENCIES VARIABLES=indtot mcs pss_fr female
  /NTILES=4
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /HISTOGRAM
  /ORDER=ANALYSIS.

* ============================================.
* Let's also look at the correlation matrix
* ============================================.

CORRELATIONS
  /VARIABLES=indtot mcs pss_fr female
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
NONPAR CORR
  /VARIABLES=indtot mcs pss_fr female
  /PRINT=SPEARMAN TWOTAIL NOSIG
  /MISSING=PAIRWISE.

* ============================================.
* We will be treating pss_fr and female as covariates
* We will also be interested in the interaction between
* mcs and female and between mcs and pss_fr
* so, let's mean center mcs and pss_fr and then create
* interaction variables mcsC-x-pss_frC and mcsC-x-female
*
* The mean of mcs = 31.677
* The mean of pss_fr = 6.71
*
* We will also be exploring the affect of the contrast comparison
* tests run for gender and the default handling of 0,1 coding
* so, let's also code a variable "male" which is the reverse coding
* of "female".
* ============================================.

compute male = female=0.
compute mcsC = mcs - 31.677.
compute pss_frC = pss_fr - 6.71.
compute mcsC_x_pss_frC = mcsC * pss_frC.
compute mcsC_x_female = mcsC * female.
compute mcsC_x_male = mcsC * male.
execute.

* ============================================.
* ANCOVA using a regression approach
* DV = indtot; IV = mcs; CV = pss_fr
*
* put in main effect for mcsC
* then add covariate pss_frC
* and then test for homogenity of slopes - add in interaction term
* ============================================.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT indtot
  /METHOD=ENTER mcsC
  /METHOD=ENTER pss_frC
  /METHOD=ENTER mcsC_x_pss_frC
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).

* ============================================.
* ANCOVA using ANOVA approach
* ============================================.

UNIANOVA indtot WITH mcsC pss_frC
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=PARAMETER ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=mcsC pss_frC mcsC*pss_frC.

* ============================================.
* Make a plot of the potential interaction between 
* mcs and pss_fr
* first let's split pss_fr into high and low categories using a median split
* the median of pss_fr = 7.0
* ============================================.

compute pss_fr_gt7 = pss_fr > 7.
execute.

GRAPH
  /SCATTERPLOT(BIVAR)=mcs WITH indtot
  /PANEL COLVAR=pss_fr_gt7 COLOP=CROSS
  /MISSING=LISTWISE.

* ============================================.
* double click on graph and click "elements/fit line to total"
* to add a fit line for each group - the lines are close to parallel
* ============================================.

* ============================================.
* ANCOVA using a regression approach
* DV = indtot; IV = mcs; CV = female
*
* put in main effect for mcsC
* then add covariate female
* and then test for homogenity of slopes - add in interaction term
* ============================================.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT indtot
  /METHOD=ENTER mcsC
  /METHOD=ENTER female
  /METHOD=ENTER mcsC_x_female
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).

* ============================================.
* ANCOVA using a regression approach
* DV = indtot; IV = mcs; CV = female
*
* put in main effect for mcsC
* then add covariate male
* and then test for homogenity of slopes - add in interaction term
* ============================================.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT indtot
  /METHOD=ENTER mcsC
  /METHOD=ENTER male
  /METHOD=ENTER mcsC_x_male
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).

* ============================================.
* Make a plot of the potential interaction between 
* mcs and female
* ============================================.

GRAPH
  /SCATTERPLOT(BIVAR)=mcs WITH indtot
  /PANEL COLVAR=female COLOP=CROSS
  /MISSING=LISTWISE.

* ============================================.
* notice that the slope of the lines are different for the 
* 2 genders - the slope is mostly flat for males
* and has a negative slope for the females
* this is WHY the test for mcs is DIFFERENT in the 2 regression models
* ============================================.

* ============================================.
* ANCOVA using ANOVA approach
* first using "female" and then with "male"
* ============================================.

UNIANOVA indtot BY female WITH mcsC
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) WITH(mcsC=MEAN) 
  /EMMEANS=TABLES(female) WITH(mcsC=MEAN) 
  /PRINT=PARAMETER ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=mcsC female female*mcsC.

UNIANOVA indtot BY male WITH mcsC
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) WITH(mcsC=MEAN) 
  /EMMEANS=TABLES(male) WITH(mcsC=MEAN) 
  /PRINT=PARAMETER ETASQ HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=mcsC male male*mcsC.

* ============================================.
* pay CLOSE attention to the differences between the
* Tests of Between-Subjects Effects
* and the Parameter Estimates tables
* ============================================.
