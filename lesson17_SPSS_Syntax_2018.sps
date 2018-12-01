* Encoding: UTF-8.
* ========================================.
* SPSS SYNTAX
* Lesson 17 on Covariates and Interactions
* i.e. moderators
* ========================================.

* Let's look at correlations between some
* conceptual covariates like age, gender,
* and perceived social support among friends
* and associations with potential outcomes of
* interest: pcs, mcs, cesd and indtot

* overall correlations.

CORRELATIONS
  /VARIABLES=age female pss_fr pcs mcs cesd indtot
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

* suppose we also consider gender as a potential
* "moderator" of these correlations
* SPLIT the dataset by gender and rerun the
* correlations - look for differences in the
* correlations by gender.

SORT CASES  BY female.
SPLIT FILE LAYERED BY female.

CORRELATIONS
  /VARIABLES=age female pss_fr pcs mcs cesd indtot
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

SPLIT FILE OFF.

* look at the differences in the correlations
* by gender for the association between
* mcs and pcs - mental and physical composite
* scores from the SF36 - the association
* is stronger for females.

* test if there is an interaction effect between
* pcs and gender for mcs as the outcome.
* in SPSS for the linear regression module,
* we need to 1st compute an interaction
* effect between pcs and gender. Since gender is
* already coded 0,1, we can just compute the interaction
* without mean-centering. For continuous predictors,
* we should 1st mean center for effects.

compute pcs_x_female = pcs * female.
execute.

* run a regression model
* put in gender - female
* then put in pcs
* then put in the interaction effect between them
* look at the p-values and changes in r2 for each step.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mcs
  /METHOD=ENTER female
  /METHOD=ENTER pcs
  /METHOD=ENTER pcs_x_female
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).

* notice that gender is a significant predictor
* conceptually it is a significant "covariate"
* you can see this from Model 1 (BLOCK 1)
* AFTER adjusting for gender, pcs is a 
* significant predictor of mcs - see Model 2
* However, once you add the interaction term
* you'll notice that pcs_x_female is significant
* and the pcs term is no longer significant
* there is a significant interaction between
* pcs and gender - gender is a significant moderator
* of the effect between pcs and mcs.


* let's try this using an ANCOVA approach
* Analysis of Covariance Approach
* set mcs = dependent variable
* set female = fixed factor (because it is categorical)
* set pcs = covariate (because it is continuous)
* note that model defaults to full factorial
* but his does NOT include the interaction effect
* so select custom and build a model
* with the 2 main effects plus the interaction effect
* see /DESIGN= in the SYNTAX below
* in the options be sure to check the box
* for parameter effects to see the coefficients
* similar to the regression output.

UNIANOVA mcs BY female WITH pcs
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=ETASQ DESCRIPTIVE PARAMETER HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=female pcs female*pcs.

* compare the parameter estimates table from the ANCOVA
* to the coefficients tables from the regression
* what do you notice? The intercept and interaction 
* coefficients are different - why?
* the default contrast computed in SPSS actually flips
* the coding for males and females

* this has to do with contrast coding in SPSS
* so let's flip the coding for gender
* compute males to set male=1 and female=0.
compute male = female=0.
execute.

* rerun the ANCOVA model.
UNIANOVA mcs BY male WITH pcs
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=ETASQ DESCRIPTIVE PARAMETER HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=male pcs male*pcs.

* let's make a plot of mcs by pcs by gender.

GRAPH
  /SCATTERPLOT(BIVAR)=pcs WITH mcs BY female
  /MISSING=LISTWISE.

* and let's overlay the fitted lines for each gender
* we can compute these by hand.
* regression line
*     mcs = 29.849 - 15.620(female) + 0.055(pcs) + 0.272(pcs*female)
* when female=0 for MALES
*     mcs = 29.849 + 0.055(pcs)
* when female=1 for FEMALES
*     mcs = [29.849 - 15.620] + [0.055 + 0.272](pcs)
*     mcs = 14.229 + 0.327(pcs)


