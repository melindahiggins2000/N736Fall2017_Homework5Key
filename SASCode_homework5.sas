* ============================================
  N736: Homework 5 - Analysis of Covariance (ANCOVA)

  by Melinda Higgins, PhD
  dated 13 November 2017

  For this exercise we'll be working with the
  HELP dataset - using these variables
  indtot, mcs, pss_fr and female
  ============================================;

* ============================================ 
  We will be looking at
  DV = indtot
  IV = mcs
  CV = pss_fr or female
  ============================================;

data work.helpmkh;
  set library.helpmkh;
  run;

* ============================================
  we will also want to compute the interaction
  effects - so let's mean center mcs and pss_fr
  and compute the interaction terms

  we'll use PROC MEANS to get the means
  and standard deviations
  ============================================;

proc means data=helpmkh mean std;
  var mcs pss_fr;
  run;

* ============================================
  we're also going to flip the coding for female
  and compute male to compare the contrasts
  and coefficients in the models
  ============================================;

data help2;
  set helpmkh;
  male = female=0;
  mcsC = mcs - 31.677;
  pss_frC = pss_fr-6.7;
  mcsC_x_pss_frC = mcsC * pss_frC;
  mcsC_x_female = mcsC * female;
  mcsC_x_male = mcsC * male;
  run;

proc freq data=help2;
  table female male;
  run;

* ============================================
  run ANCOVA using PROC REG for the association
  between indtot and mcs adjusting for pss_fr
  and testing for the interaction between 
  mcs and pss_fr
  ============================================;

proc reg data=help2;
  model indtot = mcsC pss_frC mcsC_x_pss_frC;
  run;

* ============================================
  Use PROC GLM to compute the interaction 
  term on the fly automatically - the use
  the store option to save the model fit
  followed by PROC PLM to make the effectplot
  we show the plot at -2, -1 0, 1 and 2 
  standard deviations below and above the mean
  for the centered pss_frC
  ============================================;

proc glm data=help2;
  model indtot = mcsC|pss_frC / solution;
  store glmmodel;
run;

proc plm source=glmmodel;
  effectplot fit (x=mcsC) / at(pss_frC = -7.99 -3.995 0 3.995 7.99);
run;

* ============================================
  notice that the lines are all mostly parallel
  which supports the non-significant interaction
  term in the model
  ============================================;

* ============================================
  NOTE: if you try PROC ANOVA you get an error
  in SAS you cannot have only
  continuous terms when running proc anova
  ============================================;
  
* ============================================
  let's next run ANCOVA looking
  at gender as our covariate
  ============================================;

proc reg data=help2;
  model indtot = mcsC female mcsC_x_female;
  run;

* ============================================
  try also using the male coding
  ============================================;

proc reg data=help2;
  model indtot = mcsC male mcsC_x_male;
  run;

* ============================================
  also do via PROC GLM with PROC PLM
  to get the effectplots
  ============================================;

proc glm data=help2;
  model indtot = mcsC|female / solution;
  store glmmodel2;
run;

proc plm source=glmmodel2;
  effectplot fit (x=mcsC) / at(female = 0 1);
run;



