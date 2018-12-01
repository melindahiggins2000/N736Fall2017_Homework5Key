* ============================================
  N736: LESSON 18 - Covariates, Interactions/Moderators
        and Analysis of Covariance (ANCOVA)

  by Melinda Higgins, PhD
  dated 6 November 2018

  For this exercise we'll be working with the
  HELP dataset - using these variables
  mcs, pcs, and female
  ============================================;

* load help dataset apply formats;

libname library 'C:\MyGithub\N736Fall2017_HELPdataset\' ;

proc format library = library ;
   value TREAT
      0 = 'usual care'  
      1 = 'HELP clinic' ;
   value FEMALE
      0 = 'Male'  
      1 = 'Female' ;
   value HOMELESS
      0 = 'no'  
      1 = 'yes' ;
   value G1B
      0 = 'no'  
      1 = 'yes' ;
   value F1A
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1B
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1C
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1D
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1E
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1F
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1G
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1H
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1I
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1J
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1K
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1L
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1M
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1N
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1O
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1P
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1Q
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1R
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1S
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value F1T
      0 = 'Not at all or less than 1 day'  
      1 = '1-2 days'  
      2 = '3-4 days'  
      3 = '5-7 days or nearly every day for 2 weeks' ;
   value SATREAT
      0 = 'no'  
      1 = 'yes' ;
   value DRINKSTATUS
      0 = 'no'  
      1 = 'yes' ;
   value ANYSUBSTATUS
      0 = 'no'  
      1 = 'yes' ;
   value LINKSTATUS
      0 = 'no'  
      1 = 'yes' ;

proc datasets library = library;
modify helpmkh / correctencoding="WLATIN1";
   format     treat TREAT.;
   format    female FEMALE.;
   format  homeless HOMELESS.;
   format       g1b G1B.;
   format       f1a F1A.;
   format       f1b F1B.;
   format       f1c F1C.;
   format       f1d F1D.;
   format       f1e F1E.;
   format       f1f F1F.;
   format       f1g F1G.;
   format       f1h F1H.;
   format       f1i F1I.;
   format       f1j F1J.;
   format       f1k F1K.;
   format       f1l F1L.;
   format       f1m F1M.;
   format       f1n F1N.;
   format       f1o F1O.;
   format       f1p F1P.;
   format       f1q F1Q.;
   format       f1r F1R.;
   format       f1s F1S.;
   format       f1t F1T.;
   format   satreat SATREAT.;
   format drinkstatus DRINKSTATUS.;
   format anysubstatus ANYSUBSTATUS.;
   format linkstatus LINKSTATUS.;
quit;

* make a copy to WORK;
data helpmkh;
  set library.helpmkh;
  run;

* the formatting is now applied and should work;
proc freq data=helpmkh;
  tables female;
  run;

proc freq data=helpmkh;
  table f1a;
  run;

* see contents and formatting details
  and labelling;
proc contents data=helpmkh;
run;

* ============================================ 
  We will be looking at
  DV = mcs
  IV = pcs
  CV = female
  ============================================;

data work.helpmkh;
  set library.helpmkh;
  run;

* ============================================
  we're also going to flip the coding for female
  and compute male to compare the contrasts
  and coefficients in the models - and compute interactions
  ============================================;

data help2;
  set helpmkh;
  male = female=0;
  pcs_x_female = pcs * female;
  pcs_x_male = pcs * male;
  run;

proc freq data=help2;
  table female male;
  run;

* ============================================
  run ANCOVA using PROC REG for the association
  between mcs and pcs adjusting for female
  and testing for the interaction between 
  pcs and female
  ============================================;

proc reg data=help2;
  model mcs = female pcs pcs_x_female;
  run;

* ============================================
  Use PROC GLM to compute the interaction 
  term on the fly automatically - the use
  the store option to save the model fit
  followed by PROC PLM to make the effectplot
  we show the plot at female = 0 or 1 for pcs
  ============================================;

proc glm data=help2;
  model mcs = pcs|female / solution;
  store glmmodel;
run;

proc plm source=glmmodel;
  effectplot fit (x=pcs) / at(female = 0 1);
run;

* ============================================
  notice that the lines (slopes) are different
  for males and females supporting the signficant
  interaction (moderation) effect
  ============================================;

* ============================================
  let's do this one more time using PROC ANOVA
  ============================================;
  
proc anova data=help2;
  model mcs = pcs*female;
  run;

* ============================================
  WHAT? yep - you get an error
  in SAS you cannot have only
  continuous terms when running proc anova
  ============================================;

* ============================================
  let's next run ANCOVA looking
  at gender as our covariate
  ============================================;

proc reg data=help2;
  model mcs = pcs female pcs_x_female;
  run;

* ============================================
  try also using the male coding
  ============================================;

proc reg data=help2;
  model mcs = pcs male pcs_x_male;
  run;


