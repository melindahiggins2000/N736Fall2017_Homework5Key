#' =====================================
#' N736 - Homework 5
#'        Analysis of Covariance (ANCOVA)
#'        and Moderation (Interaction Effects)
#'
#' Answer Key
#'
#' dated 11/28/2018
#' Melinda Higgins, PhD.
#' =====================================

library(tidyverse)
library(haven)

help1 <- haven::read_spss("helpmkh.sav")

#' =====================================
#' select the variables we want for 
#' this lesson: 
#' DV: mcs
#' IV: age
#' CV: homeless
#'
#' let's also recode homeless
#' by flipping the 0,1
#' and compute the interaction between
#' age and homeless
#' =====================================

help2 <- help1 %>%
  select(mcs, age, homeless) %>%
  mutate(nothomeless = as.numeric((homeless==0)),
         age_x_homeless = age * homeless,
         age_x_nothomeless = age * nothomeless)

#' =====================================
#' [MODEL 1] Run a model testing to see if there is a difference 
#' in mental health mcs scores by homelessness (homeless) 
#' (run as a regression model with mcs as the outcome)
#' Discuss the interpretation of the intercept (i.e. when homeless = 0) 
#' and discuss the interpretation of the slope 
#' (i.e. what happens to mcs scores when going from 
#' not homeless (homeless=0) to homeless (homeless=1)).
#' =====================================

m1 <- lm(mcs ~ homeless, data=help2)
summary(m1)

# optional use of the olsrr package
# to get nicer output

library(olsrr)
olsrr::ols_regress(m1)

#' =====================================
#' [MODEL 2] Run a model testing for an association 
#' between mcs as the outcome with age as the predictor 
#' adjusting for homelessness (homeless) as a “covariate”.
#' Use a regression approach and show the stepwise 
#' approach comparing
#'   model 1: mcs = homeless
#'   model 2: mcs = homeless + age
#' present the change in R2 between these 2 models 
#' and the test for significant change in R2 discuss 
#' these results - 
#'   is there a significant association between mcs and 
#'   age after adjusting for homeless?
#' =====================================

m2 <- lm(mcs ~ homeless + age, data=help2)
summary(m2)

# check with flipped scoring nothomeless
m2flip <- lm(mcs ~ nothomeless + age, data=help2)
summary(m2flip)

# optional use of the olsrr package
olsrr::ols_regress(m2)

# to get p-values of r2 change
# works for "nested" models
anova(m1,m2)

# r2 change
summary(m1)$r.squared - summary(m2)$r.squared

#' =====================================
#' [MODEL 3] Run a full model with both main effects 
#' and an interaction (moderation) effect 
#' (using a regression, ANOVA, or GLM approach - your choice) 
#' for the association between the SF36 Mental Component Score (mcs) 
#' and Age (age) adjusting for homelessness (homeless). 
#' Remember to:
#'   check for the assumption of homogenity of 
#'   slopes (i.e. is the interaction term significant?) 
#'   make an “effects plot” plot of the interaction 
#'   between age and homeless 
#'   and additionally report the change in R2 for 
#'   model 3: mcs = homless + age + homeless_x_age 
#' discuss the results - 
#'   does homelessness moderate the association between mcs and age?
#' =====================================
  
m3 <- lm(mcs ~ homeless * age, data=help2)
summary(m3)

# check with flipped nothomeless
# notice that the p-value for age is different
m3flip <- lm(mcs ~ nothomeless * age, data=help2)
summary(m3flip)

# optional use of the olsrr package
olsrr::ols_regress(m3)

# to get p-values of r2 change
# of adding the interaction term
anova(m2,m3)

# r2 change
summary(m2)$r.squared - summary(m3)$r.squared

#' plots of the interaction effects

#' create "factor" class type for homeless
#' this is so the effects package
#' function will work correctly
help2$homeless.f <- factor(help2$homeless,
                           levels = c(0,1),
                           labels = c("not homeless",
                                      "homeless"))

# for the effects package plots to work
# we also need to put homeless.f first and then age
m3f <- lm(mcs ~ age * homeless.f, data=help2)
summary(m3f)

library(effects)
plot(allEffects(m3f))

# another option to sjPlot package
library(sjPlot)

# learn more at http://www.strengejacke.de/sjPlot/reference/plot_model.html
# put homeless as second term to get
# interaction plot with lines by homelessness
# use m3f model as above
plot_model(m3f, type="int",
           show.ci=TRUE,
           facet.grid=TRUE)
