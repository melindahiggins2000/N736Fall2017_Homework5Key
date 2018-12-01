# =====================================
# N736 - Homework 5 - Analysis of Covariance (ANCOVA)
#
# dated 11/28/2018
# Melinda Higgins, PhD.
# =====================================

library(tidyverse)
library(haven)

help1 <- haven::read_spss("helpmkh.sav")

# =====================================
# select the variables we want for 
# this lesson: 
# DV: mcs
# IV: age
# CV: homeless
#
# let's also recode homeless
# by flipping the 0,1
# and compute the interaction between
# age and homeless
# =====================================

help2 <- help1 %>%
  select(mcs, age, homeless) %>%
  mutate(nothomeless = as.numeric((homeless==0)),
         age_x_homeless = age * homeless,
         age_x_nothomeless = age * nothomeless)

# =====================================
# using a regression approach - look
# at homeless as a covariate for
# the relationship between
# age and mcs
# note the use of * which computes
# the interaction for you
# =====================================

m1 <- lm(mcs ~ age * homeless, data=help2)
summary(m1)

library(olsrr)
ols_regress(m1)

# =====================================
# let's also try the aov() function
# =====================================

m1aov <- aov(mcs ~ age * homeless, data=help2)
summary(m1aov)

# =====================================
# now let's look at homeless as a covariate
# first run for homeless
# then run for nothomeless
# =====================================
m2f <- lm(mcs ~ age * homeless, data=help2)
summary(m2f)
ols_regress(m2f)

m2m <- lm(mcs ~ age * nothomeless, data=help2)
summary(m2m)
ols_regress(m2m)

# =====================================
# let's also try the aov() function
# =====================================

m2f.aov <- aov(mcs ~ age * homeless, data=help2)
summary(m2f.aov)

# check type III SS using Anova() from car package
library(car)
car::Anova(m2f.aov, type=3)

# run again for nothomeless
m2m.aov <- aov(mcs ~ age * nothomeless, data=help2)
summary(m2m.aov)
car::Anova(m2m.aov, type=3)

# =====================================
# let's also make the effectplot
# showing the interaction
# or lack thereof between
# age and homeless for mcs
# =====================================

summary(m2f)

library(effects)
plot(effect("age*homeless", m2f, 
            xlevels=list(homeless=0:1, age=40)),
     multiline=TRUE, ylab="MCS", rug=FALSE)

allEffects(m2f)

# as you can see the association betwee
# age and mcs is stronger and positive for homeless
# but is weak and negative for nothomeless

# install the HH package
# debug(utils:::unpackPkgZip)
# install.packages("HH", dependencies = TRUE)
# install.packages("multcomp")
# install.packages("zoo")
# install.packages("Formula")
# install.packages("acepack", dependencies = TRUE)
# install.packages("htmlTable", dependencies = TRUE)
# install.packages("data.table", dependencies = TRUE)
library(HH)

# note: to use the ancova() function in the HH
# package, the categorical variable homeless
# must be of factor class.
class(help2$homeless)
help2$homelessF <- as.factor(help2$homeless)
class(help2$homelessF)

hhaov <- HH::ancova(mcs ~ age * homelessF, data=help2)
# see summary and associated effect plot
hhaov

# can also be done using qplot()
# from the ggplot2 package
# and overlay a smooth regression fit line
# NOTE: ggplot2 should already be loaded
# when we loaded the tidyverse package above
qplot(x=age, y=mcs, facets=~homeless, data=help2) +
  geom_smooth(method="lm")

# can also use the sjPlot package
# install.packages("sjPlot", dependencies = TRUE)
# install.packages("stringdist", dependencies = TRUE)
# install.packages("DT", dependencies = TRUE)
library(sjPlot)

# learn more at https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html

m3f <- lm(mcs ~ homeless * age, data=help2)

# interaction plot
sjPlot::plot_model(m3f, type="int")
