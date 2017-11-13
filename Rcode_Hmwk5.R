# =====================================
# N736 - Homework 5 - Analysis of Covariance (ANCOVA)
#
# dated 11/12/2017
# Melinda Higgins, PhD.#
# =====================================

library(tidyverse)
library(haven)

help1 <- haven::read_spss("helpmkh.sav")

# =====================================
# select the variables we want for 
# this lesson: 
# DV: indtot
# IV: mcs
# CV: pss_fr or female
#
# let's also recode female to male
# by flipping the 0,1
# and let's mean center mcs and pss_fr
# since we'll also look at the interaction
# between these variables
# plus the interactions between
# mcs and gender
# =====================================

help2 <- help1 %>%
  select(indtot, mcs, pss_fr, female) %>%
  mutate(mcsC = mcs - mean(mcs),
         pss_frC = pss_fr - mean(pss_fr),
         male = as.numeric((female==0)),
         mcsC_x_pss_frC = mcsC * pss_frC,
         mcsC_x_female = mcsC * female,
         mcsC_x_male = mcsC * male)

# =====================================
# using a regression approach - look
# at pss_fr as a covariate for
# the relationship between
# indtot and mcs
# note the use of * which computes
# the interaction for you
# =====================================

m1 <- lm(indtot ~ mcsC * pss_frC, data=help2)
summary(m1)

library(olsrr)
ols_regress(m1)

# =====================================
# let's also try the aov() function
# =====================================

m1aov <- aov(indtot ~ mcsC * pss_frC, data=help2)
summary(m1aov)
ols_regress(m1aov)

# =====================================
# let's also make the effectplot
# showing te interaction
# or lack thereof between
# mcs and pss_fr for indtot
# =====================================

library(effects)
plot(effect("mcsC:pss_frC", m1, 
            xlevels=list()),
     multiline=TRUE, ylab="INDTOT", rug=FALSE)

# =====================================
# now let's look at gender as a covariate
# first run for female
# then run for male
# =====================================
m2f <- lm(indtot ~ mcsC * female, data=help2)
summary(m2f)
ols_regress(m2f)

m2m <- lm(indtot ~ mcsC * male, data=help2)
summary(m2m)
ols_regress(m2m)

# =====================================
# let's also try the aov() function
# =====================================

m2f.aov <- aov(indtot ~ mcsC * female, data=help2)
summary(m2f.aov)
ols_regress(m2f.aov)

# check type III SS using Anova() from car package
library(car)
car::Anova(m2f.aov, type=3)

# run again for male
m2m.aov <- aov(indtot ~ mcsC * male, data=help2)
car::Anova(m2m.aov, type=3)

# =====================================
# let's also make the effectplot
# showing the interaction
# or lack thereof between
# mcs and female for indtot
# =====================================

summary(m2f)

library(effects)
plot(effect("mcsC*female", m2f, 
            xlevels=list(female=0:1)),
     multiline=TRUE, ylab="INDTOT", rug=FALSE)

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
# package, the categorical variable female
# must be of factor class.
class(help2$female)
help2$femaleF <- as.factor(help2$female)
class(help2$femaleF)

hhaov <- HH::ancova(indtot ~ mcsC * femaleF, data=help2)
# see summary and associated effect plot
hhaov

# can also be done using qplot()
# from the ggplot2 package
# and overlay a smooth regression fit line
# NOTE: ggplot2 should already be loaded
# when we loaded the tidyverse package above
qplot(x=mcsC, y=indtot, facets=~female, data=help2) +
  geom_smooth(method="lm")

# can also use the sjPlot package
# install.packages("sjPlot", dependencies = TRUE)
# install.packages("stringdist", dependencies = TRUE)
# install.packages("DT", dependencies = TRUE)
library(sjPlot)

# learn more at http://www.strengejacke.de/sjPlot/sjp.int/ 
m3f <- lm(indtot ~ female * mcsC, data=help2)

sjp.int(m3f, type="eff",
        show.ci=TRUE,
        facet.grid=TRUE)

