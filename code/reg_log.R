##---------------------------------------
# projet stat pour la sante
# 22 / 01 / 2017
# Cedric B
##---------------------------------------

##===============================================
# Preparation Environnement
##===============================================
rm(list = ls())

# import packages
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(questionr)

# load couples
load('stat_sante_copy/data/couples.RData')
load('stat_sante_copy/data/couples_init.RData')

source('stat_sante_copy/code/functions.R')

##===============================================
# Reg Log
##===============================================

couples$traitement <- relevel(couples$traitement, "Aucun")

reg_1 <- glm(
    enfant ~ bmi_h + spermo + age_f + traitement,
    data = couples,
    family = binomial(logit)
)
OR <- exp(reg_1$coefficients)
summary(reg_1)

questionr::odds.ratio(reg_1)

##===============================================
# Reg Log Training
##===============================================

samples_couples <- train_test_split(couples)
couples_train <- samples_couples$train
couples_test <- samples_couples$test

table(couples_train$enfant)

reg_1_train <- glm(
    enfant ~ bmi_h + spermo + age_f + traitement,
    data = couples_train,
    family = binomial(logit)
)

anova(reg_1_train)
predict.glm(reg_1_train, newdata = couples_test, type = "response")

##===============================================
# Sub Reg Log
##===============================================


sub_couples_ls <- split(couples, factor(!is.na(couples$bilan_f),
                                        levels = c(TRUE, FALSE),
                                        labels = c("bilan", 'incomplet')))


glm(enfant ~ bmi_h + spermo + age_f + traitement + bilan_f,
    data = sub_couples_ls$'bilan',
    family = binomial(logit))

glm(enfant ~ bmi_h + spermo + age_f + traitement,
        data = sub_couples_ls$'incomplet',
        family = binomial(logit))


# summary(glm(enfant ~ age_f + diplome_f + bilan_f * complet_f,
#             data = couples,
#             family = binomial))
# 
# summary(glm(enfant ~ age_f + bilan_f * complet_f,
#             data = couples,
#             family = binomial))
# 
# summary(glm(enfant ~ age_f + bilan_f,
#             data = dplyr::filter(couples, complet_f),
#             family = binomial))
