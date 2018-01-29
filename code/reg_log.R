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

# load couples
load('stat_sante_copy/data/couples.RData')
load('stat_sante_copy/data/couples_init.RData')

source('stat_sante_copy/code/functions.R')

##===============================================
# Reg Log
##===============================================


summary(
    glm(enfant ~ spermo + age_f + traitement,
        data = couples,
        family = binomial(logit))
)

sub_couples_ls <- split(couples, factor(!is.na(couples$bilan_f), levels =))


summary(
    glm(enfant ~ bmi_h + spermo + age_f + traitement + bilan_f,
        data = sub_couples_ls$"TRUE",
        family = binomial(logit))
)

summary(
    glm(enfant ~ bmi_h + spermo + age_f + traitement,
        data = sub_couples_ls$"FALSE",
        family = binomial(logit))
)


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
