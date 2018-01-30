##===============================================
# Preparation Environnement
##===============================================
rm(list = ls())

# import packages
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(survival)

# load couples
load('stat_sante_copy/data/couples.RData')

source('stat_sante_copy/code/functions.R')

##===============================================
# Preparation Environnement
##===============================================


couples <- couples %>%
    dplyr::mutate(enfant = enfant=="Oui")


couples$kaplan <- with(data = couples, Surv(delta, enfant))


kaplan_1 <- survfit(kaplan~1, data = couples)


plot(kaplan_1, col = c(1:6), mark.time = F)

kaplan_spermo <- survfit(kaplan~spermo,
                         data = couples)

plot(kaplan_spermo, col = c(1:6), mark.time = F)

