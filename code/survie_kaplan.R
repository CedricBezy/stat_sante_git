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

kaplan <- survfit(kaplan~spermo + traitement,
                  data = couples)

plot(kaplan, col = c(1:6), mark.time = TRUE)

