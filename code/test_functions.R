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
# test functions
##===============================================

var_name <- "bmi_h_class"
var_title <- var_name
data <- couples
display_na <- TRUE

make_summary_quali(var_name, couples)

.build_barplot_variable(var_name, couples, empile = TRUE)
.build_barplot_variable(var_name, couples)


.build_barplot_enfant(var_name, couples, empile = TRUE)
.build_barplot_enfant(var_name, couples, empile = FALSE)

