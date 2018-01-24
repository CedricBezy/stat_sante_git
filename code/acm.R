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
library(FactoMineR)

# load couples
load('stat_sante_copy/data/couples.RData')

##===============================================
# ACM
##===============================================

couples_acm <- couples %>%
    tibble::column_to_rownames("id") %>%
    dplyr::select_("enfant", "age_h",  "bmi_h", "age_f", "duree_infertilite",
                   "patho_h_regroup", "cryptorchidie", "spermo", "bh_f","ct_f",
                   "patho_f_regroup", "fecondite", "traitement") %>%
    lapply(function(x){
        if(is.character(x)){
            x <- factor(x)
        }else{
            if(is.numeric(x)){
                moyx <- mean(x, na.rm = TRUE)
                x[which(is.na(x))] <- moyx
            }
        }
        return(x)
    }) %>%
    as.data.frame()

sapply(couples_acm, class)

acm <- FactoMineR::MCA(
    couples_acm,
    quanti.sup = unname(which(sapply(couples_acm, is.numeric))),
    quali.sup = 1
)


afdm <- FactoMineR::FAMD(couples_acm, sup.var = 1)


    