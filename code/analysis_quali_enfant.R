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
# Analysis Enfant
##===============================================

df_summa_enfant <- couples %>%
    dplyr::group_by(enfant) %>%
    dplyr::summarise(eff = n()) %>%
    dplyr::mutate(
        pct = 100 * eff / sum(eff),
        pct_str = paste(formatC(pct, digits = 1, format = "f"), "%")
    )

## PLOT : couples$enfant

build_barplot("enfant",
               data = couples,
               var_title = "Capacite pour un couple à avoir un enfant",
               palette = palette_enfant)


##===============================================
# FUNCTIONS 
##===============================================

var_name <- "ct_f"




vect_quali <- c(
    diplome_h = "Diplome Homme",
    patho_h_bin = "Anomalie Pathologie de l'homme",
    cryptorchidie = "Cryptorchidie",
    spermo = "Grade d\'infertilite de l\'homme",
    diplome_f = "Diplome Femme",
    bh_f = "Bilan Hormonale de la femme",
    ct_f = "Ovulation de la femme (selon courbe de temperature)",
    patho_f_bin = "Anomalie Pathologie de la femme",
    bilan_f = "Bilan Feminin",
    fecondite = "type d\'infecondite",
    traitement = "Type de traitement de l\'infecondite"
)
N <- length(vect_quali)

plot_univar_ls <- mapply(
    FUN = build_barplot,
    var_name = names(vect_quali)[1:N],
    var_title = vect_quali[1:N],
    MoreArgs = list(
        data = couples,
        with_enfant = FALSE
    ),
    SIMPLIFY = FALSE
)
# multiplot(plotList = plot_univar_ls)

plot_bivar_ls <- mapply(
    FUN = build_barplot,
    var_name = names(vect_quali)[1:N],
    var_title = vect_quali[1:N],
    MoreArgs = list(
        data = couples,
        empile = FALSE,
        with_enfant = TRUE
    ),
    SIMPLIFY = FALSE
)
# multiplot(plotList = plot_bivar_ls)

for(j in 1:N){
    multiplot(plotList = rbind(plot_univar_ls, plot_bivar_ls)[,j])
}

summary(glm(enfant ~ age_f + diplome_f + bilan_f * complet_f,
            data = couples,
            family = binomial))

summary(glm(enfant ~ age_f + bilan_f * complet_f,
            data = couples,
            family = binomial))

summary(glm(enfant ~ age_f + bilan_f,
            data = dplyr::filter(couples, complet_f),
            family = binomial))

