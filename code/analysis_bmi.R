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

# load couples and palette_enfant
load('stat_sante_copy/data/couples.RData')
load('stat_sante_copy/data/palette_enfant.RData')

##===============================================
# Boxplot
##===============================================

df_bmi <- couples %>%
    dplyr::select(id, enfant, contains("bmi")) %>%
    tidyr::gather(variable, values, -c(id, enfant)) %>%
    dplyr::mutate(
        genre = factor(sub("\\w+_([hf])$", "\\1", variable),
                       levels = c('h', 'f'),
                       labels = c('Homme', 'Femme')),
        feature = sub("(\\w+)_[hf]", "\\1", variable)
    )


dens_bmi_enfant <- ggplot(data = df_bmi,
                          mapping = aes(values, fill = enfant)) +
    geom_density(
        na.rm = TRUE,
        alpha = 0.4
    ) +
    scale_fill_manual(values = c("#00BFC4", "#F8766D"))
dens_bmi_enfant 


box_bmi_enfant <- ggplot(data = df_bmi) +
    geom_boxplot(
        mapping = aes(x = genre, y = values, fill = enfant),
        na.rm = TRUE
    )
box_bmi_enfant