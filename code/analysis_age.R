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
library(scales)

# load couples
load('stat_sante_copy/data/couples.RData')

##===============================================
# Boxplot
##===============================================

df_age <- couples %>%
    dplyr::select(id, enfant, contains("age")) %>%
    tidyr::gather(variable, values, -c(id, enfant)) %>%
    dplyr::mutate(
        genre = factor(sub("\\w+_([hf])$", "\\1", variable),
                       levels = c('h', 'f'),
                       labels = c('Homme', 'Femme')),
        feature = sub("(\\w+)_[hf]", "\\1", variable)
    )


dens_age <- ggplot(data = df_age) +
    geom_density(
        mapping = aes(values, fill = genre),
        na.rm = TRUE,
        alpha = 0.4
    ) +
    scale_fill_manual(values = c("#00BFC4", "#F8766D"))
dens_age

box_age <- ggplot(data = df_age) +
    geom_boxplot(
        mapping = aes(genre, values, fill = genre),
        na.rm = TRUE,
        alpha = 0.5
    ) +
    scale_fill_manual(values = c("#00BFC4", "#F8766D"))
box_age

##===============================================
# Age
##===============================================

df_diffage <- couples %>%
    dplyr::mutate(diff_age = age_h - age_f) %>%
    dplyr::select(id, enfant, diff_age)

dens_diffage <-  ggplot(data = df_diffage) +
    geom_density(
        mapping = aes(diff_age),
        fill = "#C77CFF",
        na.rm = TRUE,
        alpha = 0.5
    )
dens_diffage


box_diffage_enfant <- ggplot(data = df_diffage) +
    geom_boxplot(
        mapping = aes(x = enfant, y = diff_age, fill = enfant),
        na.rm = TRUE
    ) +
    scale_fill_manual(values = c("#00BA38", "#F8766D"))
box_diffage_enfant





