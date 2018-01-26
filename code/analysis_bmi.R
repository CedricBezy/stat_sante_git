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
load('stat_sante_copy/data/couples_init.RData')
load('stat_sante_copy/data/palette_enfant.RData')

##===============================================
# Boxplot
##===============================================

dens_bmi_enfant <- ggplot(data = couples_init,
                          mapping = aes(bmi_h, fill = enfant)) +
    geom_histogram(
        mapping = aes(y = ..density..),
        na.rm = TRUE,
        binwidth = 5,
        col = "black"
    ) +
    geom_density(
        na.rm = TRUE,
        alpha = 0.4
    ) +
    scale_fill_manual(values = palette_enfant) +
    facet_wrap(~enfant) +
    ggtitle("Density: BMI", "before removing outlers")
dens_bmi_enfant

box_bmi_enfant <- ggplot(data = couples_init,
                         mapping = aes(x = "Homme", y = bmi_h, fill = enfant)) +
    geom_boxplot(
        na.rm = TRUE
    ) +
    scale_fill_manual(values = palette_enfant) +
    ggtitle("Boxplot: BMI", "before removing outlers")
box_bmi_enfant


couples_init %>% dplyr::filter(bmi_h > 45)


ggplot(data = couples_init,
       mapping = aes(x = spermo, y = bmi_h, fill = spermo)) +
    geom_boxplot(
        na.rm = TRUE
    )


##===============================================
# Boxplot
##===============================================

dens_bmi_enfant <- ggplot(data = couples,
                          mapping = aes(bmi_h, fill = enfant)) +
    geom_histogram(
        mapping = aes(y = ..density..),
        na.rm = TRUE,
        binwidth = 5,
        col = "black"
    ) +
    geom_density(
        na.rm = TRUE,
        alpha = 0.4
    ) +
    scale_fill_manual(values = palette_enfant) +
    facet_wrap(~enfant) +
    ggtitle("Density: BMI", "after removing outlers")
dens_bmi_enfant


box_bmi_enfant <- ggplot(data = couples,
                         mapping = aes(x = "Homme", y = bmi_h, fill = enfant)) +
    geom_boxplot(
        na.rm = TRUE
    ) +
    scale_fill_manual(values = palette_enfant) +
    ggtitle("Boxplot: BMI", "fter removing outlers")
box_bmi_enfant


