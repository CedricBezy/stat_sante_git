##---------------------------------------
# projet stat pour la sante
# 30 / 01 / 2018
# Cedric B, Nabil H.
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

source('stat_sante_copy/code/functions.R')
source('stat_sante_copy/code/multiplot.R')

##===============================================
# data summary
##===============================================

##===============================================
# AVANT D ENLEVER LES OUTLERS
##===============================================

plots_bmi_outiers <- list(
    # Density
    dens = ggplot(data = couples_init,
                  mapping = aes(bmi_h, fill = enfant)) +
        facet_wrap(~enfant) +
        geom_density(
            na.rm = TRUE,
            alpha = 0.8
        ) +
        scale_fill_manual(values = palette_enfant) +
        ggtitle("Density: BMI", "before removing outlers") +
        theme_bw(),
    # Histogramme
    hist = ggplot(data = couples_init,
                  mapping = aes(bmi_h, fill = enfant)) +
        facet_wrap(~enfant) +
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
        ggtitle("Histogram: BMI", "before removing outlers") +
        theme_bw(),
    # Boxplot
    box = ggplot(data = couples_init,
                 mapping = aes(x = "Homme", y = bmi_h, fill = enfant)) +
        geom_boxplot(
            na.rm = TRUE
        ) +
        scale_fill_manual(values = palette_enfant) +
        ggtitle("Boxplot: BMI", "before removing outlers")
)


multiplot(
    plots_bmi_outiers$dens, plots_bmi_outiers$box,
    mainTitle = "BMI before removing outliers",
    row.heights = 3,
    col.widths = c(5, 4)
)



##===============================================
# APRES AVOIR ENLEVER LES OUTLERS
##===============================================

dens_bmi_enfant <- ggplot(data = couples,
                          mapping = aes(bmi_h, fill = enfant)) +
    facet_wrap(~enfant) +
    geom_density(
        na.rm = TRUE,
        alpha = 0.8
    ) +
    scale_fill_manual(values = palette_enfant) +
    ggtitle("Density: BMI", "after removing outlers")

hist_bmi_enfant <- ggplot(data = couples,
                          mapping = aes(bmi_h, fill = enfant)) +
    facet_wrap(~enfant) +
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
    ggtitle("Histogram: BMI", "after removing outlers")

box_bmi_enfant <- ggplot(data = couples,
                         mapping = aes(x = "Homme", y = bmi_h, fill = enfant)) +
    geom_boxplot(
        na.rm = TRUE
    ) +
    scale_fill_manual(values = palette_enfant) +
    ggtitle("Boxplot: BMI", "after removing outlers")


multiplot(
    dens_bmi_enfant, box_bmi_enfant,
    mainTitle = "Distribution BMI hommmes",
    row.heights = 3,
    col.widths = c(5, 4)
)

##===============================================
# Age
##===============================================

df_age <- couples %>%
    dplyr::select(enfant, contains("age_")) %>%
    tidyr::gather(variable, values, -c(enfant)) %>%
    dplyr::mutate(
        features = factor(
            paste(variable, enfant, sep = "_"),
            levels = c("age_h_Oui", "age_h_Non", "age_f_Oui", "age_f_Non"),
            labels = c("HO", "HN", "FO", "FN")
        ),
        variable = factor(
            variable,
            levels = c("age_h", "age_f"),
            labels = c("Homme", "Femme")
        ),
        enfant = factor(
            enfant,
            levels = c("Oui", "Non"),
            labels = paste("Enfant =", c("Oui", "Non"))
        )
    )

palette_genre_enfant <- c("#58D3F7","#08088A", "#E378AA", "#80063F")

dens_age <- ggplot(mapping = aes(values, fill=features),
                   data = df_age) +
    facet_grid(variable~enfant) +
    geom_density(
        na.rm = TRUE,
        alpha = 0.6
    ) +
    scale_fill_manual(values = palette_genre_enfant) +
    guides(fill = FALSE) +
    xlab("Age") +
    ylab("Densite") +
    ggtitle("Densite Age Homme et Femme") +
    theme_bw()


box_age <- ggplot(data = df_age) +
    facet_grid(~variable, scales = "free_x") +
    geom_boxplot(
        mapping = aes(x = enfant, y = values, fill = features),
        na.rm = TRUE,
        alpha = 0.7
    ) +
    scale_fill_manual(values = palette_genre_enfant) +
    guides(fill = FALSE) +
    xlab("Age") +
    ylab("Densite") +
    ggtitle("Boxplot Age Homme et Femme") +
    theme_bw()

##===============================================
# Age
##===============================================

df_diffage <- couples %>%
    dplyr::select(id, enfant, diff_age, diplome_h, age_h, age_f, diplome_f) %>%
    dplyr::mutate(
        variable = "Difference",
        enfant = factor(
            enfant,
            levels = c("Oui", "Non"),
            labels = paste("Enfant =", c("Oui", "Non"))
        )
    )

palette_diff_enfant <- c("#B185D9", "#310D48")

dens_diffage <- ggplot(mapping = aes(diff_age, fill=enfant),
                   data = df_diffage) +
    facet_grid(variable~enfant) +
    geom_density(
        na.rm = TRUE,
        alpha = 0.8
    ) +
    guides(fill = FALSE) +
    scale_fill_manual(values = palette_diff_enfant) +
    theme_bw()


box_diffage <- ggplot(mapping = aes(x = enfant, y = diff_age, fill = enfant),
                      data = df_diffage) +
    facet_grid(~variable, scales = "free_x") +
    geom_boxplot(
        na.rm = TRUE,
        alpha = 0.8
    ) +
    guides(fill = FALSE) +
    scale_fill_manual(values = palette_diff_enfant) +
    theme_bw()

multiplot(
    dens_age, box_age, dens_diffage, box_diffage,
    nrow = 2, ncol = 2,
    col.widths = c(5, 4),
    row.heights = c(4, 2),
    mainTitle = "Age Homme et Femme + Difference, selon l'obtention d'un enfant"
)


##===============================================
# duree infertilite
##===============================================

dens_infertil_enfant <- ggplot(data = couples,
                          mapping = aes(duree_infertilite, fill = enfant)) +
    facet_wrap(~enfant) +
    geom_density(
        na.rm = TRUE,
        alpha = 0.8
    ) +
    scale_fill_manual(values = palette_enfant) +
    ggtitle("Density")


box_infertil_enfant <- ggplot(data = couples,
                         mapping = aes(x = enfant, y = duree_infertilite, fill = enfant)) +
    geom_boxplot(
        na.rm = TRUE
    ) +
    scale_fill_manual(values = palette_enfant) +
    ggtitle("Boxplot")


multiplot(
    dens_infertil_enfant, box_infertil_enfant,
    mainTitle = "Duree infertilite avant consultation",
    row.heights = 3,
    col.widths = c(5, 4)
)
