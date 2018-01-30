##---------------------------------------
# projet stat pour la sante
# 30 / 01 / 2018
# Cedric B, Nabil H., Riwan M.
##---------------------------------------

##===============================================
# Preparation Environnement
##===============================================
rm(list = ls())

# import packages
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# load couples
load('stat_sante_copy/data/couples.RData')

source('stat_sante_copy/code/functions.R')
source('stat_sante_copy/code/multiplot.R')

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
# Analysis factors
##===============================================

df_factors <- couples %>%
    purrr::keep(is.factor) %>%
    dplyr::select(-enfant)

sapply(df_factors, function(x){
    table(droplevels(x), couples$enfant, useNA = "no")
})

df_summary_factors <- data.frame(
    variable = colnames(df_factors),
    nb_levels = sapply(df_factors, nlevels),
    nb_na = sapply(df_factors, count_na),
    khi2_enfant_pvalue = sapply(df_factors, function(x){
        chisq.test(table(droplevels(x), couples$enfant, useNA = "no"))$p.value
    })
) %>%
    tibble::remove_rownames()

df_summary_factors <- df_summary_factors %>%
    dplyr::arrange(khi2_enfant_pvalue)

##===============================================
# Analysis factors
##===============================================

vect_quali <- c(
    diplome_h = "Diplome Homme",
    bmi_h_class_6 = "Bmi Homme regroupe par classe (6)",
    bmi_h_class_2 = "Bmi Homme regroupe par classe (2)",
    patho_h_bin = "Anomalie Pathologie de l'homme : Oui / Non",
    patho_h_regroup = "Anomalie Pathologie de l'homme, regroupement",
    cryptorchidie = "Cryptorchidie",
    spermo = "Grade d\'infertilite de l\'homme",
    diplome_f = "Diplome Femme",
    bh_f = "Bilan Hormonale de la femme",
    ct_f = "Ovulation de la femme (selon courbe de temperature)",
    patho_f_bin = "Anomalie Pathologie de la femme",
    patho_f_regroup = "Anomalie Pathologie de la femme, regroupement",
    bilan_f = "Bilan Feminin",
    fecondite = "type d\'infecondite",
    traitement = "Type de traitement de l\'infecondite",
    duree_infertilite_class = "Duree d\'infertilite par classe (Inferieur / Superieur a 2 ans)"
)
N <- length(vect_quali)

##---------------------------
# Univarie
##---------------------------

plots_univar_ls <- mapply(
    FUN = build_barplot,
    var_name = names(vect_quali)[1:N],
    MoreArgs = list(
        data = couples,
        with_enfant = FALSE,
        var_title = "Distribution Univariee"
    ),
    SIMPLIFY = FALSE
)
# multiplot(plotList = plots_univar_ls)

##---------------------------
# Bivarie : croise avec enfant
##---------------------------

plots_bivar_ls <- mapply(
    FUN = build_barplot,
    var_name = names(vect_quali)[1:N],
    MoreArgs = list(
        data = couples,
        empile = FALSE,
        with_enfant = TRUE,
        display_na = FALSE,
        var_title = "Distribution croisee avec \"enfant\""
    ),
    SIMPLIFY = FALSE
)
# multiplot(plotList = plots_bivar_ls)



##===============================================
# Exportation
##===============================================

mat_plots <- rbind(plots_univar_ls, plots_bivar_ls)

ok_plots <- readline("Display Plots (y/n): ")%in% c("y", "1")

ok_git <- readline("Update Github (y/n): ")%in% c("y", "1")

write.csv2(
    df_summary_factors,
    "stat_sante_copy/output/tabs/descr_summary_factors.csv",
    row.names = FALSE
)

if(ok_git){
    write.csv2(
        df_summary_factors,
        "stat_sante_git/output/tabs/descr_summary_factors.csv",
        row.names = FALSE
    )
}


for(j in colnames(mat_plots)){
    print(j)
    if(ok_plots){
        multiplot(
            plotList = mat_plots[,j],
            row.heights = 3,
            col.widths = c(2, 3),
            mainTitle = sprintf("\"%s\" : %s", j, vect_quali[j])
        )
    }
    png(filename = sprintf("stat_sante_copy/output/plots/%s.png", j),
        width = 800, height = 400)
    multiplot(
        plotList = mat_plots[,j],
        row.heights = 3,
        col.widths = c(2, 3),
        mainTitle = sprintf("\"%s\" : %s", j, vect_quali[j])
    )
    dev.off()
    if(ok_git){
        png(filename = sprintf("stat_sante_git/output/plots/%s.png", j),
            width = 800, height = 400)
        multiplot(
            plotList = mat_plots[,j],
            row.heights = 3,
            col.widths = c(2, 3),
            mainTitle = sprintf("\"%s\" : %s", j, vect_quali[j])
        )
        dev.off()
    }
}

