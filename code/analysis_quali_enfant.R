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
load('stat_sante_copy/data/palette_enfant.RData')

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

ggplot(data = df_summa_enfant,
       mapping = aes(enfant, pct)) +
    geom_bar(
        mapping = aes(fill = enfant),
        stat = "identity",
        col = "black"
    ) +
    geom_text(
        mapping = aes(label = eff),
        position = position_stack(vjust = 0.6)
    ) +
    geom_text(
        mapping = aes(label = pct_str),
        position = position_stack(vjust = 0.5),
        size = 6,
        fontface = "bold"
    ) +
    scale_fill_manual(values = palette_enfant) +
    scale_y_continuous(
        breaks = seq(0, 100, 10)
    ) +
    xlab("Enfant") +
    ylab("Effectif") +
    ggtitle("Capacité pour un couple à avoir un enfant") +
    theme_bw(base_size = 12)

##===============================================
# FUNCTIONS 
##===============================================

name <- "traitement"
name <- "bh_f"
pareto <- FALSE


make_summary <- function(name)
{
    df_var <- couples %>%
        dplyr::select_("enfant", name)
    
    ## summarise
    df_summa_var <- df_var %>%
        dplyr::group_by_(name) %>%
        dplyr::summarise(eff_tot = n())
    df_summa_var_enfant <- df_var %>%
        dplyr::group_by_(name, "enfant") %>%
        dplyr::summarise(eff = n())
    
    ## result
    res_df <- merge(df_summa_var_enfant, df_summa_var, by = name) %>%
        dplyr::arrange_(name, "enfant") %>%
        dplyr::mutate(
            pct = 100 * eff / eff_tot,
            pct_str = paste(formatC(pct, digits = 1, format = "f"), "%")
        )
    return(res_df)
}


make_bar_plot <- function(var_name, var_label = var_name, empile = FALSE)
{
    # make data
    df_plot <- make_summary(var_name)
    tab_var <- unique(df_plot[[var_name]])
    # Labels
    moda_eff <- paste(
        df_plot[[var_name]],
        paste0("(", df_plot$eff_tot, ")")
    )
    x_labels <- unique(moda_eff)
    ## Khi.square test
    khi2 <- chisq.test(
        table(couples[[var_name]], couples$enfant)
    )
    khi2_pvalue <- formatC(
        khi2$p.value,
        digits = 3,
        format = ifelse(khi2$p.value < 0.001, "e", "f")
    )
    
    ## Plot
    if(empile){
        resplot <- ggplot(mapping = aes_string(var_name, "pct", fill = "enfant"),
                          data = df_plot) +
            geom_hline(
                mapping = aes(yintercept = 50),
                col = "black",
                size = 1.5
            ) +
            geom_bar(
                stat = "identity",
                col = "black"
            ) +
            scale_x_discrete(
                name = var_label,
                labels = x_labels
            )
    }else{
        df_plot[["x_labels"]] <- factor(moda_eff, levels = unique(x_labels))
        resplot <- ggplot(mapping = aes_string("enfant", "pct", fill = "enfant"),
                          data = df_plot) +
            facet_wrap(~x_labels) +
            geom_bar(
                stat = "identity",
                col = "black"
            )
    }
    resplot <- resplot +
        geom_text(
            mapping = aes(label = pct_str),
            position = position_stack(vjust = 0.5),
            size = 6,
            fontface = "bold"
        ) +
        scale_fill_manual(values = palette_enfant) +
        scale_y_continuous(
            name = "Frequence (%)",
            breaks = seq(0, 100, 10)
        ) +
        ggtitle(paste("Distribution de la variable ", var_label),
                paste("Khi2 p.value =", khi2_pvalue))
    return(resplot)
}



vect_quali <- c(
    diplome_h = "Diplome Homme",
    diplome_f = "Diplome Femme",
    patho_h_regroup = "Pathologie de l'homme",
    patho_f_regroup = "Pathologie de la femme",
    cryptorchidie = "Cryptorchidie",
    spermo = "Grade d\'infertilite de l\'homme",
    bh_f = "Bilan Hormonale de la femme",
    ct_f = "Ovulation de la femme (selon courbe de temperature)",
    fecondite = "type d\'infecondite",
    traitement = "Type de traitement de l\'infecondite"
)


for(i in 1:length(vect_quali)){
    var_name <- names(vect_quali)[i]
    print(var_name)
    print(make_bar_plot(var_name, vect_quali[i]))
}

