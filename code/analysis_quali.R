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

palette_enfant <- c("Oui" = "#A1D99B", "Non" = "#41AB5D")

##===============================================
# Analysis Enfant
##===============================================

df_summa_enfant <- couples %>%
    dplyr::group_by(enfant) %>%
    dplyr::summarise(eff = n()) %>%
    dplyr::mutate(
        pct = 100 * eff / sum(eff),
        pct = paste(formatC(pct, digits = 1, format = "f"), "%")
    )

ggplot(data = df_summa_enfant,
       mapping = aes(enfant, eff)) +
    geom_bar(
        mapping = aes(fill = enfant),
        stat = "identity"
    ) +
    geom_text(
        mapping = aes(label = eff)
    ) +
    geom_text(
        mapping = aes(label = pct),
        position = position_stack(vjust = 0.5),
        size = 6,
        fontface = "bold"
    ) +
    scale_fill_manual(values = palette_enfant) +
    xlab("Enfant") +
    ylab("Effectif") +
    ggtitle("Capacité pour un couple à avoir un enfant") +
    theme_bw(base_size = 12)



##===============================================
# Barplot
##===============================================


v_namesquali <- c("diplome_h",
                  "diplome_f",
                  "patho_h_regroup",
                  "patho_f_regroup",
                  "cryptorchidie",
                  "spermo",
                  "bh_f",
                  "ct_f",
                  "fecondite",
                  "traitement")

v_titlequali <- c("Diplome Homme", "Diplome Femme",
                  "Pathologie Homme", "Pathologie Femme",
                  "cryptorchidie",
                  "spermo",
                  "bh_f",
                  "ct_f",
                  "Fecondite", "Traitement")



.make_bar_plot <- function(var_name, title_name = var_name){
    df_var <- couples %>%
        dplyr::select_("enfant", var_name)
    variable <- df_var[[var_name]]
    
    df_summa_var <- df_var %>%
        dplyr::group_by_(var_name, "enfant") %>%
        dplyr::summarise(eff = n()) %>%
        dplyr::mutate(
            pct = 100 * eff / sum(eff),
            pct = paste(formatC(pct, digits = 1, format = "f"), "%")
        )
    
    khi2 <- chisq.test(table(variable, df_var$enfant))
    khi2_pvalue <- formatC(khi2$p.value,
                           digits = 3,
                           format = ifelse(khi2$p.value < 0.001, "e", "f"))
    print(khi2)
    
    if(!is.ordered(variable)){
        order_table <- sort(table(variable), decreasing = TRUE)
        df_var[[var_name]] <- ordered(variable,
                                   levels = names(order_table))
    }
    
    ## Plot
    resplot <- ggplot(data = df_var) +
        geom_bar(
            mapping = aes_string(var_name, fill = "enfant"),
            col = "black",
            position = "dodge"
        ) +
        scale_fill_manual(values = palette_enfant) +
        xlab(title_name) +
        ylab("Count") +
        ggtitle(paste("Distribution de la variable ", title_name),
                paste("Khi2 p.value =", khi2_pvalue))
    return(resplot)
}

for(i in 1:length(v_namesquali)){
    var_name <- v_namesquali[i]
    print(var_name)
    print(.make_bar_plot(var_name, v_titlequali[i]))
}

