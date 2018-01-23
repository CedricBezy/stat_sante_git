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

##===============================================
# Barplot
##===============================================

v_namesquali <- c("diplome_h",
                  "diplome_f",
                  "patho_h_regroup",
                  "patho_f_regroup",
                  "fecondite",
                  "traitement")

v_titlequali <- c("Diplome Homme", "Diplome Femme",
                  "Pathologie Homme", "Pathologie Femme",
                  "Fecondite", "Traitement")



.make_bar_plot <- function(var_name, title_name = var_name){
    df_var <- couples %>%
        dplyr::select_("enfant", var_name)
    variable <- df_var[[var_name]]
    
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
        scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
        xlab(title_name) +
        ylab("Count") +
        ggtitle(paste("Distribution :", title_name))
    return(resplot)
}

for(i in 1:length(v_namesquali)){
    var_name <- v_namesquali[i]
    print(var_name)
    print(.make_bar_plot(var_name, v_titlequali[i]))
}



