
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

##==================================================
# palette_enfant
##==================================================

brewer_pal(palette = "Greens", direction = 1)(9)
# [1] "#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D"
# [7] "#238B45" "#006D2C" "#00441B"

palette_enfant <- c("Oui" = "#A1D99B", "Non" = "#238B45")

##==================================================
# Make Data Summary
##==================================================

count_na <- function(x){sum(is.na(x))}

##==================================================
# Make Data Summary
##==================================================
##----------------------
# quali
##----------------------

make_summary_quali <- function(var_name,
                               data = couples,
                               with_enfant = (var_name != "enfant"),
                               keep_na = TRUE)
{
    ## Cette fonction cree une data.frame de resume de la variable donnee par var_name (string).
    ## dans la data frame donnee en data (par defaut, couple)
    ## Si with_enfant est TRUE, alors le resume est croisee avec la variable enfant,
    ## sinon l'etude est univarie
    
    df_var <- data %>%
        dplyr::select_("enfant", var_name)
    
    ## summarise
    df_summa_var <- df_var %>%
        dplyr::group_by_(var_name) %>%
        dplyr::summarise(eff_tot = n())
    
    if(with_enfant & (var_name != "enfant")){
        df_summa_var_enfant <- df_var %>%
            dplyr::group_by_(var_name, "enfant") %>%
            dplyr::summarise(eff = n())
        
        ## result
        res_df <- merge(df_summa_var_enfant, df_summa_var, by = var_name) %>%
            dplyr::arrange_(var_name, "enfant") %>%
            dplyr::mutate(
                pct = 100 * eff / eff_tot
            )
        if(!keep_na){
            res_df <- res_df %>%
                dplyr::filter_(paste0("!is.na(", var_name, ")"))
        }
    }else{
        res_df <- df_var %>%
            dplyr::group_by_(var_name) %>%
            dplyr::summarise(eff = n()) %>%
            tibble::add_column(variable = var_name, .before = var_name)
        if(!keep_na){
            res_df <- res_df %>%
                dplyr::filter_(paste0("!is.na(", var_name, ")"))
        }
        res_df <- res_df %>%
            dplyr::mutate(
                pct = 100 * eff / sum(eff)
            )
    }
    return(res_df)
}

##------------------------.
# Graph Univarie
##------------------------.

.build_barplot_variable <- function(var_name,
                                     data = couples,
                                     var_title = var_name,
                                     palette = NA,
                                     empile = FALSE,
                                     display_na = TRUE, ...)
{
    ## Cette fonction cree un diagramme en barre univarie sur la variable en question.
    ## Pour cela, utilisation de ggplot2
    
    # make data for plotting
    df_plot <- make_summary_quali(
        var_name = var_name,
        data = data,
        with_enfant = FALSE,
        keep_na = display_na
    ) %>%
        dplyr::mutate(
            eff_pct = sprintf("%d (%s%%)", eff, formatC(pct, digits = 1, format = "f")),
            pct_str = sprintf("%s%%", formatC(pct, digits = 1, format = "f"))
        )
    
    x_labels = sprintf("%s (%d)", df_plot[[var_name]], df_plot$eff)
    df_plot[["x_labels"]] <- ordered(
        x_labels,
        levels = unique(x_labels)
    )
    ## Plot Title
    main_title <- var_title
    
    ## Plot
    if(empile){
        resplot <- ggplot(mapping = aes_string("variable", "pct", fill = "x_labels"),
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
            geom_text(
                mapping = aes(label = eff_pct),
                position = position_stack(vjust = 0.5),
                size = 5,
                fontface = "bold"
            )
    }else{
        # Plot
        resplot <- ggplot(mapping = aes_string(var_name, "pct", fill = var_name),
                          data = df_plot) +
            facet_wrap(~variable) +
            geom_bar(
                stat = "identity",
                col = "black"
            ) +
            geom_text(
                mapping = aes(label = eff),
                position = position_stack(vjust = 0.70),
                size = 4
            ) +
            geom_text(
                mapping = aes(label = pct_str),
                position = position_stack(vjust = 0.45),
                size = 5,
                fontface = "bold"
            )
    }
    
    resplot <- resplot +
        ggtitle(main_title) +
        scale_y_continuous(
            name = "Frequence (%)",
            breaks = seq(0, 100, 10)
        )
    if(var_name == "enfant"){
        palette <- palette_enfant
    }
    if(length(palette) >= 2){
        resplot <- resplot + scale_fill_manual(
                labels = x_labels,
                values = palette
            )
    }else{
        if(!is.na(palette)){
            scale_fill_brewer(
                labels = x_labels,
                palette = palette,
                direction = -1
            )
        }
    }
    return(resplot)
}

##------------------------.
# Graph Bivarie selon enfant
##------------------------.

.build_barplot_enfant <- function(var_name,
                                  data = couples,
                                  var_title = var_name, 
                                  empile = TRUE,
                                  display_na = TRUE, ...)
{
    # make data for plotting
    df_plot <- make_summary_quali(
        var_name = var_name,
        data = data,
        with_enfant = TRUE,
        keep_na = display_na
    ) %>%
        dplyr::mutate(
            pct_str = sprintf("%s%%", formatC(pct, digits = 1, format = "f"))
        )
    # x_labels
    x_labels = sprintf("%s (%d)", df_plot[[var_name]], df_plot$eff_tot)
    df_plot[["x_labels"]] <- ordered(
        x_labels,
        levels = unique(x_labels)
    )
    # Khi2.test
    tab_var <- table(droplevels(data[[var_name]]), data$enfant, useNA = "no")
    khi2_test <- chisq.test(tab_var)
    khi2_pvalue <- formatC(
        khi2_test$p.value,
        digits = 3,
        format = ifelse(khi2_test$p.value < 0.001, "e", "f")
    )
    
    ## Plot Title
    main_title <- var_title
    sub_title <- paste("Khi2 p.value =", khi2_pvalue)
    
    ## Plot
    if(empile){
        resplot <- ggplot(mapping = aes_string("x_labels", "pct", fill = "enfant"),
                          data = df_plot) +
            geom_hline(
                mapping = aes(yintercept = 50),
                col = "black",
                size = 1.5
            ) +
            geom_bar(
                stat = "identity",
                col = "black"
            )
    }else{
        # Plot
        resplot <- ggplot(mapping = aes_string("enfant", "pct", fill = "enfant"),
                          data = df_plot) +
            facet_wrap(~x_labels) +
            geom_bar(
                stat = "identity",
                col = "black",
                position = "dodge"
            )
    }
    
    resplot <- resplot +
        geom_text(
            mapping = aes(label = eff),
            position = position_stack(vjust = 0.75),
            size = 4
        ) +
        geom_label(
            mapping = aes(label = pct_str),
            position = position_stack(vjust = 0.50),
            size = 5,
            fontface = "bold"
        )
    
    resplot <- resplot +
        ggtitle(main_title, sub_title) +
        scale_y_continuous(
            name = "Frequence (%)",
            breaks = seq(0, 100, 10)
        ) + 
        scale_fill_manual(values = palette_enfant)
    
    return(resplot)
}

##------------------------.
# Fonction principale
##------------------------.
# ... :
# var_title = var_name
# palette = NA,
# empile = FALSE,
# display_na = TRUE

build_barplot <- function(var_name,
                           data = couples,
                           with_enfant = (var_name != "enfant"),
                           ...)
{
    ## Cette fonction cree un diagramme en barre univarie ou bivarie (selon "with_enfant")
    ## sur la variable donnee par var_name
    ## Requiert l'utilisation de ggplot2
    
    with_enfant <- with_enfant & (var_name != "enfant")
    if(with_enfant){
        resplot <- .build_barplot_enfant(var_name,
                                          data = data,
                                          ... = ...)
    }else{
        resplot <- .build_barplot_variable(var_name,
                                            data = data,
                                            ... = ...)
    }
    return(resplot)
}



##==================================================
# Make Train Test
##==================================================

train_test_split <- function(df, p_train = 0.75){
    N <- nrow(df)
    n_train <- floor(p_train * N)
    rows_train <- sample(1:N, n_train, replace = FALSE)
    rows_test <- setdiff(1:N, rows_train)
    df_train <- df[rows_train,]
    df_test <- df[rows_test,]
    res_list <- list(train = df_train, test = df_test)
    return(res_list)
}

##==================================================
# Make Train Test
##==================================================

get_tab_features <- function(tab){
    if(any(dim(tab)!=c(2,2))){
        warning("Dimension incorect, Resultat réduit a une matrice 2-2")
    }
    TP <- tab[1, 1]
    FP <- tab[1, 2]
    FN <- tab[2, 1]
    TN <- tab[2, 2]
    ntot <- TP + FP + FN + TN
    pct_error <- (FN + FP)/ntot
    sensi <- TP / (TP + FN)
    speci <- TN / (TN + FP)
    res_list <- list(
        sensibility = sensi,
        specificity = speci,
        error = pct_error
    )
    return(res_list)
}

##==================================================
# Plotting Regression Lineaire
##==================================================

build_roc <- function(reg, col = "#F8766D",
                      main_title = "Courbe Roc", sub_title = NULL){
    # Roc curve
    roc_glm <- lroc(reg, graph = FALSE)
    # make data
    df_plot <- as.data.frame.matrix(roc_glm$diagnostic.table) %>%
        tibble::remove_rownames()
    df_plot[,1] <- 1 - df_plot[,1]
    colnames(df_plot) <- c("Sp", "Se")
    auc <- roc_glm$auc
    
    # Init Plot
    rocplot <- ggplot(data = df_plot) + 
        ggtitle(main_title, sub_title)
    
    # theme
    rocplot <- rocplot +
        geom_segment(x = 0, y = 0, xend = 0, yend = 1, col = "black", linetype = 3) +
        geom_segment(x = 0, y = 1, xend = 1, yend = 1, col = "black", linetype = 3) +
        geom_segment(x = 0, y = 0, xend = 1, yend = 0, col = "black", linetype = 3) +
        geom_segment(x = 1, y = 0, xend = 1, yend = 1, col = "black", linetype = 3) +
        scale_x_continuous(
            name = "1 - Specificity",
            breaks = seq(0, 1, 0.25),
            limits = c(0, 1)
        ) +
        scale_y_continuous(
            name = "Sensibility",
            breaks = seq(0, 1, 0.25),
            limits = c(0, 1)
        )
    
    # Layers
    rocplot <- rocplot +
        geom_line(
            mapping = aes(x = 1-Sp, y = Se),
            col = col,
            lwd = 1
        ) +
        geom_ribbon(
            mapping = aes(x = 1-Sp, ymin = 1-Sp, ymax = Se),
            fill = col,
            alpha = 0.5
        ) +
        geom_segment(x = 0, y = 0, xend = 1, yend = 1, col = "black")
    
    # Text
    rocplot <- rocplot +
        geom_text(
            label = paste("AUC = ", round(auc, 4)),
            x = 0.70,
            y = 0.31,
            size = 5
        )
    reslist <- list(
        plot = rocplot,
        data = df_plot,
        auc = auc
    )
    return(reslist)
}
    
    
    


# build_glm_rocs <- function(..., regs = list(), palette = NA){
#     regs <- c(list(...), regs)
#     if(is.null(names(regs))){
#         names(regs) <- paste("GML", 1:length(regs))
#     }
#     rocs_ls <- sapply(output_data_glm)
#     df_plot <- bind_rows(rocs_ls["diagnostic",]) %>%
#         dplyr::mutate("reg_log" = factor(reg_log))
#     df_auc <- data.frame(
#         "reg_log" = names(regs),
#         "auc" = unlist(rocs_ls["auc",])
#     ) %>%
#         dplyr::mutate(
#             auc = round(auc, 4)
#         )
#     
#     resplot <- ggplot(data = df_plot) +
#         facet_wrap(~reg_log) +
#         geom_line(
#             mapping = aes(x = 1-Sp, y = Se, col = reg_log)
#         ) +
#         geom_ribbon(
#             mapping = aes(x = x, ymin = x, ymax = y, fill = reg_log),
#             alpha = 0.5
#         ) +
#         geom_text(
#             mapping =  aes(label = paste("AIC = ", auc)),
#             x = 0.60, y = 0.25,
#             data = df_auc
#         )
#     
#     resplot <- resplot +
#         guides(col = FALSE, fill = FALSE)
#     
#     resplot <- resplot +
#         geom_segment(x = 0, y = 0, xend = 1, yend = 1, col = "black") +
#         geom_segment(x = 0, y = 0, xend = 0, yend = 1, col = "black", linetype = 3) +
#         geom_segment(x = 0, y = 1, xend = 1, yend = 1, col = "black", linetype = 3) +
#         geom_segment(x = 0, y = 0, xend = 1, yend = 0, col = "black", linetype = 3) +
#         geom_segment(x = 1, y = 0, xend = 1, yend = 1, col = "black", linetype = 3)
#     
#     
#     if(length(palette) >= max(2, length(regs))){
#         resplot <- resplot + scale_fill_manual(values = palette)
#     }else{
#         if(!is.na(palette)){
#             resplot <- resplot + scale_fill_brewer(
#                 palette = palette,
#                 direction = -1
#             )
#         }
#     }
#      +
#         ggtitle("ROC curves")
#     return(resplot)
# }


