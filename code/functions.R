
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

make_summary_quali <- function(name,
                               data = couples,
                               with_enfant = (name != "enfant"),
                               keep_na = TRUE)
{
    df_var <- data %>%
        dplyr::select_("enfant", name)
    
    ## summarise
    df_summa_var <- df_var %>%
        dplyr::group_by_(name) %>%
        dplyr::summarise(eff_tot = n())
    
    if(with_enfant & (name != "enfant")){
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
        if(!keep_na){
            res_df <- res_df %>%
                dplyr::filter_(paste0("!is.na(", name, ")"))
        }
    }else{
        res_df <- df_var %>%
            dplyr::group_by_(name) %>%
            dplyr::summarise(eff = n()) %>%
            tibble::add_column(variable = name, .before = name)
        if(!keep_na){
            res_df <- res_df %>%
                dplyr::filter_(paste0("!is.na(", name, ")"))
        }
        res_df <- res_df %>%
            dplyr::mutate(
                pct = 100 * eff / sum(eff),
                pct_str = paste(formatC(pct, digits = 1, format = "f"), "%")
            )
    }
    return(res_df)
}

##------------------------.
# Graph Univarie
##------------------------.

.build_bar_plot_variable <- function(var_name,
                                     data,
                                     var_title = var_name,
                                     palette = NA,
                                     empile = FALSE,
                                     display_na = TRUE, ...)
{
    # make data for plotting
    df_plot <- make_summary_quali(
        name = var_name,
        data = data,
        with_enfant = FALSE,
        keep_na = display_na
    )
    # Labels
    
    ## Khi.square test
    moda_eff <- sprintf("%s (%d)", df_plot[[var_name]], df_plot$eff)
    x_labels <- unique(moda_eff)
    
    ## Plot Title
    main_title <- var_title
    
    ## Plot
    if(empile){
        resplot <- ggplot(mapping = aes_string("variable", "pct", fill = var_name),
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
        resplot <- ggplot(mapping = aes_string(var_name, "pct", fill = var_name),
                          data = df_plot) +
            facet_wrap(~variable) +
            geom_bar(
                stat = "identity",
                col = "black"
            ) +
            geom_text(
                mapping = aes(label = eff),
                vjust = -1,
                size = 4
            )
            
    }
    
    resplot <- resplot +
        ggtitle(main_title) +
        geom_text(
            mapping = aes(label = pct_str),
            position = position_stack(vjust = 0.5),
            size = 5,
            fontface = "bold"
        ) +
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
        resplot <- resplot + if(!is.na(palette)){
            scale_fill_brewer(
                labels = x_labels,
                palette = palette,
                direction = -1
            )
        }else{
            scale_fill_hue(labels = x_labels)
        }
    }
    return(resplot)
}

##------------------------.
# Graph Bivarie selon enfant
##------------------------.

.build_bar_plot_enfant <- function(var_name,
                                   data,
                                   var_title = var_name, 
                                   empile = TRUE,
                                   display_na = TRUE, ...)
{
    # make data
    df_plot <- make_summary_quali(
        name = var_name,
        data = data,
        with_enfant = TRUE,
        keep_na = display_na
    )
    
    # Make Moda
    moda_eff <- sprintf("%s (%d)", df_plot[[var_name]], df_plot$eff_tot)
    x_labels <- unique(moda_eff)
    
    # Khi2.test
    tab_var <- table(data[[var_name]], data$enfant, useNA = "no")
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
                name = var_name,
                labels = x_labels
            )
    }else{
        df_plot[["x_labels"]] <- factor(moda_eff, levels = unique(x_labels))
        # Plot
        resplot <- ggplot(mapping = aes_string("enfant", "pct", fill = "enfant"),
                          data = df_plot) +
            facet_wrap(~x_labels) +
            geom_bar(
                stat = "identity",
                col = "black"
            )
    }
    resplot <- resplot +
        ggtitle(main_title, sub_title) +
        geom_text(
            mapping = aes(label = pct_str),
            position = position_stack(vjust = 0.5),
            size = 5,
            fontface = "bold"
        ) +
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

build_bar_plot <- function(var_name,
                           data = couples,
                           with_enfant = (var_name != "enfant"),
                           ...)
{
    with_enfant <- with_enfant & (var_name != "enfant")
    if(with_enfant){
        resplot <- .build_bar_plot_enfant(var_name,
                                          data = data,
                                          ... = ...)
    }else{
        resplot <- .build_bar_plot_variable(var_name,
                                            data = data,
                                            ... = ...)
    }
    return(resplot)
}

##==================================================
# Dimensions for viewer
##==================================================

niceDimensions <- function(n, nrow = NULL, ncol = NULL){
    sqn <- sqrt(n)
    chooseDims <- c(!is.null(nrow) & !is.null(ncol), 
                    !is.null(nrow) & is.null(ncol),
                    is.null(nrow) & !is.null(ncol),
                    is.null(nrow) & is.null(ncol))
    Dims <- switch(
        which(chooseDims),
        "1" = c(nrow, ncol),
        "2" = c(nrow, ceiling(n / nrow)),
        "3" = c(ceiling(n / ncol), ncol),
        "4" = c(ceiling(n / ceiling(sqn)), ceiling(sqn))
    )
    return(Dims)
}


##================================================
# Multiplot
##================================================

# ...       : ggplots           # (plots can be named except with a still-exist-parameter's name)
# plotlist  : list of ggplots   # (plots can be named)
# ncol      : number of columns
# byrow     : TRUE / FALSE      # if list graphics is sorted by row (TRUE) or by columns (FALSE)
# plotsTitle
# mainTitles


multiplot <- function(...,
                      plotList = NULL,
                      nrow = NULL,
                      ncol = NULL,
                      byrow = TRUE,
                      row.heights = NULL,
                      col.widths = NULL,
                      withPlotsTitle = TRUE,
                      mainTitle = NULL ){
    
    require(grid)
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Make a list from the ... arguments and plotlist
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Lplots <- c(list(...), plotList)
    nb.plots <- length(Lplots)
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # DIMENSION OF THE LAYOUT
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Dims <- niceDimensions(nb.plots, nrow, ncol)
    NR <- Dims[1]
    NC <- Dims[2]
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # MAKE THE LAYOUT PANEL
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mat_layout <- matrix( 
        seq(1, NC * NR),
        nrow = NR,
        ncol = NC,
        byrow = byrow
    )
    
    # Parameters not available yet to be chosen by users
    
    # Display graphes
    if(nb.plots== 1){
        print(Lplots[[1]]) 
    }else{
        okMainTitle <- !is.null(mainTitle)
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## DIMENSION LAYOUT
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # hauteur de ligne
        nrowLayout <- 2 * NR - 1
        nbHei <- length(row.heights)
        row.heights <- c(
            row.heights[1:min(nbHei, NR)], 
            if(nbHei < NR){rep(1, NR - nbHei)}
        )
        layout_rows <- c(3, rep(c(0.3, 3), times = NR-1))
        seqrow <- seq(1, nrowLayout, 2)
        layout_rows[seqrow] <- layout_rows[seqrow] * row.heights
        
        if(okMainTitle){
            mt <- 1
            margeTitle <- 1 * length(gregexpr("\n", mainTitle)[[1]])
            layout_rows <- c(margeTitle, layout_rows)
            nrowLayout <- nrowLayout + 1
        }else{
            mt <- 0
        }
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## largeur de colonne
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        okMainLegend <- FALSE
        ncolLayout <- NC + ifelse(okMainLegend, 1, 0)
        nbWid <- length(col.widths)
        col.widths <- c(col.widths[1:min(nbWid, NC)], 
                        if(nbWid < NC){rep(1, NC - nbWid)})
        layout_cols <- c(5 * col.widths, 
                         if(okMainLegend){ 2 } )
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Set up the page
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        grid.newpage()
        pushViewport( viewport(
            layout = grid.layout( 
                nrow = nrowLayout, 
                ncol = ncolLayout,
                widths = grid::unit(layout_cols, units = "null"),
                heights = grid::unit(layout_rows, units = "null")
            )
        ))
        mainTitleSets <- bquote(bold(.(mainTitle)))
        ## Title of the Page
        if(okMainTitle){
            grid.text(
                mainTitleSets, 
                vp = viewport(
                    layout.pos.row = 1, 
                    layout.pos.col = 1:ncolLayout
                )
            )
        }
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Make each plot, in the correct location
        # Get the i,j matrix positions of the regions that contain this subplot
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for(i in 1:nb.plots){
            # coordonnees Layout of the subplot
            matchidx <- as.data.frame(
                which(mat_layout == i, arr.ind = TRUE)
            )
            iPlot <- Lplots[[i]]
            if( length(iPlot)!=0 & !(class(iPlot)[1] %in% c("logical", "NULL")) ){
                # title of the subplot
                if(is.na(withPlotsTitle)){
                    iTitle <- names(Lplots)[i]
                    iPlot <- iPlot +
                        ggtitle(iTitle)
                }else{
                    if(withPlotsTitle){
                        iPlot <- iPlot
                    }else{
                        iPlot <- iPlot +
                            ggtitle(NULL)
                    }
                }
                
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # print the plot
                ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                iRow <- 2 * matchidx$row - 1 + mt
                iCol <- matchidx$col
                print(
                    iPlot,
                    vp = viewport(
                        layout.pos.row = iRow,
                        layout.pos.col = iCol
                    )
                )
            } # end if
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # the ith graph is printed
            ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        } # end for
    } # end if
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # END
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    invisible()
}
