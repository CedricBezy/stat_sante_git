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