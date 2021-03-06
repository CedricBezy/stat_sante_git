#---------------------------------------------------
# Cedric Bezy
# 22 / 01 / 2018
# Projet Stat Sante
#---------------------------------------------------

##==================================================
# Package
##==================================================
rm(list = ls())

library(dplyr)
library(tibble)
library(scales)

##==================================================
# functions
##==================================================

contains_values <- function(text, vect){
    any(sapply(vect, grepl, x = text))
}

## Replace the accents
replace_accents <- function(x){
    if(is.character(x)){
        x <- gsub('[éè]', 'e', x)
        x <- gsub('[à]', 'a', x)
        x <- gsub('[ù]', 'u', x)
    }
    return(x)
}


##==================================================
# Import Data
##==================================================

couples_init <- read.csv(
    'stat_sante_copy/data/couples.csv',
    na.strings = c('.', ''),
    stringsAsFactors = FALSE
)
class(couples_init$patho_h)

couples_init <- couples_init %>%
    dplyr::mutate(
        ## Encodage
        patho_h = replace_accents(patho_h),
        patho_f = replace_accents(patho_f),
        traitement = replace_accents(traitement)
    )
unique(couples_init$patho_h)


couples_init <- couples_init %>%
    dplyr::mutate(
        ## Relevel
        enfant = factor(enfant,
                        levels = c(0, 1),
                        labels = c("Non", "Oui")),
        # format date
        dconsultation = as.Date(dconsultation, format = '%d/%m/%Y'),
        dconception = as.Date(dconception, format = '%d/%m/%Y'),
        ddn = as.Date(ddn, format = '%d/%m/%Y'),
        
        # diplome
        diplome_h = factor(diplome_h,
                            levels = c('Bac-', 'Bac', 'Bac+'),
                            labels = c('Inf_Bac', 'Bac', 'Sup_Bac')),
        diplome_f = factor(diplome_f,
                            levels = c('Bac-', 'Bac', 'Bac+'),
                            labels = c('Inf_Bac', 'Bac', 'Sup_Bac')),
        
        spermo = factor(spermo,
                        levels = c('normal', 'anormal', 'azoo')),
        cryptorchidie = factor(cryptorchidie,
                                levels = c('Oui', 'Non')),
        bh_f = factor(bh_f,
                       levels = c('normal', 'anormal')),
        ct_f = factor(
            ct_f,
            levels = c("ovulation", "dysovulation", "anovulation")
        ),
        fecondite = factor(
            fecondite,
            levels = c('primaire', 'secondaire')
        ),
        traitement = factor(
            traitement,
            levels = c("ICSI", "IAC", "FIV", "IAD", "Medical", "Aucun")
        )
    )

save(couples_init, file = 'stat_sante_copy/data/couples_init.RData')

if(readline("Update Github data (y/n): ")%in% c("y", "1")){
    # Couple Init
    save(couples_init, file = 'stat_sante_git/data/couples_init.RData')
    message("Substitution of data : done!")
}else{
    message("No substitution of data")
}
