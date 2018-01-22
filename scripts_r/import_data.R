#---------------------------------------------------
# Cedric Bezy
# 22 / 01 / 2018
# Projet Stat Sante
#---------------------------------------------------

##==================================================
library(dplyr)
##==================================================

rm(list = ls())

data_couples <- read.csv(
    'stat_sante_copy/data/couples.csv',
    na.strings = c('.', ''),
    stringsAsFactors = FALSE
) 
couples <- data_couples %>%
    dplyr::mutate(
        diplome_h = factor(diplome_h,
                           levels = c('Bac-', 'Bac', 'Bac+'),
                           labels = c(-1, 0, 1)),
        diplome_f = factor(diplome_f,
                           levels = c('Bac-', 'Bac', 'Bac+'),
                           labels = c(-1, 0, 1)),
        cryptorchidie = factor(cryptorchidie,
                               levels = c('Oui', 'Non'),
                               labels = c(1, 0)),
        fecondite = factor(fecondite,
                           levels = c('primaire', 'secondaire')),
        enfant = factor(enfant,
                        levels = c(1, 0)),
        dconsultation = as.Date(dconsultation, format = '%d/%m/%Y'),
        dconception = as.Date(dconception, format = '%d/%m/%Y'),
        ddn = as.Date(ddn, format = '%d/%m/%Y'),
        patho_h = gsub('[éèê]', 'e', patho_h),
        patho_f = gsub('[éèê]', 'e', patho_f)
    )

summary(couples)
sapply(couples, class)

save(couples, file = 'stat_sante_copy/rdata/couples.RData')