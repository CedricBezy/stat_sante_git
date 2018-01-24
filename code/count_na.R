##==================================================
# Package
##==================================================
rm(list = ls())

library(dplyr)

##==================================================
# Import Data
##==================================================

data_couples <- read.csv(
    'stat_sante_copy/data/couples.csv',
    na.strings = c('.', ''),
    stringsAsFactors = FALSE
)

count_na <- sapply(data_couples, function(x){sum(is.na(x))})
count_na <- count_na[which(count_na != 0)]

barplot(count_na)

dplyr::filter(data_couples, is.na(diplome_h))
dplyr::filter(data_couples, is.na(age_f))



