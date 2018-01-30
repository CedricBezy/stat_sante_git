#---------------------------------------------------
# Cedric Bezy
# 25 / 01 / 2018
# Projet Stat Sante
#---------------------------------------------------

rm(list = ls())

library(dplyr)
library(tibble)

load('stat_sante_copy/data/couples_init.RData')

##==================================================
# Functions
##==================================================

contains_values <- function(text, vect){
    any(sapply(vect, grepl, x = text))
}

count_na <- function(x){
    sum(is.na(x))
}

na_barplot <- function(df){
    nb_na <- sapply(df, count_na)
    nb_na <- nb_na[which(nb_na != 0)]
    if(length(nb_na)){
        barplot(nb_na, main="Number of NA")
    }
    return(nb_na)
}

strfind <- function(x, vect, xsep = ";"){
    return(any(strsplit(x, split = xsep)[[1]] %in% vect))
}


##================================================================
# Valeurs manquantes
##================================================================
##-----------------------------------.
# Bar_Plot
##-----------------------------------.

na_barplot(couples_init)

##-----------------------------------.
# Filter
##-----------------------------------.

couples <- couples_init %>%
    dplyr::filter(
        !is.na(bmi_h) & between(bmi_h, 15, 45),
        !is.na(diplome_h),
        !is.na(age_f)
    )

# barplot
na_barplot(couples)

##================================================================.
# Creation de variables
##================================================================.
##-----------------------------------.
# Difference age
##-----------------------------------.

diff_age <- couples$age_h - couples$age_f

couples <- couples %>% 
    tibble::add_column(diff_age, .after = "fecondite")

##-----------------------------------.
# Duree Infertilite
##-----------------------------------.
duree_infertilite_class <- with(couples, {
    cut(duree_infertilite,
        breaks = c(0, 24, max(duree_infertilite, na.rm = TRUE) + 1),
        labels = c("inf_24", "24_sup"),
        include.lowest = FALSE,
        right = TRUE,
        ordered_result = FALSE
    )
})
couples <- couples %>% add_column(duree_infertilite_class, .after = "duree_infertilite")

##-----------------------------------.
# BMI
##-----------------------------------.

# <16 : Anorexie ; 
# 16 < Maigreur < 18,5 ;
# 18,5< normal < 25 ;
# 25< surpoids < 30 ; 
# 30 < obese < 40 ; 
# >40 massive

bmi_h_class_6 <- with(couples, {
    cut(bmi_h,
        breaks = c(10, 16, 18.5, 25, 30, 40, 60),
        labels = c("Anorexie", "Maigreur", "Normal", "Surpoids", "Obese", "Massive"),
        include.lowest = FALSE,
        right = TRUE,
        ordered_result = FALSE
    )
})

bmi_h_class_2 <- with(couples, {
    cut(bmi_h,
        breaks = c(16, 25, 60),
        labels = c("Normal", "Surpoids"),
        include.lowest = FALSE,
        right = TRUE,
        ordered_result = FALSE
    )
})

couples <- couples %>%
    tibble::add_column(bmi_h_class_6, bmi_h_class_2, .after = "bmi_h")



##-----------------------------------.
# Pathologie Homme
##-----------------------------------.

patho_h <- couples$patho_h
patho_h <- gsub(" *, *", ",", patho_h)
patho_h <- gsub(" +", "_", patho_h)
patho_h <- gsub(",", ";", patho_h)


all_patho_h <- table(unlist(strsplit(patho_h, ";")))
all_patho_h

# [1] "non"                     "chimiotherapie"                     
# [3] "autre"                   "pathologies_respiratoire_chroniques"
# [5] "hodgkin"                 "radiotherapie"                      
# [7] "sinusites_chroniques"    "diabete"                            
# [9] "cancer_testis"           "sarcome"                            
# [11] "neurologique" 

table(couples_init$patho_h)
# autre 
# 227 
# cancer testis , chimiotherapie 
# 2 
# chimiotherapie 
# 5 
# chimiotherapie , radiotherapie 
# 2 
# diabete 
# 7 
# hodgkin , chimiotherapie , radiotherapie 
# 1 
# neurologique 
# 1 
# non 
# 842 
# pathologies respiratoire chroniques 
# 9 
# sarcome , chimiotherapie 
# 1 
# sinusites chroniques 
# 33 
# sinusites chroniques , pathologies respiratoire chroniques 
# 1 


# Chimio
v_chimio <- c("chimiotherapie",
              "cancer_testis",
              "radiotherapie",
              "hodgkin",
              "sarcome")

v_chronic <- c("pathologies_respiratoire_chroniques",
               "sinusites_chroniques",
               "diabete")

v_autre <- setdiff(all_patho_h, c("non", v_chimio, v_chronic))


patho_h_bin <- factor(patho_h != 'non',
                       levels = c(TRUE, FALSE),
                       labels = c(1, 0))

x <- patho_h[832]
vect <- v_chimio
# Chimiotherapie
patho_h_regroup = factor(
    ifelse(
        patho_h == "non",
        "non",
        ifelse(
            sapply(patho_h, strfind, vect = v_chimio, xsep = ";"),
            "chimio",
            ifelse(
                sapply(patho_h, strfind, vect = v_chronic, xsep = ";"),
                "chronic",
                "autre"
            )
        )
    ),
    levels = c("chimio", "chronic", "autre", "non")
)
table(patho_h_regroup)


couples <- couples %>%
    dplyr::mutate(
        patho_h = patho_h
    ) %>%
    tibble::add_column(
        patho_h_regroup,
        patho_h_bin,
        .after = "patho_h"
    )

##-----------------------------------.
# Pathologie Femme
##-----------------------------------.

patho_f <- couples$patho_f
patho_f <- gsub(" *, *", ",", patho_f)
patho_f <- gsub(" +", "_", patho_f)
patho_f <- gsub(",", ", ", patho_f)

all_patho_f <- unique(unlist(strsplit(patho_f, ", ")))
all_patho_f
table(patho_f)

# autre          endometriose          hydrosalpinx 
#    18                    17                     2 
# non  pb tubaire bilateral pb tubaire unilateral 
# 647                    14                    65

patho_f_bin <- factor(
    (patho_f != 'non'),
    levels = c(TRUE, FALSE),
    labels = c(1, 0)
)

patho_f_regroup <- factor(
    ifelse(
        test = is.na(patho_f),
        yes = NA,
        no = ifelse(
            test = (patho_f %in% c("non", "endometriose")),
            yes = patho_f,
            no = ifelse(
                test = grepl("tubaire", patho_f),
                yes = "tubaire",
                no = "autre"
            )
        )
    ),
    levels = c("endometriose", "tubaire", "autre", "non")
)
summary(patho_f_regroup)


couples <- couples %>%
    dplyr::mutate(
        patho_f = patho_f
    ) %>%
    tibble::add_column(
        patho_f_regroup,
        patho_f_bin,
        .after = "patho_f"
    )

##-----------------------------------.
# Bilan Femme
##-----------------------------------.

df_bilan <- couples %>%
    dplyr::select(id, enfant, bh_f, ct_f, patho_f)


nb_na_bilan_f <- with(couples, is.na(bh_f) + is.na(ct_f) + is.na(patho_f))

complet_f = (nb_na_bilan_f == 0)


bilan_f <- with(couples, {
    factor(
        ifelse(
            test = (nb_na_bilan_f == 3),
            yes = NA,
            no = ifelse(
                test = (is.na(bh_f) | bh_f == "normal") &
                    (is.na(ct_f) | ct_f == "ovulation") &
                    (is.na(patho_f) | patho_f == "non"),
                yes = 0,
                no = 1
            )
        ),        
        levels = c(1, 0),
        labels = c("dysfonc", "normal")
    )
})

## ADD TO couples
couples <- couples %>%
    tibble::add_column(
        bilan_f = bilan_f,
        complet_f = complet_f,
        .before = "bh_f"
    )

##================================================================.
# remake couples
##================================================================.
couples <- droplevels(couples) %>%
    tibble::add_column(
        delta = with(couples, {
            ifelse(!is.na(dconception),
                   dconception - dconsultation,
                   ddn - dconsultation)
        }),
        .after = "ddn"
    )

##================================================================.
# Save
##================================================================.

save(couples, file = 'stat_sante_copy/data/couples.RData')

if(readline("Update Github data (y/n): ")%in% c("y", "1")){
    save(couples, file = 'stat_sante_git/data/couples.RData')
    message("Substitution of data : done")
}else{
    message("No substitution of data")
}
