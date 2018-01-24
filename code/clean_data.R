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
# palette_enfant
##==================================================

brewer_pal(palette = "Greens", direction = 1)(9)
# [1] "#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D"
# [7] "#238B45" "#006D2C" "#00441B"

palette_enfant <- c("Oui" = "#A1D99B", "Non" = "#238B45")

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

data_couples <- read.csv(
    'stat_sante_copy/data/couples.csv',
    na.strings = c('.', ''),
    stringsAsFactors = FALSE
)
class(data_couples$patho_h)

couples <- data_couples %>%
    dplyr::mutate(
        ## Encodage
        patho_h = replace_accents(patho_h),
        patho_f = replace_accents(patho_f),
        traitement = replace_accents(traitement)
    )
unique(couples$patho_h)


couples <- couples %>%
    dplyr::mutate(
        ## Relevel
        enfant = factor(enfant,
                        levels = c(1, 0),
                        labels = c("Oui", "Non")),
        
        # format date
        dconsultation = as.Date(dconsultation, format = '%d/%m/%Y'),
        dconception = as.Date(dconception, format = '%d/%m/%Y'),
        ddn = as.Date(ddn, format = '%d/%m/%Y'),
        
        # diplome
        diplome_h = ordered(diplome_h,
                            levels = c('Bac-', 'Bac', 'Bac+'),
                            labels = c('Bac--', 'Bac', 'Bac++')),
        diplome_f = ordered(diplome_f,
                            levels = c('Bac-', 'Bac', 'Bac+'),
                            labels = c('Bac--', 'Bac', 'Bac++')),
        
        spermo = ordered(spermo,
                         levels = c('normal', 'anormal', 'azoo')),
        cryptorchidie = ordered(cryptorchidie,
                                levels = c('Oui', 'Non')),
        bh_f = ordered(bh_f,
                       levels = c('normal', 'anormal')),
        ct_f = factor(
            ct_f,
            levels = c("ovulation", "dysovulation", "anovulation")
        ),
        fecondite = ordered(
            fecondite,
            levels = c('primaire', 'secondaire')
        ),
        traitement = factor(
            traitement,
            levels = c("ICSI", "IAC", "FIV", "IAD", "Medical", "Aucun")
        )
    ) %>%
    dplyr::filter(
        !is.na(diplome_h)
    )

##==================================================
# Duree Infertilite
##==================================================

diff_age <- couples$age_h - couples$age_f

couples <- couples %>% add_column(diff_age, .after = "fecondite")


duree_infertilite_class <- with(couples, {
    cut(duree_infertilite,
        breaks = c(0, 24, max(duree_infertilite, na.rm = TRUE) + 1),
        labels = c("inf_24", "24_sup"),
        include.lowest = FALSE,
        right = TRUE,
        ordered_result = TRUE
    )
})
couples <- couples %>% add_column(duree_infertilite_class, .after = "duree_infertilite")

# BMI
# <16 : Anorexie ; 
# 16 < Maigreur < 18,5 ;
# 18,5< normal < 25 ;
# 25< surpoids < 30 ; 
# 30 < obese < 40 ; 
# >40 massive

bmi_h_class <- with(couples, {
    cut(bmi_h,
        breaks = c(10, 16, 18.5, 25, 30, 40, 60),
        labels = c("Anorexie", "Maigreur", "Normal", "Surpoids", "Obese", "Massive"),
        include.lowest = FALSE,
        right = TRUE,
        ordered_result = TRUE
    )
})
couples <- couples %>% add_column(bmi_h_class, .after = "bmi_h")



##==================================================
# Pathologie Homme
##==================================================

patho_h <- couples$patho_h
patho_h <- gsub(" *, *", ",", patho_h)
patho_h <- gsub(" +", "_", patho_h)
patho_h <- gsub(",", ", ", patho_h)


all_patho_h <- unique(unlist(strsplit(patho_h, ", ")))
all_patho_h

# [1] "non"                     "chimiotherapie"                     
# [3] "autre"                   "pathologies_respiratoire_chroniques"
# [5] "hodgkin"                 "radiotherapie"                      
# [7] "sinusites_chroniques"    "diabete"                            
# [9] "cancer_testis"           "sarcome"                            
# [11] "neurologique" 


# Pathologie

v_chimio <- c("chimiotherapie",
              "cancer_testis",
              "radiotherapie",
              "hodgkin",
              "sarcome")

v_chronic <- c("pathologies_respiratoire_chroniques",
               "sinusites_chroniques",
               "diabete")

v_autre <- setdiff(all_patho_h, c("non", v_chimio, v_chronic))


patho_h_bin <- ordered(patho_h != 'non',
                       levels = c(TRUE, FALSE),
                       labels = c(1, 0))

# Chimiotherapie
patho_h_regroup = ordered(
    ifelse(patho_h == "non", "non",
           ifelse(patho_h %in% v_chimio, "chimio",
                  ifelse(patho_h %in% v_chronic, "chronic", "autre"))),
    levels = c("chimio", "chronic", "autre", "non")
)
table(patho_h_regroup)





# patho_h_chimio <- factor(
#     sapply(patho_h,
#            contains_values,
#            vect = v_chimio,
#            USE.NAMES = FALSE),
#     levels = c(TRUE, FALSE),
#     labels = c(1, 0)
# )
# 
# 
# patho_h_chronic <- factor(
#     sapply(patho_h,
#            contains_values,
#            vect = v_chronic,
#            USE.NAMES = FALSE),
#     levels = c(TRUE, FALSE),
#     labels = c(1, 0)
# )
# 
# patho_h_autre <- factor(
#     sapply(patho_h,
#            contains_values,
#            vect = v_autre,
#            USE.NAMES = FALSE),
#     levels = c(TRUE, FALSE),
#     labels = c(1, 0)
# )

couples <- couples %>%
    dplyr::mutate(
        patho_h = patho_h
    ) %>%
    tibble::add_column(
        patho_h_regroup,
        patho_h_bin,
        .after = "patho_h"
    )

##==================================================
# Pathologie Femme
##==================================================

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

patho_f_bin <- ordered(
    (patho_f != 'non'),
    levels = c(TRUE, FALSE),
    labels = c(1, 0)
)

patho_f_regroup <- ordered(
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



# patho_f_endometriose <- factor(
#     !is.na(patho_f) & patho_f == "endometriose",
#     levels = c(TRUE, FALSE),
#     labels = c(1, 0)
# )
# 
# patho_f_tubaire <- factor(grepl("tubaire", patho_f),
#                           levels = c(TRUE, FALSE),
#                           labels = c(1, 0))

couples <- couples %>%
    dplyr::mutate(
        patho_f = patho_f
    ) %>%
    tibble::add_column(
        patho_f_regroup,
        patho_f_bin,
        .after = "patho_f"
    )

##==================================================
# Save
##==================================================

summary(couples)
sapply(couples, class)

save(couples, file = 'stat_sante_copy/data/couples.RData')
save(couples, file = 'stat_sante_git/data/couples.RData')

save(palette_enfant, file = 'stat_sante_copy/data/palette_enfant.RData')
save(palette_enfant, file = 'stat_sante_git/data/palette_enfant.RData')

