#---------------------------------------------------
# Cedric Bezy
# 25 / 01 / 2018
# Projet Stat Sante
#---------------------------------------------------

rm(list = ls())
load('stat_sante_git/data/couples_init.RData')

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

##================================================================
# Valeurs manquantes
##================================================================
##-----------------------------------.
# Bar_Plot
##-----------------------------------.

na_barplot(couples_init)

##-----------------------------------.
# Data 
##-----------------------------------.

nb_na_rows <- apply(couples_init, 1, count_na) - 1

df_for_selection <- couples_init %>%
    dplyr::select(id, enfant, contains("_h"), contains("_f")) %>%
    tibble::add_column(nb_na = nb_na_rows) %>%
    dplyr::mutate(
        nb_na_f = (is.na(ct_f) + is.na(bh_f) + is.na(patho_f)),
        ok_diplome_f = !is.na(diplome_f),
        ok_bilan_f = (is.na(ct_f) + is.na(bh_f) + is.na(patho_f)) <= 2,
        ok_diplome_h = !is.na(diplome_h),
        ok_bmi_h = !is.na(bmi_h) & bmi_h > 15 & bmi_h < 45
    )

##================================================================.
# Filtres
##================================================================.

couples <- couples_init %>%
    dplyr::filter(
        !is.na(bmi_h) & between(bmi_h, 15, 45),
        !is.na(diplome_h),
        !is.na(age_f)
    )

##-----------------------------------.
# Bar_Plot
##-----------------------------------.

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
        ordered_result = TRUE
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



##-----------------------------------.
# Pathologie Homme
##-----------------------------------.

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


##================================================================.
# couples homme
##================================================================.

couples_hommes <- couples %>%
    dplyr::select(id, enfant, contains("_h"), spermo, cryptorchidie,
                  fecondite, duree_infertilite, duree_infertilite_class, traitement)

na_barplot(couples_hommes)

couples_femmes <- couples %>%
    dplyr::select(id, enfant, contains("_f"),
                  fecondite, duree_infertilite, duree_infertilite_class, traitement) %>%
    dplyr::filter(!is.na(diplome_f),
                  (is.na(bh_f) + is.na(ct_f) + is.na(patho_f)) < 2)

na_barplot(couples_femmes)

##================================================================.
# Save
##================================================================.

summary(couples)
sapply(couples, class)

if(readline("Remove data (y/n): ")%in% c("y", "1")){
    save(couples, file = 'stat_sante_copy/data/couples.RData')
    save(couples, file = 'stat_sante_git/data/couples.RData')
    
    save(couples_hommes, file = 'stat_sante_copy/data/couples_hommes.RData')
    save(couples_hommes, file = 'stat_sante_git/data/couples_hommes.RData')
    
    save(couples_femmes, file = 'stat_sante_copy/data/couples_femmes.RData')
    save(couples_femmes, file = 'stat_sante_git/data/couples_femmes.RData')
    message("Substitution of data : done")
}else{
    message("No substitution of data")
}
