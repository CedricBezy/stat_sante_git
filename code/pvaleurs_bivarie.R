##===============================================
# Preparation Environnement
##===============================================
rm(list = ls())

# import packages
library(dplyr)
library(tibble)
library(tidyr)

# load couples
load('stat_sante_copy/data/couples.RData')
source('stat_sante_copy/code/functions.R')

##=================================================================
# Variables
##=================================================================

vect_quali <- c(
    age_h = "Age Homme",
    diplome_h = "Diplome Homme",
    bmi_h = "BMI Homme",
    bmi_h_class_6 = "Bmi Homme regroupe par classe (6)",
    bmi_h_class_2 = "Bmi Homme regroupe par classe (2)",
    patho_h_bin = "Anomalie Pathologie de l'homme : Oui / Non",
    patho_h_regroup = "Anomalie Pathologie de l'homme, regroupement",
    cryptorchidie = "Cryptorchidie",
    spermo = "Grade d\'infertilite de l\'homme",
    age_f = "Age Femme",
    diplome_f = "Diplome Femme",
    bh_f = "Bilan Hormonale de la femme",
    ct_f = "Ovulation de la femme (selon courbe de temperature)",
    patho_f_bin = "Anomalie Pathologie de la femme",
    patho_f_regroup = "Anomalie Pathologie de la femme, regroupement",
    bilan_f = "Bilan Feminin",
    fecondite = "type d\'infecondite",
    duree_infertilite = "Duree d\'infertilite",
    duree_infertilite_class = "Duree d\'infertilite par classe (Inferieur / Superieur a 2 ans)",
    traitement = "Type de traitement de l\'infecondite"
)
N <- length(vect_quali)

##=================================================================
# Make data
##=================================================================

pvals_df <- lapply(
    names(vect_quali), 
    function(var_name){
        # reg log
        reg_log <- glm(
            formula(paste("enfant ~", var_name)),
            data = couples,
            family = binomial("logit")
        )
        p_reglog <- anova(reg_log, test = "Chisq")[2,5]
        p_reglog <- format_pvalue(p_reglog, 3)
        
        # factor
        if(is.factor(couples[[var_name]])){
            p_chisq <- chisq.pvalue(
                table(couples[[var_name]], couples[["enfant"]])
            )
            p_chisq <- format_pvalue(p_chisq, 3)
            reslist <- list(
                variable = var_name,
                reg_log = p_reglog,
                chi2_enfant = p_chisq
            )
        }else{
            p_wilcox <- wilcox.test(
                formula(paste(var_name, "~ enfant")),
                data = couples
            )$p.value
            p_wilcox <- format_pvalue(p_wilcox, 3)
            reslist <- list(
                variable = var_name,
                reg_log = p_reglog,
                wilcox = p_wilcox
            )
        }
        return(reslist)
    }
) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(variable)

##=================================================================
# Write CSV
##=================================================================

write.csv2(
    pvals_df,
    "stat_sante_copy/output/tabs/pvaleurs_reglog_wilcox_khi2.csv",
    row.names = FALSE
)

write.csv2(
    pvals_df,
    "stat_sante_git/output/tabs/pvaleurs_reglog_wilcox_khi2.csv",
    row.names = FALSE
)
