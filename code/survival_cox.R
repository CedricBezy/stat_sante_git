##===============================================
# Preparation Environnement
##===============================================
rm(list = ls())

# import packages
library(dplyr)
library(tibble)
library(tidyr)
library(survival)
library(ggplot2)
library(GGally)

# load couples
load('stat_sante_copy/data/couples.RData')

source('stat_sante_copy/code/functions.R')

ok_github <- any(readline("Output on Github (y / n): ") %in% c("y", "Y", "1"))

##=================================================================
# Preparation Environnement
##=================================================================

dfCouples <- couples %>%
    tibble::column_to_rownames("id") %>%
    dplyr::mutate(
        ok_enfant = (enfant=="Oui"),
        enfant = relevel(enfant, ref = "Non"),
        traitement = relevel(traitement, ref = "Aucun")
    ) %>%
    tibble::add_column(
        ct_f_bin = factor(
            ifelse(couples$ct_f == "ovulation", "non", "oui"),
            levels = c("non", "oui")
        )
    )

dfCouples$kaplan <- with(data = dfCouples, Surv(delta, ok_enfant))
sapply(dfCouples, class)
summary(dfCouples)

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
    ct_f_bin =  "Probleme d'Ovulation de la femme (oui / non)",
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
# Preparation Environnement
##=================================================================

var_name <- "age_f"

pvals_df <- lapply(
    names(vect_quali), 
    function(var_name, Data = dfCouples){
        # reg log
        reg_log <- glm(
            formula(paste("enfant ~", var_name)),
            data = Data,
            family = binomial("logit")
        )
        p_reglog <- anova(reg_log, test = "Chisq")[2,5]
        
        cox <- coxph(
            formula(paste("kaplan ~", var_name)),
            data = Data   
        )
        p_likelihood <- summary(cox)$logtest['pvalue']
        p_wald <- summary(cox)$waldtest['pvalue']
        p_logrank <- summary(cox)$sctest['pvalue']
        # factor
        reslist <- list(
            variable = var_name,
            is_factor = ifelse(is.factor(dfCouples[[var_name]]), "x", NA),
            reg_log = p_reglog,
            log_rank = p_logrank,
            likelihood = p_likelihood,
            wald = p_wald
        )
        return(reslist)
    }
) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(variable)

if(ok_github){
    write.csv2(
        pvals_df,
        "stat_sante_copy/output/tabs/pvaleurs_reglog_survival.csv",
        row.names = FALSE
    )
    write.csv2(
        pvals_df,
        "stat_sante_git/output/tabs/pvaleurs_reglog_survival.csv",
        row.names = FALSE
    )
}

##=================================================================
# Kaplan-Meyer Curve, manuellement
##=================================================================
##-------------------.
# Survie Global
##-------------------.

kaplan_1 <- survfit(kaplan~1, data = dfCouples)


kmplot <- ggsurv(
    kaplan_1,
    size.est = 1,
    size.ci = 0,
    surv.col = c("black"),
    cens.shape = 3
) +
    scale_x_continuous(
        name = "Time",
        breaks = seq(0, 3652.5, 365.25),
        labels = seq(0, 10)
    ) +
    theme_bw()

if(ok_github){
    png("stat_sante_git/output/plots_kaplan/0_basique.png", width = 500, height = 350)
    kmplot
    dev.off()
}


##=================================================================
# Kaplan-Meyer Curve, automatisation
##=================================================================

meta_vars <- list(
    bh_f = list(
        name = "bh_f",
        intitule = "Bilan Hormonale de la femme"
    ),
    bilan_f = list(
        name = "bilan_f",
        intitule = "Bilan Feminin"
    ),
    bmi_h_class_2 = list(
        name = "bmi_h_class_2",
        intitule = "Bmi Homme regroupe par classe (2)"
    ),
    bmi_h_class_6 = list(
        name = "bmi_h_class_6",
        intitule = "Bmi Homme regroupe par classe (6)"
    ),
    ct_f = list(
        name = "ct_f",
        intitule = "Ovulation de la femme (selon courbe de temperature)"
    ),
    ct_f_bin = list(
        name = "ct_f_bin",
        intitule = "Probleme d'Ovulation de la femme (oui / non)"
    ),
    cryptorchidie = list(
        name = "cryptorchidie",
        intitule = "Cryptorchidie"
    ),
    diplome_f = list(
        name = "diplome_f",
        intitule = "Diplome Femme"
    ),
    diplome_h = list(
        name = "diplome_h",
        intitule = "Diplome Homme"
    ),
    duree_infertilite_class = list(
        name = "duree_infertilite_class",
        intitule = "Duree d\'infertilite : Inferieur / Superieur a 2 ans"
    ),
    patho_h_bin = list(
        name = "patho_h_bin",
        intitule = "Anomalie Pathologie de l'homme : Oui / Non"
    ),
    patho_h_regroup = list(
        name = "patho_h_regroup",
        intitule = "Anomalie Pathologie de l'homme, regroupement"
    ),
    patho_f_bin = list(
        name = "patho_f_bin",
        intitule = "Anomalie Pathologie de la femme"
    ),
    patho_f_regroup = list(
        name = "patho_f_regroup",
        intitule = "Anomalie Pathologie de la femme, regroupement"
    ),
    fecondite = list(
        name = "fecondite",
        intitule = "type d\'infecondite"
    ),
    spermo = list(
        name = "spermo",
        intitule = "Grade d\'infertilite de l\'homme",
        colors = c("#66BD63", "#FDAE61", "#D73027")
    ),
    traitement = list(
        name = "traitement",
        intitule = "Type de traitement de l\'infecondite",
        colors = c(
            Aucun = "#5D5756",
            FIV = "#F8766D",
            Medical = "#01F911",
            IAD = "#3C92E8",
            ICSI = "#F901B7",
            IAC = "#E6A535"
        )
    )
)
N <- length(meta_vars)

var_name <- "traitement"
metavar <- meta_vars[[var_name]]
Data <- dfCouples

build_plot_survival <- function(metavar, Data = dfCouples, ...){
    # verify variable
    var_name <- metavar$name
    ok_fact <- is.factor(Data[[var_name]])
    
    if(ok_fact){
        ## fits and diff
        iform <- formula(paste("kaplan ~", var_name))
        kmfit <- survfit(iform, data = dfCouples)
        kmdiff <- survdiff(iform, data = dfCouples)
        p_val <- 1 - pchisq(kmdiff$chisq, length(kmdiff$n) - 1)
        
        # initialize plot
        if(!is.null(metavar$colors)){
            resplot <- ggsurv(kmfit, surv.col = metavar$colors, ...)
        }else{
            resplot <- ggsurv(kmfit, ...)
        }
        
        resplot <- resplot +
            scale_x_continuous(
                name = "Time (Year)",
                breaks = seq(0, 3652.5, 365.25),
                labels = seq(0, 10)
            ) +
                scale_y_continuous(
                    name = "Proportion",
                    breaks = seq(0, 1, 0.1)
                ) +
                ggtitle(
                    sprintf("Survival Diagramme for %s", var_name),
                    sprintf("Log Rank = %s", format_pvalue(p_val, 4))
                )
        return(resplot)
    }else{
        invisible(NULL)
    }
}


plots_ls <- lapply(meta_vars, build_plot_survival, Data = dfCouples, size.est = 1)
# plots_ls

if(ok_github){
    for(name in names(plots_ls)){
        if(!is.null(plots_ls[[name]])){
            print(name)
            png(filename = sprintf("stat_sante_git/output/plots_kaplan/%s.png", name),
                width = 500, height = 350)
            print(plots_ls[[name]])
            dev.off()
        }
    }
}

##=================================================================
# Modele de Cox
##=================================================================

cox_fit_plain <- coxph(
    formula = kaplan ~ traitement + duree_infertilite_class + spermo +
        age_h + age_f + bmi_h + diplome_h ,
    data = dfCouples
)
cox_fit_plain
summary(cox_fit_plain)


cox_fit_1 <- coxph(
    formula = kaplan ~ traitement + duree_infertilite_class + spermo +
        age_h + age_f + bmi_h + diplome_h + patho_f_bin,
    data = dfCouples
)
cox_fit_1
summary(cox_fit_1)


cox_fit_2 <- coxph(
    formula = kaplan ~ traitement + duree_infertilite_class + spermo +
        age_h + age_f + bmi_h + diplome_h + patho_f_bin + ct_f,
    data = dfCouples
)
cox_fit_2
summary(cox_fit_2)

    