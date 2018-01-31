##---------------------------------------
# projet stat pour la sante
# 22 / 01 / 2017
# Cedric B
##---------------------------------------

##===============================================
# Preparation Environnement
##===============================================
rm(list = ls())

# import packages
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(scales)
library(questionr)
library(effects)
library(ROCR)
library(epiDisplay)

# load couples
load('stat_sante_copy/data/couples.RData')
source('stat_sante_copy/code/functions.R')
source('stat_sante_copy/code/multiplot.R')

couples$traitement <- relevel(couples$traitement, "Aucun")

levels(couples$bilan_f) <- c(levels(couples$bilan_f), "no_bilan")
couples$bilan_f[is.na(couples$bilan_f)] <- "no_bilan"

##===============================================
# Reg Log
##===============================================

reg_age <- glm(
    enfant~age_h * age_f,
    data = couples,
    family = binomial()
)

anova(reg_age, test = "Chisq")

build_roc(reg_age)

reg_infertil <- glm(
    enfant~duree_infertilite * traitement,
    data = couples,
    family = binomial()
)
anova(reg_infertil, test = "Chisq")
build_roc(reg_infertil)


reg_test <- glm(
    enfant ~ age_h * age_f + diplome_h + bmi_h_class_2 + patho_h_bin + bilan_f +
        spermo * traitement + duree_infertilite * traitement,
    data = dplyr::filter(couples, bilan_f != 'no_bilan') %>% droplevels(),
    family = binomial(logit)
)

reg_test <- glm(
    enfant ~ age_h * age_f + diplome_h + bmi_h_class_2 + patho_h_bin + bilan_f +
        spermo * traitement + duree_infertilite * traitement,
    data = couples,
    family = binomial(logit)
)
anova(reg_test, test = "Chisq")
summary(reg_test)
# step
reg_test_step <- step(reg_test)
anova(reg_test_step, test = "Chisq")


multiplot(
    build_roc(reg_test, col = "#F8766D")$plot,
    build_roc(reg_test_step, col = "#00BFC4")$plot
)

anova(reg_1, test = "Chisq")
summary(reg_1)
OR <- exp(reg_1$coefficients)

plot(allEffects(reg_1))
par(mfrow = c(2, 2))
plot(reg_1)
par(mfrow = c(1, 1))
questionr::odds.ratio(reg_1)

##===============================================
# Reg Log Training
##===============================================

samples_couples <- train_test_split(couples)
couples_train <- samples_couples$train
couples_test <- samples_couples$test

table(couples_train$enfant)

reg_1_train <- glm(
    enfant ~ age_h * age_f + diplome_h + bmi_h_class_2 + patho_h_bin + bilan_f +
        spermo * traitement + duree_infertilite * traitement,
    data = couples_train,
    family = binomial(logit)
)
anova(reg_1_train, test = "Chisq")

reg_1_step <- step(reg_1_train)
anova(reg_1_step, test = "Chisq")


y_pred <- predict.glm(
    reg_1_step, 
    newdata = couples_test,
    type = "response"
)
y_pred_fact <- factor(
    ifelse(y_pred < 0.5, "Oui", "Non"),
    levels = c("Oui", "Non")
)

hue_colors <- hue_pal()(2)

tab_confus <- table(y_pred_fact, couples_test$enfant)
get_tab_features(tab_confus)
roc_train <- build_roc(
    reg_1_train,
    main_title = "Starting Regression",
    col = hue_colors[1]
)


roc_step <- build_roc(
    reg_1_step,
    main_title = "AIC Regression",
    col = hue_colors[2]
)

multiplot(roc_train$plot, roc_step$plot)




# Package ROCR
# pred <- prediction(
#     y_pred,
#     couples_test$enfant,
#     label.ordering = levels(couples_test$enfant)
# )
# perf_acc <- performance(pred, "acc")
# plot(perf_acc)
# abline(c(0, 0), c(1, 1), col = "red")
# 
# perf_auc <- performance(pred, "auc")
# perf_auc@y.values[[1]]
# 
# perf_rch <- performance(pred,  measure = "tpr", x.measure = "fpr")
# plot(perf_rch)

# x = perf_rch@x.values
# y = perf_rch@y.values[[1]] - perf_rch@x.values[[1]]

# abline(c(0, 0), c(1, 1), col = "red")



# Doesnt work
# dfr <- output_data_glm(reg)$diagnostic
# dfr$x <- 1 - dfr$Sp
# dfr$y <- dfr$Se
# 
# z <- dfr$y - dfr$x
# dfr$x
# fz <- function(z){dfr$y[which.min(abs(z - dfr$x))]}
# sum(lintegrate(dfr$x, dfr$y, dfr$x))

##===============================================
# Sub Reg Log
##===============================================


# sub_couples_ls <- split(couples, factor(!is.na(couples$bilan_f),
#                                         levels = c(TRUE, FALSE),
#                                         labels = c("bilan", 'incomplet')))
# 
# 
# glm(enfant ~ bmi_h + spermo + age_f + traitement + bilan_f,
#     data = sub_couples_ls$'bilan',
#     family = binomial(logit))
# 
# glm(enfant ~ bmi_h + spermo + age_f + traitement,
#         data = sub_couples_ls$'incomplet',
#         family = binomial(logit))


# summary(glm(enfant ~ age_f + diplome_f + bilan_f * complet_f,
#             data = couples,
#             family = binomial))
# 
# summary(glm(enfant ~ age_f + bilan_f * complet_f,
#             data = couples,
#             family = binomial))
# 
# summary(glm(enfant ~ age_f + bilan_f,
#             data = dplyr::filter(couples, complet_f),
#             family = binomial))
