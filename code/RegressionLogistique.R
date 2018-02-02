library(effects)
library(dplyr)
library(tidyr)
library(questionr)
library(broom)
library(ggplot2)

load("/Users/nabil/Desktop/BESante/couples.RData")
couples.bis <- couples[,-1]
couples.bis$traitement <- relevel(couples.bis$traitement, "Aucun")
couples.bis$enfant <- relevel(couples.bis$enfant, "Non")

####
#                        Construction du model 
#  Sans valeur manquntes
couples.bis <- subset(couples.bis, !is.na(couples.bis$bilan_f))
# Modele complet
reg <- glm(enfant~age_h+age_f + diplome_h + bmi_h_class_2 + spermo + cryptorchidie + fecondite +bilan_f+duree_infertilite_class+traitement,data = couples,family = binomial())
anova(reg, test="Chisq")
# modèle trivial réduit à la constante
str_constant <- "~ 1"
# modèle complet incluant toutes les explicatives potentielles
str_all <- "~ age_h+age_f + diplome_h + bmi_h_class_2 + cryptorchidie +spermo +fecondite +bilan_f+duree_infertilite_class+traitement"
require(MASS)

# Stepwise
modele <- glm(enfant ~ 1, data =couples.bis , family = binomial)
modele.stepwise <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all), 
                          trace = TRUE, data = couples, direction = "both")
# affichage du modèle final
summary(modele.stepwise)
anova(modele.stepwise, test="Chisq")

tmp <- tidy(modele.stepwise,conf.int = TRUE, exponentiate = TRUE)
str(tmp)

# Plot les coeufficients avec IC
ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
                  xmax = conf.high) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()

odds.ratio(modele.stepwise)
plot(allEffects(modele.stepwise))
# Courbe de ROC
build_roc(modele.stepwise)
attach(couples.bis)
# Histo Proba
predict(modele.stepwise,type="response")
prob.pred = predict(modele.stepwise, type="response")
par(mfrow=c(1,2))
hist(prob.pred[enfant=="Oui"], probability=T, col='light blue')
lines(density(prob.pred[enfant=='Oui']),col='red',lwd=3)
hist(prob.pred[enfant=='Non'], probability=T, col='light blue')
lines(density(prob.pred[enfant=='Non']),col='red',lwd=3)

####
#                        Construction du model 
# Le modele global
reg <- glm(enfant~age_h+age_f + diplome_h + bmi_h_class_2 + spermo + cryptorchidie + fecondite +bilan_f+duree_infertilite_class+traitement,data = couples,family = binomial())
anova(reg, test="Chisq")
#  En considerant les NA's comme une classe
levels(couples.bis$bilan_f) <- c(levels(couples.bis$bilan_f),"pas_Bilan")
couples.bis$bilan_f[is.na(couples.bis$bilan_f)] <- "pas_Bilan"
# modèle trivial réduit à la constante
str_constant <- "~ 1"
# modèle complet incluant toutes les explicatives potentielles
str_all <- "~ age_h+age_f + diplome_h + bmi_h_class_2 + spermo + cryptorchidie + fecondite +bilan_f+duree_infertilite_class+traitement"

# Stepwise
modele <- glm(enfant ~ 1, data =couples.bis , family = binomial)
modele.stepwise <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all), 
                           trace = TRUE, data = couples, direction = "both")
# affichage du modèle final
summary(modele.stepwise)
anova(modele.stepwise, test="Chisq")

tmp <- tidy(modele.stepwise,conf.int = TRUE, exponentiate = TRUE)
str(tmp)

# Plot les coeufficients avec IC
ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
                  xmax = conf.high) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()

odds.ratio(modele.stepwise)
plot(allEffects(modele.stepwise))
# Courbe de ROC
build_roc(modele.stepwise)
# Histo Proba
predict(modele.stepwise,type="response")
prob.pred = predict(modele.stepwise, type="response")
par(mfrow=c(1,2))
hist(prob.pred[enfant=="Oui"], probability=T, col='light blue')
lines(density(prob.pred[enfant=='Oui']),col='red',lwd=3)
hist(prob.pred[enfant=='Non'], probability=T, col='light blue')
lines(density(prob.pred[enfant=='Non']),col='red',lwd=3)
