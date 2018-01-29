library(ggplot2)
library(effects)
library(dplyr)
library(tidyr)
library(questionr)
library(broom)

load("/Users/nabil/Desktop/BESante/couples.RData")
couples.bis <- couples[,-1]

# Regression Logistique
reg <- glm(enfant ~ age_h, data = couples.bis, family = binomial(logit)) #+ age_f + diplome_h + diplome_f + bmi_h + patho_h_regroup + cryptorchidie + fecondite +  duree_infertilite + traitement
summary(reg)

# Coefficient 
exp(coef(reg))
# Coefficients & IC
exp(cbind(coef(reg), confint(reg)))
# Intervalle de confiance & p-value
tmp <- tidy(reg,conf.int = TRUE, exponentiate = TRUE)
str(tmp)
# Plot les coeufficients avec IC
ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
                  xmax = conf.high) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()

# Anova
anova(reg, test="Chisq")
# Pour voir l'effet de chaque variable du modele
plot(allEffects(reg))
# Regression descendante 
res <- step(reg)
#Odd Ratio
odds.ratio(reg)

####
#                        Construction du model 

# modèle trivial réduit à la constante
str_constant <- "~ 1"
# modèle complet incluant toutes les explicatives potentielles
str_all <- "~age_h+age_f+diplome_h+bmi_h+duree_infertilite+traitement"
require(MASS)

# Forward
modele <- glm(enfant ~ 1, data = couples, family = binomial)
modele.forward <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all), trace = TRUE, data = couples, direction = "forward")
# affichage du modèle final
summary(modele.forward)
# Anova
anova(modele.forward, test="Chisq")

# Backward
modele <- glm(paste("enfant",str_all), data =couples.bis , family = binomial)
modele.backward <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all), 
                           trace = TRUE, data = couples, direction ="backward")
summary(modele.backward)
# Stepwise
modele <- glm(enfant ~ 1, data = , family = binomial)
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
# Validation du modèle : Indicateurs de qualité et de robustesse
par(mfrow = c(1, 1))
plot(rstudent(modele.stepwise), type = "p", cex = 0.5, ylab = "Résidus studentisés ", 
     col = "springgreen2", ylim = c(-3, 3))
abline(h = c(-2, 2), col = "red")

