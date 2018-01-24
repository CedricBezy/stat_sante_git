library(broom)
library(ggplot2)
library(effects)

load("/Users/nabil/Desktop/BESante/couples.RData")
couples.bis <- couples[,-1]

# Regression Logistique
reg <- glm(enfant ~ age_h + age_f + diplome_h + diplome_f + bmi_h + dconsultation + patho_h_regroup + patho_h_bin + cryptorchidie + fecondite +  duree_infertilite + traitement , 
           data = couples, family = binomial(logit))
summary(reg)

# Coefficient 
exp(coef(reg))
# Coefficients & IC
exp(cbind(coef(reg), confint(reg)))
# Intervalle de confiance & p-value
tmp <- tidy(reg,conf.int = TRUE, exponentiate = TRUE)
str(tmp)
# Plot les coeufficients avec IC
ggplot(tmp) + aes(x = estimate, y = term, xmin = 0, 
                  xmax = 100) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()
# Anova
anova(reg, test="Chisq")

# Pour voir l'effet de chaque variable du modele
plot(allEffects(reg))

# Regression descendante 
res <- step(reg)
