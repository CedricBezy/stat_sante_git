load("/Users/nabil/Desktop/BESante/couples.RData")
couples.bis <- couples[,-1]

# Refression Logistique
reg <- glm(enfant ~ age_h + age_f + diplome_h + diplome_f + bmi_h + dconsultation + patho_h_regroup + patho_h_bin + cryptorchidie + fecondite +  duree_infertilite + traitement , 
           data = couples, family = binomial(logit))
res <- step(reg)

summary(reg)
# Coeufficient 
exp(coef(reg))

# Intervalle de confiance 
library(broom)
tmp <- tidy(reg, conf.int = TRUE, exponentiate = TRUE)
str(tmp)
library(ggplot2)
ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
                  xmax = conf.high) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()




