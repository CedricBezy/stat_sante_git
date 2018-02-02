library(dplyr)

load("/Users/nabil/Desktop/BESante/couples.RData")
load("/Users/nabil/Desktop/BESante/couples_hommes.RData")
couples.bis <- couples[,-1]

eliminer = c("patho_f_bin"," patho_f_regroup","ddn","dconception"," bh_f","ct_f")
# Supprimer les lignes qui ont des NAN's
supprimer = c("age_f", "diplome_h")
remplacer = c("diplome_f","bmi_h")

# bmi_h   : Remplacer NA par la moyenne
couples$bmi_h.imp.mean <- ifelse(is.na(couples$bmi_h), mean(couples$bmi_h, na.rm=TRUE), couples$bmi_h)
couples.bis$bmi_h <- couples$bmi_h.imp.mean
# diplome_f  :  Remplacer NA par la modalite la plus pertinente
couples.bis$diplome_f <- as.factor(couples.bis$diplome_f)
couples.bis$diplome_f[is.na(couples.bis$diplome_f)] <- 3

# Supprimer les variables qui ont plus de 300 NA's
couples.bis <-dplyr::select(couples.bis, -c(patho_f_bin,patho_f_regroup,ddn,dconception,bh_f,ct_f))

# bh_f : 359 NA's replacer par normal 
couples.bis$bh_f <- as.factor(couples.bis$bh_f)
couples.bis$bh_f[is.na(couples.bis$bh_f)] <- "normal"
# ct_f : 355 NA’s : 614 ovulation
couples.bis$ct_f[is.na(couples.bis$ct_f)] <- "ovulation"
# patho_f : 368 NA’s : 80% non
couples.bis$patho_f[is.na(couples.bis$patho_f)] <- "non"


# Supprimer les individies qui ont 3 NA's ou moins
couples.bis <- subset(couples.bis, !is.na(couples.bis$diplome_h))
couples.bis <- subset(couples.bis, !is.na(couples.bis$age_f))

summary(couples.bis)

reg <- glm(enfant ~ age_h + age_f + diplome_h + diplome_f + bmi_h + dconsultation + patho_h + patho_h_regroup + patho_h_bin + cryptorchidie + fecondite +  duree_infertilite + traitement + spermo, 
           data = couples.bis, family = binomial(logit))
res <- step(reg)

tmp <- tidy(reg, exponentiate = TRUE)
str(tmp)
library(ggplot2)
ggplot(tmp) + aes(x = estimate, y = term, xmin = 0, 
                  xmax = 100) + geom_vline(xintercept = 1) + 
  geom_errorbarh() + geom_point() + scale_x_log10()
