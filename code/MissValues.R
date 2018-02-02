#missForest
#install.packages("missForest")
library(missForest)
library(VIM)
library(lattice)
library(mice)

load("/Users/nabil/Desktop/BESante/couples_init.RData")
couples.bis <- couples_init[,-c(1:5)]
couples.bis <- couples.bis[,which(sapply(couples.bis, function(x){sum(is.na(x))})>=1)]
# bmi_h   : Remplacer NA par la moyenne
#couples$bmi_h.imp.mean <- ifelse(is.na(couples$bmi_h), mean(couples$bmi_h, na.rm=TRUE), couples$bmi_h)
#couples.bis$bmi_h <- couples$bmi_h.imp.mean
# Supprimer les individies qui ont 3 NA's ou moins
couples.bis <- subset(couples.bis, !is.na(couples.bis$diplome_h))
couples.bis <- subset(couples.bis, !is.na(couples.bis$age_f))

######
#         missFOREST
summary(couples)
couples.imp <- missForest(couples, maxiter = 10, ntree = 100, variablewise = FALSE,
                          decreasing = FALSE, verbose = FALSE, replace = TRUE,
                          classwt = NULL, cutoff = NULL, strata = NULL,
                          sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                          xtrue = NA, parallelize = c('no', 'variables', 'forests'))
couples.imp$OOBerror
#####
couples_aggr = aggr(couples.bis, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(couples.bis), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
couples_aggr = aggr(couples_hommes, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(couples_hommes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
### Variables qualitatives 
couples.cat <- subset(couples.bis, select = -c(ddn,dconsultation,dconception,age_h,age_f,bmi_h,diff_age,duree_infertilite))
cat = mice(couples.cat, m=20, printFlag=FALSE, maxit = 40,method = 'polyreg', seed=500)
### Variables quantitatives
couples.nm <- subset(couples.bis, select = -c(enfant ,ddn,dconsultation,dconception,bh_f,ct_f,patho_f_bin, patho_f_regroup,diplome_f,diplome_h))
num = mice(couples.nm, m=5, printFlag=FALSE, maxit = 40,method = 'pmm', seed=2525)

# Missing Values
couples.bis$age_f[is.na(couples.bis$age_f)] <- couples.bis$age_h - mean(couples.bis$diff_age,na.rm = T)
couples.bis$diff_age[is.na(couples.bis$diff_age)] <- mean(couples.bis$diff_age,na.rm = T)
couples.bis$bmi_h[is.na(couples.bis$bmi_h)] <- mean(couples.bis$bmi_h,na.rm = T)
