library(ggplot2)
load("/Users/nabil/Desktop/BESante/couples.RData")
couples.bis <- couples[,-1]

qualit <- c("enfant","patho_h","cryptorchidie","spermo","bh_f","ct_f","patho_f","fecondite","traitement")
quant <- c("age_h","age_f","bmi_h","duree_infertilite")

### Etude de deux variables qualitatives 
# Tableau de contingence 
# Enfant & Traitement
effecTr <- table(couples$enfant,couples$traitement)
prop.table(effec)
# Enfant & fecondite
effecFec <- table(couples$enfant,couples$fecondite)
prop.table(effecFec)
# Test Khi-2
chisq.test(couples$enfant,couples$traitement)
chisq.test(couples$enfant,couples$fecondite)

### Etude d'une variable qualitative avec une variable quantitative
# Age Homme et Enfant
boxplot(couples$age_h ~ couples$enfant, col = "purple", border = "black",
        main = "Enfant en fonction de l'age",
        ylab = "âges[années]")
# Age femme et Enfant
boxplot(couples$age_f ~ couples$enfant, col = "purple", border = "black",
        main = "Enfant en fonction de l'age",
        ylab = "âges[années]")
# bmi_h et Enfant
boxplot(couples$bmi_h ~ couples$enfant, col = "purple", border = "black",
        main = "Enfant en fonction de IMC ",
        ylab = "IMC")

# duree infertitude et Enfant
boxplot(couples$duree_infertilite ~ couples$enfant, col = "purple", border = "black",
        main = "Enfant de la duree d'infertitude ",
        ylab = "duree d'nfertitude")
