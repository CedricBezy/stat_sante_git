library(ggplot2)
load("/Users/nabil/Desktop/BESante/couples.RData")
couples.bis <- couples[,-1]
summary(couples.bis)

# Représentation graphique d’une variable quantitative
#Histogram age homme
ggplot(data=couples, aes(couples$age_h)) + 
  geom_histogram(breaks=seq(20, 50, by =2), 
                 col="red", 
                 aes(fill=..count..))
#Histogram age femme
ggplot(data=couples, aes(couples$age_f)) + 
  geom_histogram(breaks=seq(20, 50, by =2), 
                 col="red", 
                 aes(fill=..count..))
#Histogram bmi_h
ggplot(data=couples, aes(couples$bmi_h)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")
#Histogram duree infertile
ggplot(data=couples, aes(couples$duree_infertilite)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")
