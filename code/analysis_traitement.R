##---------------------------------------
# projet stat pour la sante
# 22 / 01 / 2017
# Mouster riwan
##---------------------------------------

p<-ggplot(data=couples, aes(x=factor(traitement,levels=c('ICSI','IAC','Aucun','FIV','IAD','Medical')))) +
  geom_bar(stat="count", width=0.7, fill="steelblue")
p

table(couples$traitement, couples$enfant)
chisq.test(table(couples$traitement, couples$enfant))
