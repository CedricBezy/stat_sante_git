library(ggplot2)
load("/Users/nabil/Desktop/BESante/couples.RData")
couples.bis <- couples[,-1]

attributs <- c("enfant","patho_h","cryptorchidie","spermo","bh_f","ct_f","patho_f","fecondite","traitement")
for (i in attributs){
  print(summary(couples[i]))
} 

for (i in attributs){
  barplot(table(couples[i]), 
          horiz = TRUE,las=1, 
          col = "purple", 
          border = "white",
          main =i,xlab = "Effectifs")
}
  
for (i in attributs){
print(i)
  }