#algo grubbs methode cesure
while(grubbs.test(datafinal$concentration)[3]<0.05){
  g<-grubbs.test(datafinal$concentration)
  if(g$p.value<0.05){
    maximum<-as.numeric(strsplit(g$alternative," ")[[1]][3])
    print('valeur supprimé')
    print(maximum)
    for (i in 1:length(datafinal$concentration)){
      if(round(datafinal[i,'concentration'],digits = 4)==round(maximum,digits = 4) & !is.na(datafinal[i,'concentration'])){datafinal <- datafinal[-i,]
      
      }
    }
  } 
}