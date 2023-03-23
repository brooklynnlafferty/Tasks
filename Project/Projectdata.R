setwd('~/Desktop/Evolution/Tasks/Project')
Data<- read.csv('flylanding_experiments.csv')
head(Data)
#Biting flies are more attracted to brown coated animals
GZ=c(177,60,0)
PZ=c(187,59,0)
IM=c(0,0,1155)
Wood=c(0,0,195)
data.table=rbind(GZ,PZ,IM,Wood)
data.table
chisq.test(data.table)
#for a 95% CI with df=6 the theoretical value is 1.635; the calculated
#Chi-Squared analysis value is 1833.4 so we reject the null hypothesis. 
#This shows that the results were NOT due to random chance. 
slices<- c(19.8,6.5,62.8,10.9)
lbls<- c("Black","White","Brown","Wood")
pct<-(slices/sum(slices)*100)
lbls<- paste(lbls,pct)
pie(slices,labels=lbls,col=rainbow(length(lbls)),main="Percentages of Colors Landed on by Biting Flies")
