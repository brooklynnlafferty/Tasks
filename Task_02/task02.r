setwd('~/Desktop/Evolution/Tasks/Task_02')
Data1<- read.csv('http://jonsmitchell.com/data/beren.csv',stringsAsFactors=F)
Data2<- read.csv('http://jonsmitchell.com/data/cyrus.csv',stringsAsFactors=F)
write.csv(Data1,'rawdata.csv',quote=F)
Data1
head(Data1)
GlargleBrgle<- Data1
head(GlargleBrgle)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1[1,]
Data1[2,]
Data1[1:3,]
Data1[1:3,4]
Data1[1:5,1:3]
Feeds <-which(Data1[,9]=='bottle')
berenMilk<- Data1[Feeds,]
head(berenMilk)
Feeds<- which(Data1[,'event']=='bottle')
Feeds<- which (Data1$event == 'bottle')
head(Feeds)
Feeds
'bottle'
dayID <- apply(Data1,1,function(x)paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format= "%Y-%m-%d", origin="2019-04-18")
Data1$age <- dateID [which(Data1$event == 'birth')]
head(Data1)
beren2 <- Data1
beren3<- beren2[order(beren2$age),]
head(beren)
head(beren2)
head(beren3)
write.csv(beren3,'beren_new.csv',quote=F, row.names=FALSE)
Feeds<- which(beren3$value[Feeds])
Feeds<- which(beren3$event =="bottle")
avgMilk<- mean(beren3$value[Feeds])
variable.names(avgMilk)
avgFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
head(avgFeed)
varFeed<- tapply(beren3$value[Feeds],beren3$age[Feeds],var)
totalFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds],sum)
numFeeds<- tapply(beren3$value[Feeds], beren3$age[Feeds],length)
cor(beren3$value[Feeds],beren3$age[Feeds])
cor.test(beren3$value[Feeds],beren3$age[Feeds])
berenCor<- cor.test(beren3$value[Feeds],beren3$age[Feeds])
summary(berenCor)
berenANOVA<- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds],xlab="who gave the bottle", ylab="amount of milk consumed (oz)" )
?par
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b",pch=16, xlab="age in days",ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2,col='red')
pdf(''r02b-totalMilkByDay.pdf'',height=4,width=4)
pdf("r02b-totalMilkByDay.pdf",height=4,width=4)
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(as.numeric(names(totalFeed)),totalFeed, type="b",pch=16,xlab="age in days",ylab="ounces of milk")
abline(h=mean(totalFeed),lty=2,col='red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf")
Data2<- read.csv('http://jonsmitchell.com/data/cyrus.csv',stringsAsFactors=F)
write.csv(Data2, 'rawdata.csv',quote=F)
length(Data2)
nrow(Data2)
ncol(Data2)
colnames(Data2)
head(Data2)
Data2[1,]
Data2[2,]
Data2[1:3,]
Data2[1:3,4]
Data2[1:5,1:3]
Mass<- which(Data2[,9]=='trait_mass')
cyrusMilk<- Data2[Mass,]
head(cyrusMilk)
Mass<- which(Data2$event =='trait_mass')
dayID<- apply(Data2,1, function(x)paste(x[1:3], collapse='-'))
dateID<- sapply(dayID, as.date, format="%Y-%m-%d", origin="2019-04-18")
Data2$age<- dateID-dateID [which(Data2$event =='birth')]
head(Data2)
cyrus2<- Data2
cyrus3<- cyrus2[order(cyrus2$age),]
write.csv(cyrus3, 'cyrus_new.csv',quote=F, row.names=FALSE)
Mass<- which(cyrus3$event =="trait_mass")
avgMass<- mean(cyrus3$value[Mass])
avgMass<- tapply(cyrus3$value[Mass], cyrus3$age[Mass], mean)
varMass<- tapply(cyrus3$value[Mass], cyrus3$age[Mass], var)
totalMass<- tapply(cyrus3$value[Mass], cyrus3$age[Mass],sum)
numMass<- tapply(cyrus3$value[Mass], cyrus3$age[Mass], length)
cor(cyrus3$value[Mass], cyrus3$age[Mass])
cor.test(cyrus3$value[Mass],cyrus3$age[Mass])
head(cyrus3)
head(totalMass)
cor.test(cyrus3$value[Mass],cyrus3$age[Mass])

dev.off()
write.csv(Data1, 'rawdata', quote=F)
Data1[1,]
Data1[2,]
Data1[1:3,]
Data1[1:3, 4]
Data1[1:5, 1:3]
Mass<- which(Data1[,9] == "trait_mass")
berenMass<- Data1[Mass,]
head(berenMass)
Mass<- which(Data1[,'event'] == "trait_mass")
Mass <- which(Data1$event=='trait_mass')
dayID<- apply(Data1, 1, function(x) paste(x[1:3], collapse='-'))
dateID<-sapply(dayID, as.Date, format="%Y-%m-%d", origin = "2019-04-18")
Data1$age<- dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2<-Data1
beren3<-beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Mass<-which(beren3$event == "trait_mass")
avgMass<- mean(beren3$value[Mass])
avgMass<- tapply(beren3$value[Mass], beren3$age[Mass], mean)
varMass<- tapply(beren3$value[Mass], beren3$age[Mass], var)
totalMass<- tapply(beren3$value[Mass], beren3$age[Mass],sum)
numMass<- tapply(beren3$value[Mass], beren3$age[Mass], length)
cor(beren3$value[Mass], beren3$age[Mass])
cor.test(beren3$value[Mass], beren3$age[Mass])
boxplot( beren3$value[Mass]~beren3$age[Mass], xlab= "Age in days", ylab=" Mass in kg", col="blue")
par(las=1, mar= c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab= "Age in days", ylab=" Mass in kg", col="blue")


cyrus3$value[Mass]



Data2<- read.csv('http://jonsmitchell.com/data/cyrus.csv',stringsAsFactors=F)
write.csv(Data2, 'rawdata.csv',quote=F)
length(Data2)
nrow(Data2)
ncol(Data2)
colnames(Data2)
head(Data2)
Data2[1,]
Data2[2,]
Data2[1:3,]
Data2[1:3,4]
Data2[1:5,1:3]
cMass<- which(Data2[,9]=='trait_mass')
cyrusMass<- Data2[cMass,]
head(cyrusMass)
cyrusMass<- which(Data2$event =='trait_mass')
dayID<- apply(Data2,1, function(x)paste(x[1:3], collapse='-'))
dateID<- sapply(dayID, as.Date, format="%Y-%m-%d", origin="2022-04-12")
Data2$age<- dateID-dateID [which(Data2$event =='birth')]
head(Data2)
cyrus2<- Data2
cyrus3<- cyrus2[order(cyrus2$age),]
write.csv(cyrus3, 'cyrus_new.csv',quote=F, row.names=FALSE)
cyrusMass<- which(cyrus3$event =="trait_mass")
avgcyrusMass<- mean(cyrus3$value[cyrusMass])
avgcyrusMass<- tapply(cyrus3$value[cyrusMass], cyrus3$age[cyrusMass], mean)
varcyrusMass<- tapply(cyrus3$value[cyrusMass], cyrus3$age[cyrusMass], var)
totalcyrusMass<- tapply(cyrus3$value[cyrusMass], cyrus3$age[cyrusMass],sum)
numcyrusMass<- tapply(cyrus3$value[cyrusMass], cyrus3$age[cyrusMass], length)
cor(cyrus3$value[cyrusMass],cyrus3$age[cyrusMass])
cor.test(cyrus3$value[cyrusMass],cyrus3$age[cyrusMass])
head(cyrus3)
head(totalcyrusMass)
data.frame(Data2)

par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalMass)), totalMass, type="b", pch=16, xlab= "Age in days", ylab=" Mass in kg", col="blue",xlim=c(0,1400),ylim=c(0, 25))
title("Beren and Cyrus Age Vs. Mass")
points(cyrus3$age[cyrusMass],cyrus3$value[cyrusMass]/1000, pch=16, type="b",col="red",xlab="Age in days",ylab="Mass in kg")
legend(1000,6,legend=c("Beren","Cyrus"),col=c("blue","red"),lty=1:1,cex=0.8,title="Kids",text.font=4,bg="white")
pdf("002_massPlot.pdf")
