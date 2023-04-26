setwd('~/Desktop/Evolution/Tasks/Task_10')
x<- rnorm(n=100, mean=0,sd=2)
y<- x*5+2+runif(100,min=0,max=0.1)
model<- lm(y~x)
summary(model)
slope<- vector("numeric",100)
intercept<- vector("numeric",100)
z<- vector("numeric",100)
for (i in 1:100){
x<- rnorm(n=100,mean=0,sd=2)  
z[i]<- runif(1,min=0.5,max=2)
y<- x*5*z[i]+2+runif(100, min=0,max=0.1)
model <- lm(y~x)
slope[i]<- coef(model)[2]*z[i]
intercept[i]<- coef(model)[1]*z[i]+2
}
plot(z, slope,xlab="z",ylab="Estimated Slope")
abline(lm(slope~z),col="blue")
