#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
library (swirl)
swirl()
Brooklynn
1
2
getwd()
ls()
x<-9
ls()
list.files()
?list.files
args()
args(list.files)
old.dir<-x
old.dir<-getwd()
dir.create
dir.create("testdir")
setwd("testdir")
file.create(mytest.R)
file.create()
file.create("mytest.R")
ls()
list.files()
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R")
file.rename("mytest2.R")
$"mytest.R" file.rename("mytest2.R")
file.rename("mytest.R")
file.rename("mytest.R"):"mytest2.R"
file.rename("mytest.R":"mytest2.R")
file.rename ("mytest.R")to "mytest2.R"
file.rename()
file.rename("mytest.R","mytest2.R")
file.copy("mytest2.R","mytest3.R")
file.path("mytest3.R")
file.path(folder1,folder2)
args(folder1)
file.path
file.path("folder1","folder2")
?dir.create
dir.create("testdir2")file.path("testdir3")
dir.create("testdir2")file.path("testdir3")
dir.create("testdir3")file.path("testdir2")
dir.create("testdir2",file.path("testdir3"))
dir.create(file.path('testdir2','testdir3'),recursive=TRUE)
setwd(old.dir)
2
1
3
1:20
pi:10
15:1
?:
?':'
?':'
seq(1,20)
seq(0,10,by=0.5)
my_seq<-seq(5,10,length=30)
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0,times=40)
rep(c(0,1,2),times=10)
rep(c(0,1,2),each=10)
1
1
5
x<-c(44,NA,5,NA)
x*3
y<-rnorm(1000)
z<- rep(NA,1000)
my_data<- sample(c(y,z),100)
my_na<- is.na(my_data)
my_na
my_data==NA
sum(my_na)
my_data
0/0
Inf-Inf
2
1
6
x
x[1:10]
1
x[is.na(x)]
y<-x[!is.na(x)]
y
1
1
2
y[y>0]
x[x>0]
x[!is.na(x)&x>0]
c(3,5,7)
[c(3,5,7)]
x[c(3,5,7)]
(i.e.x[0])
x[0]
x[3000]

x[c(-2,-10)]
x[-c(2,10)]
vect<-c(foo=11,bar=2,norf=NA)
vect
names(vect)
vect2<-c(11,2,NA)
names(vect2)<-c("foo","bar","norf")
identical(vect,vect2)
2
2
vect["bar"]
vect[c("foo","bar")]
1
1
7
my_vector<-c(1:20)
my_vector<- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector)<- c(4,5)
dim(my_vector)
attributes(my_vector)
my_vector
class(my_vector)
my_matrix<-my_vector
?matrix()
?matrix
my_matrix2<- data(1-20)nrow(4)ncol(5)
matrix(1:20)nrow(4)ncol(5)
matrix(data=1-20,nrow=4,ncol=5)
my_matrix2<- matrix(data=1:20,nrow=4,ncol=5)
identical(my_matrix,my_matrix2)
patients<- matrix(data="Bill","Gina","Kelly","Sean")
patients<- matrix(data="Bill,Gina,Kelly,Sean")
patients<- c("Bill,Gina,Kelly,Sean")
patients<-["Bill,Gina,Kelly,Sean"]
info()
patients<- c("Bill","Gina","Kelly","Sean")
cbind(patients,my_matrix)
my_data<- data.frame(patients,my_matrix)
my_data
class(my_data)
cnames<- c("patient","age","weight","bp","rating","test")
colnames(my_data)
colnames(my_data)<- cnames
my_data
1
1
8
True==True
==
True=True  
TRUE==TRUE
FALSE==TRUE
(FALSE==TRUE)==FALSE
6==7
6<7
10<=10
4
1
5!=7
!7=5
5=!7
!TRUE5=7
!TRUE(5=7)
!(5==7)
2
1
2
1
1
FALSE&FALSE
TRUE& c(TRUE,FALSE,FALSE)
TRUE && c(TRUE,FALSE,FALSE)
TRUE | c(TRUE,FALSE,FALSE)
TRUE|| c(TRUE,FALSE,FALSE)
5>8 || 6!=8 && 4>3.9
1
3
isTRUE(6>4)
1
2
2
3
identical('twins','twins')
4
xor(5==6, !FALSE)
4
ints<- sample(10)
ints
ints>5
which(ints>7)
3
any(ints<0)
all(ints>0)
3
4
3
2
1
9
Sys.Date()
mean()c(2,4,5)
c(2,4,5)mean()
mean(c(2,4,5))
library(swirl)
swirl()
Brooklynn
1
15
data(cars)
?cars
head(cars)
plot(cars)
?plot
plot(x=cars$speed,y=cars$dist)
plot(x=cars$dist,y=cars$speed)
plot(x=cars$speed,y=cars$dist,xlab="Speed")
plot(x=cars$speed,y=cars$dist,ylab="Stopping Distance")
plot(x=cars$speed,y=cars$dist,xlab="Speed",ylab="Stopping Distance")
plot(x=cars$speed,y=cars$dist,xlab="Speed",ylab="Stopping Distance")main("My Plot")
plot(main("My Plot"))
main("My Plot")
plot(main="My Plot")
skip()
plot(cars,main="My Plot",sub("My Plot Substitute"))
plot(cars,sub="My Plot Substitute")
plot(cars, sub="My Plot Substitle")
plot(cars, sub = "My Plot Subtitle")
plot(cars,col=2)
plot(cars, xlim=c(10,15))
plot(cars,pch=2)
mtcars
data(mtcars)
?boxplot
boxplot(mpg~cyl,data=mtcars)
hist(mtcars$mpg)
2
