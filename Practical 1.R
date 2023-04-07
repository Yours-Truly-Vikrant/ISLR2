#Practical_1 
rm(list=ls(all=T))
#Q.1 Consider College data available in ISLR2 library in R.
library("ISLR2")
data("College")
fix(College)
dim(College)
college.names=rownames(College)
college=College[,-1];college
#Obtain numerical summary of the variables in the dataset.
summary(college)
which.max(college$Apps)
#obtain scatterplot matrix of the first ten columns or variables of the data
pairs(college[,1:10])
#Use plot function to produce side by side boxplot of outstate and private
boxplot(Outstate~Private,data=College)
#Create new qualitative variable, called Elite, by binning the Top!0 percent variable. Divide the universities into two groups based on whether or not the proportion of students coming from the top 10% of their high school classes exceeds 50%
Elite=rep("No",nrow(college));Elite
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite);
college$Elite
Elite=ifelse(college$Top10perc>50,"Yes","No");Elite
View(college)
summary(college$Elite)
boxplot(Outstate~Elite,data=college)

#Explore the relationship between Grad.rate and S.F.Ratio
lines(hist(college$Apps, xlab = "Applications Received", main = "")) 
#hist(college$Apps)
#hist(college$Apps,xlim=c(0,25000))
#hist(college$Apps,xlim=c(0,25000),breaks=50,freq=F)
plot(density(college$Apps))
hist(college$perc.alumni, col=2, xlab = "% of alumni who donate", main = "")
hist(college$S.F.Ratio, col=3, breaks=50, xlab = "Student/faculty ratio", main = "")
hist(college$Expend, breaks=100, xlab = "Instructional expenditure per student", main = "")



#
par(mfrow=c(1,1))
plot(college$S.F.Ratio,college$Grad.Rate,xlab="student to faculty ratio",ylab="graduation rate",main="plot of grad.rate vs sf ratio")
abline(lm(college$Grad.Rate~college$S.F.Ratio),col=3)


#Q.2 Consider Auto data available in ISLR2 library in R. Make sure that the missing values have been removed from the data.
data("Auto")
fix("Auto")
is.na(Auto)
Auto_=na.omit(Auto)
dim(Auto)
dim(Auto_)
#There are not any missing values in data set as dimension of original and modified data set is same.
x=c(0,1,2,3,4)
rm(x)
sapply(Auto[,1:7], range)
sapply(Auto[,1:7],mean)
sapply(Auto[,1:7],sd)
Auto.reduced=Auto[-c(10:84)]

sapply(Auto.reduced[,1:7], range)
sapply(Auto.reduced[,1:7],mean)
sapply(Auto.reduced[,1:7],sd)
pairs(Auto[,1:7])
cor(Auto[,1:7])


#Q.3
library("MASS")
View(Boston)
dim(Boston)
pairs(Boston) #matrix_scatterplot
?Boston
pairs(Boston[c(1,3,5,6,7,8,10:14),])
pairs(~crim+nox+dis+tax+medv,data=Boston)
sapply(Boston[,1:14],range)
cor(Boston[-1],Boston$crim)

High_tax=Boston[which(Boston$tax > mean(Boston$tax) + 2*sd(Boston$tax)),]
High_tax
range(Boston$tax)

a=ifelse(Boston$chas==1,1,0);a
sum(Boston$chas)
median(Boston$ptratio)

#
eps=matrix(rnorm(100*50),100,50)
dim(eps)
f_=function(x){
  3+2*x
}
x=matrix(rnorm(100*50,2,4),100,50)
y=matrix(f_(x)+eps,100,50)
dim(y)
#Irreducible Error
var_eps=apply(eps,1,var)
mean(var_eps)
#Reducible Error
lm.fit=lm(y~f_(x))
names(lm.fit)
predict.fit=lm.fit$fitted.values
Red.comp=(f_(x)-predic.fit)^2
predict.fit=matrix(lm.fit$fitted.values,100,50)


#or
y_hat=y-eps
Red.comp=(f_(x)-y_hat)^2











