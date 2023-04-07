library("ISLR2")
data("Smarket")
fix("Smarket")
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach(Smarket)
train=
plot(Volume)
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family = binomial)
summary(glm.fits)
glm.fits$coefficients
glm.probs=predict(glm.fits,type="response")
glm.probs
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.pred]
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket,subset=train)
