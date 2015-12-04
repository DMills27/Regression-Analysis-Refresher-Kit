sat=read.csv("state.csv",header=T,sep=",")
plot(sat)
pairs(sat)
summary(sat)

str=as.data.frame(sat)
str[,10]=str$Popn*1000/str$Area
colnames(str)[10]="Density"
str[1:5,1:10]

summary(str[2:10])
cor(str[2:10])

************************************************************************************************************
library(MASS)
mod1=lm(Life.Exp~Popn+Income+Illiteracy+Murder+HS.Grad+Frost+Area+Density,data=str)
par(mfrow=c(2,2))
plot(mod1)

b=boxcox(Life.Exp~Popn+Income+Illiteracy+Murder+HS.Grad+Frost+Area+Density,lambda=seq(-1,2,0.5),data=str)

b=boxcox(mod1,lambda=seq(-3,2,0.5),data=str)

************************************VARIABLE SELECTION********************************************

null=lm(Life.Exp~1,data=st)
full=lm(Life.Exp~.,data=st)
step(null,scope=list(lower=null,upper=full),direction="forward")#forward elimination

step(full,direction="backward",data=st)#backward elimination

step(null,scope=list(upper=full),direction="both")# stepwise regression


--------------

library(leaps)
mod2=regsubsets(Life.Exp~Popn+Income+Illiteracy+Murder+HS.Grad+Frost+Area+Density,data=str,nbest=2)

plot(mod2,scale="Cp")
plot(mod2,scale="adjr")

library(car)
subsets(mod2,statistic="cp",main="Cp plot,subset regression")
abline(1,1,lty=2,col="blue")

***************************************REGRESSION MODELLING*********************************************************************
mod3=lm(Life.Exp~Popn+Murder+HS.Grad+Frost,data=str)
par(mfrow=c(2,2))
plot(mod3)

library(car)
ncvTest(mod3)#constant variance
spreadLevelPlot(mod3)#constant variance
qqPlot(mod3,labels=row.names(str),id.method="identify",simulate=T)#normality with confidence band

vif(mod3)#variance infation factor
sqrt(vif(mod3))>2

outlierTest(mod3)

summary(powerTransform(str$Life.Exp))

boxTidwell(Life.Exp~Popn+Murder,data=str)#violation of linearity
