library(knitr)
library(olsrr)
library(lmtest)
library(EnvStats)
library(lattice)
library(car)
library(trafo)
data=read.table("RTdata.txt")
Tax=data$V2
Income=data$V3
Miles=data$V4
L_prop=data$V5
Petrol=data$V6
colnames(data)=c("Serial No.","Petrol tax (cents per gallon)","Average income (dollars)","Paved Highways (miles)","Proportion of population with driver's licenses","Consumption of petrol (millions of gallons)")
kable(head(data))
plot(data[-1],col="blue",main="Scatterplot Matrix")
f<-function(x)
{
  x=(x-mean(x))/sqrt(sum((x-mean(x))^2))
  return (x)
}
pfit=lm(Petrol~f(Tax)+I(f(Tax)^2)+f(Income)+f(Miles)+f(L_prop))
summary(pfit)
ols_step_all_possible(ffit)
ffit=lm(Petrol~f(Tax)+f(Income)+f(Miles)+f(L_prop))
summary(ffit)
a<-ols_step_all_possible(ffit)
write.csv(a,file="E:\\ISID M1\\RT files\\a.csv")
t<-read.table("E:\\ISID M1\\RT files\\b.csv",sep=",")
colnames(t)=c("Predictors","rsquare","adjr","predrsq","cp","aic")
kable(t)
M<-lm(Petrol~f(Tax)+f(Income)+f(L_prop))
summary(M)
xyplot(Petrol + M$fitted.values ~1:48, auto.key = list(space = "right"),grid = TRUE,  
       main = "Observed and Fitted Response Variable",xlab = "Observation Number", type = "b" )
plot(Petrol,cex=0.5,col="brown")
ols_plot_cooksd_bar(M)
ols_plot_dffits(M)
ols_plot_dfbetas(M)
ols_plot_resid_stud(M)
ans=ls.diag(M)
hi=ans$hat
plot(hi,ylab="Hi",xlab="Index",typ="p",main="Index plot of Hatmatrix Diagonals(Hi)")
which(hi>8/48)
which(covratio(M)<0.75&&covratio(M)>1.25)
f<-function(x)
{
  x=(x-mean(x))/sqrt(sum((x-mean(x))^2))
  return (x)
}

data=read.table("E:\\ISID M1\\RT files\\RTdata.txt")
tax=data$V2
income=data$V3
miles=data$V4
l_prop=data$V5
petrol=data$V6
M=lm(petrol~f(tax)+f(income)+f(l_prop))
data=data[-c(19,40,45),]
Tax=data$V2
Income=data$V3
Miles=data$V4
L_prop=data$V5
Petrol=data$V6
M1=lm(Petrol~f(Tax)+f(Income)+f(L_prop))
summary(M)$adj.r.squared
summary(M1)$adj.r.squared
summary(M)$sigma^2
summary(M1)$sigma^2
summary(M1)
ols_plot_comp_plus_resid(M1)
xyplot(Petrol + M1$fitted.values ~1:45, auto.key = list(space = "right"),grid = TRUE,  
       main = "Observed and Fitted Response Variable",xlab = "Observation Number", type = "b" )
set.seed(100)
Tax.n<-Tax+rnorm(length(Tax),0,0.2)
Model<-(lm(Petrol~f(Tax.n)+f(Income)+f(L_prop)))
summary(Model)
summary(Model)
ols_plot_comp_plus_resid(Model)
xyplot(Petrol + Model$fitted.values ~1:45, auto.key = list(space = "right"),grid = TRUE,   
       main = "Observed and Fitted Response Variable",xlab = "Observation Number", type = "b" )
qqPlot(Model)
ols_test_normality(Model)$shapiroo
ls_test_breusch_pagan(Model)$p
plot(Model$fitted.values,Model$residuals, xlab = "Fitted values", ylab = "Residuals",   
     main = "Residual vs Fitted Plot")
ans=ls.diag(Model)
e=ans$std.res
s=NULL
for(i in 1:26)
{
  s[i]=sum((e[i:i+19])^2)/20
}
plot(s,cex=0.5,ylim=c(0,0.5),typ="b", col="red",ylab="Moving average values of residuals",  
     main="Plot Showing the Moving Averages of the Residuals")
boxcox(Model)$lambdahat
Petrol.tb=((Petrol^ 0.6467)-1)/ 0.6467
M.tb=lm(Petrol.tb~f(Tax.n)+f(Income)+f(L_prop))
summary(M.tb)
logshiftopt(Model)$lambdahat
Petrol.tl=log(Petrol+369.9999)
M.tl=lm(Petrol.tl~f(Tax.n)+f(Income)+f(L_prop))
summary(M.tl)
sqrtshift(Model)$lambdahat
Petrol.ts=sqrt(Petrol-342.99)
M.ts=lm(Petrol.ts~f(Tax.n)+f(Income)+f(L_prop))
summary(M.ts)
M.tl<-lm(Petrol.tl~f(Tax)+f(Income)+f(L_prop))
summary(M.tl)
ols_plot_cooksd_bar(M.tl)
ols_plot_dffits(M.tl)
f<-function(x)
{
  x=(x-mean(x))/sqrt(sum((x-mean(x))^2))
  return (x)
}

summary(M.tl)$adj.r.squared
Tax.nl=Tax.n[-c(18,32)]
Income.l=Income[-c(18,32)]
L_prop.l=L_prop[-c(18,32)]
Petrol.tld=Petrol.tl[-c(18,32)]
Model.tl=lm(Petrol.tld~f(Tax.nl)+f(Income.l)+f(L_prop.l))
summary(M.tl)$adj.r.squared
summary(Model.tl)$adj.r.squared
summary(M.tl)$sigma^2
summary(Model.tl)$sigma^2
qqPlot(Model.tl)
ols_test_normality(Model.tl)$shapiro
ols_test_breusch_pagan(Model.tl)$p
plot(Model.tl$fitted.values,Model.tl$residuals, xlab = "Fitted values", ylab = "Residuals",  
     main = "Residual vs Fitted Plot")
ans.l=ls.diag(Model.tl)
el=ans$std.res
sl=NULL
for(i in 1:24)
{
  sl[i]=sum((el[i:i+19])^2)/20
}
plot(sl,ylim=c(0,0.5),cex=0.5,col="blue",ylab="Moving average values of residuals",  
     main="Plot Showing the Moving Averages of the Residuals")
ols_coll_diag(Model.tl)
dwtest(Model.tl)
acf(residuals(Model.tl))
pacf(residuals(Model.tl))
summary(Model.tl)
ols_plot_comp_plus_resid(Model.tl)
xyplot(Petrol.tld + Model.tl$fitted.values ~1:43, auto.key = list(space = "right"),grid = TRUE,  
       main = "Observed and Fitted Response Variable",xlab = "Observation Number", type = "b" )
