pds=read.table("production.txt",header=T)
pds.mod=lm(RunTime~RunSize,data=pds)
yfit=fitted.values(pds.mod)
resid=residuals(pds.mod)
std.resid=rstandard(pds.mod)
st.resid=rstudent(pds.mod) #deleted studentised residuals
par(mfrow=c(2,2))
plot(pds.mod) 
---------------------------------------------Formal Statistical Tests--------------------------------------------------------------------

ncvTest(pds.mod) #load car pkge test for noncontant variance 
shapiro.test(std.resid) #Shapiro-Wilk normality test 
lillie.test(std.resid)  #Lilliefors (Kolmogorov-Smirnov) normality test 
cook.dist=cooks.distance(pds.mod)
H=hat(model.matrix(pds.mod))#hatmatrix
diag(H)#gives leverage values
lev=hatvalues(pds.mod)#leverage values 
