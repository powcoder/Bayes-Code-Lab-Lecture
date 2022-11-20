#First fit the standard frequentist linear statistical model
data(mtcars)
help(mtcars)
#creating a new dataframe with only mpg,drat,wt, and qsec as variables
vars=names(mtcars)%in%c("cyl", "disp", "hp", "vs","am","gear","carb") #variables to exclude
mtcars1=mtcars[!vars]

fit=lm(mpg~drat+wt+qsec,data=mtcars1)
summary(fit)

#Now fit the Bayesian version of the model using JAGS
require(rjags)

#covariates and response
y=mtcars1$mpg; drat=mtcars1$drat; wt=mtcars1$wt; qsec=mtcars1$qsec
n=nrow(mtcars1)

#model in BUGS language
model_string <- "model{

# Likelihood
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]=beta[1]+beta[2]*drat[i]+beta[3]*wt[i]+beta[4]*qsec[i]
}

# Prior for beta
for(j in 1:4){
beta[j]~dnorm(mu0,tau0)
}
tau0=1/sigma02

# Prior for the inverse variance
tau~dgamma(a, b)

# Compute the variance
sigma2=1/tau
}"

#hyperparameters for the betas and tau
mu0=0; sigma02=1000; a=0.1; b=0.1
# list with data and hyperparameters
data=list(y=y,drat=drat,wt=wt,qsec=qsec,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b)

#passing the model to rjags
model=jags.model(textConnection(model_string),n.chains=1,data=data)

#burn-in
update(model,100000,progress.bar="none")
resthin=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=500000,thin=50,progress.bar="none")

autocorr.plot(resthin[[1]][,"beta[1]"])
autocorr.plot(resthin[[1]][,"beta[2]"])
autocorr.plot(resthin[[1]][,"beta[3]"])
autocorr.plot(resthin[[1]][,"beta[4]"])
autocorr.plot(resthin[[1]][,"sigma2"])

acf(resthin[[1]][,"beta[1]"],lag.max=500)
acf(resthin[[1]][,"beta[2]"],lag.max=500)
acf(resthin[[1]][,"beta[3]"],lag.max=500)
acf(resthin[[1]][,"beta[4]"],lag.max=500)
acf(resthin[[1]][,"sigma2"],lag.max=500)

effectiveSize(resthin[[1]][,"beta[1]"])
effectiveSize(resthin[[1]][,"beta[2]"])
effectiveSize(resthin[[1]][,"beta[3]"])
effectiveSize(resthin[[1]][,"beta[4]"])
effectiveSize(resthin[[1]][,"sigma2"])

plot(density(resthin[[1]][,"beta[1]"]))
plot(density(resthin[[1]][,"beta[2]"]))
plot(density(resthin[[1]][,"beta[3]"]))
plot(density(resthin[[1]][,"beta[4]"]))
plot(density(resthin[[1]][,"sigma2"]))

### next section is about checks based on studentised residuals
resmat=as.matrix(resthin)
niterf=nrow(resmat)
beta0=resmat[,1]; beta1=resmat[,2] ;beta2=resmat[,3]; beta3=resmat[,4]; sigma=sqrt(resmat[,5])
x=cbind(rep(1,n),mtcars1$drat,mtcars1$wt,mtcars1$qsec)
H=x%*%solve((t(x)%*%x))%*%t(x)

#fitted values
fittedvalues=matrix(0,nrow=n,ncol=niterf)
for(l in 1:niterf){
fittedvalues[,l]=beta0[l]*x[,1]+beta1[l]*x[,2]+beta2[l]*x[,3]+beta3[l]*x[,4]
}

#studentised residuals
studentisedred=matrix(0,nrow=n,ncol=niterf)
for(l in 1:niterf){
for(i in 1:n){
studentisedred[i,l]=(y[i]-fittedvalues[i,l])/(sigma[l]*sqrt((1-diag(H)[i]))) 
}  
}  

#posterior mean of studentised residuals
studentisedredm=numeric(n)
for(i in 1:n){
studentisedredm[i]=mean(studentisedred[i,])  
}

#QQ-plot
qqnorm(studentisedredm,xlim=c(-3,3),ylim=c(-3,3),lwd=2)
qqline(studentisedredm,col=2,lwd=2)

#checking independence of error terms
plot(seq_along(studentisedredm),studentisedredm,xlab="Index",ylab="Bayesian studentised residual",ylim=c(-3,3))

#posterior mean fitted values
fittedvaluesm=numeric(n)
for(i in 1:n){
fittedvaluesm[i]=mean(fittedvalues[i,])  
}

plot(fittedvaluesm,studentisedredm,xlab="Fitted value (posterior mean)",ylab="Bayesian Studentised residual (posterior mean)")


#predictive checks
#replicated data
yrep=matrix(0,nrow=n,ncol=niterf)
for(l in 1:niterf){
for(i in 1:n){
yrep[i,l]=rnorm(1,beta0[l]*x[i,1]+beta1[l]*x[i,2]+beta2[l]*x[i,3]+beta3[l]*x[i,4],sigma[l])  
}  
}

#statistics of interest in this case (and different from the ones included in the model)
yrepmin=apply(yrep,2,min)
yrepmax=apply(yrep,2,max)
yrepmedian=apply(yrep,2,median)
require(fBasics)
yrepskewness=apply(yrep,2,skewness)
yrepkurtosis=apply(yrep,2,kurtosis)

hist(yrepmin,col="gray40")
abline(v=min(y),col="red",lwd=2)

hist(yrepmax,col="gray40")
abline(v=max(y),col="red",lwd=2)

hist(yrepmedian,col="gray40")
abline(v=median(y),col="red",lwd=2)

hist(yrepskewness,col="gray40")
abline(v=skewness(y),col="red",lwd=2)

hist(yrepkurtosis,col="gray40")
abline(v=kurtosis(y),col="red",lwd=2)

#plot some realisations of the replicated data against the density of the observed data
plot(density(yrep[,1]),col="lightskyblue1",ylim=c(0,0.1))
for(i in 2:1000){
lines(density(yrep[,i]),col="lightskyblue1")  
}
lines(density(y),col="black",lwd=4)
