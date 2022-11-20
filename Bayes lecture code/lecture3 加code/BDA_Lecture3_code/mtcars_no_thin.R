data(mtcars)
#help(mtcars)

#creating a new dataframe, mtcars1, with only mpg,drat,wt, and qsec as variables
vars=names(mtcars)%in%c("cyl", "disp", "hp", "vs","am","gear","carb") #variables to exclude
mtcars1=mtcars[!vars]

#frequentist OLS fit
fit=lm(mpg~drat+wt+qsec,data=mtcars1)
summary(fit)
confint(fit) # for 95% confidence intervals

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

model=jags.model(textConnection(model_string),n.chains=1,data=data)

#burn in
update(model,10000,progress.bar="none")

resnothin=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=20000,thin=1,progress.bar="none")

#autocorrelation plots
autocorr.plot(resnothin[[1]][,"beta[1]"])
autocorr.plot(resnothin[[1]][,"beta[2]"])
autocorr.plot(resnothin[[1]][,"beta[3]"])
autocorr.plot(resnothin[[1]][,"beta[4]"])
autocorr.plot(resnothin[[1]][,"sigma2"])

#alternative autocorrelation plots
acf(resnothin[[1]][,"beta[1]"],lag.max=500)
acf(resnothin[[1]][,"beta[2]"],lag.max=500)
acf(resnothin[[1]][,"beta[3]"],lag.max=500)
acf(resnothin[[1]][,"beta[4]"],lag.max=500)
acf(resnothin[[1]][,"sigma2"],lag.max=500)

#effective sample size
effectiveSize(resnothin[[1]][,"beta[1]"])
effectiveSize(resnothin[[1]][,"beta[2]"])
effectiveSize(resnothin[[1]][,"beta[3]"])
effectiveSize(resnothin[[1]][,"beta[4]"])
effectiveSize(resnothin[[1]][,"sigma2"])


#Gelman-Rubin convergence statistics (need to run at least two chains)
#gelman.plot(res)
#gelman.diag(res)





