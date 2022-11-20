data(mtcars)
#creating a new dataframe with only mpg,drat,wt, and qsec as variables
vars=names(mtcars)%in%c("cyl", "disp", "hp", "vs","am","gear","carb") #variables to exclude
mtcars1=mtcars[!vars]

require(rjags)
n=nrow(mtcars1)
#response and centered covariates 
y=mtcars1$mpg; drat=mtcars1$drat-mean(mtcars1$drat); wt=mtcars1$wt-mean(mtcars1$wt); qsec=mtcars1$qsec-mean(mtcars1$qsec)

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

update(model,10000,progress.bar="none")
rescovcent=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=50000,thin=1,progress.bar="none")
summary(rescovcent)

acf(rescovcent[[1]][,"beta[1]"],lag.max=500)
acf(rescovcent[[1]][,"beta[2]"],lag.max=500)
acf(rescovcent[[1]][,"beta[3]"],lag.max=500)
acf(rescovcent[[1]][,"beta[4]"],lag.max=500)
acf(rescovcent[[1]][,"sigma2"],lag.max=500)

effectiveSize(rescovcent[[1]][,"beta[1]"])
effectiveSize(rescovcent[[1]][,"beta[2]"])
effectiveSize(rescovcent[[1]][,"beta[3]"])
effectiveSize(rescovcent[[1]][,"beta[4]"])
effectiveSize(rescovcent[[1]][,"sigma2"])
