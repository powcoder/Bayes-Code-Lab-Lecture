require(rjags)
n=nrow(mtcars1)
#covariates and response
y=mtcars1$mpg; drat=mtcars1$drat; wt=mtcars1$wt; qsec=mtcars1$qsec
x=cbind(rep(1,n),drat,wt,qsec)

model_string <- "model{

# Likelihood
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]=inprod(beta[],x[i,])
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

mu0=0; sigma02=1000; a=0.1; b=0.1
data=list(y=y,x=x,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b)
model=jags.model(textConnection(model_string),n.chains=1,data=data)
update(model,100000,progress.bar="none")
res=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=500000,thin=50,progress.bar="none")
summary(res)


