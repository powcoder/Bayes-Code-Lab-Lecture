#JAGS script for normally distributed data with both mean and variance unknown
require(rjags)

#generate data (the same we have used for the Gibbs sampler example)
set.seed(123)
n=150; y=rnorm(n,2,3)

#Prior information for the mean and variance. 
#Note that in the BUGS language the normal dist. is parameterised in terms of the precision=1/variance
mu0=0; sigma02=100; a=0.1; b=0.1

#writing the model, in essence similar syntax to WinBUGS/OpenBUGS
model_string <- "model{

  # Likelihood
  for(i in 1:n){
    y[i]~dnorm(mu,inv.var)
  }

  # Prior for mu
  mu~dnorm(mu0,inv.var0)
  inv.var0=1/sigma02

  # Prior for the inverse variance
  inv.var~dgamma(a, b)

  # Compute the variance
  sigma2=1/inv.var
}"

#data
data=list(y=y,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b)

#list of initial values (if not supplied, the function jags.model will generate initial values)
inits=list(mu=mean(y),inv.var=1/var(y))

#passing the model to jags *format*
model=jags.model(textConnection(model_string),n.chains=3,data=data,inits=inits)

#Burnin of 1000 samples
update(model,1000,progress.bar="none")

# Running the model
res=coda.samples(model,variable.names=c("mu","sigma2"),n.iter=10000, progress.bar="none")

#summary of the results (posterior mean, std deviation, quantiles,ect)
summary(res)

#trace and density plots for each of the parameters monitored (mu and sigma2 in this case)
plot(res)

#autocorrelation function
autocorr.plot(res)

#gelman rubin statistic (only works when number of chains >=2)
gelman.plot(res)

#extracting the chains for mu and sigma 2
str(res)
#mu for chain 1
res[[1]][,1]
#sigma2 for chain 1
res[[1]][,2]
