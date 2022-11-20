hills=read.table("hills.txt",header=TRUE)
y=hills$time; climb=hills$climb; dist=hills$dist
n=nrow(hills)

#normal errors
model_string <- "model{
# Likelihood
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]=beta[1]+beta[2]*climb[i]+beta[3]*dist[i]
}

# Prior for beta
for(j in 1:3){
beta[j]~dnorm(mu0,tau0)
}
tau0=1/sigma02

# Prior for the inverse variance
tau~dgamma(a, b)

# Compute the variance
sigma2=1/tau
}"


mu0=0; sigma02=1000; a=0.1; b=0.1
data=list(y=y,climb=climb,dist=dist,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b)
model=jags.model(textConnection(model_string),n.chains=1,data=data)
update(model,10000,progress.bar="none")
resn=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=50000,thin=1,progress.bar="none")
summary(resn)
autocorr.plot(resn)
effectiveSize(resn[[1]][,"beta[1]"])

#t_5 errors
model_string <- "model{
# Likelihood
for(i in 1:n){
y[i]~dt(mu[i],tau,nu)
mu[i]=beta[1]+beta[2]*climb[i]+beta[3]*dist[i]
}

# Prior for beta
for(j in 1:3){
beta[j]~dnorm(mu0,tau0)
}
tau0=1/sigma02

# Prior for the inverse variance
tau~dgamma(a, b)

# Compute the variance
sigma2=1/tau
}"


mu0=0; sigma02=1000; a=0.1; b=0.1; nu=5
data=list(y=y,climb=climb,dist=dist,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b,nu=nu)
model=jags.model(textConnection(model_string),n.chains=1,data=data)
update(model,10000,progress.bar="none")
rest=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=50000,thin=1,progress.bar="none")
summary(rest)
autocorr.plot(rest)
effectiveSize(rest[[1]][,"beta[2]"])
plot(density(rest[[1]][,"beta[1]"]))

#t errors but prior on degrees of freedom
model_string <- "model{
# Likelihood
for(i in 1:n){
y[i]~dt(mu[i],tau,nu)
mu[i]=beta[1]+beta[2]*climb[i]+beta[3]*dist[i]
}

# Prior for beta
for(j in 1:3){
beta[j]~dnorm(mu0,tau0)
}
tau0=1/sigma02

# Prior for the inverse variance
tau~dgamma(a, b)

# Compute the variance
sigma2=1/tau

#Prior for nu
nu~dgamma(c,d)
}"


mu0=0; sigma02=1000; a=0.1; b=0.1; c=0.1; d=0.1
data=list(y=y,climb=climb,dist=dist,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b,c=c,d=d)
model=jags.model(textConnection(model_string),n.chains=1,data=data)
update(model,10000,progress.bar="none")
restnu=coda.samples(model,variable.names=c("beta","sigma2","nu"),n.iter=50000,thin=1,progress.bar="none")
summary(restnu)
autocorr.plot(restnu)
effectiveSize(restnu[[1]][,"beta[2]"])

