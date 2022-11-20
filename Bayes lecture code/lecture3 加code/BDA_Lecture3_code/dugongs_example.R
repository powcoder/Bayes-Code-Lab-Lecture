#Data
x=c( 1.0,  1.5,  1.5,  1.5, 2.5,   4.0,  5.0,  5.0,   7.0,
       8.0,  8.5,  9.0,  9.5, 9.5,  10.0, 12.0, 12.0,  13.0,
       13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)

y=c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
      2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
      2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57)

n=length(y)

#Model in JAGS
model_string <- "model{
# Likelihood
for(i in 1:n){
y[i]~dnorm(mu[i],tau)
mu[i]=alpha-beta*pow(gamma,x[i])
}

alpha~dunif(0,10)

beta~dunif(0,10)

gamma~dunif(0.5,1)

# Prior for the inverse variance
tau~dgamma(a, b)

# Compute the variance
sigma2=1/tau
}"


a=0.1; b=0.1
data=list(y=y,x=x,n=n,a=a,b=b)
model=jags.model(textConnection(model_string),n.chains=1,data=data)
update(model,1000,progress.bar="none")
resnl=coda.samples(model,variable.names=c("alpha","beta","gamma","sigma2"),n.iter=50000,thin=40,progress.bar="none")
#summary(resnl)
#autocorr.plot(resnl)
#effectiveSize(resnl[[1]])

alpha=resnl[[1]][,"alpha"]
beta=resnl[[1]][,"beta"]
gamma=resnl[[1]][,"gamma"]
niters=length(alpha)

#fitted values
fitted=matrix(0,nrow=n,ncol=niters)
for(l in 1:niters){
fitted[,l]=alpha[l]-beta[l]*(gamma[l]^x)  
}

ql=function(x){quantile(x,0.025)}; qh=function(x){quantile(x,0.975)}
fittedm=apply(fitted,1,mean); fittedl=apply(fitted,1,ql); fittedh=apply(fitted,1,qh)

plot(x,y,xlim=c(0,40),ylim=c(1.5,2.8),xlab="Age(years)",ylab="Length(m)",lwd=2)
lines(x,fittedm,type="l",col="blue2",lwd=2)
lines(x,fittedl,type="l",col="blue2",lty=2,lwd=2)
lines(x,fittedh,type="l",col="blue2",lty=2,lwd=2)
