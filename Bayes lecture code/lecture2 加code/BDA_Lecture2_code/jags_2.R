require(rjags)

#Data from drug example
n=20; y=15; a=9.2; b=13.8; m=40; ncrit=25

#writing the model, in essence similar syntax to WinBUGS/OpenBUGS
model_string <- "model{

# Likelihood
y~dbinom(theta,n)

# Prior
theta~dbeta(a, b)

#y.pred
y.pred~dbinom(theta,m)

P.crit<-step(y.pred-ncrit+0.5)
}"

#passing the model to jags *format*
model=jags.model(textConnection(model_string),data = list(y=y,n=n,a=a,b=b,m=m,ncrit=ncrit),n.chains=1)

# Burnin for 1000 samples
update(model,1000,progress.bar="none")

# Running the model
samp=coda.samples(model, variable.names=c("theta","y.pred","P.crit"), n.iter=20000,progress.bar="none")

summary(samp)

autocorr.plot(samp)


plot(samp)

#gelman.diag(samp)
#effectiveSize(samp)