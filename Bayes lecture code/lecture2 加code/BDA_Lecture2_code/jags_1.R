require(rjags)

#Data from drug example
n=20; y=15; a=9.2; b=13.8

#writing the model, in essence similar syntax to WinBUGS/OpenBUGS
model_string <- "model{

# Likelihood
y~dbinom(theta,n)

# Prior
theta~dbeta(a, b)
}"

#passing the model to jags *format*
model=jags.model(textConnection(model_string),data = list(y=y,n=n,a=a,b=b))

# Burnin for 1000 samples
update(model,1000,progress.bar="none")

# Running the model
samp=coda.samples(model, variable.names=c("theta"), n.iter=20000, progress.bar="none")

summary(samp)

plot(samp)
