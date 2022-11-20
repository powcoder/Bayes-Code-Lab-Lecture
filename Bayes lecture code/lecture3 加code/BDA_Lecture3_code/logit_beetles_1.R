beetles=read.table(file="beetles.txt",header=TRUE)
require(rjags)

## Likelihood
model_string <- "model{
for (i in 1:N) {
  y[i] ~ dbinom(theta[i], n[i])
  logit(theta[i]) <- beta[1] + beta[2]*dose[i]
}
for (j in 1:2) {
  beta[j] ~ dnorm(0, 0.0001)
}

}"

y=beetles$y; n=beetles$n; N=nrow(beetles); dose=beetles$Dose
data=list(y=y,dose=dose,n=n,N=N)

model=jags.model(textConnection(model_string),n.chains=1,data=data)
update(model,5000,progress.bar="none")
res=coda.samples(model,variable.names=c("beta"),n.iter=1000000,thin=50,progress.bar="none")

effectiveSize(res)

autocorr.plot(res[[1]][,"beta[1]"])
autocorr.plot(res[[1]][,"beta[2]"])