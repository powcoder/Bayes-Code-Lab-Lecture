data(mtcars)
vars=names(mtcars)%in%c("cyl", "disp", "hp", "vs","am","gear","carb") #variables to exclude
mtcars1=mtcars[!vars]
require(rjags)
n=nrow(mtcars1)
#covariates and response
y=mtcars1$mpg; drat=mtcars1$drat; wt=mtcars1$wt; qsec=mtcars1$qsec
x=cbind(rep(1,n),drat,wt,qsec)

model_string <- "model{

# Likelihood
for(i in 1:n){
y[i]~dnorm(mu[i],inv.var)
mu[i]=inprod(beta[],x[i,])
}

beta~dmnorm(mu.beta,tau.beta)

# Prior for the inverse variance
inv.var~dgamma(a, b)

# Compute the variance
sigma2=1/inv.var
}"

#this coincides with the previous prior, as off diagonal entries are zero
mu.beta=rep(0,4); tau.beta=diag(0.0001,4); a=0.001; b=0.001
data=list(y=y,x=x,n=n,mu.beta=mu.beta,tau.beta=tau.beta,a=a,b=b)
model=jags.model(textConnection(model_string),n.chains=1,data=data)
update(model,100000,progress.bar="none")
res=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=500000,thin=50,progress.bar="none")
summary(res)


