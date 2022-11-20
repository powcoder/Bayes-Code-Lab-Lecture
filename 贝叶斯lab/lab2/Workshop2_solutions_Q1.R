load("dmft.Rdata")
y=dmft; n=length(y)

hist(y,freq=F,xlab="dmft-index",col="gray80",ylab="Density",main="Caries study: histogram of dmft index")

#exact
a=3; b=1
apost=a+sum(y); bpost=b+n
meanpost=apost/bpost
sdpost=sqrt(apost)/bpost
ci=qgamma(c(0.025,0.975),shape=apost,rate=bpost)
cat("Mean=",round(meanpost,3),"SD=",round(sdpost,3),
    "LB=",round(ci[1],3),"UB=",round(ci[2],3),"\n")

#rejection sampling
#unnormalised posterior
unposterior=function(theta,data){
  likelihood=prod(dpois(data,theta))    
  prior=dgamma(theta,3,1)  
  unpost=likelihood*prior
  return(unpost)
}

#proposal distribution
g=function(theta,m){
  g=dexp(theta,rate=1/m)  
}

#auxiliar function to determine the optimal value of M; need to find maximum of unnormalised posterior/g 
aux=function(theta,data){
  likelihood=prod(dpois(data,theta))    
  prior=dgamma(theta,3,1)  
  unpost=likelihood*prior
  g=dexp(theta,rate=1/mean(data))      
  aux=unpost/g
  return(aux)
}

M=optimize(aux,interval=c(0,20),data=y,maximum=TRUE)$objective
cat("Optimal M=",signif(M,4),"\n")

# Look at M times unnormalized posterior-
m=1000
thetagrid=seq(0,20,len=m)
unnormal.post.ord=numeric(m)
for(i in 1:m){
  unnormal.post.ord[i]=unposterior(theta=thetagrid[i],data=y) 
}

plot(thetagrid,M*g(thetagrid,m=mean(y)),type="l",col="red",xlab=expression(theta),ylab="Density")
lines(thetagrid,unnormal.post.ord)


#rejection sampling algorithm
n.samples=10000
count=0; attempts=0; thetapost=rep(0,n.samples)
t1=proc.time()["elapsed"]
while(count<n.samples){
  attempts=attempts+1  
  theta.c=rexp(1,1/mean(y))
  u=runif(1,0,1)
  alpha=unposterior(theta=theta.c,data=y)/(M*g(theta.c,mean(y)))
  if(u<=alpha){
    count=count+1
    thetapost[count]=theta.c
  }
}
t2=proc.time()["elapsed"]
cat("Speed=",t2-t1,"\n")
cat("Acceptance rate=",round(n.samples/attempts,3),"\n")

hist(thetapost,freq=F,breaks=20,xlab=expression(theta),ylab="Density",main="")

cat("E=",round(mean(thetapost),3), "SD=",round(sd(thetapost),3),
    "LB and UB=",round(unlist(quantile(thetapost,c(0.025,0.975))),3),"\n")

#SIR
n.samples=10000
t1=proc.time()["elapsed"]
theta.g=rexp(n.samples,1/mean(y))
w=numeric(n.samples)
for(s in 1:n.samples){
  w[s]=unposterior(theta=theta.g[s],data=y)/dexp(theta.g[s],1/mean(y)) 
}
q=w/sum(w)
theta.f=sample(theta.g,n.samples,replace=TRUE,prob=q)
t2=proc.time()["elapsed"]
cat("Speed=",t2-t1,"\n") 

hist(theta.f,freq=F,breaks=20,xlab=expression(theta),ylab="Density",main="")
cat("E=",round(mean(theta.f),3), "SD=",round(sd(theta.f),3),
    "LB and UB=",round(unlist(quantile(theta.f,c(0.025,0.975))),3),"\n")

#JAGS code
require(rjags)

model_string <- "model{
# Likelihood 
for(i in 1:n){
y[i]~dpois(theta)
}

# Prior
theta~dgamma(a,b)
}"

# No initial values 
model=jags.model(textConnection(model_string),n.chains=1, data=list(y=y,n=n,a=3,b=1))
update(model,1000,progress.bar="none")
res=coda.samples(model, variable.names=c("theta"),n.iter=10000,progress.bar="none")
summary(res); plot(res); autocorr.plot(res)
