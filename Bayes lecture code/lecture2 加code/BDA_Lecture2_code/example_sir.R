#generate data
set.seed(123)
n=100; y=rnorm(n,2,1)

#unnormalised posterior
unposterior=function(theta,data){
likelihood=prod(dnorm(data,theta,1))
prior=dnorm(theta,0,10)
unpost=likelihood*prior
return(unpost)
}

n.samples=10000
theta.g=rnorm(n.samples,mean(y),sd(y))
w=numeric(n.samples)
for(s in 1:n.samples){
w[s]=unposterior(theta=theta.g[s],data=y)/dnorm(theta.g[s],mean(y),sd(y)) 
}
q=w/sum(w)
theta.f=sample(theta.g,n.samples,replace=TRUE,prob=q)

#true posterior distribution for theta
posterior=function(theta,data){
n=length(data)
thetam=((0/100)+(n*mean(data)/1))/((1/100)+n/1)
thetasd=sqrt(1/((1/100)+n/1))
post=dnorm(theta,mean=thetam,sd=thetasd)  
return(post)
}

m=1000
thetagrid=seq(-1,5,len=m)

pdf("sir.pdf")
hist(theta.f,freq=F,breaks=20,xlab=expression(theta),ylab="Density",main="")
lines(thetagrid,posterior(thetagrid,y),type="l")
dev.off()
