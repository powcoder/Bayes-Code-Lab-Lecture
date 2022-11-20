#generate data
set.seed(123)
n=100; y=rnorm(n,2,1)

#true posterior distribution for theta
posterior=function(theta,data){
n=length(data)
thetam=((0/100)+(n*mean(data)/1))/((1/100)+n/1)
thetasd=sqrt(1/((1/100)+n/1))
post=dnorm(theta,mean=thetam,sd=thetasd)  
return(post)
}

#unnormalised posterior
unposterior=function(theta,data){
likelihood=prod(dnorm(data,theta,1))    
prior=dnorm(theta,0,10)  
unpost=likelihood*prior
return(unpost)
}

#proposal distribution
g=function(theta,m,s){
g=dnorm(theta,mean=m,sd=s)  
}

#auxiliar function to determine the optimal value of M; need to find maximum of unnormalised posterior/g 
aux=function(theta,data){
likelihood=prod(dnorm(data,theta,1))    
prior=dnorm(theta,0,10) 
post=likelihood*prior
g=dnorm(theta,mean=mean(data),sd=sd(data))    
aux=post/g
return(aux)
}

M=optimize(aux,interval=c(-1,5),data=y,maximum=TRUE)$objective

m=1000
thetagrid=seq(-1,5,len=m)
unnormal.post.ord=numeric(m)
for(i in 1:m){
unnormal.post.ord[i]=unposterior(theta=thetagrid[i],data=y) 
}

plot(thetagrid,unnormal.post.ord,type="l")
lines(thetagrid,M*g(thetagrid,m=mean(y),s=sd(y)),col="red")

#rejection sampling algorithm
n.samples=10000
count=0; attempts=0; thetapost=rep(0,n.samples)
while(count<n.samples){
attempts=attempts+1  
theta.c=rnorm(1,mean(y),sd(y))
u=runif(1,0,1)
alpha=unposterior(theta=theta.c,data=y)/(M*g(theta.c,mean(y),sd(y)))
if(u<=alpha){
count=count+1
thetapost[count]=theta.c
}
}

pdf("rs.pdf")
hist(thetapost,freq=F,breaks=20,xlab=expression(theta),ylab="Density",main="")
lines(thetagrid,posterior(theta=thetagrid,data=y))
dev.off()