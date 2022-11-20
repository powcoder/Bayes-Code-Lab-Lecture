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

#the range of the data is -0.4 and 4.2, so the mean needs to be within this range, I will let it to be between -1 and 5
m=1000
thetagrid=seq(-1,5,len=m)

#evaluating unnormalised posterior at grid points
unnormal.post.ord=numeric(m)
for(i in 1:m){
unnormal.post.ord[i]=unposterior(theta=thetagrid[i],data=y) 
}
  
#sample grid values 
theta.f=sample(thetagrid,size=m,replace=TRUE,prob=unnormal.post.ord)

#draw histogram of the data versus true posterior density
hist(theta.f,freq=F,breaks=20,xlab=expression(theta),ylab="Density",main="")
lines(thetagrid,posterior(theta=thetagrid,data=y),type="l")
