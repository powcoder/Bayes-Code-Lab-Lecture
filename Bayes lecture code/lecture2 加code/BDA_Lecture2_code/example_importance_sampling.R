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

n.samples=10000; theta.p=weight.p=numeric(n.samples)
for(i in 1:n.samples){
#draw a value from g the proposal distribution
theta.p[i]=rnorm(1,mean(y),sd(y))  
weight.p[i]=unposterior(theta=theta.p[i],data=y)/dnorm(theta.p[i],mean(y),sd(y))
}

#posterior mean via importance sampling
pmis=sum(theta.p*weight.p)/sum(weight.p)

#posterior mean from the true posterior
thetam=((0/100)+(n*mean(y)/1))/((1/100)+n/1)



