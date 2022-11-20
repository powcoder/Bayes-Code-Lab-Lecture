#generate data
set.seed(123)
n=150; y=rnorm(n,4,1)

#known variance and prior
sigma2=1; mu0=0; sigma20=100
S=10000; mu=numeric(S); mu[1]=rnorm(1,mu0,sqrt(sigma20))
count=0
s2=0.4;
#s2=0.001
#s2=2

for(i in 2:S){
mustar=rnorm(1,mean=mu[i-1],sd=sqrt(s2))  
p1=sum(dnorm(y,mean=mustar,sd=sqrt(sigma2),log=TRUE))+dnorm(mustar,mean=mu0,sd=sqrt(sigma20),log=TRUE)
p2=sum(dnorm(y,mean=mu[i-1],sd=sqrt(sigma2),log=TRUE))+dnorm(mu[i-1],mean=mu0,sd=sqrt(sigma20),log=TRUE)
u=runif(1)
if(log(u)<min(0,p1-p2)){
mu[i]=mustar; count=count+1  
}
else{mu[i]=mu[i-1]
}
}

100*(count/S); mean(mu[501:S])
plot(1:S,mu,type="l"); acf(mu,lag.max=500)
