#generate data
set.seed(123)
n=150; sigma2=1; y=rnorm(n,4,sqrt(sigma2))

#prior specification
mu0=0; sigma20=100

#number iterations, store results, and initial value
S=10000; mu=numeric(S); mu[1]=rnorm(1,mu0,sqrt(sigma20))

count=0 #(number of candidates accepted)

#tunning parameter
s2=0.1 #(good acceptance rate)
#s2=0.001 #(very high acceptance rate)
#s2=2 #(very low acceptance rate)

#in the following, to avoid numerical instabilities we work on the log scale

for(i in 2:S){
  #generate candidate
  mustar=rnorm(1,mean=mu[i-1],sd=sqrt(s2))  
  
  #numerator of acceptance probability
  p1=sum(dnorm(y,mean=mustar,sd=sqrt(sigma2),log=TRUE))+dnorm(mustar,mean=mu0,sd=sqrt(sigma20),log=TRUE)
  
  #denominator of acceptance probability
  p2=sum(dnorm(y,mean=mu[i-1],sd=sqrt(sigma2),log=TRUE))+dnorm(mu[i-1],mean=mu0,sd=sqrt(sigma20),log=TRUE)
  
  u=runif(1)
  if(log(u)<min(0,p1-p2)){
    mu[i]=mustar; count=count+1  
  }
  else{mu[i]=mu[i-1]
  }
}

nburn=500
100*(count/S); mean(mu[(nburn+1):S])
plot(1:S,mu,type="l"); acf(mu,lag.max=500)
