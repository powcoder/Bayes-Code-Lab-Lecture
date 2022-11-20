require(pscl) #to simulate from the inverse gamma

#generate data
set.seed(123)
n=150; y=rnorm(n,2,3)

#number iterations
S=5000
mu=sigma2=numeric(S)

#prior specification
mu0=0; sigma02=100; a=b=0.1

#initial values
mu[1]=mean(y); sigma2[1]=var(y)
#mu[1]=rnorm(1,0,10); sigma2[1]=rigamma(1,a,b)

for(i in 2:S){
meanmu=((mu0/sigma02)+((n*mean(y))/(sigma2[i-1])))/((1/sigma02)+(n/sigma2[i-1]))
varmu=1/((1/sigma02)+(n/sigma2[i-1]))
#sampling mu
mu[i]=rnorm(1,meanmu,sqrt(varmu))

a1=a+(n/2)
b1=b+0.5*sum((y-mu[i])^2)
#sampling sigma2
sigma2[i]=rigamma(1,a1,b1)
}

#traceplots
plot(1:S,mu,type="l")
plot(1:S,sigma2,type="l")

#ACFs
acf(mu,lag.max=100)
acf(sigma2,lag.max=100)
