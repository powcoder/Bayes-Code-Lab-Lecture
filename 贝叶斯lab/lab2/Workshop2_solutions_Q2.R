require(rjags)
#Question 2 (i)
n=20; y=15; a=9.2; b=13.8

#model in BUGS syntax
model_string <- "model{
#likelihood
y~dbin(theta,n)

#prior
theta~dbeta(a, b)
}"

#compiling model
model=jags.model(textConnection(model_string),data = list(n=n,y=y,a=a,b=b),n.chains=1)

# Burnin for 1000 samples
update(model,1000,progress.bar="none")

# Running the model, monitoring the variable theta
res=coda.samples(model,variable.names=c("theta"), n.iter=20000,progress.bar="none")
summary(res); plot(res)

#Question 2 (ii)
mean(res[[1]]>0.6)

#Question 2 (iii)
n=20; y=15; a=9.2; b=13.8; m=40

#model in BUGS syntax
model_string <- "model{
#likelihood
y~dbin(theta,n)

#prior
theta~dbeta(a, b)

#predictive distribution
ypred~dbin(theta,m)
p25=step(ypred-25)     #equals 1 if ypred-25 >= 0, so an indicator
}"

#compiling model
model=jags.model(textConnection(model_string),data = list(n=n,y=y,a=a,b=b,m=m),n.chains=1)

# Burnin for 1000 samples
update(model,1000,progress.bar="none")

# Running the model, monitoring the variable theta, ypred, and p25
res=coda.samples(model,variable.names=c("theta","ypred","p25"), n.iter=20000,progress.bar="none")
summary(res) 

#Alternatively, we do not need the variable p25, we can simply do mean(res[[1]][,"ypred"]>=25)

#Question 2 (iv)
model_string <- "model{
theta ~ dbeta(a, b)
y~dbin(theta,n)
p15=step(y-15)
}"

model=jags.model(textConnection(model_string),data=list(n=n,a=a,b=b),n.chains=1)
update(model,1000,progress.bar="none")
res=coda.samples(model,variable.names=c("y","p15"),n.iter=20000,progress.bar="none")
summary(res)

#As above, we could also simply do mean(res[[1]][,"y"]>=15)

#Question 2 (v) and (vi)
n=20; y=15; a=c(9.2,12); b=c(13.8,3); p=c(0.95,0.05); m=40

model_string <- "model{
y~dbinom(theta,n) 
theta~dbeta(a[pick], b[pick])         # a[1]=9.2, b[1]=13.8;  a[2]=12;  b[2] = 3 
pick~dcat(p[1:2])                     # pick takes value 1 or 2 with prior prob p[1] or p[2]
ypred~dbin(theta,m)                   
p25=step(ypred-25)                 
}"

model=jags.model(textConnection(model_string),data = 
                   list(y=y,n=n,m=m,a=a,b=b,p=p),n.chains=1)

# Burnin for 1000 samples
update(model,1000,progress.bar="none")

# Running the model
res=coda.samples(model, variable.names=c("theta","ypred","p25"), 
                 n.iter=20000,progress.bar="none")
summary(res)
plot(res)
autocorr.plot(res)

# Question 2 (v)
#prob theta>0.6
mean(res[[1]][,"theta"] > 0.6) 

# Question 2 (vi): Pr(X>=25|n=40, data)
mean(res[[1]][,"p25"])
# or mean(res[[1]][,"ypred"]>=25)

#Question 2 (vii)
n <- 20; y <- 15
model_string <- "model{
y~dbinom(theta,n) 
theta~dbeta(a[pick], b[pick])       
pick~dcat(p[1:2])                    
p15=step(y-15)
}"

model=jags.model(textConnection(model_string),data = list(n=n,a=a,b=b,p=p),n.chains=1)
update(model,1000,progress.bar="none")
res=coda.samples(model,variable.names=c("y","p15"),n.iter=20000,progress.bar="none")
summary(res)

#or mean(res[[1]][,"y"]>=15)
