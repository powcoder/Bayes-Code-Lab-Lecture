theta=seq(0,1,len=500)
a=9.2; b=13.8; y=15; n=20
prior=dbeta(theta,a,b)
likelihood=dbinom(y,n,theta)
posterior=dbeta(theta,a+y,b+n-y)

f=function(x,size,prob){dbinom(x,size,prob)}
area=integrate(f,x=y,size=n,lower=0,upper=1)$value
const=1/area

pdf("priorlikepost.pdf",pointsize=15)
plot(theta,prior,col="darkgreen",ylab="Density",xlab=expression(theta),type="l",ylim=c(0,5.5),lwd=2)
lines(theta,const*likelihood,col="blue2",lwd=2)
lines(theta,posterior,col="red",lwd=2)
legend("topright",legend=c("prior","scaled likelihood","posterior"),lty=c(1,1,1),lwd=c(2,2,2),col=c("darkgreen","blue2","red"),bty="n")
dev.off()