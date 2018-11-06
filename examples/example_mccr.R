
library(mccr)
library(cmprsk)
set.seed(3454)

# Simulate hypothetical study data --------------------

simulate<-function(h1=2,h2=4,s1=1.25,s2=1,pm1x1=0.1,pm2x1=0.1,pm1x2=0.3,pm2x2=0.3,n=1000,p0=0.3) {
  diffx<-rbinom(n=n, size=1, prob=0.5)
  p<-(diffx==1)*p0+(diffx==0)*(p0+0.3)
  c<-rbinom(n=n,size=1,prob=p)
  c<-ifelse(c==0,2,c)
  h<-(log(10)-log(10)*diffx+h1)*(c==1)+(log(5)-log(5)*diffx+h2)*(c==2)
  diffx<-ifelse(diffx==1, 1, 2)
  s<-s1*(c==1)+s2*(c==2)
  t<-rweibull(n=n, shape=s, scale=h)	
  u<-runif(n=n,min=0,max=5)
  x<-ifelse(u<t,u,t)
  c<-ifelse(u<t,0,c)
  trueci1<- p*(1-exp(-x/h))
  ## Simulate misclassified (observed) cause of failure
  u<-runif(n)
  r<-1-((u<=pm1x1)*(c==1)*(diffx==1)+(u<=pm2x1)*(c==2)*(diffx==1)+(u<=pm1x2)*(c==1)*(diffx==2)+(u<=pm2x2)*(c==2)*(diffx==2))
  c_obs<-c*(r==1)+(3-c)*(r==0)
  ## Return data frame
  data<-as.data.frame(cbind(x,c_obs,trueci1,c, diffx))	
 # data <- data[with(data, order(x)),]
  data
}

main <- simulate()

# Simulate some validation data -----------------

n<-200
c <- rbinom(n, 1, 0.5)
diffx <- rbinom(n, 1, 0.3)
c_obs <- diffx*((c==1)*rbinom(n, 1, 0.9)+(c==0)*rbinom(n, 1, .1))+(1-diffx)*((c==1)*rbinom(n, 1, 0.7)+(c==0)*rbinom(n, 1, .3))
c <- ifelse(c==0, 2, 1)
c_obs <- ifelse(c_obs==0, 2, 1)
diffx <- ifelse(diffx==0, 2, 1)
val2 <- as.data.frame(cbind(c, c_obs, diffx))

#Call functions --------------------------------

ndiff <- cif_adj_nondiff(data = main, data_evd = val2, t = "x", obs="c_obs", 
                         true = "c", level = 1, tau = 1)
tail(ndiff, n=1)

diff <- cif_adj(data = main, data_evd = val2, t = "x", obs="c_obs", true = "c", 
                level = 1, tau = 1, diff_by = "diffx")
tail(diff, n=1)

naive <- cif_naive(data = main, t = "x", type="c_obs", level = 1, tau = 1)
tail(naive, n=1)
