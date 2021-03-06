#' A function to account for differential misclassification in cumulative incidence functions
#'
#' @param data input data source containing main study data
#' @param data_evd intput data source containing validation data with variables true outcome and misclassified outcome as well as any variable by which misclassification may be differential. 
#' @param t name of the time variable in main study data
#' @param obs name of variable denoting possibly misclassified outcome type in both main and validation datasets
#' @param true name of variable denoting true outcome type (in validation data only)
#' @param level level of outcome for which to estimate cumulative incidence
#' @param tau maximum follow-up time
#' @param diff_by name of variable in main study and validation dataset by which misclassification is differential
#' @return data frame with event times and corrected cumulative incidence function for outcome of interest
#' @keyword misclassification

#' @export
#' @examples
#' cif_adj(data = mydata, data_evd = valdata, t = "x", obs = "c_obs", true = "c", level = 1, tau = 2, diff_by = "diffx")



cif_adj<-function(data = data, data_evd = data_evd,  t = "x", 
                  obs = "c_obs", true = "c", level = 1, tau = 1, diff_by = NA){
  set.seed(123)
  data_evd <- data_evd
  data$x <- data[, t]
  data$c_obs <- data[, obs]
  data_evd$c <- data_evd[, true]
  data_evd$c_obs <- data_evd[, obs]
  if(!is.na(diff_by)) data$diffx <- data[, diff_by]
  jitter<-rnorm(n=length(data$x), mean=0, sd=0.0001)
  data$xj<-data$x+jitter
  data$c1<-ifelse(data$c_obs==1,1,0)
  data$c2<-ifelse(data$c_obs==2,1,0)
  data$c3<-ifelse(data$c_obs==2|data$c_obs==1,1,0)
  if(!is.na(diff_by)){
    ax1<-misclassification(data_evd, c, c_obs, diffx)[1]
    ax2<-misclassification(data_evd, c, c_obs, diffx)[2]
    bx1<-misclassification(data_evd, c, c_obs, diffx)[3]
    bx2<-misclassification(data_evd, c, c_obs, diffx)[4]
  }
  if(is.na(diff_by)){
    ax1<-misclassificationnd(data_evd, c, c_obs)[1]
    ax2<-misclassificationnd(data_evd, c, c_obs)[1]
    bx1<-misclassificationnd(data_evd, c, c_obs)[2]
    bx2<-misclassificationnd(data_evd, c, c_obs)[2]
  }
  s<-summary(survfit(Surv(data$xj, data$c3)~1))$surv
  alltimes<-summary(survfit(Surv(data$xj, data$c3)~1))[[2]]
  times1<-summary(survfit(Surv(data$xj, data$c1)~data$diffx))[[2]]
  nrisk<-summary(survfit(Surv(data$xj, data$c3)~1))[[3]]
  nevent1<-summary(survfit(Surv(data$xj, data$c1)~data$diffx))[[4]]
  strata1<-summary(survfit(Surv(data$xj, data$c1)~data$diffx))[[8]]
  times2<-summary(survfit(Surv(data$xj, data$c2)~data$diffx))[[2]]
  nevent2<-summary(survfit(Surv(data$xj, data$c2)~data$diffx))[[4]]
  strata2<-summary(survfit(Surv(data$xj, data$c2)~data$diffx))[[8]]
  
  temp=as.data.frame(cbind(times1, nevent1, strata1))
  nriskt<-as.data.frame(cbind(alltimes, nrisk))
  names(nriskt)<-c("times1", "nrisk")
  nrisk1<-merge(nriskt, temp, by="times1")
  nrisk1$h1 = ((nrisk1$strata1==1)*((1-bx1)/(1-ax1-bx1))*nrisk1$nevent1+(nrisk1$strata1==2)*((1-bx2)/(1-ax2-bx2))*nrisk1$nevent1)/(nrisk1$nrisk)
  
  temp=as.data.frame(cbind(times2, nevent2, strata2))
  names(nriskt)<-c("times2", "nrisk")
  nrisk2<-merge(nriskt, temp, by='times2')
  nrisk2$deltat1=nrisk2$times2-lagpad(nrisk2$times2,1)
  nrisk2$h2 = ((nrisk2$strata2==1)*((bx1)/(1-ax1-bx1))*nrisk2$nevent2+(nrisk2$strata2==2)*((bx2)/(1-ax2-bx2))*nrisk2$nevent2)/(nrisk2$nrisk)
  os<-as.data.frame(cbind(alltimes, s))
  os$times<-os$alltimes
  ds1<-nrisk1[c("times1", "h1")]
  ds1$h2<-0
  names(ds1)<-c("times","h1", "h2")
  ds2<-nrisk2[c("times2", "h2")]
  ds2$h1<-0
  names(ds2)<-c("times","h2", "h1")
  keepers<-c("times", "h1", "h2")
  ds<- (rbind(ds1, ds2))
  ds <- ds[with(ds, order(times)),]
  dsx<- merge(ds, os, by="times")
  
  
  for (t in 1:length(dsx$times)){
    dsx$ch1[t]=sum(dsx$h1[1:t])
    dsx$ch2[t]=sum(dsx$h2[1:t])
    dsx$ch[t]=dsx$ch1[t]-dsx$ch2[t]
    dsx$ch[t]=max(0, cummax(dsx$ch[1:t]))
  }
  dsx$lastch<-lagpad(dsx$ch, 1)
  dsx$lasts<-lagpad(dsx$s, 1)
  dsx$lasts[1]<-1
  dsx$deltat=dsx$times-lagpad(dsx$times,1)
  dsx$dch=dsx$ch-dsx$lastch
  dsx$ft=dsx$lasts*dsx$dch
  for (t in 1:length(dsx$times)){
    dsx$ci1adj[t]=sum(dsx$ft[1:t])
  }
  dsx<-dsx[dsx$times<=tau,]

  ci1adj<-as.data.frame(cbind(dsx$times, dsx$ci1adj))
  ci1adj
  
}
