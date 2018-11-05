#' A function to account for nondifferential misclassification in cumulative incidence functions
#'
#' @param data input data source containing main study data
#' @param data_evd intput data source containing validation data with variables true outcome and misclassified outcome
#' @param t name of the time variable
#' @param obs name of variable denoting possibly misclassified outcome type in both main and validation datasets
#' @param true name of variable denoting true outcome type (in validation data only)
#' @param level level of outcome for which to estimate cumulative incidence
#' @param tau maximum follow-up time
#' @return data frame with event times and corrected cumulative incidence function for outcome of interest
#' @export
#' @examples
#' cif_adj_nondiff(data = mydata, data_evd = valdata, t = "x", obs = "c_obs", true = "c", level = 1, tau = 2)


cif_adj_nondiff<-function(data = data, data_evd = data_evd, t = "x", 
                          obs = "c_obs", true = "c", level = 1, tau = 1)
  {
  fdata <- data
  fdata_evd <- data_evd
  fdata$x <- data[, t]
  fdata$c_obs <- data[, obs]
  fdata_evd$c <- data_evd[, true]
  fdata_evd$c_obs <- data_evd[, "c_obs"]
  times <- fdata$x[fdata$x <= tau]
  ci<-cuminc(fdata$x, fdata$c_obs, cencode=0)
  cif<-stepfun(ci[[level]]$time,c(0,ci[[level]]$est),right=TRUE)
  ci1<-cif(times)
  cif<-stepfun(ci[[(level==1)*2+(level==2)*1]]$time,c(0,ci[[(level==1)*2+(level==2)*1]]$est),right=TRUE)
  ci2<-cif(times)
  a<-misclassificationnd(fdata_evd, c, c_obs)[1]
  b<-misclassificationnd(fdata_evd, c, c_obs)[2]
  ci1adj<-((1-b)/(1-a-b))*ci1 - (b/(1-a-b))*ci2
  ci1adj<-cummax(ci1adj) 
  ci1adj<-as.data.frame(cbind(times, ci1adj))
  ci1adj
}