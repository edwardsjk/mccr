#' A function to account for nondifferential misclassification in cumulative incidence functions
#'
#' @param data input data source containing main study data, specifically time variable x and possibly misclassified outcome c_obs
#' @param data_evd intput data source containing validation data with variables true outcome c and misclassified outcome c_obs
#' @param tau maximum follow-up time
#' @return data frame with event times and corrected cumulative incidence function for outcome of interest
#' @export
#' @examples
#' cif_adj_nondiff(mydata, valdata, 2)


cif_adj_nondiff<-function(data,data_evd,tau){
  times<-data$x[data$x<=tau]
  ci<-cuminc(data$x,data$c_obs,cencode=0)
  cif<-stepfun(ci[[1]]$time,c(0,ci[[1]]$est),right=TRUE)
  ci1<-cif(times)
  cif<-stepfun(ci[[2]]$time,c(0,ci[[2]]$est),right=TRUE)
  ci2<-cif(times)
  a<-misclassificationnd(data_evd, c, c_obs)[1]
  b<-misclassificationnd(data_evd, c, c_obs)[2]
  ci1adj<-((1-b)/(1-a-b))*ci1-(b/(1-a-b))*ci2
  ci1adj<-cummax(ci1adj) 
  ci1adj<-as.data.frame(cbind(times, ci1adj))
  ci1adj
}