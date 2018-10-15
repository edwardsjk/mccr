
#' A function to estimate a nonparametric cumulative incidence function
#'
#' @param data input data source containing main study data, specifically time variable x and outcome type c
#' @param c level of outcome for which to estimate cumulative incidence
#' @param tau final follow-up time
#' @return data frame with event times and cumulative incidence function for outcome of interest
#' @export
#' @examples
#' cif_naive(mydata, 1, 2)

cif_naive<-function(data,c,tau){
  times<-data$x[data$x<=tau]
  ci<-cuminc(data$x,data$c_obs,cencode=0)
  cif<-stepfun(ci[[c]]$time,c(0,ci[[c]]$est),right=TRUE)
  ci1<-cif(times)
  tail(ci1, n=1)
}