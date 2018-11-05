
#' A function to estimate a nonparametric cumulative incidence function
#'
#' @param data input data source containing main study data
#' @param t name of the time variable
#' @param type name of variable denoting outcome type
#' @param level level of outcome for which to estimate cumulative incidence
#' @param tau final follow-up time
#' @return data frame with event times and cumulative incidence function for outcome of interest
#' @export
#' @examples
#' cif_naive(mydata, t = "x", type = "c_obs", level = 1, tau = 2)

cif_naive<-function(data = data, t = "x", type = "c_obs", level = 1, tau = 1){
  fdata <- data
  fdata$x <- data[, t]
  fdata$c_obs <- data[, type]
  times <- fdata$x[fdata$x<=tau]
  ci <- cuminc(fdata$x,fdata$c_obs,cencode=0)
  cif <- stepfun(ci[[level]]$time,c(0,ci[[level]]$est), right=TRUE)
  ci1 <- cif(times)
  ci1p<-as.data.frame(cbind(times, ci1))
  ci1p
}