
#' A function to estimate misclassification parameters separately for each level of a binary covariate.
#'
#' @param data input data source containing external validation data 
#' @param c variable name containing true outcome type
#' @param c_obs variable name containing misclassfied outcome type
#' @param diffx variable name containing covariate by which misclassification may be differential
#' @return values of a and b  used in subsequent steps
#' @keyword misclassification
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' misclassification(data, c, c_obs, diffx)

misclassification<-function(data, c, c_obs, cov){
  pm1x1<-with(data,sum(c_obs==2 & c==1 & diffx==1)/sum(c==1 & diffx==1))
  pm1x2<-with(data,sum(c_obs==2 & c==1 & diffx==2)/sum(c==1 & diffx==2))
  pm2x1<-with(data,sum(c_obs==1 & c==2 & diffx==1)/sum(c==2 & diffx==1))
  pm2x2<-with(data,sum(c_obs==1 & c==2 & diffx==2)/sum(c==2 & diffx==2))
  c(pm1x1, pm1x2,pm2x1, pm2x2)
}

#' A function to estimate misclassification parameters overall
#'
#' @param data input data source containing external validation data 
#' @param c variable name containing true outcome type
#' @param c_obs variable name containing misclassfied outcome type
#' @return values of a and b  used in subsequent steps
#' @export
#' @examples
#' misclassificationnd(data, y, y_obs)
misclassificationnd<-function(data, c, c_obs){
  pm1<-with(data,sum(c_obs==2 & c==1)/sum(c==1))
  pm2<-with(data,sum(c_obs==1 & c==2 )/sum(c==2))
  c(pm1, pm2)
}

#' A helper function to account for differential misclassification
#'
#' @param x thing to lag
#' @param k how much to lag it
#' @return vector
#' @export
#' @examples
#' lagpad(dsx$times,1)


lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(0, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(0, -k)));
  }
}
