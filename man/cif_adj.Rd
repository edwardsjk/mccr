% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cif_adj.R
\name{cif_adj}
\alias{cif_adj}
\title{A function to account for differential misclassification in cumulative incidence functions}
\usage{
cif_adj(data = data, data_evd = data_evd, t = "x", obs = "c_obs",
  true = "c", level = 1, tau = 1, diff_by = NA)
}
\arguments{
\item{data}{input data source containing main study data}

\item{data_evd}{intput data source containing validation data with variables true outcome and misclassified outcome as well as any variable by which misclassification may be differential.}

\item{t}{name of the time variable in main study data}

\item{obs}{name of variable denoting possibly misclassified outcome type in both main and validation datasets}

\item{true}{name of variable denoting true outcome type (in validation data only)}

\item{level}{level of outcome for which to estimate cumulative incidence}

\item{tau}{maximum follow-up time}

\item{diff_by}{name of variable in main study and validation dataset by which misclassification is differential}
}
\value{
data frame with event times and corrected cumulative incidence function for outcome of interest
}
\description{
A function to account for differential misclassification in cumulative incidence functions
}
\examples{
cif_adj(data = mydata, data_evd = valdata, t = "x", obs = "c_obs", true = "c", level = 1, tau = 2, diff_by = "diffx")
}
