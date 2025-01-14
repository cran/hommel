\name{Concentration set}

\alias{concentration}

\title{Concentration set calculation}

\description{Calculates the concentration set threshold. The concentration set is the subset of the p-values that contains all discoveries at confidence level 1-alpha.}

\usage{ 
  concentration (hommel, alpha=0.05, what = c("p", "z", "u")) 
}

\arguments{
  \item{hommel}{The hommel object for the complete multiple testing problem.}
  \item{alpha}{The type I error allowed.}
  \item{what}{The scale of the number returned: p: maximal p-value; z: maximal p-value rank; u: maximal category} 
}

\value{P-values over a certain threshold contribute no discoveries in this data set at this level of alpha and may be disregarded. The function returns either (1) the p-value threshold p; (2) the rank z of that p-value; or (3) the category u of that p-value.}

\author{Jelle Goeman.}

\references{
Goeman, J. J., Meijer, R. J., Krebs, T. J., & Solari, A. (2019). Simultaneous control of all false discovery proportions in large-scale multiple hypothesis testing. Biometrika, 106(4), 841-856.
}

\examples{
  #Generate a vector of pvalues
  set.seed(102)
  m <- 10
  pvalues <- c(runif(0.5*m,0,0.02), runif(0.5*m,0,1))
  
  # First step: create a hommel object.
  # Use Simes'test (simes = TRUE) or Hommel's robust test (simes = FALSE)
  hom <- hommel(pvalues, simes = TRUE)

  # Find the concentration set bound
  threshold <- concentration(hom)
  
  # Find the concentration set itself
  set <- pvalues <= threshold
  sum(set)
}