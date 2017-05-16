\name{Local test}

\alias{localtest}

\title{P-values and adjusted p-values of local tests}

\description{An intersection hypothesis is defined for any collection of null hypotheses as the hypothesis that is true if and only if all contributing null hypotheses are true. A test for an intersection hypothesis is called a local test. This function calculates unadjusted and familywise error adjusted p-values for local tests. Familywise error control is over all possible local tests simultaneously.}

\usage{ 
  localtest (hommel, ix) 
}

\arguments{
  \item{hommel}{The hommel object for the complete multiple testing problem.}
  \item{ix}{The selection of hypotheses defining the intersection hypothesis. Any selector that works on the original vector of p-values (index, negative index, logical, names) is allowed. If ix is missing, the global null hypothesis is used.}
}

\value{Returns a list with two components. p: the unadjusted p-value; adjusted: the familywise error adjusted p-value.}

\details{For the global null hypothesis the adjusted and unadjusted p-values are always identical. Adjusted p-values for an intersection hypothesis of size 1 are identical to the adjusted p-values contained in the hommel object and retrieved via p.adjust.}

\author{Jelle Goeman.}

\references{
Meijer, R., Krebs, T., Solari, A., & Goeman, J. (2016). Simultaneous Control of All False Discovery Proportions by an Extension of Hommel's Method. arXiv preprint arXiv:1611.06739.
}

\examples{
  #Generate a vector of pvalues
  set.seed(102)
  m <- 10
  pvalues <- c(runif(0.5*m,0,0.02), runif(0.5*m,0,1))
  
  # First step: create a hommel object.
  # Use Simes'test (simes = TRUE) or Hommel's robust test (simes = FALSE)
  hom <- hommel(pvalues, simes = TRUE)

  # Raw and adjusted p-values
  # In any intersection hypothesis of interest
  localtest(hom)  
  localtest(hom, 1:2)
  
  # a bound for the false discovery proportion:
  set <- 1:5
  localtest(hom, set)
}