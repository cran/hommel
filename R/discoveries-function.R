discoveries <- function(hommel, ix, incremental=FALSE, alpha=0.05)
{
  m <- length(hommel@p)
  if (missing(ix) & incremental==FALSE) {
    h <- findHalpha(hommel@jumpalpha, alpha, m)
    return(m-h)
  } 
  if (missing(ix) & incremental==TRUE) {
    stop('Found incremental=TRUE but missing ix.')
  }
  
  if (!missing(ix)) {
    k <- length(hommel@p[ix])
  }

  if (any(is.na(hommel@p[ix])))
    stop('NAs produced by selecting with ix.')

  if (k == 0) {
    warning('empty selection')
    return(0)
  }

  h <- findHalpha(hommel@jumpalpha, alpha, m)

  simesfactor <- hommel@simesfactor[h+1]

  allsortedp <- hommel@p[hommel@sorter]
  
  ix_sortedp <- integer(m)
  names(ix_sortedp) <- names(hommel@p)
  ix_sortedp[hommel@sorter] <- 1:m
  ix_sortedp <- ix_sortedp[ix]

  discoveries <- findDiscoveries(ix_sortedp, allsortedp, simesfactor, h, alpha, k, m)

  if(!incremental) 
    return(discoveries[k+1])
  else 
    return(discoveries[-1])

}


tdp <- function(hommel, ix, incremental=FALSE, alpha=0.05)
{
  m <- length(hommel@p)
  if (missing(ix)) {
    k <- m
    d <- discoveries(hommel, incremental=incremental, alpha=alpha)
  } else {
    k <- length(hommel@p[ix])
    d <- discoveries(hommel, ix, incremental=incremental, alpha=alpha)
  }
  d/k
}

fdp <- function(hommel, ix, incremental = FALSE, alpha=0.05)
{
  1-tdp(hommel, ix, incremental=incremental, alpha=alpha)
}
