discoveries <- function(hommel, ix, alpha=0.05) 
{
  m <- length(hommel@p)
  if (missing(ix)) {
    p = hommel@p
    k = m
  } else {
    p <- hommel@p[ix]
    k <- length(p)
  }

  if (any(is.na(p)))
    stop('NAs produced by selecting with ix')
  
  if (k == 0) {
    warning('empty selection')
    return(0)  
  }    
    
  h <- findHalpha(hommel@jumpalpha, alpha, m)
  
  simesfactor <- hommel@simesfactor[h+1]
  
  allsortedp <- hommel@p[hommel@sorter]
  
  discoveries <- findDiscoveries(p, allsortedp, simesfactor, h, alpha, k, m)

  return(discoveries)
    
}

tdp <- function(hommel, ix, alpha=0.05) 
{
  m <- length(hommel@p)
  if (missing(ix)) {
    d <- discoveries(hommel, alpha=alpha)
    k <- m
  } else {
    p <- hommel@p[ix]
    k <- length(p)
    d <- discoveries(hommel, ix, alpha=alpha)
  }
  d/k
}

fdp <- function(hommel, ix, alpha=0.05) 
{
  1-tdp(hommel, ix, alpha=alpha) 
}

localtest_old <- function(hommel, ix) 
{
  m <- length(hommel@p)
  if (missing(ix)) {
    p = hommel@p
    k = m
  } else {
    p <- hommel@p[ix]
    k <- length(p)
  }
  
  if (any(is.na(p)))
    stop('NAs produced by selecting with ix')
  
  if (k == 0) {
    warning('empty selection')
    return(p=1, adjusted=1)
  }

  sortedp <- sort(p)
  
  pI <- min(sortedp/(1:k))
  
  rawp <- hommel@simesfactor[k+1] * pI
  if (!hommel@simes) 
    rawp <- min(rawp, 1)
  
  adjustedp <- adjustedIntersection(pI, hommel@jumpalpha, m, hommel@simesfactor)
  
  list (p = rawp, adjusted = adjustedp)
}

localtest <- function(hommel, ix, tdp=0)
{
  m <- length(hommel@p)
  if (missing(ix)) {
    p = hommel@p
    n = m
  }
  else {
    p <- hommel@p[ix]
    n <- length(p)
  }
  
  if (tdp<0 | tdp>1)
    stop("'tdp' must be chosen from [0,1]")
  
  if (any(is.na(p)))
    stop('NAs produced by selecting with ix')
  
  if (n == 0)
    stop('empty selection')
  
  k<-tdp*n
  sortedp <- sort(p)
  pI <- sortedp[rank(sortedp)>k]
  pI <- min(pI/rank(pI))
  
  adjustedp <- adjustedIntersection(pI, hommel@jumpalpha, m, hommel@simesfactor)
  
  return(adjustedp)
}

