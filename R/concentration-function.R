concentration <- function(hommel, alpha = 0.05, what = c("p", "z", "u")) {
  
  m <- length(hommel@p)
  
  h <- findHalpha(hommel@jumpalpha, alpha, m)
  simesfactor <- hommel@simesfactor[h+1]

  sortedp <- hommel@p[hommel@sorter]
  
  z <- findConcentration(sortedp, simesfactor, h, alpha, m)
  
  what = match.arg(what)
  
  if (what == "z") return(z)
  if (what == "p") return(sortedp[z])
  if (what == "u") return(z - m + h + 1)

}
  