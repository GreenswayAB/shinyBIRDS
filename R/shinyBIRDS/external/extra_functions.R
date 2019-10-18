# Extra functions
funM<-function(x, y, ...){
  return(max(c(x,y), na.rm=T))
}

IgnComb <- function(x,y,o05,o05spp, ...) {
  IgnOI <- o05spp/(o05spp + x)
  IgnOb <- o05/(o05 + y)
  # return( Ign <- 1-((1-IgnOI)*(1-IgnOb)) )  ## why not 1-...
  return( Ign <- 1-sqrt((1-IgnOI)*(1-IgnOb)) )  ## why not 1-...
  # return( Ign <- sqrt(IgnOI*IgnOb) )
}

##Estimate mode
est.mode <- function(x,from=min(x, na.rm = TRUE),to=max(x, na.rm = TRUE)) {
  d <- density(x,from=from, to=to, na.rm = TRUE)
  d$x[which.max(d$y)]
}
# options(digits=4)
