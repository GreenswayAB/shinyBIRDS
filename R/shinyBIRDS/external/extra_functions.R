# Extra functions

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


cleanCoordinates <- function(x,
                             lon="decimallongitude",
                             lat="decimallatitude",
                             species="scientificname"){
  logs<-""
  print("Running data cleaning module")
  
  if ("countryCode" %in% colnames(x)){
    x$countryCode <- ISOcodes::ISO_3166_1$Alpha_3[match(x$countryCode, ISOcodes::ISO_3166_1$Alpha_2)]   
  } else {
    x$countryCode <- NA
  }
  
    
  #Run coordinate cleaner
  try(sp.data.clean <- CoordinateCleaner::clean_coordinates(x,
                                                            lon=lon,
                                                            lat=lat,
                                                            species=species,
                                                            countries = "countryCode",
                                                            value="clean",
                                                            tests=c("countries","capitals","centroids", "equal", "gbif",
                                                                    "institutions", "outliers", "seas","zeros")))
  if(exists("sp.data.clean")){
    x <- sp.data.clean
    logs <- paste(logs, nrow(x), "records remain after running CoordinateCleaner\n")
  } else {
    logs <- paste(logs, "CoordinateCleaner failed. Trying now without country test\n")
    tryCatch({sp.data.clean <- CoordinateCleaner::clean_coordinates(x,
                                                                    lon=lin,
                                                                    lat=lat,
                                                                    species=species,
                                                                    countries = NULL,
                                                                    value="clean",
                                                                    tests=c("capitals","centroids", "equal", "gbif",
                                                                            "institutions", "seas","zeros"))
    x <- sp.data.clean
    logs <- paste(logs, nrow(x), "records remain after running CoordinateCleaner\n")},
    error = function(e) {
      logs <- paste(logs, e)
      logs <- paste(logs, "CoordinateCleaner failed. No data cleaning performed.\n")
    })
  }
  return(list(x, logs))
}


