# Assets Functions --------------------------------------------------------

# Calculating Assets and Total Net Worth

CalcCowAssets <- function(t, herd, p.cow, sell.year = NA, replace.year = NA) {
  # Function: CalcCowAssets
  # Description: Calculates the cow assets for each year.
  
  # Inputs:
  #  herd
  #  p.cow
  #  sell.year = Single numeric value. Equal to t where year 1 is t=1.
  #  replace.year = Single numeric value. Equal to t where year 1 is t=1.
  
  # Output:
  #  6x1 vector of cow assets for each year, including t=0
  
  cow.assets <- rep(NA,t)
  
  if(is.na(sell.year)) {
    cow.assets[1:t] <- herd * p.cow
    if(t != 1){
      cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    }
    return(cow.assets)
  }
  
  if(sell.year > 0 & is.na(replace.year)) {
    cow.assets[1:sell.year] <- herd * p.cow  # Allows for sale of herd outside of year 1
    cow.assets[sell.year:t] <- 0  # Replaces sell year with 0, leaves prior years with herd, all subsequent years with no herd
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }
  
  if(sell.year > 0 & replace.year > 0 ) {
    cow.assets[1:sell.year] <- herd * p.cow  # Allows for sale of herd outside of year 1
    cow.assets[sell.year:(replace.year-1)] <- 0  # Replaces sell year with 0, leaves prior years with herd
    cow.assets[replace.year:5] <- herd * p.cow  # After replacing, assumes no additional sales
    cow.assets <- c(herd * p.cow, cow.assets)  # Adding time 0 cow assets to make a 6x1 vector
    return(cow.assets)
  }
}