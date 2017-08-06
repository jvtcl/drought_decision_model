
# Forage Functions -------------------------------------------------------

getForagePotential <- function(station.gauge, styear, herd, carryingCap,
                      decision = FALSE, decisionMonth = 5, farmYearStart = 11){
  
  "
  Returns a weight representing
  annual forage production for a
  given gridcell or station
  gauge's annual precip record.
  
  By default, computes the sum of weighted product of
  forage potential and long-term precipitation deviation
  from average for a given year relative to a grid cell
  or station gauge's period of record.
  
  A 'decision making under uncertainty' mode is also
  available (when 'decision' is set to TRUE):
  
  To build monthly weights, generates a typology
  of years 1948-2015 (k-medoids) based on
  monthly precip values observed at the
  station.
  
  To assemble the weights:
  
  Use the product of deviation in precip from
  long-term (1948-2015) average * monthly precip weights
  for months occurring before & during the
  decision month.
  
  For months occurring after
  the decision month, use the product of group
  average deviation from the long-term
  average * precip weight weight.
  
  This approach roughly approximates a 'best guess'
  scenario based on rain gauge observations -
  what should my precip for the remainder of the year
  look like given what I know by the decision month?
  
  **EVENTUALLY NEEDS INPUTS FOR
  STATE**
  
  stgg: station gauge or grid cell precip record
  stzone: state zone
  monthlyPrecipWeights: weights for state zone
  styear: year of interest
  decision: use 'decision under uncertainty' mode
  (default FALSE)
  
  "
  
  ## Subset precip weight weights and prep index
  
  yprecip <- station.gauge$stgg[Year %in% (styear-1):styear, ]  # monthly precip amounts for start year
  monthlyPrecipWeights <- station.gauge$monthlyPrecipWeights
  yprecip <- cbind((yprecip[Year == styear - 1, c("NOV", "DEC")]),  # adding Nov and Dec from previous year to rainfall
                   (yprecip[Year == styear, -c("NOV", "DEC", "Year")]))
  monthly.averages <- station.gauge$avg  # monthly average rainfall for each of the 12 months
  yearAvg <- rbindlist(list(yprecip, monthly.averages), use.names = T)
  
  
  
  ##****This is an interesting approach does it work? CV results needed
  # Also the year we're working with should be removed
  if(decision){ #"decision making under uncertainty" mode
    
    ## Group years in period of record by monthly precip
    #****Do we want to change the way year is done  here?
    #****This method gives very high forage potentials I might have messed something up
    
    # cper <- station.gauge$stgg[1:which(Year == 2015),] # subset by period 1948-2015
    # cper_clust <- pamk(cper[, -1]) # Find optimal groups of years, k = 2-10
    # yy_group <- cper_clust[[1]]$clustering[which(cper$Year == styear)] # Group membership for target year
    # yy_ave <- colMeans(cper[which(cper_clust[[1]]$clustering == yy_group), ][, -1]) # Group mean vector
    # yy_ave[c(1:decisionMonth - 1)] <- yprecip[c(1:decisionMonth-1)]
    # yidx <- yy_ave/ave # Expected index values for year (group mean vector / long-term average)
    # Generate forage potential weights
    # not sure why the rows are subsetting as lists!
    # foragewt <- unlist(c((monthlyPrecipWeights * pidx), (monthlyPrecipWeights * yidx)))
    
    # Replace Months forage in unknown months with the average
    
    yearAvg[1, (names(monthly.averages)[decisionMonth:(farmYearStart -1)]) := monthly.averages[,decisionMonth:(farmYearStart -1)]] 
  }
  
  # Monthly precip "index"
  precip.index  <- yearAvg[1,] / yearAvg[2,] 
  
  #Compute Forage Weight Potentials
  foragewt = monthlyPrecipWeights * precip.index[, names(monthly.averages), with = F]
    
  
  # Compute annual forage for zone
  forage.production <- sum(foragewt)
  
  ## Adjust for carying Capacity
  forage.production <-forage.production/carryingCap
  
  return(forage.production)
  
}

whatIfForage <- function(station.gauge, monthlyPrecipWeights, styear, herd, carryingCap,
                         currentMonth, farmYearStart = 11, expectedFuture){
  "
  Function: whatIfForage
  Description: calculate uncertain forage based given broad scenarios
  
  Inputs:
  station.gauge = list of station gauge info from simRuns/pars``
  monthlyPrecipWeights = precip weight weights adjusted based on previous decision and use
  styear = year the simulation started
  herd = current size of herd
  carryingCap = carrying capacity of range in number of calf/cow paris
  currentMonth = month to start the uncertian decision from
  farmYearStart = when does the farm year start, or the month after calves
    are sold
  expectedFuture = scenario selection either, 'normal', 'low', or 'high'
  
  Outputs:
  forage.production = the predicted amount of forage available based on 
    scenario selection
  "
  yprecip <- station.gauge$stgg[Year %in% (styear-1):styear, ]  # monthly precip amounts for start year
  yprecip <- cbind((yprecip[Year == styear - 1, c("NOV", "DEC")]), 
                   (yprecip[Year == styear, -c("NOV", "DEC", "Year")]))

  monthly.averages <- station.gauge$avg
  yearAvg <- rbindlist(list(yprecip, monthly.averages), use.names = T)
  
  if(expectedFuture == "normal"){
    yearAvg[1, (names(monthly.averages)[currentMonth:(farmYearStart -1)]) := monthly.averages[,currentMonth:(farmYearStart -1)]]
  }
  if(expectedFuture == "low"){
    yearAvg[1, (names(monthly.averages)[currentMonth:(farmYearStart -1)]) := monthly.averages[,currentMonth:(farmYearStart -1)] * .5]
  }
  if(expectedFuture == "high"){
    yearAvg[1, (names(monthly.averages)[currentMonth:(farmYearStart -1)]) := monthly.averages[,currentMonth:(farmYearStart -1)] * 1.5]
  }
  
  # Monthly precip "index"
  precip.index  <- yearAvg[1,] / yearAvg[2,]

  #Compute Forage Weight Potentials
  foragewt = monthlyPrecipWeights * precip.index[, names(monthly.averages), with = F]
  
  
  # Compute annual forage potential weight for zone
  forage.production <- sum(foragewt)
  ## Adjust for carying Capacity
  if(herd != 0){
    carryingCap <- herd/carryingCap
    forage.production <- forage.production/carryingCap  
  }
  return(forage.production)
  
  
}

getMRLAWeights<-function(state.code){
  
  "
  Computes forage potential weights using the
  mean of plant growth curves by MRLA for a
  specified state.
  
  Data source:
  
  https://esis.sc.egov.usda.gov/WelcomeFSG/pgFSGSelectFormat.aspx
  
  Inputs:
  
  state.code: two-letter state code (for referencing
  plant growth potential curves)
  
  "
  
  forage_mlra=read.table(paste0("data/",state.code,"_mlra.txt"),sep="|")
  forage_mlra[,1]=substr(forage_mlra[,1],3,4) # Get MRLA code
  forage_mlra=forage_mlra[!forage_mlra$V1 %in% c("00","01","99"),] # Remove placeholder plant growth sites
  forage_mlra=forage_mlra[,c(1,which(names(forage_mlra)=="V4"):ncol(forage_mlra))] # subset ID and plant growth curve
  names(forage_mlra)=c("MLRA","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  forage_mlra=aggregate(.~MLRA,data=forage_mlra,FUN=mean) # Compute mean plant growth curves by MLRA
  forage_mlra[,-1]=forage_mlra[,-1]/100 # convert to decimal to match drought calculator fgp weights
  
  return(forage_mlra)
}

COOP_in_MRLA<-function(coop){
  
  "
  Returns the MLRA in which a specified
  coop site is located.
  "
  
  coop.pt <- SpatialPoints(t(rev(coop$loc)), proj4string = CRS(proj4string(mlra)))
  # fwt=forage_mlra[forage_mlra$MLRA==(coop.pt %over% mlra)$MLRARSYM,][,-1]
  return(as.numeric(as.character(((coop.pt %over% mlra)$MLRARSYM))))
  
}