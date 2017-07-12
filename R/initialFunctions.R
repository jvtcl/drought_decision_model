# Variable Assignment Functions -------------------------------------------

getConstantVars <- function(){
  
  "
  Reads in constant variables into a
  `constvars` environment using the
  file `data/constant_vars.csv`.
  "
  
  
  cvars=read.csv("data/constant_vars.csv",stringsAsFactors = F)
  cvars.list <- split(cvars$Value, seq(nrow(cvars)))
  names(cvars.list) <- cvars$Variable
  return(cvars.list)
}

getSimVars <- function(station.gauge,
                      constvars,
                      start_year = 'random', 
                      sim_length = 5,
                      use.forage = FALSE,
                      autoSelect.insurance = FALSE,
                      clv = 0.9,
                      acres = 3000,
                      pfactor = 1,
                      random.coverage = FALSE,
                      random.acres = FALSE,
                      random.productivity = FALSE,
                      drought.adaptation.cost.factor = 1){
  
  "
  Note that this will default to the excel
  model var's
  
  Inputs:
  station.gauge = list of station gauge variables generated using the getstationgauge function
  constvars = list of constant variables generated using the getconstantVars function
  random.starts = TRUE: Choose random year to start simulations, then years following are temporally sequencial
  random.starts = FALSE: 2002 is the start year
  use.forage = TRUE: Calf weights without adaptation follow forage potential predictions
  autoSelect.insurance = TRUE: Choose optimal insurance coverage periods based on forage potential
  autoSelect.insurance = FALSE: Insure 50% for 3rd interval and 50% for 5th interval
  clv: Coverage level of insurance
  acres: Size of ranching operation. Modeled to only impact the insurance premium/payout.
  pfactor: Productivity factor relative to other farms in the grid. Relevant only to insurance premium/payout.
  drought.adaptation.cost.factor: Adjusts the impact of the low forage potential on costs of adaptation.
  
  "
  
  simvars <- new.env()
  
  ## Range of years
  # if specified, use a random starting year
  if(start_year == 'random'){
    assign("styr", round(runif(1, 1948, 2010)), envir = simvars)
  }else{
    assign("styr", start_year, envir = simvars) # starting year in five-year period
  }
  assign("sim_length", sim_length, env = simvars)
  
  # Static vs dynamic vars, based on forage
  attach(constvars) # use direct reference to `constvars` environment
  
  # if(use.forage){
  #   assign("wn.wt", calfWeanWeight(get("styr", simvars), sim_length), envir = simvars) # dynamic by year based on precip/forage
  # }else{
  #   assign("wn.wt", c(calfWeanWeight(get("styr", simvars), sim_length)[1], rep(normal.wn.wt, sim_length-1)), envir = simvars) # year 1 only based on precip/forage
  # }
  # 
  # Drought action var's
  assign("drought.action", ifelse(1:sim_length %in% act.st.yr:act.end.yr, 1, 0), envir = simvars)
  assign("calf.loss", ifelse(get("drought.action", simvars) == 1, 2, 0), envir = simvars)
  assign("calf.wt.adj", ifelse(get("drought.action", simvars) == 1, -0.1, 0), envir = simvars)
  detach(constvars)
  assign("drought.adaptation.cost.factor", drought.adaptation.cost.factor, envir = simvars)
  
  ## Wean weights
  # make this constant??
  # previously 'p.wn.yr1', now vectorized for iteration
  # (I only have this here because vectors can't be represented well in `constant_vars.csv`)
  # ****We likely are going to want to begin varying these but I'll leave them for now
  assign("p.wn", rep(1.30, sim_length), envir = simvars)
  
  ## set target insurance years
  assign("yyr", simvars$styr:((sim_length-1) + simvars$styr), envir = simvars) # all five years
  
  
  ## Set Insurance variables
  # ****We can probably leave this and then just change things as
  # users decide whether to buy insurance or not
  assign("clv", clv, envir = simvars) # insurance coverage level (0.7 - 0.9 in increments of 0.05)
  assign("acres", acres, envir = simvars) # ranch acres
  assign("pfactor", pfactor, envir = simvars) # productivity factor (0.6 - 1.5)
  
  # insurance coverage level (0.7 - 0.9 in increments of 0.05)
  # ****Remove this?
  if(random.coverage){
    assign("clv",round_any(runif(1,0.7,0.9),0.05),envir=simvars)
  }else{
    assign("clv",clv,envir=simvars)
  }
  
  # ranch acres
  # sample from a random normal distribution with
  # mean ranch size = 3000 (default) and sd = 500
  # these are arbitrary choices!! should revise as needed
  if(random.acres){
    assign("acres",round(sample(rnorm(10000,mean=3000,sd=500),size=1)),envir=simvars)
  }else{
    assign("acres",acres,envir=simvars)
  }
  
  # ranch productivity factor (0.6 - 1.5)
  if(random.productivity){
    assign("pfactor",round_any(runif(1,0.6,1.2),0.01),envir=simvars)
  }else{
    assign("pfactor",pfactor,envir=simvars)
  }
  
  # Insurance purchases
  # Use Excel model choices by default,
  # otherwise automatically allocate based
  # upon forage potential
  attach(station.gauge) # reference the `station gauge` environment's vars
  if(autoSelect.insurance == TRUE){
    assign("insp", insAlloc(fpwt = zonewt, niv = 2), envir = simvars) # automatic selection
  }else{
    assign("insp", rbind(c(5, 0.5), c(7, 0.5)), envir = simvars) # insurance purchase
  }
  detach(station.gauge)
  
  ## Precip, Forage Potential, and Calf Weight variables
  
  
  #****These  probably need to go
  assign("dr_start", constvars$act.st.m, envir = simvars) # Drought adaptive action starts
  assign("dr_end", constvars$act.end.m, envir = simvars) # Drought action ends
  
  return(as.list(simvars))
  
}


# Weather Functions -------------------------------------------------------

getStationGauge <- function(target.loc="CPER"){
  
  "
  Author: Joe
  
  Returns precipitation record and locational attributes for the target
  location. Default is Central Plains Experimental Range (CPER) but alternative
  locations at COOP sites across Colorado may be specified.
  
  If CPER (default):
  
  Zone Weights `zonewt` are read from the Excel model which is based on [xx]
  drought calculator state forage potential weights that we cannot reproduce.
  We are missing spatial reference information necessary to assign each target
  location to a state zone.
  
  Weights Zone `stzone` which corresponds to the state weights zone in which
  CPER resides (Colorado Zone 3).
  
  Station Gauge `stgg`, historical precipitation totals dating back to 1948,
  which are also read in from the Excel model. Precip totals are collected at
  CPER itself and do not rely on precip data from COOP sites.
  
  Target Grid Cell `tgrid`, for reading in PRF index values at a given point
  in time. `tgrid` is assigned to the PRF grid cell where we assume CPER exists,
  25002.
  
  If COOP site (user-specified):
  
  Zone Weights `zonewt` are based upon the Major Land Resource Area in which
  the COOP site resides. The MLRA forage potential is an average of plant growth
  curves calculated for a series of ecological site surveys (ESS) performed for
  that MLRA (using functions `COOPinMRLA` `getMLRAWeights`). Our decision to
  use an average plant growth curve is a placeholder that could be replaced,
  for example, by a regression framework like the one used to calculate
  the state weights. Alternatively, we could contact the authors of the
  [xx] state weights and use these instead of MLRAs (since these are
  likely more accurate than the weights used here).
  
  Weights Zone which in this case corresponds to the MLRA site within which
  the target COOP site resides.
  
  Station Gauge `stgg`, which is read from the historical precipitation data
  `precip` attached to the target COOP site list `target.coop`.
  
  Target Grid Cell which belongs to the list of COOP site attributes
  `target.coop`. This variable is computed by converting the coordinates of
  the COOP site to a SpatialPoint object and finding the underlying PRF
  grid cell.
  
  Upon completion of the function `getStationGauge`, a new sub-environment
  `station.gauge` is generated, which contains `zonewt`, `stzone`, `stgg`,
  and `tgrd` based on the target location.
  
  "
  ## This isn't necessary because were simply going to be writing over the list named
  ## station.gauge in the master.R file
  # clear station gauge environment if previously written
  # if(exists("station.gauge",envir = globalenv())){
  #   rm("station.gauge",envir=globalenv())
  # }
  
  if(target.loc=="CPER"){ # Use COOP sites or CPER: Default to CPER
    
    ## Zone Weights
    stzone <- 1 # state forage zone
    
    zonewt <- matrix(c(0.0, 0.0, 0.02, 0.08,0.20,0.28,0.15,0.12,0.10,0.05,0.0,0.0), 
                     nrow = 1, ncol = 12)
    colnames(zonewt) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                          "Aug", "Sep", "Oct", "Nov", "Dec")
    
    ## Station precip gauge
    # stgg <- data.frame(read_excel("misc/One_Drought_User_Interface_w_NOAA_Index.xlsx", "CPER Precip", skip = 1))
    # stgg <- stgg[, -which(names(stgg) %in% c("TOTAL", "Var.15"))]
    # stgg <- stgg[stgg$Year %in% c(1948:2016, "AVE"), ]
    
    # load("data/cperPrecip.RData")
    load("data/noaaPrecip.RData")
    
    ## Target grid cell
    tgrd = 25002  # target grid cell - CPER default
    
  }else{ #Custom location specified (COOP site and MLRA forage potential weights)
    
    ## Fetch data
    wrc.state <- "co" # For pulling COOP sites & mlra forage weights
    load("data/coops.RData") # Shortcut for sourcing 'R/coop_scraper.R'
    # source("R/coop_scraper.R") # the long way
    mlra <- readOGR("data", "mlra_v42") # load MLRA zone data
    target.coop <- coops[[which(names(coops) == target.loc)]]
    
    ## Zone weights
    mlra.idx <- COOP_in_MRLA(target.coop) # MLRA index
    zonewt <- getMRLAWeights(wrc.state) # zone weights
    stzone <- which(zonewt[, 1] == mlra.idx) # not a great workaround...should fix 'getForagePotential' function instead
    zonewt <- zonewt[, -1] # not a great workaround...should fix 'getForagePotential' function instead
    
    ## Station precip gauge
    stgg <- target.coop$precip
    stgg <- rbind(stgg, rep(NA, ncol(stgg)))
    stgg[nrow(stgg), ][, 1] <- "AVE"
    stgg[nrow(stgg), ][, -1] <- colMeans(stgg[-nrow(stgg), ][, -1],na.rm = TRUE)
    
    ## Target grid cell
    tgrd <- target.coop$grid  # target grid cell - custom site
    
  }
  
  # Write vars to new env
  ## Previously used to create a spatial point on the gridcell "tgrd_pt" = rastPt[rastPt@data$layer == tgrd, ]
  station.gauge <- vector("list", 5)
  station.gauge <- list("zonewt" = zonewt[stzone,], "stgg" = data.table(stgg),
                        "tgrd" = tgrd, avg = data.table(stgg[nrow(stgg), ][, -1]))
  return(station.gauge)
}

createResultsFrame <- function(pars = NULL){
  "
  Function: createResultsFrame
  Description: This function creates a theoretical previous result from the year before the simulation begins
    right now this assumes that there was no drought the year before the simulation and 
    revenues were 0. These assumptions are likely unrealistic and can be adjusted to accomodate different
    scenarios.
  
  Inputs:
  pars = state variables, simRuns in Master

  Outputs:
  sim_results = data table to for filling in future results
  "
  resultNames <- c("yr","adapt_choice","rev.calf", "rev.ins","rev.int", 
                   "rev.tot", "cost.op", "cost.ins", "cost.adpt",
                   "cost.int", "cost.tot", "profit", "taxes", "aftax.inc", 
                   "cap.sales", "cap.purch", "cap.taxes", "assets.cow", 
                   "assets.cash", "net.wrth", "wn.succ", "forage.production", 
                   "herd", "calves.sold", "cows.culled", "zone.change", "Gt",
                   "forage.potential", "rangeHealth")
  ## fills in rows using initial variables from pars
  if(!is.null(pars)){
    sim_results <- data.table(matrix(0, pars$sim_length + 1, length(resultNames)))
    setnames(sim_results, resultNames )
    sim_results[1, herd := pars$herd]
    sim_results[1, assets.cow := with(pars, CalcCowAssets(t = 1, herd = herd, p.cow = p.cow))]
    #The line directly below sets bank balance to 90000. column 19 is assets.cash. 
    sim_results[1,19] = 90000
    sim_results[1, net.wrth := assets.cow + assets.cash]
    sim_results[, adapt_choice := as.character(adapt_choice)]
    sim_results[1, adapt_choice := "noadpt"]
    sim_results[1, forage.production := 1]
    sim_results[1, wn.succ := pars$normal.wn.succ]
    sim_results[1, calves.sold := herd * wn.succ * pars$calf.sell]
    sim_results[1, cows.culled := herd * pars$cull.num]
    sim_results[1, zone.change := 1]
    sim_results[1, Gt := 0]
    sim_results[1, forage.potential := 1]
    sim_results[1, total.forage := 1]
    
  ## if pars isn't prsent fills in everything with 0's 
  }else{
    sim_results <- data.table(matrix(0, 1, length(resultNames)))
    setnames(sim_results, resultNames ) 
  }
  return(sim_results)
}