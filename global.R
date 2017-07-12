#### Source Scripts ####
source("R/load.R")
source("R/shinySupportFunctions.R")
source("R/simUI.R")
source("R/forageFunctions.R")
source("R/adaptationFunctions.R")
source("R/costRevenueFunctions.R")
source("R/initialFunctions.R")
source("R/calfCowFunctions.R")
source("R/assetFunctions.R")


#### Javascript Setup ####
## Code to disable tab
jscode <- '
shinyjs.init = function() {
$(".nav").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}
'
## js code to reset
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

## Css for disabled tabs
css <- '
.disabled {
background: white !important;
cursor: default !important;
color: white !important;
}
'

#### Variable Assignments ####

# Populate a new environment with rainfall gauge info.
# Default location is CPER site
station.gauge <- getStationGauge()

monthlyNOAA_long <- station.gauge$stgg

monthlyNOAA_long <- monthlyNOAA_long[!Year %in% c("2016"),]

monthlyNOAA_long <- melt(monthlyNOAA_long, id.vars = "Year")
monthlyNOAA_long <- merge(monthlyNOAA_long[Year != "AVG",], monthlyNOAA_long[Year == "AVG"], 
                          by = c("variable"), all.x = T)
monthlyNOAA_long[, Year.y := NULL]
setnames(monthlyNOAA_long, c("value.x", "value.y", "Year.x"), c("realValue", "AVG", "Year"))
monthlyNOAA_long[, index := realValue/AVG * 100]

monthlyNOAA_long[,value := realValue/AVG * 100]
monthlyNOAA_long[, grid := 25002]



# Populate a new environment with constant (user) variables
constvars <- getConstantVars()

## Acres of range
acres <- 3000

## Carrying capacity based on acres and carrying.cap constant
carryingCapacity <- constvars$carrying.cap * acres

## Set Starting Years
startYear <- 1999
startYearprac <- 1951

## Set simulation length
simLength <- 10
practiceLength <- 5

## create state variables for practice runs
practiceVars <- getSimVars(
  station.gauge,
  constvars,
  start_year = startYearprac,
  sim_length = practiceLength,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=FALSE,
  acres)

## create state variables for full runs
simvars <- getSimVars(
  station.gauge,
  constvars,
  start_year = startYear,
  sim_length = simLength,
  use.forage = T,
  random.acres=FALSE,
  random.productivity=FALSE,
  acres)

## Create list of constant vars, state vars, and station gauges
practiceRuns <- (append(append(station.gauge, constvars), (practiceVars)))
practiceRuns$p.wn <- rep(1.40, length(practiceRuns$p.wn))
simRuns <- (append(append(station.gauge, constvars), (simvars)))
simRuns$p.wn <- rep(1.40, length(simRuns$p.wn))


#### Additional Settings ####
## Create JS to switch between year tabs
yearHandler <- paste0('if(typeMessage == ', 1:simLength, '){
  console.log("got here");
  $("a:contains(Year ', 1:simLength, ')").click();
}', collapse = "")

NUM_PAGES <- 5
currentPage <- 1

debugMode <<- TRUE

`%then%` <- shiny:::`%OR%`
genericWrong <- "This is incorrect. Please try again."
