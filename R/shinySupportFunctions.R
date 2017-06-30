## Support functions for the shiny app

getJulyInfo <- function(currentYear, name, startYear, myOuts){
  
  "
  Function: getJulyInfo
  Description: Calculates available and predicted forage in july, creates a
    ui to display info and allows user to select adaptation level
  
  Inputs:
  currentYear = the current year
  
  Outputs:
  tagList = list of ui elements to be displayed
  "
  
  ## Establish current state variables 
  myYear <- startYear + currentYear - 1
  herd <- myOuts[currentYear, herd]
  zones <- station.gauge$zonewt
  
  myOuts[currentYear, mTurkID := ID]
  ## Calcualte available forage for normal, high, and low precip over remaining months
  forargeList <- vector("numeric", 3)
  if(currentYear == 1){
    zones <- zones * (1 - (0)/simRuns$forage.constant)
  }else{
    zones <- myOuts[currentYear - 1, zone.change] * zones * 
      (1 - (myOuts[currentYear - 1, Gt])/simRuns$forage.constant)
  }

  forageList <- vector("numeric", 3)
  forageList[1] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "normal")
  forageList[2] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "high")
  forageList[3] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "low")
  
  ## Calculate cost of Adaptaiton
  adaptInten <- sapply(forageList, calculateAdaptationIntensity)
  adaptInten <- c(adaptInten, 1)
  fullAdaptCost <- sapply(adaptInten, getAdaptCost, adpt_choice = "feed", pars = simRuns, 
                           days.act = 180, current_herd = herd)
  adaptMax <- max(fullAdaptCost)
  ## Round outputs for display
  forageList <- round(forageList, 2) * 100
  fullAdaptCost <- prettyNum(round(fullAdaptCost, -2), big.mark=",",scientific=FALSE)
  
  
  #Adding $ sign to Adaptation Cost/Hay Cost
  fullAdaptCost1 = paste("You should buy $",sep="", fullAdaptCost, " of hay" )
  forageList1 = paste("Your herd will have ", forageList, sep="", "% of the grass it needs")
  #code for rainplot
  Rain1 <- c(forageList1[1], fullAdaptCost1[1])
  Rain2 <- c(forageList1[2], fullAdaptCost1[2])
  Rain3 <- c(forageList1[3], fullAdaptCost1[3])
  RainfallL <- data.table(Rain1, Rain2, Rain3)
  colnames(RainfallL) <- c("If you expect Jul-Oct rainfall to be normal", "If you expect Jul-Oct rainfall to be above average ", "If you expect Jul-Oct rainfall to be below average")
  
  plotOutput(paste0("RangeHealthPlot", name))
  
  #creating code for above/below/average rainfall
  
  #subsetting NOAA monthly precipitation values based on myYear - the current year the simulation is running on
  SubsetNOAAyear <- subset(monthlyNOAA_long, Year == myYear)
  
  #renaming "variable" column to "Month", value to percentage of rainfall 
  names(SubsetNOAAyear)[names(SubsetNOAAyear) == "variable"] <- "Month"
  names(SubsetNOAAyear)[names(SubsetNOAAyear) == "value"] <- "RainfallP"
  
  #removing useless columns
  SubsetNOAAyear[,c("AVG","index","grid","realValue","Year")] <- NULL

  #Creating FOrage Potential dataframe
  ForageMonthly <- data.frame(station.gauge$zonewt)
  ForageMonthly <- setNames(cbind(rownames(ForageMonthly), ForageMonthly, row.names = NULL), 
           c("Month", "FPvalue"))

  #Combining Subsetted NOAA precipitation data with Forage Potential Values
  CombinedForageandRain <- data.frame(SubsetNOAAyear, ForageMonthly)
  
  #Removing second column of months
  CombinedForageandRain[,c("Month.1")] <- NULL
  
  #Creating Weighted Values for all months
  CombinedForageandRain$`Weighted Values` <- CombinedForageandRain$RainfallP*CombinedForageandRain$FPvalue
  
  #Finding the overall percentage of rainfall from January to June, November to December. Also Rounds it to a whole number. 
  ForageValue <- round(sum(CombinedForageandRain$`Weighted Values`[c(1:6,11,12)]) /
                         sum(CombinedForageandRain$FPvalue[c(1:6,11,12)]), digits = 0)
  
  ForageValueAll <- round(sum(CombinedForageandRain$`Weighted Values`)/
                         sum(CombinedForageandRain$FPvalue), digits = 0)
  ForageValueAllp <<-if(ForageValueAll >= 110){
    p(span("Your rainfall for this year has been above average at",style = "font-size:normal"),
      span(ForageValueAll, style = "font-weight:bold;font-size:large;color:green"), "%", 
      span("of the amount needed for optimal grass growth.", style = "font-size:normal"))  
  } else if(ForageValueAll<110 & ForageValueAll>100){
    p(span("Your rainfall for this year has been average at",style = "font-size:normal"), 
      span(ForageValueAll, style = "font-weight:bold;font-size:large;color:green"), "%",
      span("of the amount needed for optimal grass growth.", style = "font-size:normal")) 
  } else {
    p(span("Your rainfall for this year has been below average at", style = "font-size:normal"),
      span(ForageValueAll, style = "font-weight:bold;font-size:large;color:red"), "%",
      span("of the amount needed for optimal grass growth.", style = "font-size:normal")) 
  }

    #do weighted average(value*forage potential )


  ## Create taglist showing all adpatation
  tagList(
    tags$head(tags$style(HTML(
      # CSS formating for the rollover buttons
      ".inTextTips{
                      color:rgb(0, 0, 0);
                      text-align: left;
                      border-color: rgb(255,255,255);
                      background-color: rgb(255, 255, 255);
                                  }
                      .inTextTips:hover{
                      color:rgb(0, 0, 0);
                      text-align: left;
                      border-color: rgb(255,255,255);
                      background-color: rgb(255, 255, 255);"))),
    h3(paste0("Year ", currentYear, ": Summer Adaptation Investment Decision")),
    p("It is now the end of June, and you are over halfway through the growing season for forage on your rangeland. Good rainfall levels in July and August can further increase the forage for your herd. However, low rainfall levels will provide limited forage levels for your herd. It is your choice to decide how much hay to supplement in order to compensate the possible low amounts of grass on your range. Below indicates three options if you choose to invest in hay.",
      bsButton("Precipitation", label = "", icon = icon("question"), style = "info", class="inTextTips", size = "extra-small"),
      bsPopover(id = "Precipitation", title = "Precipitation",content = paste0("The ranch operates by putting mother cows on rangeland to graze. In years with less precipitation the ranch must purchase extra hay because there is less grass available and calves are unable to reach their target weight. Thinner calves mean less revenue when they go to market. Feeding the mother cows more hay keeps them healthy and able to produce milk for the calves. Also, if the mother cows are not healthy, they will not produce as many calves next season."),
                placement = "bottom", 
                trigger = "hover", 
                options = list(container = "body"))),
    #Pastes/shows if the rainfall was below, at, or above average.
    if(ForageValue >= 110){
      p(span("Your rainfall so far for this year has been above average at",style = "font-size:normal"),
        span(ForageValue, style = "font-weight:bold;font-size:large;color:green"), "%", 
        span("of the amount needed for optimal grass growth.", style = "font-size:normal"))  
      } else if(ForageValue<110 & ForageValue>100){
     p(span("Your rainfall so far for this year has been average at",style = "font-size:normal"), 
       span(ForageValue, style = "font-weight:bold;font-size:large;color:green"), "%",
       span("of the amount needed for optimal grass growth.", style = "font-size:normal")) 
    } else {
     p(span("Your rainfall so far has been below average at", style = "font-size:normal"),
       span(ForageValue, style = "font-weight:bold;font-size:large;color:red"), "%",
       span("of the amount needed for optimal grass growth.", style = "font-size:normal")) 
    },
    br(),
    plotOutput(paste0("rainGraph", name)),

    tbl <- renderTable({ head( RainfallL, n =  )},width = '100%', colnames = TRUE),
    tableOutput('tbl'),

    # p("If rainfall for the rest of the year is average your available forage will be ", span((forageList[1]),style="font-weight:bold;font-size:medium"), "% of normal. In this case,
    #          you should buy $", span((fullAdaptCost[1]),style="font-weight:bold;font-size:medium"), " of hay to get your herd in ideal shape for market."),
    # p("If rainfall for the rest of the year is above average your available forage will be ", span((forageList[2]),style="font-weight:bold;font-size:medium"), "% of normal.
    #          In this case, you should buy $", span((fullAdaptCost[2]),style="font-weight:bold;font-size:medium"), " of hay to get your herd in ideal shape for market."),
    # p("If rainfall for the rest of the year is below average your available forage will be ", span((forageList[3]),style="font-weight:bold;font-size:medium"), "% of normal.
    #          In this case, you should buy $", span((fullAdaptCost[3]),style="font-weight:bold;font-size:medium"), " of hay to get your herd in ideal shape for market.")
    # 
    # ,
    br(),
    p("Remember that if you do not have enough money in the bank to cover the cost of hay you will automatically borrow at a 6.5% interest."),
    numericInput(paste0("d", name, "adaptExpend"), "How much hay, if any, do you want to purchase for your herd?",
                min = 0, max = adaptMax, value = 0, step = 100, width = "100%")
  )
}


getCowSell <- function(totalForage, wean, currentYear, name, myOuts){
  "
  Function: getCowSell
  Description: Creates a UI for the user to select how many cows and calves to sell.
  
  Inputs:
  totalForage = the available forage after adaptation has been applied
  wean = wean success
  currentYear = the current year 
  
  Outputs:
  tagList = UI elements in a tag list
  "
  
  ## Calcualte how many cows to sell
  ## Establish current state variables
  herd <- myOuts[currentYear, herd]
  
  
  ## Calculate weaned Calves
  calvesAvailable <- round(herd * wean)
  ## Calculate Standard Sales
  standardCowSale <- round(herd * simRuns$cull.num)
  standardCalfSale <- round(calvesAvailable * simRuns$calf.sell)
  weanWeight <- round(calfDroughtWeight(simRuns$normal.wn.wt, totalForage), 0)
  
  ## Create UI elements
  tagList(
    br(),
    br(),
    h3(paste0("Year ", currentYear, ": Fall Cow and Calf Sales")),
    p("It is the end of the season and it is time to take your stock to market.
      Use the information below to decide how many cows and calves you want to 
      sell this year."),
    br(),
    
    if((weanWeight)<600){
      h5(p("Your weaned calves weigh ", 
           span((weanWeight), 
                style="font-weight:bold;font-size:large;color:red"), 
           " pounds, on average.", 
           
           " Your weaned calves weigh ", 
           span((600 - weanWeight), 
                style="font-weight:bold;font-size:large;color:red"), 
           " pounds below their target weight.

           This means that you're losing out on $", 
           span(paste0((simRuns$p.wn[1]*(600 - weanWeight)), "0"), 
                style="font-weight:bold;font-size:large:color:red"), 
           " for each calf you sell."))
    }else{
      h5(p("Your weaned calves weigh ", 
           span((weanWeight), 
                style="font-weight:bold;font-size:large;color:green") , 
           " pounds, on average."))
    },
    
    p("If your calves are lighter than 600 lbs, it is because the mother cows
                   may not have had sufficient feed due to low rainfall, insufficient hay, or too many cows on the range."),
    br(),
    h5(paste0("You currently have ", myOuts[currentYear, herd], " cows and ", calvesAvailable, " calves.")),
    tags$li(paste0("With the current market price of $",paste0(simRuns$p.wn[1], '0'), "/pound, each calf you sell will bring in $", 
                   round(weanWeight * simRuns$p.wn[1], 0) , " of cash.")), 
    tags$li(paste0("At the normal target weight, each calf you sell would bring in $", simRuns$p.wn[1]*600, " of cash.")),
    tags$li("For every cow you sell, you will bring in $850 of cash."),
    br(),
    h5("This decision will affect your herd size in future years."),
    h5(tags$li(paste("If your herd is at full health (normal weight calves, full reproductive potential), your herd will stay
            the same size as it is now if you sell", standardCalfSale, "calves and",  standardCowSale,  "cows.
            If you sell more, then your herd size will decrease.If you sell fewer, then your herd size will grow."))),
    h5(tags$li(p("Selling cows will affect your herd size starting next year, while selling or keeping calves will affect your herd size in two years when those calves could become mother cows."))),
    h5(tags$li(paste0("Remember, the carrying capacity of your range is ",simRuns$carrying.cap * simRuns$acres, " cow-calf pairs. 
              If your herd is larger than this you risk damaging your range and producing less grass for your herd."))),
    br(),
    sliderInput(paste0("calves", name, "Sale"), "How many calves do you want to sell?",
                min = 0, max = calvesAvailable, value =  standardCalfSale, step = 1, width = "600px"),
    # p(bsButton("calfherd", label = "", icon = icon("question"), style = "info", class="quest", size = "extra-small"),bsPopover(id = "calfherd", title = "Calf Description",content = paste0("selling or keeping calves will affect your herd size in two years, when those calves could become mother cows."))),
    sliderInput(paste0("cow", name, "Sale"), "How many cows do you want to sell?",
                min = 0, max = myOuts[currentYear, herd], value = standardCowSale, step = 1, width = "600px"),
    br()
    
      )
}


shinyInsurance <- function(yy, clv, acres, pfactor, insPurchase, tgrd){
  "
  Author: Adam (based loosely on Joe's work)
  
  Calculates premium and indemnification for a specific year and
  grid cell. Currently returns are summed but this could be done
  on a index interval basis instead.
  
  yy: Year of interest.
  
  clv: RMA coverage level. Accepted values
  are 0.7, 0.75, 0.8, 0.85, 0.9
  
  acres: Insured acres of grazing land.
  
  pfactor: Productivity factor of grazing land.
  
  insPurchase: a matrix of intervals from 1-11
  for which insurance is purchased. For example,
  purchases for the April-May and May-June intervals
  at 50% protection each would be entered as
  
  `rbind(c(3,0.5),c(5,0.5))`
  
  Consecutive intervals are not allowed.
  Returns a data.table of outputs:
  
  year: year of calculations
  prod_prem: producer premium
  indemnity: total indemnity
  full_prem: premium without subsidy
  
  "

  ##Get subsidy rate based on coverage level
  sbdy <- .51 #.51 is the subsidy for the .9 coverage level
  
  ##Set up insurance purchase vector
  ip = rep(0, 11)
  ip[insPurchase[, 1]] = insPurchase[, 2]  # replaces 0's with interval allocations
  insPurchase = ip
  
  ##Calculate policy rate
  plrt = prod(clv, acres, pfactor) * 30.4  # is the baseprice for the cper station
  
  ## Calculate the protection for each index interval
  monthProtec <- plrt * insPurchase
  
  ## Calculate Premium for each Month Below are premiums per 100 for CPER gridcell
  monthPrem <- (monthProtec * .01) * c(24.82, 23.30, 17.96, 16, 13.58, 14.69, 14.54, 17, 21.96, 23.30, 26.63)
  names(monthPrem) <- paste0("i", 1:11)
  ## Calculate Premium subsidy for each month
  subSidy <- round(monthPrem * sbdy, 2)
  
  ## Calculate subsidised premium for each month
  subPrem <- round(monthPrem * (1-sbdy), 2)
  
  ## Fetch index intervals
  intervalIndicies <- data.table(t(intervalNOAA[Year == yy, value]))
  setnames(intervalIndicies, names(subPrem))
  
  ## Calcualte the percent coverage for each month
  ## Rounding can make a significant difference here RMA uses 3 decimals
  coverageAmount <- round(clv - (intervalIndicies * .01), 3)
  
  ## Calculate Indemnities
  indem <- unlist(ifelse(coverageAmount > 0, coverageAmount/clv * monthProtec, 0))
  
  returnTable = data.table(matrix(nrow = 1, ncol = 4, data = c(yy, sum(subPrem), sum(indem), sum(monthPrem))))  # empty matrix - year, indemnity, producer premium x number years
  setnames(returnTable, c("year", "producer_prem", "indemnity", "full_prem"))
  
  return(returnTable)
}


inputToDF <- function(inputList){
  inputList <- inputList[names(inputList) != "enviro"]
  maxLength <- max(sapply(inputList, length))
  returnTable <- data.table("names" = names(inputList))
  returnTable[, paste0("value",1:maxLength) := list(rep("", nrow(returnTable)))]
  returnTable <- returnTable[names != "enviro"]
  for(i in 1:maxLength){
    returnTable[, paste0("value", i) := data.table(sapply(inputList, function(x)as.character(x[i])))]
    returnTable[, paste0("value", i) := as.character(get(paste0("value", i)))]
  }
  
  return(returnTable)
}


createOutputs <- function(practiceRuns, simRuns, indem, indemprac){
  practiceOuts <- createResultsFrame(practiceRuns)
  practiceOuts[1, cost.ins := indemprac[[1]]$producer_prem]
  myOuts <- createResultsFrame(simRuns)
  myOuts[1, cost.ins := indem[[1]]$producer_prem]
  practiceOuts <- practiceOuts
  myOuts <- myOuts
  # rangeHealthList <- rep(NA, 11)
}


rangeHealth <- function(currentYear, myOuts){
  #source("R/shinny_support.R")
  ## Calculates available forage for normal, high, and low precip over remaining months
  ## Establish current state variables 
  
  myYear <- startYear + currentYear - 1
  herd <- myOuts[currentYear, herd]
  zones <- station.gauge$zonewt
  
  ## Calculate available forage for normal, high, and low precip over remaining months
  forargeList <- vector("numeric", 3)
  if(currentYear == 1){
    zones <- zones * (1 - (0)/simRuns$forage.constant)
  }else{
    zones <- myOuts[currentYear - 1, zone.change] * zones * 
      (1 - (myOuts[currentYear - 1, Gt])/simRuns$forage.constant)
  }
  
  forageList <- vector("numeric", 3)
  forageList[1] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "normal")
  forageList[2] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "high")
  forageList[3] <- whatIfForage(station.gauge, zones, myYear, herd, carryingCapacity, 7, 11, "low")
  
  ## Calculate cost of Adaptaiton
  adaptInten <- sapply(forageList, calculateAdaptationIntensity)
  adaptInten <- c(adaptInten, 1)
  fullAdaptCost <- sapply(adaptInten, getAdaptCost, adpt_choice = "feed", pars = simRuns, 
                           days.act = 180, current_herd = herd)
  adaptMax <- max(fullAdaptCost)
  
  ## Round outputs for display
  forageList <- round(forageList, 2) * 100
  fullAdaptCost <- prettyNum(round(fullAdaptCost, -2), big.mark=",",scientific=FALSE)
  expectCost <<- fullAdaptCost
  precipexpec <<- forageList
}


simPageOutput <- function(rv, name = ""){
  yearStartTime <<- Sys.time()
  page <- paste0(name, rv$page)
  fluidRow(
    column(9,
           uiOutput(paste0("winterInfo", page)),
           uiOutput(paste0("start", page)),
           uiOutput(paste0("decision", page)),
           uiOutput(paste0("continue", page)),
           uiOutput(paste0("insuranceUpdate", page)),
           uiOutput(paste0("cowSell", page)),
           uiOutput(paste0("sellButton", page)),
           uiOutput(paste0("profits", page)),
           uiOutput(paste0("nextButton", page))
    )
  )
}