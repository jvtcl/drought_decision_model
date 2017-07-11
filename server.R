
function(input, output, session) {
  
  ## Calcualte indemnities for all years of the simulation
  indem <- lapply(startYear:(startYear + simLength - 1), function(x){
    with(simRuns, shinyInsurance(yy = x, clv = clv, acres = acres,
                              pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
  })
  
  indemprac <- lapply(startYearprac:(startYearprac + practiceLength - 1), function(x){
    with(practiceRuns, shinyInsurance(yy = x, clv = clv, acres = acres,
                                   pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
  })
  
  ## Calculate binary variable for hypothetical payout based on the weather
  ## If 
  indemnity <- lapply(indem, "[[", 3) # Pulling the value of the indemnity from the (list of) dataframes
  whatifIndem <- sapply(indemnity > 0, ifelse, 1, 0)  # Creating a binary variable where a year is eligible for a payout if you have insurance
  
  indemnityprac <- lapply(indemprac, "[[", 3) # Pulling the value of the indemnity from the (list of) dataframes
  whatifIndemprac <- sapply(indemnityprac > 0, ifelse, 1, 0)
  
  practiceOuts <- createResultsFrame(practiceRuns)
  practiceOuts[1, cost.ins := indemprac[[1]]$producer_prem]
  myOuts <- createResultsFrame(simRuns)
  myOuts[1, cost.ins := indem[[1]]$producer_prem]
  # rangeHealthList <- rep(0, simLength)
  # rangeHealthListprac <- rep(0, practiceLength)
  
  ## Is insurance purchased?
  purchaseInsurance <- T
  
  # Set reactive values--------------------------------------------------------
  # Reactive values used to track when inputs/outputs are saved at the end of practice round
  #  and regular round. Once values become TRUE simulation contineus
  values <- reactiveValues("saveComplete" = FALSE, "practSaveComplete" = FALSE)
  
  # Reactive value used to track what page to display, once value changes display page changes
  rv <- reactiveValues(page = 1, scrollPage = F)
  rvPrac <- reactiveValues(page = 1)
  

  
  if(!debugMode){
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Practice Simulation']")
    toggleClass(class = "disabled",
                 selector = "#navBar li a[data-value='Ranch Simulation']")
  }
  


  # Create pratice and simulation tabs-----------------------------------------
  
  # Each lapply cycles through the simCreator function to create all the ui and
  #   output for each simulaiton, everything is contained in a single tab and the
  #   rv reactive values are used to track page number
  
  # Create main simulation ui/output
  lapply(1:simLength, function(i){
    simCreator(input, output, session, i, rv, simLength, startYear, myOuts, indem, purchaseInsurance, whatifIndem)
  }) 
  
  # Create practice simulation ui/output, everything is the same except "prac" 
  #   is appended to the end of all object names
  lapply(1:practiceLength, function(i){
    simCreator(input, output, session,i, rvPrac, practiceLength, startYearprac, myOuts, indemprac, purchaseInsurance, whatifIndemprac, name = "prac")
  })

  # Observers for practice simulation------------------------------------------
  
  # Observer triggered when user starts practice round switches to prac
  #   simulation tab and allows user to begin game
  observeEvent(input$pracStart, {
    
    # Checks to see if user has been randomly assigned insurnace or not
    if(as.numeric(input$user.ID) >= 2000000){ # Mturk >= 2000000 is no insurance
      
      # Sets ins to false and resets all ins variables to zero, recreates output frames
      purchaseInsurance <<- FALSE
      indem <<- lapply(indem, function(x){
        x[, c("producer_prem", "indemnity", "full_prem") := 0]
        return(x)
      })
      indemprac <<- lapply(indemprac, function(x){
        x[, c("producer_prem", "indemnity", "full_prem") := 0]
        return(x)
      })
      myOuts[1, cost.ins := indemprac[[1]]$producer_prem]
      print("its F")
    }else{ # Excuted for all users with insurance
      
      # Sets ins to false and resets all ins variables to zero, recreates output frames
      purchaseInsurance <<- TRUE
      print("its T")
      indem <<- lapply(startYear:(startYear + simLength - 1), function(x){
        with(simRuns, shinyInsurance(yy = x, clv = clv, acres = acres,
                                  pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
      })
      
      indemprac <<- lapply(startYearprac:(startYearprac + practiceLength - 1), function(x){
        with(practiceRuns, shinyInsurance(yy = x, clv = clv, acres = acres,
                                       pfactor = pfactor, insPurchase  =  insp, tgrd = tgrd))
      })
      myOuts[1, cost.ins := indemprac[[1]]$producer_prem]
    }
    
    # Disable elements and move active tab
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Welcome']")
    disable("pracStart")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Practice Simulation']")
    updateTabsetPanel(session, "mainPanels", selected = "Practice Simulation")
  })
  
  observeEvent(input$prevBtnprac,{ 
    navPagePrac(-1)
    # rangeHealthList <<- appendRangeHealth(ifelse(round(sum(get(paste0("currentZones", name))()) * 100, 0) > 100, 100, round(sum(get(paste0("currentZones", name))()) * 100, 0)), rangeHealthList, rv$page)
    })
  observeEvent(input$nextBtnprac, navPagePrac(1))
  navPagePrac <- function(direction) {
    rvPrac$page <- rvPrac$page + direction
  }
  
  # Observers for real simulation----------------------------------------------
  
  # Triggered when a user clicks the begin ranch game button after practice 
  #   round has been completed disable elements and switch active tab
  observeEvent(input$simStart, {
    myOuts <<- createResultsFrame(simRuns)
    myOuts[1, cost.ins := indem[[1]]$producer_prem]
    disable("simStart")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Practice Simulation']")
    toggleClass(class = "disabled",
                selector = "#navBar li a[data-value='Ranch Simulation']")
    updateTabsetPanel(session, "mainPanels", selected = "Ranch Simulation")
  })
  
  # Observers and functions to advance simulation pages
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }  
  
  # Output and UI for practice and real simulation--------------------------------------
  
  # Generates output for practice simulation tab
  output$practiceOut <- renderUI({
    if(rvPrac$page <= practiceLength){
      simPageOutput(rvPrac, "prac")  
    }else{  # Executed when practice simulation has been completed
      fluidRow(
        h4(paste0("Congratulations! You've completed ", practiceLength, " years of ranching.")),
        br(),
        h4(p(paste0("You have accumulated $", prettyNum(round(myOuts$assets.cash[practiceLength + 1], 0), digits = 2, 
                                                        big.mark = ",", scientific = FALSE), " in cash." ))),
        h4(p(paste0("Your herd is now worth $", prettyNum(round(myOuts$assets.cow[practiceLength + 1], 0),
                                                          digits = 2, big.mark = ",", scientific = FALSE), "."))),
        h4(p(paste0("Based on your savings account and your herd worth, your total net worth is $", 
                    prettyNum(round(myOuts$net.wrth[practiceLength + 1], 0), digits = 2, big.mark = ",",
                              scientific = FALSE), "."))), 
                # With a conversation rate of $500,000 of simulation money to $1 of MTurk bonus money, you've earned $", 
                # round(myOuts$net.wrth[practiceLength + 1] * 1/simRuns$mturk.conv, 2),"."))),
        h4(p(paste0("This is a practice simulation, so the money you have earned in this simulation does not count."))),
        br(),
        h4("The practice simulation is now over. In the next section, you will complete the 10 round game that will affect your 
           chances at winning the bonus gift card."),
        hide("infoPanePrac"),
        actionButton("savePracInputs", "Save practice round"),
        uiOutput("practComplete")
        )
    }
  })
  
  output$practiceStart <- renderUI({
    validate(
      need(as.numeric(input$user.ID) >= 1000000 & 
             as.numeric(input$user.ID) <= 2999999, "Please enter a valid code.")
    )
    actionButton("pracStart", "Begin Practice Ranch Game")
  })
  
  # Generates output for simulation tab
  output$pageOut <- renderUI({
  if(rv$page <= simLength){  
    simPageOutput(rv, name = "")
  }else{  # Executed when simulation has been completed
    fluidRow(
      hide("infoPane"),
      column(width = 10,
      h4(paste0("Congratulations! You've completed ", simLength, " years of ranching.")),
      br(),
      p(paste0("You have accumulated $", 
               prettyNum(round(myOuts$assets.cash[simLength + 1], 0),
                         digits = 2, big.mark = ",", scientific = FALSE), " in cash." )),
      p(paste0("Your herd is now worth $", 
               prettyNum(round(myOuts$assets.cow[simLength + 1], 0),
                         digits = 2, big.mark = ",", scientific = FALSE), ".")),
      p(paste0("Based on your savings account and your herd worth, your total net worth is $", 
               prettyNum(round(myOuts$net.wrth[simLength + 1], 0),
                         digits = 2, big.mark = ",", scientific = FALSE), ".")),
              # With a conversation rate of $500,000 of simulation money to $1 of MTurk bonus money, you've earned $", 
              # round(myOuts$net.wrth[simLength + 1] * 1/simRuns$mturk.conv, 2),".")),
      p("You have now completed the ranching simulation. Save your inputs to receive the code you
        need to complete the rest of the survey."),
      actionButton("saveInputs", "Save results and receive completion code"),
      span((endTime <<- Sys.time()), style = "color:white"),
      span((simTime <<- endTime - startTime), style = "color:white"),
      uiOutput("complete"),
      offset = .5)
    )
  }
  })
  
  # Output once data has been saved, provides completetion code
  output$complete <- renderUI({
    req(values$saveComplete)
    h4(p("Your Data has been saved, your completion code is: ", span(sample(10000:99999,1), style="color:green")), 
       p("Please copy and paste your completion code into the designated box in the qualtrics survey.
         Once you have the code entered, you can close this window."))
  })
  
  # Output once practice data has been saved allows user to start ranch game
  output$practComplete <- renderUI({
    req(values$practSaveComplete == TRUE)
    hide("savepractInputs")
    h4("Practice rounds complete, continue on to begin ranching game")
    actionButton("simStart", "Begin Ranch Game")
  })
  
  # Observers to save data-----------------------------------------------------
  
  # Observer triggered when user saves current state in non-web mode only used
  #   in debug mode, saves results locally overwrites previously saved data
  observeEvent(input$saveState, {
      myDir <- "results"
      saveData <<- reactiveValuesToList(input)
      saveData <- inputToDF(saveData)

      # Remove first row of variable names
      write.csv(saveData, file = paste0("results/input", input$fileName, ".csv"), row.names = F)
      write.csv(myOuts, file = paste0("results/output", input$fileName, ".csv"), row.names = F)
  })

  # Observer to save real simulation inputs  
  observeEvent(input$saveInputs, {
    shinyjs::disable("saveInputs")
    files <- gs_ls()$sheet_title
    
    # Determines last file number in directory    
    if(length(files) == 0){
      lastFile <- 0
    }else{
      lastFile <- regmatches(files, gregexpr('[0-9]+',files))
      lapply(lastFile, as.numeric) %>% unlist() %>% max() -> lastFile
    }
    
    # Prepare inputs for saving
    saveData <<- reactiveValuesToList(input)
    saveData <- inputToDF(saveData)

    # Pivot save data to horizontal
    saveData <- t(saveData)
    
    # Saves data to MySQL
    withProgress(message = "Saving Data", value = 1/3, {
      con <- dbConnect(MySQL(),
                       user = 'cowgame',
                       password = 'cowsrock',
                       host = 'teamriskcowgame.cvkdgo9ryjxd.us-west-2.rds.amazonaws.com',
                       dbname = 'cowgame')
      incProgress(1/3)
      dbWriteTable(conn = con, name = 'cowGameOutputs', value = as.data.frame(myOuts), overwrite=FALSE, append = TRUE)

    })
    values$saveComplete <- TRUE
  })
  
  # Observer to save web inputs mid simulation, only for debug
  observeEvent(input$saveStateWeb, {
    files <- gs_ls()$sheet_title
    
    if(length(files) == 0){
      lastFile <- 0
    }else{
      lastFile <- regmatches(files, gregexpr('[0-9]+',files))
      lapply(lastFile, as.numeric) %>% unlist() %>% max() -> lastFile
    }
    saveData <<- reactiveValuesToList(input)
    saveData <- inputToDF(saveData)
    
    # Pivot save data to horizontal
    
    saveData <- t(saveData)
    # Remove first row of variable names
    print("hello")
    inputSheet <- gs_title("cowGameInputsTest")
    print("2")
    gs_add_row(inputSheet, ws="Inputs", input = saveData)
    print("3")
    outputSheet <- gs_title("cowGameOutputsTest")
    print("4")
    gs_add_row(outputSheet, ws="Outputs", input = myOuts)
  })
  
  
  observeEvent(input$savePracInputs, {
    
    # Currently this code isn't in use but I'm keeping it becuase it looks 
    #   like the saving of inputs is a bit foobarred
    
    # files <- gs_ls()$sheet_title
    # 
    # if(length(files) == 0){
    #   lastFile <- 0
    # }else{
    #   lastFile <- regmatches(files, gregexpr('[0-9]+',files))
    #   lapply(lastFile, as.numeric) %>% unlist() %>% max() -> lastFile
    # }
    saveData <<- reactiveValuesToList(input)
    # save(saveData, file = "newSave.RData")
    saveData <- inputToDF(saveData)
    #saveData$names <- NULL
    # Pivot save data to horizontal
    saveData <- t(saveData)
    # Remove first row of variable names
    withProgress(message = "Saving Data", value = 1/3, {
      con <- dbConnect(MySQL(),
                       user = 'cowgame',
                       password = 'cowsrock',
                       host = 'teamriskcowgame.cvkdgo9ryjxd.us-west-2.rds.amazonaws.com',
                       dbname = 'cowgame')

      outputTable <- myOuts[1:6]
      incProgress(1/3)
      dbWriteTable(conn = con, name = 'practiceGameOutputs', value = as.data.frame(outputTable), overwrite=FALSE, append = TRUE)
      
    })
    values$practSaveComplete <- TRUE
    
    shinyjs::disable("savePracInputs")
    # write.csv(saveData, file = paste0("results/input", lastFile + 1, ".csv"), row.names = F)
    # write.csv(myOuts, file = paste0("results/output", lastFile + 1, ".csv"), row.names = F)
  })
  
  # Code for debugging---------------------------------------------------------
  observeEvent(input$reset_button, {
    createOutputs(practiceRuns, simRuns, indem, indemprac)
    js$reset()
  })

  observeEvent(input$runCode, {
    eval(parse(text = input$code))
  })
  
  observeEvent(input$Exit, {
    js$closewindow();
    stopApp()
  })

}


