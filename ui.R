# TODOs
  # Add dollar signs to all of the dollar inputs
  # Change some numeric inputs to text inputs and create a reactive
  #   statement in the server to change them to numerics (http://stackoverflow.com/questions/24960407/r-shiny-numeric-input-without-selectors)

 
## HTML and JS to dynamically create tabs and move between tabs
tagList(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "init"),
  extendShinyjs(text = "shinyjs.closewindow = function() { window.close(); }"),
  extendShinyjs(text = jsResetCode),
  tags$style(css),
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
    color: red;
    }
    ")),
    tags$script(paste0('Shiny.addCustomMessageHandler("myCallbackHandler",
                  function(typeMessage) {console.log(typeMessage)
                  if(typeMessage == 6){
                  console.log("got here");
                  $("a:contains(Background)").click();
                  }
                  if(typeMessage == 7){
                  console.log("got here");
                  $("a:contains(Quiz)").click();
                  }
                  if(typeMessage == 1){
                  console.log("got here");
                  $("a:contains(Year-1)").click();
                  }', yearHandler, '
                  });')
    ),
    tags$script(
      '
      Shiny.addCustomMessageHandler("scrollCallbackIns",
      function(msg) {
      console.log(msg)
      window.scrollTo(0, document.getElementById(msg).getBoundingClientRect().bottom + 680);
      }
      );'
  ),
  tags$script(
    '
    Shiny.addCustomMessageHandler("scrollCallbackCow",
    function(msg) {
    console.log(msg)
    window.scrollTo(0, document.getElementById(msg).getBoundingClientRect().bottom + 915);
    }
    );'
  ),
  tags$script(
    '
    Shiny.addCustomMessageHandler("scrollCallbackTop",
    function(msg) {
    console.log(msg)
    window.scrollTo(0, document.body.top);
    }
    );'
  ),
  tags$script(
    '
    Shiny.addCustomMessageHandler("scrollCallbackBottom",
    function(msg) {
    console.log(msg)
    window.scrollTo(0,document.body.scrollHeight);
    }
    );'
  ),
  tags$script(
    '
      Shiny.addCustomMessageHandler("scrollCallbackRain",
      function(msg) {
      console.log(msg)
      window.scrollTo(0, document.getElementById(msg).getBoundingClientRect().top + 200);
      }
      );'
  )),



fluidPage("Ranch Drought", id = "navBar",


tabsetPanel(id = "mainPanels",
            
  ## Instruction panel
  
  
  # tabPanel("debug",
  #          fluidRow(
  #            textInput("code", "Enter Code to be Run"),
  #            actionButton("runCode", "Run Code"),
  #            textInput("insChange", "Enter True or False to use insurance or not"),
  #            actionButton("applyInsChange", "Change Insurance"),
  #            actionButton("reset_button", "Reset Page"),
  #            textInput("fileName", "Enter File Name"),
  #            actionButton("saveState", "Save Current Inputs and Outputs"),
  #            br(),
  #            textInput("name", "Enter your name or alias"),
  #            actionButton("saveStateWeb", "Save Web Inputs and Outputs"),
  #            actionButton("saveStateWeb", "Save Web Inputs and Outputs")
  #            # actionButton("saveInputs", "Save all Input")
  #          )),
  
 ## Instruction panel
 tabPanel("Welcome",
          fluidRow(includeCSS("styles.css"),
           # textInput("code", "Enter Code to be Run"),
           # actionButton("runCode", "Run Code"),
           # textInput("insChange", "Enter True or False to use insurance or not"),
           # actionButton("applyInsChange", "Change Insurance"),
           # actionButton("reset_button", "Reset Page"),
           br(),
            textInput("user.ID", "Enter your experiment code."),
           # Ensures user has entered a valid mturk id
           
           uiOutput("practiceStart")
          )),
 
 tabPanel("Practice Simulation",
          fluidPage(includeCSS("styles.css"),
            br(),
            br(),
            span((startTime <<- Sys.time()), style="color:white"),
            uiOutput("practiceOut"),
                    uiOutput(paste0("infoPane", "Prac"))
          )
 ),
 
 tabPanel("Ranch Simulation", 
          fluidPage(includeCSS("styles.css"),
            uiOutput("pageOut"),
                    uiOutput("infoPaneMain")
          )
 )
)
)
)