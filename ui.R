# ui.R
# MainPage - frailtypack

shinyUI(fluidPage(
  titlePanel(strong("Frailtypack")), 
  img(src="my_image.png", align = "center" , height = 50, width = 50),
  
  
  sidebarLayout(
  sidebarPanel(
      selectInput("modelType", 
            label = "Choose a model to fit",
            choices = c("Cox","Additive" , "Joint", "Joint General",
                        "Multivariate","Nested", "Shared")),
      
      fileInput("datafile", 
                'uload data File', 
                multiple=FALSE, 
                accept=c(".csv",".rda",".xls")),
      
      #numericInput("obs","Number of observations to view:",10),    # obsolete use of data output
      
      
      conditionalPanel(
        condition= "input.modelType =='Cox' ",
        label = h1("select arguments:"),
        selectInput("Coxselectedformula",label = "formula:",
                    choices = c("COX proportionnal hazard model with gap times",
                                "custom options")) 
      ),
      
      conditionalPanel(
        condition= "input.modelType =='Additive' ",
            label = h1("select arguments:"),
            selectInput("Additiveselectedformula",label = "formula:",
                        choices = c("ADDITIVE frailty model with no correlation between random effects",
                                    "ADDITIVE frailty model with a correlation between random effects",
                                    "custom options"))
        
        ),

      conditionalPanel(
        condition= "input.modelType =='Joint' ",
            label = h1("select arguments:"),
            selectInput("Jointselectedformula",label = "formula:",
                        choices = c("JOINT frailty model with gap times",
                                    "Stratified JOINT frailty model with gap times",
                                    "JOINT frailty model without alpha parameter",
                                    "JOINT frailty model for clustered data",
                                    "custom options"))
      ),
      
      conditionalPanel(
        condition= "input.modelType =='Joint General' ",
        selectInput("JointGselectedformula",label = "formula:",
                    choices = c("Joint General default options",
                                "custom options"))
      ),
      
      
      conditionalPanel(
        condition= "input.modelType =='Multivariate' ",
        label = h1("select arguments:"),
        selectInput("Multivariateselectedformula",label = "formula:",
                    choices = c("MULTIVARIATE frailty model with gap times",
                                "custom options"))
        
      ),
      
      conditionalPanel(
        condition= "input.modelType =='Nested' ",
            label = h1("select arguments:"),
            selectInput("Nestedselectedformula",label = "formula:",
                        choices = c("NESTED frailty model",
                                    "Stratified NESTED frailty model",
                                    "custom options"))
        
      ),
      
      conditionalPanel(
        condition= "input.modelType =='Shared' ",
            label = h1("select arguments:"),
            selectInput("Sharedselectedformula",label = "formula:",
                        choices = c("Shared frailty model with gap times",
                                    "Shared frailty model with log-normal distribution of frailties",
                                    "Stratified shared frailty model with gap times",
                                    "Shared frailty model with time-varying effect of covariates",
                                    "Shared frailty model with interval-censored data",
                                    "custom options"))
        ),
      
      
      hr(),
      conditionalPanel(
        condition=" ((input.modelType =='Joint' & input.Jointselectedformula =='custom options') |
                     (input.modelType =='Joint General' & input.JointGselectedformula =='custom options') |
                     (input.modelType =='Additive' & input.Additiveselectedformula =='custom options') |
                     (input.modelType =='Cox' & input.Coxselectedformula =='custom options') |
                     (input.modelType =='Multivariate' & input.Multivariateselectedformula =='custom options') |
                     (input.modelType =='Nested' & input.Nestedselectedformula =='custom options') |
                     (input.modelType =='Shared' & input.Sharedselectedformula =='custom options'))",
        tags$div(title="Surv(time, event) or Surv(time, time2, event) are acceptable.
        For mor information please relate to Surv() function in R.",
        selectizeInput('survArguments',label = 'Survival parameters:', choices = NULL, multiple = TRUE)),
        tags$div(title="Select the column containing the data IDs regrouped in clusters",
        selectizeInput('clusterColumn',label = 'cluster parameter:', choices = NULL, multiple = FALSE)),
        tags$div(title="select the reccurent explanatory variables",
        selectizeInput('usedArguments',label = 'reccurent explanatory variables:', choices = NULL, multiple = TRUE)),
        numericInput('knotsNb', label = "select knots:", value = 10),
        tags$div(title="kappa values must be comma-separated.\nBoth decimal and R-exponential forms are accepted
        (exemple: 21100000000 , 95300000000000 
                  or 2.11e+08 , 9.53e+11 )",
        textInput('kappaValues',label = 'kappa values:', value = "2.11e+08 , 9.53e+11")),
        selectInput('clusteredData',label = 'clustered data :', choices = c("yes","no"), selected ='no'),
        conditionalPanel(
          condition=" (input.modelType =='Joint' & input.Jointselectedformula =='custom options' & input.clusteredData == 'yes' )",
                  tags$div(title="Select the number of clusters or grupes in Your data
                             (unless they were detected automatically)",
                           numericInput('clustersNb', label = 'select number of present clusters:', value = 32)))
      ),
      
      
      hr(),
      conditionalPanel(
        condition=" ((input.modelType =='Joint' & input.Jointselectedformula =='custom options') |
                     (input.modelType =='Joint General' & input.JointGselectedformula =='custom options'))",
        tags$div(title="Select the terminal event variable",
        selectizeInput('terminalValue',label = 'Terminal event:', choices = NULL, multiple = FALSE))
      ),
      
      hr(),
      conditionalPanel(
        condition=" ((input.modelType =='Joint' & input.Jointselectedformula =='custom options') |
                     (input.modelType =='Joint General' & input.JointGselectedformula =='custom options'))",
        tags$div(title="Select the explanatory variables related to terminal event",
        selectizeInput('termEvArguments',label = 'formula.terminalEvent parameters:', choices = NULL, multiple = TRUE)),
        tags$div(title="Alpha value can only be selected to 'none' at this version of frailtypack package",
        selectInput('alphaValue',label = 'Alpha value:', choices = c("none"))),
        selectInput('recurrentAGValue',label = 'recurrentAG value:', choices = c("TRUE","FALSE"), selected = "FALSE")
      ),
      
      
      hr(),
      conditionalPanel(
        condition=" ((input.modelType =='Additive' & input.Additiveselectedformula =='custom options') |
        (input.modelType =='Cox' & input.Coxselectedformula =='custom options') |
        (input.modelType =='Nested' & input.Nestedselectedformula =='custom options') |
        (input.modelType =='Shared' & input.Sharedselectedformula =='custom options'))",
        selectizeInput('slopeValue',label = 'slope parameter:', choices = NULL, multiple = FALSE),
        selectInput('crossValidation',label = 'Cross Validation:', choices = c("TRUE", "FALSE"), selected="TRUE"),
        selectInput('correlation',label = 'correlation:', choices = c("TRUE", "FALSE"), selected="TRUE")
      ),
      
      conditionalPanel(
        condition=" !(is.null(dataFile))",
          #verbatimTextOutput('text1'),
          actionButton('initiateFit', label = "Execute")
      ),
      
      # the following HTML code displays message after "EXECUTE" message is pressed
      HTML('<script type="text/javascript">
        $(document).ready(function() {
          $("#initiateFit").click(function() {
            $("#fitResults").text("                  Loading...");
            $("#out2").text("                  Loading...");
            $("#plot2display").text("                  Loading...");
          });
        });
      </script>
      ')
     
  ),
    
      mainPanel(
        tabsetPanel(
          tabPanel("Uploaded data",
                   textOutput('fileStatusInfo'),
                   verbatimTextOutput('functionAsText'),
                   verbatimTextOutput('out2'),
                   dataTableOutput('contents')
          ),
          tabPanel("Results",
                   verbatimTextOutput('fitResults'),
                   downloadButton('downloadResultsData', 'Download results')
          ),
          tabPanel("Plots",
                   #helpText("Plots if needed"),
                   plotOutput("plot2display"),
                   downloadButton('downloadPlot', 'Download Plot')
          ),
        
        id = "conditionedPanels"  
      )
    )
  )
  )
)



