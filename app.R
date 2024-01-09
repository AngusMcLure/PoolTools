library(shiny)

ui <- fluidPage(

  ## Main navbar and pages
  navbarPage("PoolTools", id = "main_nav",


             tabPanel("Home",
                      actionButton("btnHelp", "Help (Documentation)"),
                      actionButton("btnAnalyse", "Analyse pooled data"),
                      actionButton("btnDesign", "Design a pooled survey")
             ),


             tabPanel("About",
                      h2("About PoolTools"),
                      h3("How to cite"),
                      h3("Relevant papers"),
                      h3("Contact"),
                      h3("Credits and acknowledgements"),

             ),

             tabPanel("Help",
                      h2("Documentation"),
                      h3("How to analyse pooled data"),
                      h3("How to design a pooled survey")
             ),


             tabPanel("Analyse",
                      h2("Analyse pooled data"),
                      actionButton("btnDocsAnalyse", "See instructions"),
                      hr(),

                      fileInput("fileAnalyse", "Upload CSV", accept = ".csv"),
                      uiOutput("colSelectTestResults"),
                      uiOutput("colSelectUnitNumber"),
                      uiOutput("optsSelectStructure")
             ),


             tabPanel("Design",
                      h2("Design a pooled survey"),
                      actionButton("btnDocsDesign", "See instructions"),
                      hr(),

                      ## First row of required options
                      fluidRow(
                        column(3,
                               selectInput("optsObjective",
                                           "Survey objective",
                                           choices = c("Estimate prevalence", "Detect pathogen"))
                        ),
                        column(3,
                               selectInput("optsMode",
                                           "Survey mode",
                                           choices = c("Calculate power", "Optimise cost"))
                        ),
                        column(3,
                               selectInput("optsTrapping",
                                           "Trapping time",
                                           choices = c("Fixed period", "Target sample size"))
                                           # options for optsTrapping
                        ),
                        column(3,
                               checkboxInput("optsClustered",
                                             "Clustered design",
                                             value = TRUE)
                        )
                      ),

                      ## Second row of required options
                      fluidRow(
                        column(3,
                               sliderInput("optsSensitivity",
                                           "Sensitivity",
                                           min = 0,
                                           max = 1,
                                           value = 1)
                        ),
                        column(3,
                               sliderInput("optsSpecificity",
                                           "Specificity",
                                           min = 0,
                                           max = 1,
                                           value = 1)
                        ),
                        column(3,
                               sliderInput("optsPrevalence",
                                           "Prevalence",
                                           min = 0,
                                           max = 1,
                                           value = 1)
                        ),
                        column(3,
                               conditionalPanel(condition = "input.optsClustered == true",
                                                sliderInput("optsCorrelation",
                                                            "Within-cluster correlation",
                                                            min = 0,
                                                            max = 1,
                                                            value = 1)
                               )
                        )
                      ),


                      ## For identifying cost-effective designs
                      conditionalPanel(condition = "input.optsMode == 'Optimise cost'",
                                       sidebarLayout(

                                         ## Cost-specific options
                                         sidebarPanel(
                                           textInput("optsCostUnit",
                                                     "Cost per unit",
                                                     value = 1),
                                           textInput("optsCostPool",
                                                     "Cost per pool",
                                                     value = 2),
                                           textInput("optsMaxPoolSize",
                                                     "Maximum pool size",
                                                     value = 10),

                                         ),
                                         mainPanel(
                                           h3("Identify cost-effective designs"),
                                           p("[display PoolPoweR::optimise_X() output]")
                                         )
                                       ))
             )
  )
)

server <- function(input, output, session) {

  ## Home page buttons
  observeEvent(input$btnHelp, {
    updateTabsetPanel(session, "main_nav", selected = "Documentation")
  })

  observeEvent(input$btnAnalyse, {
    updateTabsetPanel(session, "main_nav", selected = "Analyse")
  })

  observeEvent(input$btnDesign, {
    updateTabsetPanel(session, "main_nav", selected = "Design")
  })


  ## Analyse: Selecting columns for test result and unit number
  data <- reactive({
    req(input$fileAnalyse)
    read.csv(input$fileAnalyse$datapath, header = TRUE)
    # Any pre-processing or column checks
  })
  output$colSelectTestResults <- renderUI({
    req(data())
    selectInput("colTestResults",
                "Select test results column",
                choices = names(data()),
                selected = names(data())[1])
  })
  output$colSelectUnitNumber <- renderUI({
    req(data())
    selectInput("colUnitNumber",
                "Select number of units per pool column",
                choices = names(data()),
                selected = names(data())[2])
  })
  output$optsSelectStructure <- renderUI({
    req(data())
    selectInput("optsStructure",
                "Hierarchical sampling",
                choices = c("PoolPrev (No adjustment)", "HierPoolPrev", "PoolReg", "PoolRegBayes", "getPrevalence"),
                selected = "PoolPrev (No adjustment)")
  })

}

shinyApp(ui = ui, server = server)
