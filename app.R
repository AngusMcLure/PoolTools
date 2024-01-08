library(shiny)

ui <- fluidPage(

  ## Main navbar and pages
  navbarPage("PoolTools", id = "main_nav",


             tabPanel("Home",
                      wellPanel(
                        h4("Select:"),
                        actionButton("btnHelp", "Help (Documentation)"),
                        actionButton("btnAnalyse", "Analyse pooled data"),
                        actionButton("btnDesign", "Design a pooled survey")
                      )
             ),


             tabPanel("Documentation",
                      h2("Documentation"),
                      h3("About PoolTools"),
                      h3("How to cite"),
                      h3("Relevant papers"),
                      h3("Contact"),
                      h3("Credits and acknowledgements"),

             ),


             tabPanel("Analyse",
                      h2("Analyse pooled data"),
                      fileInput("fileAnalyse", "Upload CSV", accept = ".csv"),
                      uiOutput("colSelectTestResults"),
                      uiOutput("colSelectUnitNumber"),
                      uiOutput("optsSelectStructure")
             ),


             tabPanel("Design",
                      h2("Design a pooled survey"),

                      ## Select design objective and mode
                      selectInput("optsObjective",
                                  "Select objective",
                                  choices = c("Estimate prevalence", "Detect pathogen")),
                      selectInput("optsMode",
                                  "Select mode",
                                  choices = c("Calculate power", "Optimise cost")),


                      ## Options shared across all modes
                      sliderInput("optsSensitivity",
                                  "Sensitivity",
                                  min = 0,
                                  max = 1,
                                  value = 1),
                      sliderInput("optsSpecificity",
                                  "Specificity",
                                  min = 0,
                                  max = 1,
                                  value = 1),
                      sliderInput("optsPrevalence",
                                  "Prevalence",
                                  min = 0,
                                  max = 1,
                                  value = 1),
                      checkboxInput("optsClustered",
                                    "Clustered design",
                                    value = TRUE),
                      conditionalPanel(condition = "input.optsClustered == true",
                                       sliderInput("optsCorrelation",
                                                   "Within-cluster correlation",
                                                   min = 0,
                                                   max = 1,
                                                   value = 1)),


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
                                           selectInput("optsTrapping",
                                                       "Trapping time",
                                                       choices = c("Fixed period", "Target sample size")),
                                           #TODO: options for optsTrapping

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
