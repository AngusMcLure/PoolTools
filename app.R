library(shiny)
library(shinyBS)
library(sortable)
library(DT)

ui <- fluidPage(

  ## Main navbar and pages
  navbarPage("PoolTools", id = "main_nav",


             tabPanel("Home",
                      actionButton("btnHelp", "Documentation"),
                      actionButton("btnAnalyse", "Analyse pooled data"),
                      actionButton("btnDesign", "Design a pooled survey")
             ),


             navbarMenu("Help",
                        tabPanel("About",
                                 h2("About PoolTools"),
                                 h3("How to cite"),
                                 h3("Relevant papers"),
                                 h3("Contact"),
                                 h3("Credits and acknowledgements"),
                                 ),


                        tabPanel("Documentation",
                                 h2("Documentation"),
                                 h3("How to analyse pooled data"),
                                 h3("How to design a pooled survey"),
                                 )
             ),


             tabPanel("Analyse",
                      h2("Analyse pooled data"),
                      actionButton("btnDocsAnalyse", "See instructions"),
                      hr(),

                      fileInput("fileAnalyse", "Upload CSV", accept = ".csv"),
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("colSelectTestResults"),
                          uiOutput("colSelectUnitNumber"),
                          uiOutput("optsSelectStructure"),
                          uiOutput("colSelectStratify"),
                          uiOutput("colReorderStratify")
                      ),
                        mainPanel()
                      )


                      # Analyse tooltips

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


                      ##
                      ## For identifying cost-effective designs
                      ##
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
                                           conditionalPanel(condition = "input.optsClustered == true",
                                                            textInput("optsCostCluster",
                                                                      "Cost per cluster",
                                                                      value = 5)
                                                            )

                                         ),

                                         mainPanel(
                                           h3("Identify cost-effective designs"),
                                           p("[display PoolPoweR::optimise_X() output]")
                                         )
                                         )
                                       ),


                      ##
                      ## Evaluating existing designs
                      ##
                      conditionalPanel(condition = "input.optsMode == 'Calculate power'",

                                       sidebarLayout(

                                         sidebarPanel(
                                           conditionalPanel("input.optsTrapping == 'Fixed period'",
                                                            textInput("optsUnitsMean",
                                                                      "Mean units per cluster"
                                                                      ),
                                                            textInput("optsUnitsVar",
                                                                      "Variance of units"
                                                                      )
                                                            ),
                                           conditionalPanel("input.optsTrapping == 'Target sample size'",
                                                            textInput("optsPoolSize",
                                                                      "Number of units per pool"
                                                                    ),
                                                            conditionalPanel("input.optsClustered == false",
                                                                             textInput("optsPoolNum",
                                                                                       "Number of pools"
                                                                                       )
                                                                             ),
                                                            conditionalPanel("input.optsClustered == true",
                                                                             textInput("optsPoolNumClust",
                                                                                       "Number of pools per cluster"
                                                                                       )
                                                                             )
                                                            )
                                         ),


                                         mainPanel()
                                       ),
                      )



             )
  )
)

server <- function(input, output, session) {

  ##
  ## Home page buttons
  ##
  observeEvent(input$btnHelp, {
    updateTabsetPanel(session, "main_nav", selected = "Documentation")
  })

  observeEvent(input$btnAnalyse, {
    updateTabsetPanel(session, "main_nav", selected = "Analyse")
  })

  observeEvent(input$btnDesign, {
    updateTabsetPanel(session, "main_nav", selected = "Design")
  })

  ##
  ## Analyse and Design buttons to docs
  ##
  observeEvent(input$btnHelp, {
    updateTabsetPanel(session, "main_nav", selected = "Documentation")
  })

  observeEvent(input$btnAnalyse, {
    updateTabsetPanel(session, "main_nav", selected = "Analyse")
  })
  ##
  ## Analyse: Selecting columns for test result and unit number
  ##
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
    # Although the options don't change, reveal only when file is uploaded
    req(data())
    selectInput("optsStructure",
                "Hierarchical sampling",
                choices = c("PoolPrev (No adjustment)", "HierPoolPrev", "PoolReg", "PoolRegBayes", "getPrevalence"),
                selected = "PoolPrev (No adjustment)")
  })
  output$colSelectStratify <- renderUI({
    req(data())
    # Exclude columns that were selected for test results and unit number
    metadata_cols <- names(data())
    metadata_cols <- metadata_cols[!metadata_cols %in% c(input$colTestResults, input$colUnitNumber)]
    checkboxGroupInput("optsSelectStratify",
                       "Select columns to stratify",
                       choices = metadata_cols)
  })
  output$colReorderStratify <- renderUI({
    req(data())
    # Could replace with bucket_list to avoid colSelectStratify
    rank_list(
      text = "Drag the items to reflect hierarchy order (big to small)",
      input_id = "optsStratify",
      labels = input$optsSelectStratify
    )
  })

}

shinyApp(ui = ui, server = server)
