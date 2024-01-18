library(shiny)
library(shinyBS)
library(sortable)
library(DT)

library(PoolTestR)

ui <- fluidPage(

  ## Main navbar and pages
  navbarPage("PoolTools", id = "main_nav",


             tabPanel("Home",
                      p("text to describe each button with some examples"),
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
                          uiOutput("colSelectStratify"),
                          uiOutput("checkHierarchy"),
                          uiOutput("colHierarchyOrder"),
                          uiOutput("btnAnalyse")
                      ),
                        mainPanel(
                          dataTableOutput("conditionalTable")
                        )
                      )
             ),


             tabPanel("Design",
                      h2("Design a pooled survey"),
                      actionButton("btnDocsDesign", "See instructions"),
                      hr(),


                      ##
                      ## Required options
                      ##

                      ## First row of required options
                      fluidRow(
                        column(3,
                               selectInput("optsObjective",
                                           tags$span(
                                             "Survey objective",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           choices = c("Estimate prevalence", "Detect pathogen"))
                        ),
                        column(3,
                               selectInput("optsMode",
                                           tags$span(
                                             "Survey mode",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           choices = c("Calculate power", "Optimise cost"))
                        ),
                        column(3,
                               selectInput("optsTrapping",
                                           tags$span(
                                             "Trapping time",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           choices = c("Fixed period", "Target sample size"))
                        ),
                        column(3,
                               checkboxInput("optsClustered",
                                           tags$span(
                                             "Clustered design",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                             value = TRUE)
                        )
                      ),

                      ## Second row of required options
                      fluidRow(
                        column(3,
                               sliderInput("optsSensitivity",
                                           tags$span(
                                             "Sensitivity",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           min = 0,
                                           max = 1,
                                           value = 1)
                        ),
                        column(3,
                               sliderInput("optsSpecificity",
                                           tags$span(
                                             "Specificity",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           min = 0,
                                           max = 1,
                                           value = 1)
                        ),
                        column(3,
                               sliderInput("optsPrevalence",
                                           tags$span(
                                             "Prevalence",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           min = 0,
                                           max = 1,
                                           value = 1)
                        ),
                        column(3,
                               conditionalPanel(condition = "input.optsClustered == true",
                                                sliderInput("optsCorrelation",
                                                            tags$span(
                                                              "Within-cluster correlation",
                                                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                                                            ),
                                                            min = 0,
                                                            max = 1,
                                                            value = 1)
                               )
                        )
                      ),


                      ## Required options tooltips
                      bsTooltip("optsObjective", "placeholder", "right"),


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

  metadata_cols <- reactive({
    # All column names that are not the results or unit number per pool
    req(data())
    cols <- names(data())
    cols <- cols[!cols %in% c(input$colTestResults, input$colUnitNumber)]
  })


  ## Options
  output$colSelectTestResults <- renderUI({
    req(data())
    selectInput("colTestResults",
                tags$span(
                  "Test results",
                  tipify(icon("info-circle"), "Placeholder", placement = "right")
                  ),
                choices = c("Select column" = "", names(data()))
                )
  })
  output$colSelectUnitNumber <- renderUI({
    req(data())
    cols <- names(data())
    cols <- cols[!cols %in% input$colTestResults]
    selectInput("colUnitNumber",
                tags$span(
                  "Number of specimens per pool",
                  tipify(icon("info-circle"), "Placeholder", placement = "right")
                  ),
                choices = c("Select column" = "", cols)
                )
  })
  output$colSelectStratify <- renderUI({
    req(data())
    # Exclude columns that were selected for test results and unit number
    checkboxGroupInput("optsStratify",
                       tags$span(
                       "Estimate prevalence for:",
                       tipify(icon("info-circle"), "Leave empty to estimate prevalence on the whole data set", placement = "right")
                       ),
                       choices = metadata_cols())
  })
  output$checkHierarchy <- renderUI({
    # Although the options don't change, reveal only when file is uploaded
    req(data())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      checkboxInput("optsHierarchy",
                    tags$span(
                      "Adjust for hierarchical sampling?",
                      tipify(icon("info-circle"), "Placeholder", placement = "right"),
                      )
                    )
    )
  })
  output$colHierarchyOrder <- renderUI({
    req(data())
    if (input$optsHierarchy) {
      bucket_list(
        header = "Hierarchy order",
        add_rank_list(
          text = "Drag to reorder by hierarchy",
          input_id = "optsHierarchyOrder",
          labels = metadata_cols()
          ),
        add_rank_list(
          text = "Drag here to exclude columns (e.g. Time)",
          input_id = "_optsHierarchyExclude",
        )
      )
    }
  })
  output$btnAnalyse <- renderUI({
    req(data())
    actionButton("optsAnalyse", "Run!")
  })

  ## Table output
  result <- reactiveVal()

  observeEvent(input$optsAnalyse, {
    req(data(), input$colTestResults, input$colUnitNumber)
    req_args <- list(
      data = data(),
      result = input$colTestResults,
      poolSize = input$colUnitNumber,
      bayesian = F
    )

    print(input$optsHierarchy)
    print(input$optsStratify)

    if (!input$optsHierarchy) {
      if (is.null(input$optsStratify)) {
      # Estimate prevalence on whole data
      result(do.call(PoolPrev, req_args))
      } else {
        # Estimate prevalence for each selected column
        # Parse arguments
        col_args <- c(req_args, lapply(input$optsStratify, as.name))
        result(do.call(PoolPrev, col_args))
      }
    } else if (input$optsHierarchy) {
      hier_args <- req_args
      hier_args$hierarchy <- input$optsHierarchyOrder
      print(hier_args)
      result(do.call(HierPoolPrev, hier_args))
    }
    else result(NULL)
  })

  output$conditionalTable <- renderDataTable({
      req(result())
      result()
    })

}

shinyApp(ui = ui, server = server)
