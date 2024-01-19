library(shiny)
library(shinyBS)
library(sortable)
library(DT)
library(dplyr)

library(PoolTestR)
#library(PoolPoweR)

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
                                 h3("Credits and acknowledgements")
                                 ),


                        tabPanel("Documentation",
                                 h2("Documentation"),
                                 h3("How to analyse pooled data"),
                                 h3("How to design a pooled survey")
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
                          uiOutput("optsSettings"),
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
                      sidebarLayout(
                        sidebarPanel(

                          # 1. Survey options ---------------------------------
                          selectInput(
                            "optsObjective",
                            tags$span(
                              "Survey objective",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                            choices = c("Select" = "", "Estimate prevalence", "Detect pathogen")
                          ),

                          selectInput(
                            "optsMode",
                            tags$span(
                              "Survey mode",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                            choices = c("Select" = "", "Calculate power", "Optimise cost")
                          ),

                          tagList(
                            selectInput(
                              "optsTrapping",
                              tags$span(
                                "Trapping time",
                                tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                              choices = c("Select" = "",  "Fixed period", "Target sample size")
                            ),
                            tags$hr(style = "border-top: 1px solid #CCC;")
                          ),

                          # 2. Parameter options ------------------------------
                          sliderInput(
                            "optsSensitivity",
                            tags$span(
                              "Sensitivity",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            min = 0.5, max = 1, value = 1
                          ),

                          sliderInput(
                            "optsSpecificity",
                            tags$span(
                              "Specificity",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            min = 0.5, max = 1, value = 1
                          ),

                          tagList(
                            sliderInput(
                              "optsPrevalence",
                              tags$span(
                                "Prevalence",
                                tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                              min = 0, max = 1, value = 1
                            ),
                            tags$hr(style = "border-top: 1px solid #CCC;")
                          ),

                          # 3. Cluster options --------------------------------
                          checkboxInput(
                            "optsClustered",
                            tags$span(
                              "Clustered design",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ), value = TRUE
                          ),

                          conditionalPanel(
                            condition = "input.optsClustered == true",
                            sliderInput(
                              "optsCorrelation",
                              tags$span(
                                "Within-cluster correlation",
                                tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                              min = 0, max = 1, value = 1
                            )
                          ),

                          # 4a. For identifying cost-effective designs --------
                          conditionalPanel(
                            condition = "input.optsMode == 'Optimise cost'",
                            tagList(
                              tags$hr(style = "border-top: 1px solid #CCC;"),
                              textInput("optsCostUnit", "Cost per unit"),
                              textInput("optsCostPool", "Cost per pool"),
                              textInput("optsMaxPoolSize", "Maximum pool size"),

                              # 4a. If clustered
                              conditionalPanel(
                                condition = "input.optsClustered == true",
                                textInput("optsCostCluster", "Cost per cluster")
                              )
                            )
                          ),


                          # 4b. For evaluating existing designs ---------------
                          conditionalPanel(
                            condition = "input.optsMode == 'Calculate power'",

                            # 4bx. Fixed period -------------------------------
                            conditionalPanel(
                              condition = "input.optsTrapping == 'Fixed period'",
                              tagList(
                                tags$hr(style = "border-top: 1px solid #CCC;"),
                                textInput("optsUnitsMean", "Mean units per cluster"),
                                textInput("optsUnitsVar", "Variance of units")
                              )
                            ),

                            # 4by. Target sample size -------------------------
                            conditionalPanel(
                              condition = "input.optsTrapping == 'Target sample size'",
                              tagList(
                                tags$hr(style = "border-top: 1px solid #CCC;"),
                                textInput("optsPoolSize", "Number of units per pool")
                              ),

                              # 4by. Cluster options
                              conditionalPanel(
                                condition = "input.optsClustered == false",
                                textInput("optsPoolNum", "Number of pools")
                              ),

                              conditionalPanel(
                                condition = "input.optsClustered == true",
                                textInput("optsPoolNumClust", "Number of pools per cluster")
                              )
                            ) # End of 4by. Target sample size ----------------
                          ), # End of 4b. Evaluating existing designs ---------

                        ), # End of sidebarPanel ------------------------------
                        mainPanel()
                        )
             ),

             tabPanel("Old design",
                      h2("Design a pooled survey"),
                      actionButton("btnDocsDesign", "See instructions"),
                      hr(),


                      ##
                      ## Required options
                      ##

                      fluidRow(
                        column(3,
                               selectInput("optsObjective",
                                           tags$span(
                                             "Survey objective",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           choices = c("Select" = "", "Estimate prevalence", "Detect pathogen"))
                        ),
                        column(3,
                               selectInput("optsMode",
                                           tags$span(
                                             "Survey mode",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           choices = c("Select" = "", "Calculate power", "Optimise cost"))
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
                                           min = 0.5,
                                           max = 1,
                                           value = 1)
                        ),
                        column(3,
                               sliderInput("optsSpecificity",
                                           tags$span(
                                             "Specificity",
                                             tipify(icon("info-circle"), "Placeholder", placement = "right")
                                             ),
                                           min = 0.5,
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
                                       )
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
                      tipify(icon("info-circle"), "Placeholder", placement = "right")
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
          text = "Columns to exclude (e.g. Time)",
          input_id = "_optsHierarchyExclude",
          labels = metadata_cols()
        ),
        add_rank_list(
          text = "Drag items here and reorder by hierarchy (largest to smallest)",
          input_id = "optsHierarchyOrder"
          )
      )
    }
  })
  output$optsSettings <- renderUI({
    req(data())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$details(
        tags$br(),
        tags$summary("Advanced settings"),

        textInput("optsRound", "Number of decimal places", value = 4),

        checkboxInput("optsBayesian", "Bayesian calculations (slow)")

      ),
      tags$br()
    )
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

    # Debugging
    print(input$optsHierarchy)
    print(input$optsStratify)
    print(input$optsRound)

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
      # Account for hierarchical sampling structure
      hier_args <- req_args
      hier_args$hierarchy <- input$optsHierarchyOrder
      # Parse arguments for stratification
      if (!is.null(input$optsStratify)) {
        hier_args <- c(hier_args, lapply(input$optsStratify, as.name))
      }
      print(hier_args)
      result(do.call(HierPoolPrev, hier_args))
    }
    else result(NULL)
  })

  output$conditionalTable <- renderDataTable({
      req(result())
      result() %>% mutate(across(is.double, round, digits = as.integer(input$optsRound)))
    })

}

shinyApp(ui = ui, server = server)
