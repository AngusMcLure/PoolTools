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
                      fluidRow(
                        column(
                          width = 4,
                          style = "text-align: center;",
                          p("To estimate marker prevalence from", tags$br(),
                            "pooled test results, select:"),
                          actionButton("btnAnalysePage", "Analyse pooled data"),
                        ),
                        column(
                          width = 4,
                          style = "text-align: center;",
                          p("To design cost-effective tests, or evaluate", tags$br(),
                            "the power of an existing design, select:"),
                          actionButton("btnDesignPage", "Design a pooled survey")
                        )
                      )
                    ),

            tabPanel("About",
                     h2("About PoolTools"),
                     h3("How to cite"),
                     h3("Relevant papers"),
                     h3("Contact"),
                     h3("Credits and acknowledgements")
                     ),


             tabPanel("Analyse",
                      h2("Analyse pooled data"),
                      br(),

                      sidebarLayout(
                        sidebarPanel(
                          fileInput("fileAnalyse", "Upload CSV", accept = ".csv"),
                          uiOutput("colSelectTestResults"),
                          uiOutput("colSelectUnitNumber"),
                          uiOutput("colSelectStratify"),
                          uiOutput("checkHierarchy"),
                          uiOutput("colHierarchyOrder"),
                          uiOutput("optsSettings"),
                          uiOutput("btnAnalyse")
                      ),


                        mainPanel(
                          tabsetPanel(
                              type = "tabs",
                              tabPanel("Results", dataTableOutput("conditionalTable")),
                              tabPanel(
                                "Help",
                                h2("How to analyse pooled data"),
                                p("This mode estimates the prevalence of a
                                  marker in a population based on tests performed
                                  on pooled samples."),
                                p("The marker prevalence can be estimated across
                                  different categories, such as per-site or
                                  per-village, if provided."),
                                p("Lastly, a hierarchical model can be applied
                                  to avoid biased prevalence estimates."),
                                h3("Basic usage"),
                                tags$ul(
                                  tags$li("Input data requirements"),
                                  tags$li("Column selection"),
                                  tags$li("Estimate prevalence settings (PoolPrev)"),
                                  tags$li("Adjust for hierarchial sampling (HierPoolPrev)"),
                                  tags$li("Advanced settings")
                                )
                              )
                            )
                        )
                      )
             ),

             tabPanel("Design",
                      h2("Design a pooled survey"),
                      br(),
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

                          selectInput(
                            "optsTrapping",
                            tags$span(
                              "Trapping time",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            choices = c("Select" = "",  "Fixed period", "Target sample size")
                          ),

                          tagList(
                            checkboxInput(
                              "optsClustered",
                              tags$span(
                                tags$b("Clustered design?"),
                                tipify(icon("info-circle"), "Placeholder", placement = "right")
                                ), value = TRUE
                            ),
                            tags$hr(style = "border-top: 1px solid #CCC;")
                          ),

                          # 2. Parameter options ------------------------------
                          selectInput(
                            "optsSensitivity",
                            tags$span(
                              "Sensitivity",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            choices = c("Low (80%)" = 0.8,
                                        "Med. (90%)" = 0.9,
                                        "High (100%)" = 1,
                                        "Other" = "other"), # 0.5-1
                            selected = 1
                          ),
                          conditionalPanel(
                            condition = "input.optsSensitivity == 'other'",
                            textInput("optsSensitivityOther", NULL, placeholder = "Specify %")
                          ),

                          selectInput(
                            "optsSpecificity",
                            tags$span(
                              "Specificity",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            choices = c("Low (80%)" = 0.8,
                                        "Med. (90%)" = 0.9,
                                        "High (100%)" = 1,
                                        "Other" = "other"), # 0.5-1
                            selected = "1"
                          ),
                          conditionalPanel(
                            condition = "input.optsSpecificity == 'other'",
                            textInput("optsSpecificityOther", NULL, placeholder = "Specify %")
                          ),

                          selectInput(
                            "optsPrevalence",
                            tags$span(
                              "Prevalence",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            choices = c("Low (0.1%)" = 0.001,
                                        "Med. (0.5%)" = 0.005,
                                        "High (2%)" = 0.02,
                                        "Other" = "other"),
                            selected = 0.005
                            ),
                          conditionalPanel(
                            condition = "input.optsPrevalence == 'other'",
                            textInput("optsPrevalenceOther", NULL, placeholder = "Specify %"),
                          ),

                          conditionalPanel(
                            condition = "input.optsClustered == true",
                            selectInput(
                              "optsCorrelation",
                              tags$span(
                                "Within-cluster correlation",
                                tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                              # TODO: Update with acceptable values
                              choices = c("Low" = 0.001,
                                          "Med" = 0.005,
                                          "High" = 0.02,
                                          "Other" = "other"),
                              selected = 0.005
                              ),
                            conditionalPanel(
                              condition = "input.optsCorrelation == 'other'",
                              textInput("optsCorrelationOther", NULL, placeholder = "Specify %")
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

                          actionButton("btnDesign", "Run!")

                        ), # End of sidebarPanel ------------------------------

                        mainPanel(
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Results", NULL),
                            tabPanel("Help", NULL)
                          )
                        )
                      ) # End of sidebarLayout -------------------------------
             )
  )
)

server <- function(input, output, session) {

  ##
  ## Home page buttons
  ##
  observeEvent(input$btnAnalysePage, {
    updateTabsetPanel(session, "main_nav", selected = "Analyse")
  })

  observeEvent(input$btnDesignPage, {
    updateTabsetPanel(session, "main_nav", selected = "Design")
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

  ##
  ## Design
  ##
  observeEvent(input$btnDesign, {
    print("buttonhit")
    print(input$optsMode)
  })


}


shinyApp(ui = ui, server = server)
