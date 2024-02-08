library(shiny)
library(shinyBS)
library(sortable)
library(DT)
library(dplyr)

library(PoolTestR)
library(devtools)
devtools::load_all('~/GitHub/PoolPoweR/')

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
                          fileInput(
                            "fileAnalyse",
                            accept = ".csv",
                            tags$span(
                              "Upload data",
                              tipify(icon("info-circle"), "Must be a .csv file", placement = "right")
                              )
                          ),
                          uiOutput("colSelectTestResults"),
                          uiOutput("colSelectUnitNumber"),
                          uiOutput("checkStratify"),
                          uiOutput("colSelectStratify"),
                          uiOutput("checkHierarchy"),
                          uiOutput("colHierarchyOrder"),
                          uiOutput("optsSettings"),
                          uiOutput("btnAnalyse")
                      ),


                        mainPanel(
                          tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                "Results",
                                dataTableOutput("outAnalyse"),
                                uiOutput("btnDlAnalyse")),
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

                          # Survey options ------------------------------------
                          selectInput(
                            "optsObjective",
                            tags$span(
                              "Survey objective",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                            choices = c("Select" = "", "Estimate prevalence", "Detect pathogen (Coming soon...)")
                          ),

                          selectInput(
                            "optsMode",
                            tags$span(
                              "Survey mode",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                              ),
                            choices = c(
                              "Select" = "",
                              "Identify cost-effective designs",
                              "Calculate power of existing designs (Coming soon...)"
                            )
                          ),

                          selectInput(
                            "optsTrapping",
                            tags$span(
                              "Sampling strategy",
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            choices = c("Select" = "",  "Fixed sample size", "Fixed sampling period")
                          ),

                          checkboxInput(
                            "optsClustered",
                            tags$span(
                              tags$b("Clustered design?"),
                              tipify(icon("info-circle"), "Placeholder", placement = "right")
                            ),
                            value = TRUE
                          ),

                          # Main settings -------------------------------------
                          # UI are conditional based on survey options
                          uiOutput("uiRandPrev"),
                          uiOutput("uiCost"),
                          uiOutput("uiParams"),
                          uiOutput("uiAdvanced"),

                          actionButton("btnDesign", "Run!")

                        ), # End of sidebarPanel ------------------------------

                        mainPanel(
                          tabsetPanel(
                            type = "tabs",
                            tabPanel(
                              "Results",
                              tags$br(),
                              uiOutput("outDesign")
                            ),
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

  ## ANALYSE ------------------------------------------------------------------
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
  output$checkStratify <- renderUI({
    req(data(), input$colTestResults, input$colUnitNumber)
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      checkboxInput(
        "optsStratify",
        tags$span(
          tags$b("Stratify data?"),
          tipify(icon("info-circle"), "Uncheck to estimate prevalence on the whole data set", placement = "right")
        )
      )
    )
  })

  output$colSelectStratify <- renderUI({
    req(data(), input$colTestResults, input$colUnitNumber)
    if (!is.null(input$optsStratify) && input$optsStratify) {
      checkboxGroupInput(
        "optsColStratify",
        tags$span("Stratify data by:", style = "font-weight: plain;"), # plan text instead of bold
        choices = metadata_cols()
      )
    } else {
      return(NULL)
    }
  })

  output$checkHierarchy <- renderUI({
    # Although the options don't change, reveal only when file is uploaded
    req(data(), input$colTestResults, input$colUnitNumber)
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      checkboxInput("optsHierarchy",
                    tags$span(
                      tags$b("Adjust for hierarchical sampling?"),
                      tipify(icon("info-circle"), "Placeholder", placement = "right"),

                      )
                    )
    )
  })
  output$colHierarchyOrder <- renderUI({
    req(data(), input$colTestResults, input$colUnitNumber)
    if (!is.null(input$optsStratify) && input$optsHierarchy) {
      bucket_list(
        header = "Drag to include hierarchical variables and reorder from largest to smallest",
        orientation = "horizontal", # doesn't work in sidebar?
        add_rank_list(
          text = "Other variables",
          input_id = "_optsHierarchyExclude",
          labels = metadata_cols()
        ),
        add_rank_list(
          text = "Hierarchical variables",
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

        textInput("optsRoundAnalyse", "Number of decimal places to display", value = 4),

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
      poolSize = input$colUnitNumber
    )

    # Debugging
    print(input$optsBayesian)

    if (!input$optsHierarchy) {
      # Add bayesian switch for PoolPrev
      poolprev_args <- req_args
      poolprev_args$bayesian <- input$optsBayesian
      if (is.null(input$optsColStratify)) {
      # Estimate prevalence on whole data
      result(do.call(PoolPrev, poolprev_args))
      } else {
        # Estimate prevalence for each selected column (stratified)
        # Parse arguments
        col_args <- c(poolprev_args, lapply(input$optsColStratify, as.name))
        result(do.call(PoolPrev, col_args))
      }
    } else if (input$optsHierarchy) {
      # Account for hierarchical sampling structure
      hier_args <- req_args
      hier_args$hierarchy <- input$optsHierarchyOrder
      # Parse arguments for stratification
      if (!is.null(input$optsColStratify)) {
        hier_args <- c(hier_args, lapply(input$optsColStratify, as.name))
      }
      print(hier_args)
      result(do.call(HierPoolPrev, hier_args))
    }
    else result(NULL)
  })

  output$outAnalyse <- renderDataTable({
      req(result())
      result() %>% mutate(across(is.double, round, digits = as.integer(input$optsRoundAnalyse)))
    })

  output$btnDlAnalyse <- renderUI({
    req(result())
    downloadButton("dlAnalyse", "Download results")
    })

  output$dlAnalyse <- downloadHandler(
      filename <- function() {
        paste("results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(result(), file)
      }
  )

  ## DESIGN -------------------------------------------------------------------

  valid_survey <- reactive({
    !is.null(input$optsObjective) && input$optsObjective != "" &&
    !is.null(input$optsMode) && input$optsMode != "" &&
    !is.null(input$optsTrapping) && input$optsTrapping != ""
  })

  valid_cost <- reactive({
    !is.null(input$optsCostUnit) && input$optsCostUnit != "" &&
    !is.null(input$optsCostPool) && input$optsCostPool != ""
    # Add cluster and cost period here?
  })

  valid_randPrev <- reactive({
    !is.null(input$optsPoolStrat) && input$optsPoolStrat != "" &&
    !is.null(input$optsCatchMean) && input$optsCatchMean != "" &&
    !is.null(input$optsCatchVar) && input$optsCatchVar != ""
  })

  analysis_type <- reactive({
    req(valid_survey())
    if (input$optsObjective == "Estimate prevalence" &
        input$optsMode == "Identify cost-effective designs") {
      if (input$optsTrapping == "Fixed sample size")
        return("optimise_sN_prevalence")
      else if (input$optsTrapping == "Fixed sampling period")
        return("optimise_random_prevalence")
    }
  })


  ## RandPrev UI ----
  output$uiRandPrev <- renderUI({
    req(analysis_type() == "optimise_random_prevalence")
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      selectInput(
        "optsPoolStrat",
        tags$span(
          "Pooling strategy",
          tipify(icon("info-circle"), "Placeholder", placement = "right")
        ),
        choices = c(
          "Select" = "",
          "Max size" ="pool_max_size",
          "Target number" = "pool_target_number"
        )
      ),
      textInput("optsCatchMean", "Catch mean"),
      textInput("optsCatchVar", "Catch variance")
    )
  })


  ## Cost UI ----
  output$uiCost <- renderUI({
    req(valid_survey(), analysis_type())
    # Because rand prev has some additional options first
    print(analysis_type())
    print(valid_randPrev())
    if (analysis_type() == "optimise_random_prevalence") req(valid_randPrev())
    # Shared across all analysis types
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$b("Costs"),
      tags$br(),
      tags$span("Input the cost for a single:"),
      textInput("optsCostUnit", "Unit $"),
      textInput("optsCostPool", "Pool $"),
      if (input$optsClustered) {
        textInput("optsCostCluster", "Cluster $")
      },
      if (analysis_type() == "optimise_random_prevalence") {
        textInput("optsCostPeriod", "Collection period $")
      }
    )

  })


  ## Params UI ----
  output$uiParams <- renderUI({
    req(valid_cost())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$b("Design metrics"),
      tags$br(),
      tags$br(),
      selectInput(
        "optsPrevalence",
        tags$span(
          "Prevalence",
          tipify(icon("info-circle"), "Placeholder", placement = "right")
        ),
        choices = c("Low (0.01%)" = 0.001,
                    "Med. (0.5%)" = 0.005,
                    "High (2%)" = 0.02,
                    "Other" = "other"),
        selected = 0.005
      ),
      conditionalPanel(
        condition = "input.optsPrevalence == 'other'",
        textInput("optsPrevalenceOther", NULL, placeholder = "Specify %")
      ),
      if (input$optsClustered) {
        tagList(
          selectInput(
            "optsCorrelation",
            tags$span(
              "Within-cluster correlation",
              tipify(icon("info-circle"), "Placeholder", placement = "right")
            ),
            choices = c("Low (1%)" = 0.01,
                        "Med. (10%)" = 0.1,
                        "High (30%)" = 0.3,
                        "Other" = "other"),
            selected = 0.1
          ),
          conditionalPanel(
            condition = "input.optsCorrelation == 'other'",
            textInput("optsCorrelationOther", NULL, placeholder = "Specify %")
          )
        )
      }
    )
  })

  ## Advanced settings ----
  output$uiAdvanced <- renderUI({
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$details(
        tags$br(),
        tags$summary("Advanced settings"),

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
          choices = c("Low (95%)" = 0.95,
                      "Med. (99%)" = 0.99,
                      "High (100%)" = 1,
                      "Other" = "other"), # 0.5-1
          selected = 1
        ),
        conditionalPanel(
          condition = "input.optsSpecificity == 'other'",
          textInput("optsSpecificityOther", NULL, placeholder = "Specify %")
        ),

        conditionalPanel(
          condition = "input.optsTrapping == 'Fixed sampling period'",
          textInput("optsMaxPeriod", "Max sampling period", value = 10)
        ),


        # optimise_sN_prevalence ----
        if (!is.null(analysis_type()) && analysis_type() == "optimise_sN_prevalence") {
          tagList(
            textInput("optsMaxS", "Max units per pool", value = 50),
            textInput("optsMaxN", "Max pools per cluster", value = 20)
          )
        },

        textInput("optsRoundDesign", "Number of decimal places to display", value = 4)


        # optimise_random_prevalence ----
      ), # End of tags$details()

      tags$br()

    ) # End of tagList()
  })

  design_result <- reactiveVal()

  observeEvent(input$btnDesign, {
    #print(paste("prevalence:", input$optsPrevalence))
    #print(paste("cost_unit:", input$optsCostUnit))
    #print(paste("cost_pool:", input$optsCostPool))
    #print(paste("cost_cluster:", input$optsCostCluster))
    #print(paste("correlation:", input$optsCorrelation))
    #print(paste("sensitivity:", input$optsSensitivity))
    #print(paste("specificity:", input$optsSpecificity))
    #print(paste("max_s:", input$optsMaxS))
    #print(paste("max_N:", input$optsMaxN))

    req(valid_cost())
    if (input$optsClustered) {
      # replace with switch
      req(!is.null(input$optsClustered) && input$optsClustered != "")
      rho = as.numeric(input$optsCorrelation)
      cc = as.numeric(input$optsCostCluster)
    } else {
      rho = NA
      cc = NA
    }

    print(paste("rho:", rho))
    print(paste("cc:", cc))
    if (analysis_type() == "optimise_sN_prevalence") {
      out <- optimise_sN_prevalence(
        prevalence = as.numeric(input$optsPrevalence),
        cost_unit = as.numeric(input$optsCostUnit),
        cost_pool = as.numeric(input$optsCostPool),
        cost_cluster = cc,
        correlation = rho,
        sensitivity = as.numeric(input$optsSensitivity),
        specificity = as.numeric(input$optsSpecificity),
        max_s = as.numeric(input$optsMaxS),
        max_N = as.numeric(input$optsMaxN),
        form = "logitnorm"
      )
      out <-
        out %>%
        as.data.frame() %>%
        mutate(
          .keep = "none",
          `Total cost` = round(cost, digits = as.integer(input$optsRoundDesign)),
          `Optimal pool size` = as.integer(s),
          `Interval` = as.integer(catch),
          `Optimal number of pools` = as.integer(N),
        )
      if (!input$optsClustered) {
        # catch and N not calculated
        out <- out %>% select(-Interval, -`Optimal number of pools`)
      }
    }


    if (analysis_type() == "optimise_random_prevalence") {
      out <- optimise_random_prevalence(
        catch_mean = as.numeric(input$optsCatchMean),
        catch_variance = as.numeric(input$optsCatchVar),
        pool_strat_family = get(input$optsPoolStrat),
        prevalence = as.numeric(input$optsPrevalence),
        cost_unit = as.numeric(input$optsCostUnit),
        cost_pool = as.numeric(input$optsCostPool),
        cost_period = as.numeric(input$optsCostPeriod),
        cost_cluster = cc,
        correlation = rho,
        sensitivity = as.numeric(input$optsSensitivity),
        specificity = as.numeric(input$optsSpecificity),
        max_period = as.numeric(input$optsMaxPeriod),
        form = "logitnorm",
        verbose = FALSE
      )
    }

    design_result(out)
  })

  output$designTable <- renderDataTable({
    req(design_result(), analysis_type() == "optimise_sN_prevalence")
    datatable(
      design_result(), # Ensure this returns a data frame
      options = list(searching = FALSE, lengthChange = FALSE, paging = FALSE, info = FALSE),
      rownames = FALSE
    )
  })

  output$designText <- renderPrint({
    req(design_result(), analysis_type() == "optimise_random_prevalence")
    design_result() %>% unlist()
  })

  ## Output UI ----
  output$outDesign <- renderUI({
    req(design_result())
    if (analysis_type() == "optimise_sN_prevalence") {
      dataTableOutput("designTable")
    } else if (analysis_type () == "optimise_random_prevalence") {
      verbatimTextOutput("designText")
    }

  })

} # End server()


shinyApp(ui = ui, server = server)
