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

    f <- input$fileAnalyse
    ext <- tools::file_ext(f$name)
    switch(ext,
      csv = read.csv(f$datapath, header = TRUE),
      xlsx = readxl::read_xlsx(f$datapath, col_names = TRUE),
      validate("Unsupported file; Please upload a .csv or .xlsx file")
    )
  })

  metadata_cols <- reactive({
    # All column names that are not the results or unit number per pool
    req(colselect_valid())
    cols <- names(data())
    cols <- cols[!cols %in% c(input$colTestResults, input$colUnitNumber)]
  })

  ## State reactives
  colselect_exists <- reactive({
    req(data())
    is_filled(input$colTestResults) && is_filled(input$colUnitNumber)
  })

  colselect_valid <- reactive({
    req(colselect_exists())
    # Results column must be either 0 or 1
    results_ok <- is_binary_col(data(), input$colTestResults)
    # NumInPool column must be integers
    numinpool_ok <- is_integer_col(data(), input$colUnitNumber)

    results_ok && numinpool_ok
  })

  stratify_valid <- reactive({
    req(colselect_exists(), !is.null(input$optsStratify))
    (!input$optsStratify || !is.null(input$optsColStratify))
  })

  hierarchy_valid <- reactive({
    req(stratify_valid(), !is.null(input$optsHierarchy))
    (!input$optsHierarchy || (length(input$optsHierarchyOrder) >= 2))
  })

  ## Options
  output$colSelectTestResults <- renderUI({
    req(data())
    selectInputTT("colTestResults", "Test results",
      tooltip = "tooltip",
      choices = c("Select column" = "", names(data()))
    )
  })
  output$colSelectUnitNumber <- renderUI({
    req(data())
    cols <- names(data())
    cols <- cols[!cols %in% input$colTestResults]
    selectInputTT("colUnitNumber", "Number of specimens per pool",
      tooltip = "tooltip",
      choices = c("Select column" = "", cols)
    )
  })
  output$validColSelect <- renderText({
    req(colselect_exists())
    validate(
      need(is_binary_col(data(), input$colTestResults), "Error: 'Results' column must contain only 0 or 1"),
      need(is_integer_col(data(), input$colUnitNumber), "Error: 'Number in pool' column must contain integers only")
    )
  })


  output$checkStratify <- renderUI({
    req(colselect_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      checkboxInputTT("optsStratify", "Stratify data?",
        tooltip = "Uncheck to estimate prevalence on the whole data set",
        value = T
      )
    )
  })

  output$colSelectStratify <- renderUI({
    req(colselect_valid())
    if (!is.null(input$optsStratify) && input$optsStratify) {
      tagList(
        checkboxGroupInput(
          "optsColStratify",
          tags$span("Stratify data by:", style = "font-weight: plain;"), # plan text instead of bold
          choices = metadata_cols()
        ),
        textOutput("validStratify")
      )
    } else {
      return(NULL)
    }
  })

  output$validStratify <- renderText({
    # No need to validate if not stratifying
    req(input$optsStratify)
    validate(
      need(
        !is.null(input$optsColStratify),
        "Error: Select at least one strata, or deselect 'Stratify data?' to estimate prevalence on the whole dataset"
      )
    )
  })

  output$checkHierarchy <- renderUI({
    # Although the options don't change, reveal only when file is uploaded
    req(stratify_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      checkboxInputTT("optsHierarchy", "Adjust for hierarchical sampling?",
        tooltip = "tooltip", value = FALSE
      )
    )
  })
  output$colHierarchyOrder <- renderUI({
    req(stratify_valid())
    if (!is.null(input$optsHierarchy) && input$optsHierarchy) {
      tagList(
        bucket_list(
          header = "Drag to include hierarchical variables and reorder from largest to smallest",
          orientation = "horizontal", # doesn't work in sidebar?
          add_rank_list(
            text = "Hierarchical variables",
            input_id = "optsHierarchyOrder"
          ),
          add_rank_list(
            text = "Other variables",
            input_id = "_optsHierarchyExclude",
            labels = metadata_cols()
          )
        ),
        textOutput("validHierarchy")
      )
    }
  })

  output$validHierarchy <- renderText({
    req(input$optsHierarchy)
    validate(
      need(length(input$optsHierarchyOrder) >= 2, "You must select and order at least 2 strata")
    )
  })

  output$optsSettings <- renderUI({
    req(hierarchy_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$details(
        tags$br(),
        tags$summary("Advanced settings"),
        numericInput("optsRoundAnalyse", "Number of decimal places to display", value = 4),
        checkboxInput("optsBayesian", "Bayesian calculations (slow)")
      ),
      tags$br()
    )
  })


  output$btnAnalyse <- renderUI({
    req(data(), colselect_valid(), stratify_valid(), hierarchy_valid())
    actionButton("optsAnalyse", "Run!")
  })

  ## Table output
  result <- reactiveVal()

  observeEvent(input$optsAnalyse, {
    req(data(), colselect_valid(), stratify_valid(), hierarchy_valid())
    shinybusy::show_modal_spinner(text = "Analysing...")
    Sys.sleep(1)
    req_args <- list(
      data = data(),
      result = input$colTestResults,
      poolSize = input$colUnitNumber
    )

    if (!input$optsHierarchy) {
      # Add bayesian switch for PoolPrev
      poolprev_args <- req_args
      poolprev_args$bayesian <- input$optsBayesian
      if (!input$optsStratify) {
        # Estimate prevalence on whole data
        result(do.call(PoolTestR::PoolPrev, poolprev_args))
      } else {
        # Estimate prevalence for each selected column (stratified)
        # Parse arguments
        col_args <- c(poolprev_args, lapply(input$optsColStratify, as.name))
        result(do.call(PoolTestR::PoolPrev, col_args))
      }
    } else if (input$optsHierarchy) {
      # Account for hierarchical sampling structure
      hier_args <- req_args
      hier_args$hierarchy <- input$optsHierarchyOrder
      # Parse arguments for stratification
      if (input$optsStratify) {
        hier_args <- c(hier_args, lapply(input$optsColStratify, as.name))
      }
      result(do.call(PoolTestR::HierPoolPrev, hier_args))
    } else {
      result(NULL)
    }
    result(
      result() %>%
        dplyr::mutate(across(is.double, round, digits = as.integer(input$optsRoundAnalyse)))
    )
    shinybusy::remove_modal_spinner()
  })

  output$outDT <- renderDataTable({
    req(result())
    result()
  })

  output$outAnalyse <- renderUI({
    # Update this, so output only changes when clicking button
    dataTableOutput("outDT")
  })

  output$btnDlAnalyse <- renderUI({
    req(hierarchy_valid(), result())
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

  survey_exists <- reactive({
    is_filled(input$optsObjective) &&
      is_filled(input$optsMode) &&
      is_filled(input$optsTrapping)
  })

  cost_exists <- reactive({
    required <- is_filled(input$optsCostUnit) && is_filled(input$optsCostPool)
    clustered <- (!input$optsClustered || input$optsClustered && is_filled(input$optsCostCluster))
    periodic <- (analysis_type() != "optimise_random_prevalence" || (analysis_type() == "optimise_random_prevalence" && is_filled(input$optsCostPeriod)))
    required && clustered && periodic
  })

  randPrev_exists <- reactive({
    is_filled(input$optsPoolStrat) &&
      is_filled(input$optsCatchMean) &&
      is_filled(input$optsCatchVar)
  })

  analysis_type <- reactive({
    req(survey_exists())
    if (input$optsObjective == "Estimate prevalence" &
      input$optsMode == "Identify cost-effective designs") {
      if (input$optsTrapping == "Fixed sample size") {
        return("optimise_sN_prevalence")
      } else if (input$optsTrapping == "Fixed sampling period") {
        return("optimise_random_prevalence")
      }
    }
  })

  randPrev_valid <- reactive({
    req(randPrev_exists())
    input$optsCatchMean > 0 && input$optsCatchVar && input$optsCatchVar > input$optsCatchMean
  })

  cost_valid <- reactive({
    req(cost_exists())
    # Conditionally check that individual costs are non-negative
    required <- input$optsCostUnit >= 0 && input$optsCostPool >= 0
    clustered <- !input$optsClustered || (input$optsClustered && input$optsCostCluster >= 0)
    periodic <- analysis_type() != "optimise_random_prevalence" || (analysis_type() == "optimise_random_prevalence" && input$optsCostPeriod >= 0)
    # Check the conditional total cost is > $0
    cluster_cost <- ifelse(input$optsClustered, input$optsCostCluster, 0)
    period_cost <- ifelse(analysis_type() == "optimise_random_prevalence", input$optsCostPeriod, 0)

    total_cost <- input$optsCostUnit + input$optsCostPool + cluster_cost + period_cost > 0
  })

  ## RandPrev UI ----
  output$uiRandPrev <- renderUI({
    req(analysis_type() == "optimise_random_prevalence")
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      selectInputTT("optsPoolStrat", "Pooling strategy",
        tooltip = "tooltip",
        choices = c(
          "Select" = "",
          "Max size" = "pool_max_size",
          "Target number" = "pool_target_number"
        )
      ),
      numericInput("optsCatchMean", "Catch mean", value = NULL, min = 1, step = 1),
      numericInput("optsCatchVar", "Catch variance", value = NULL, min = 2, step = 1),
      textOutput("validCatch")
    )
  })


  output$validCatch <- renderText({
    req(randPrev_exists())
    # Validate RandPrevUI
    validate(
      need_gt0(input$optsCatchMean, "Catch mean"),
      need_gt0(input$optsCatchVar, "Catch variance"),
      need(input$optsCatchVar > input$optsCatchMean, "Error: Catch variance must be greater than the mean"),
    )
  })

  ## Cost UI ----
  output$uiCost <- renderUI({
    req(survey_exists(), analysis_type())
    # Because rand prev has some additional options first
    if (analysis_type() == "optimise_random_prevalence") req(randPrev_valid())
    # Shared across all analysis types
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$b("Costs"),
      tags$br(),
      tags$span("Input the cost for a single:"),
      numericInput("optsCostUnit", "Unit $", value = NULL, min = 1e-6, step = 0.5),
      numericInput("optsCostPool", "Pool $", value = NULL, min = 1e-6, step = 0.5),
      if (input$optsClustered) {
        numericInput("optsCostCluster", "Cluster $", value = NULL, min = 1e-6, step = 0.5)
      },
      if (analysis_type() == "optimise_random_prevalence") {
        numericInput("optsCostPeriod", "Collection period $", value = NULL, min = 1e-6, step = 0.5)
      },
      textOutput("validCost")
    )
  })

  output$validCost <- renderText({
    req(cost_exists())
    # Conditionally check each field is non-negative
    validate(
      need_ge0(input$optsCostUnit, "Unit $"),
      need_ge0(input$optsCostPool, "Pool $"),
    )
    if (input$optsClustered) validate(need_ge0(input$optsCostCluster, "Cluster $"))
    if (analysis_type() == "optimise_random_prevalence") validate(need_ge0(input$optsCostPeriod, "Period $"))

    # Conditionally check total is non-zero
    cluster_cost <- ifelse(input$optsClustered, input$optsCostCluster, 0)
    period_cost <- ifelse(analysis_type() == "optimise_random_prevalence", input$optsCostPeriod, 0)
    validate(
      need(
        input$optsCostUnit + input$optsCostPool + cluster_cost + period_cost > 0,
        "At least one of the costs must be > $0"
      )
    )
  })


  ## Params UI ----
  output$uiParams <- renderUI({
    req(cost_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$b("Design metrics"),
      tags$br(),
      tags$br(),
      selectInputTT("optsPrevalence", "Prevalence",
        tooltip = "tooltip",
        choices = c(
          "Low (0.1%)" = 0.001,
          "Med. (0.5%)" = 0.005,
          "High (2%)" = 0.02,
          "Other" = "other"
        ),
        selected = 0.005
      ),
      conditionalPanel(
        condition = "input.optsPrevalence == 'other'",
        numericInput("optsPrevalenceOther", NULL, value = 0.5, min = 1e-6, max = 50, step = 0.01)
      ),
      if (input$optsClustered) {
        tagList(
          selectInputTT("optsCorrelation", "Within-cluster correlation",
            tooltip = "(0-100%) The correlation between test results within a single cluster. 100% indicates that there are no differentces between units within a single cluster.",
            choices = c(
              "Low (1%)" = 0.01,
              "Med. (10%)" = 0.1,
              "High (30%)" = 0.3,
              "Other" = "other"
            ),
            selected = 0.1
          ),
          conditionalPanel(
            condition = "input.optsCorrelation == 'other'",
            numericInput("optsCorrelationOther", NULL, value = 30, min = 1e-6, max = 50, step = 0.01)
          )
        )
      }
    )
  })

  ## Advanced settings ----
  output$uiAdvanced <- renderUI({
    req(cost_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$details(
        tags$br(),
        tags$summary("Advanced settings"),
        selectInputTT("optsSensitivity", "Sensitivity",
          tooltip = "(0-100%) The probability that the test correctly identifies a true positive. 100% indicates that the test can perfectly identify all true positives.",
          choices = c(
            "Low (80%)" = 0.8,
            "Med. (90%)" = 0.9,
            "High (100%)" = 1,
            "Other" = "other"
          ), # 0.5-1
          selected = 1
        ),
        conditionalPanel(
          condition = "input.optsSensitivity == 'other'",
          numericInput("optsSensitivityOther", NULL, value = 100, min = 50, max = 100, step = 0.01)
        ),
        selectInputTT("optsSpecificity", "Specificity",
          tooltip = "(0-100%) The probability that the test correctly identifies a true negative. 100% indicates that the test can perfectly identify all true negatives.",
          choices = c(
            "Low (95%)" = 0.95,
            "Med. (99%)" = 0.99,
            "High (100%)" = 1,
            "Other" = "other"
          ), # 0.5-1
          selected = 1
        ),
        conditionalPanel(
          condition = "input.optsSpecificity == 'other'",
          numericInput("optsSpecificityOther", NULL, value = 100, min = 50, max = 100, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.optsTrapping == 'Fixed sampling period'",
          numericInput("optsMaxPeriod", "Max sampling period", value = 10, min = 1, step = 1)
        ),


        # optimise_sN_prevalence ----
        if (!is.null(analysis_type()) && analysis_type() == "optimise_sN_prevalence") {
          tagList(
            numericInput("optsMaxS", "Max units per pool", value = 50, min = 1, step = 1),
            numericInput("optsMaxN", "Max pools per cluster", value = 20, min = 1, step = 1)
          )
        },

        # optimise_random_prevalence ----
      ), # End of tags$details()

      tags$br()
    ) # End of tagList()
  }) # End Adv settings

  output$btnDesign <- renderUI({
    req(cost_valid())
    actionButton("runDesign", "Run!")
  })

  # Design output generation ----
  result_sN <- reactiveVal()
  result_randPrev <- reactiveVal()
  design_result <- reactiveVal()

  observeEvent(input$runDesign, {
    req(cost_valid())
    shinybusy::show_modal_spinner(text = "Designing...")
    Sys.sleep(1)
    # Parse input arguments ----
    if (input$optsClustered) {
      # replace with switch
      req(is_filled(input$optsClustered))
      rho <- as.numeric(input$optsCorrelation)
      cc <- as.numeric(input$optsCostCluster)
    } else {
      rho <- NA
      cc <- NA
    }

    # If "other" is selected in either sN or randPrev
    prev <- as.numeric(ifelse(input$optsPrevalence == "other", input$optsPrevalenceOther, input$optsPrevalence))
    sens <- as.numeric(ifelse(input$optsSensitivity == "other", input$optsSensitivityOther, input$optsSensitivity))
    spec <- as.numeric(ifelse(input$optsSpecificity == "other", input$optsSpecificityOther, input$optsSpecificity))
    # End parse input arguments ----

    # optimise_sN_prevalence ----
    if (analysis_type() == "optimise_sN_prevalence") {
      out <- PoolPoweR::optimise_sN_prevalence(
        prevalence = prev,
        cost_unit = as.numeric(input$optsCostUnit),
        cost_pool = as.numeric(input$optsCostPool),
        cost_cluster = cc,
        correlation = rho,
        sensitivity = sens,
        specificity = spec,
        max_s = as.numeric(input$optsMaxS),
        max_N = as.numeric(input$optsMaxN),
        form = "logitnorm"
      )
      result_sN(out)
    }

    # optimise_random_prevalence ----
    if (analysis_type() == "optimise_random_prevalence") {
      out <- PoolPoweR::optimise_random_prevalence(
        catch_mean = as.numeric(input$optsCatchMean),
        catch_variance = as.numeric(input$optsCatchVar),
        pool_strat_family = get(input$optsPoolStrat),
        prevalence = prev,
        cost_unit = as.numeric(input$optsCostUnit),
        cost_pool = as.numeric(input$optsCostPool),
        cost_period = as.numeric(input$optsCostPeriod),
        cost_cluster = cc,
        correlation = rho,
        sensitivity = sens,
        specificity = spec,
        max_period = as.numeric(input$optsMaxPeriod),
        form = "logitnorm",
        verbose = FALSE
      )
      result_randPrev(out)
    }


    ## Prepare text output ----
    if (analysis_type() == "optimise_sN_prevalence") {
      # fixed sample size ----
      req(result_sN())
      design_result(
        sn_text(result_sN(), input$optsClustered)
      )


    } else if (analysis_type() == "optimise_random_prevalence") { # End of fixed sample size
      # Fixed sampling period ----
      req(result_randPrev())
      r <- result_randPrev()

      p_units <- pluralise(r$catch$mean, "unit")

      p_strat <- ""
      p_periods <- "collection periods."
      if (r$periods < 2) p_periods <- "collection period."
      p_period <- paste("Sampling should be conducted over", r$periods, p_periods)
      p_catch <- paste(
        "We expect an average of", r$catch$mean, p_units,
        "caught per cluster (variance:", r$catch$variance, ")."
      )

      if (input$optsPoolStrat == "pool_max_size") {
        # max size ----
        p_strat <- paste(
          "For the given inputs, the optimal design is to distribute units in pools of size",
          r$pool_strat_pars$max_size,
          "with any remainder placed in a single smaller pool."
        )
      } else if (input$optsPoolStrat == "pool_target_number") {
        # target number ----
        p_strat <- paste(
          "For the given inputs, the optimal design is to distribute units into",
          r$pool_strat_pars$target_number,
          "equally sized pools, with no maximum pool size."
        )
      }
      design_result(
        tagList(p_strat, tags$br(), tags$br(), p_period, tags$br(), tags$br(), p_catch)
      )
    } # End of analysis_type() == "optimise_random_prevalence"/ fixed sampling period

    shinybusy::remove_modal_spinner()
  })


  ## Output UI ----
  output$outDesign <- renderUI({
    req(design_result())
    design_result()
  })
} # End server()