server <- function(input, output, session) {
  # Home page buttons ----
  observeEvent(input$btnAnalysePage, {
    updateTabsetPanel(session, "main_nav", selected = "Analyse")
  })

  observeEvent(input$btnDesignPage, {
    updateTabsetPanel(session, "main_nav", selected = "Design")
  })

  # ANALYSE ----
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

  observeEvent(input$fileAnalyse, {
    # Reset downstream UI-dependent inputs whenever data input is updated
    updateSelectInput(session, "colTestResults", selected = "")
    updateSelectInput(session, "colUnitNumber", selected = "")
    updateCheckboxInput(session, "optsStratify", value = FALSE)
    updateCheckboxGroupInput(session, "optsColStratify", selected = character(0))
    updateCheckboxInput(session, "optsHierarchy", value = FALSE)
    # You may also want to reset the bucket lists if needed
  })


  stratify_cols_options <- reactive({
    # All column names that are not the results or unit number per pool
    req(colselect_valid())
    cols <- names(data())
    cols <- cols[!cols %in% c(input$colTestResults, input$colUnitNumber)]
  })

  hierarchy_col_options <- reactive({
    # All column names that are not the results or unit number per pool
    req(stratify_valid())
    cols <- names(data())
    cols <- cols[!cols %in% c(input$colTestResults, input$colUnitNumber, input$optsColStratify)]
  })

  stratify_col_options_nonzero <- reactive({
    length(stratify_cols_options()) > 0
  })

  hierarchy_col_options_nonzero <- reactive({
    length(hierarchy_col_options()) > 0
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
    req(colselect_exists())
    if(stratify_col_options_nonzero()){
      req(!is.null(input$optsStratify))
      (!input$optsStratify || !is.null(input$optsColStratify))
    }else{
      TRUE
    }
  })

  hierarchy_valid <- reactive({
    req(stratify_valid())
    return(TRUE)
    if(hierarchy_col_options_nonzero()){
      req(!is.null(input$optsHierarchy))
      (!input$optsHierarchy || (length(input$optsHierarchyOrder) >= 1))
    }else{
      TRUE
    }
  })

  #Whether adjusting for hierarchy. If no columns that could be used for
  #adjusting for hierarchy then TRUE
  hierarchy_none <- reactive({
    if(hierarchy_col_options_nonzero()){
      req(!is.null(input$optsHierarchy))
      !input$optsHierarchy
    }else{
      TRUE
    }
  })


  ## Options
  output$colSelectTestResults <- renderUI({
    req(data())
    selectInputTT("colTestResults", "Test results",
      tooltip = "Select one column from your data that contains the positive (1)/negative (0) pool results",
      choices = c("Select column or type to search" = "", names(data()))
    )
  })
  output$colSelectUnitNumber <- renderUI({
    req(data())
    cols <- names(data())
    cols <- cols[!cols %in% input$colTestResults]
    selectInputTT("colUnitNumber", "Number of units per pool",
      tooltip = "Select the column from your data that contains the number of units per pool",
      choices = c("Select column or type to search" = "", cols)
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
    if(stratify_col_options_nonzero()){
      tagList(
        tags$hr(style = "border-top: 1px solid #CCC;"),
        checkboxInputTT("optsStratify", "Stratify data?",
                        tooltip = "Check box to calculate prevalence estimates for multiple strata within the dataset. Uncheck to calculate a single prevalence estimate for the whole dataset",
                        value = FALSE
        )
      )
    }else{
      return(NULL)
    }
  })

  output$colSelectStratify <- renderUI({
    req(colselect_valid())
    if (!is.null(input$optsStratify) && input$optsStratify) {
      tagList(
        checkboxGroupInput(
          "optsColStratify",
          tags$span("Select columns from your data to stratify data by:", style = "font-weight: plain;"), # plan text instead of bold
          choices = stratify_cols_options()
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
        "Select at least one column (variable) to stratify the data by, or deselect 'Stratify data?' to estimate prevalence on the whole dataset"
      )
    )
  })

  output$checkHierarchy <- renderUI({
    # Although the options don't change, reveal only when file is uploaded
    req(stratify_valid())
    if(hierarchy_col_options_nonzero()){
      tagList(
        tags$hr(style = "border-top: 1px solid #CCC;"),
        checkboxInputTT("optsHierarchy", "Cluster/hierarchical sampling?",
                        tooltip = "Adjust estimates and intervals for cluster or hierarchical sampling", value = FALSE
        )
      )
    }
  })
  output$colHierarchyOrder <- renderUI({
    req(stratify_valid())
    if (!is.null(input$optsHierarchy) && input$optsHierarchy) {
      tagList(
        bucket_list(
          header = "Drag to select variables. Reorder variables from the largest to smallest sampling area (e.g. province > village > household.)",
          orientation = "horizontal", # doesn't work in sidebar?
          add_rank_list(
            text = "Hierarchical variables",
            input_id = "optsHierarchyOrder"
          ),
          add_rank_list(
            text = "Other variables",
            input_id = "_optsHierarchyExclude",
            labels = hierarchy_col_options()
          )
        ),
        textOutput("validHierarchy")
      )
    }
  })

  output$validHierarchy <- renderText({
    req(input$optsHierarchy)
    validate(
      need(
        length(input$optsHierarchyOrder) >= 1,
        "You must select and order at least 1 strata"
      )
    )
  })


  output$uiDisplay <- renderUI({
    req(hierarchy_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$b("Display settings"),
      tags$br(),
      tags$br(),

      # Prevalence and CI/CrI rounding
      numericInput(
        "optsRoundAnalyse",
        tags$p("Decimal places (prevalence)", style = "font-weight: normal"),
        value = 4
      ),

      # Should prevalence and CI/CrI be multiplied by a value?
      numericInput(
        "optsPerPrevVal",
        tags$span(
          tags$b("Prevalence per value", style = "font-weight: normal"),
          shinyBS::tipify(
            icon("info-circle"),
            "Multiply prevalence and interval estimates by a given value",
            placement = "right"
          )
        ),
        value = 1,
        min = 1,
        step = 1
      )
    )
  })

  output$hierarchyNone <- reactive({
    hierarchy_none()
  })
  outputOptions(output, "hierarchyNone", suspendWhenHidden = FALSE)


  output$uiAnalyseAdv <- renderUI({
    req(hierarchy_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$details(
        tags$summary("Advanced settings", style = "display: list-item;"),

        # Run bayesian analysis with PoolPrev
        conditionalPanel(
          condition = "output.hierarchyNone",
          checkboxInput("optsBayesian", "Bayesian calculations (slow)")
        )
      ),
      tags$br()
    )
  })

  # JS version for conditionalPanel - this is to ensure that downstream
  # information and components do not reset each time a new version does not
  output$hierarchyValid <- reactive({
    hierarchy_valid()
  })
  outputOptions(output, "hierarchyValid", suspendWhenHidden = FALSE)

  output$btnAnalyse <- renderUI({
    conditionalPanel(
      condition = "output.hierarchyValid",
      actionButton("optsAnalyse", "Estimate prevalence")
    )
  })

  pooltestr_out <- eventReactive(input$optsAnalyse, {
    shinybusy::show_modal_spinner(text = "Analysing...")
    req_args <- list(
      data = data(),
      result = input$colTestResults,
      poolSize = input$colUnitNumber
    )
    ptr_mode <- which_pooltestr(
      input$optsStratify, input$optsHierarchy, input$optsBayesian
    )
    out <- run_pooltestr(
      ptr_mode, req_args, input$optsHierarchyOrder, input$optsColStratify
    )
    shinybusy::remove_modal_spinner()
    list(df = out, mode = ptr_mode)
  })

  formatted_out <- reactive({
    # Format pooltestr table output i.e. round values, or display prevalence
    # per value
    req(pooltestr_out())
    dt_display(
      df = pooltestr_out()$df,
      ptr_mode = pooltestr_out()$mode,
      per_val = as.integer(input$optsPerPrevVal),
      digits = as.integer(input$optsRoundAnalyse)
    )
  })

  output$outAnalyse <- renderDataTable({
    # Output the formatted data frame
    req(formatted_out())
    datatable(formatted_out(), rownames = F)
  })

  output$btnDlAnalyse <- renderUI({
    # Show download button only when the result() dataframe changes
    # result() depends on the button input$optsAnalyse
    req(formatted_out())
    downloadButton("dlAnalyse", "Download results")
  })

  output$dlAnalyse <- downloadHandler(
    filename <- function() {
      paste("results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(formatted_out(), file)
    }
  )

  # DESIGN ----

  # There are several reactive objects purely to check whether inputs have been
  # supplied or not. These are used to reveal the next UI components.
  # This one tracks whether the following three inputs are supplied
  # (Analysis mode, survey objective, collection strategy)
  survey_exists <- reactive({
    is_filled(input$optsObjective) &&
      is_filled(input$optsMode) &&
      is_filled(input$optsTrapping)
  })

  # Based on the combination of the previous inputs, an analysis_type is
  # returned. This is to determine which PoolPoweR function/settings should be
  # run, and to display the correct UI parts for it.
  #
  # With the PoolPoweR refactoring of sample_design, the logic needed to assign
  # the correct functions could be simplified throughout.
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

  ## RandPrev ----
  # RandPrev uses PoolPoweR::optimise_random_prevalence(), requiring UI inputs
  # for pooling strategy, catch mean and catch variance.

  ### UI ----
  # A lot of UI components are displayed within server components because they
  # require conditional logic (to be displayed).
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

  ### Server ----
  # These reactiveValues-observeEvent pairs are responsible for storing and
  # updating data when changed in the UI. The goal is to decouple the values
  # from the UI. Previously (and still a lot of parts of the code), depend on
  # changes or values direct from the UI, which made the app very unstable.
  random_prev <- reactiveValues(
    pool_strat = "",
    catch_mean = NA,
    catch_var = NA
  )

  # observeEvent to update the correpsonding reactiveValues when changed in the
  # UI.
  observeEvent(input$optsPoolStrat,
    {
      design_opts$pool_strat <- input$optsPoolStrat
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$CatchMean,
    {
      random_prev$catch_mean <- as.numeric(input$optsCatchMean)
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$optsCatchVar,
    {
      design_opts$catch_var <- as.numeric(input$optsCatchVar)
    },
    ignoreNULL = TRUE
  )

  ### Validation UI ----
  output$validCatch <- renderText({
    req(randPrev_exists())
    # Validate RandPrevUI
    validate(
      need_gt0(input$optsCatchMean, "Catch mean"),
      need_gt0(input$optsCatchVar, "Catch variance"),
      need(input$optsCatchVar > input$optsCatchMean, "Error: Catch variance must be greater than the mean"),
    )
  })

  ### Validation Server ----
  # Completeness and correctness checks to tell downstream components (i.e.
  # costs UI and Server) to display.
  randPrev_exists <- reactive({
    is_filled(input$optsPoolStrat) &&
      is_filled(input$optsCatchMean) &&
      is_filled(input$optsCatchVar)
  })

  randPrev_valid <- reactive({
    req(randPrev_exists())
    input$optsCatchMean > 0 && input$optsCatchVar && input$optsCatchVar > input$optsCatchMean
  })


  ## Costs ----
  # UI/Server component for cost_unit, cost_pool, cost_cluster; cost_period if
  # randPrev. Requires an analysis_type() and if randPrev, all inputs are valid

  ### UI ----
  output$uiCost <- renderUI({
    req(analysis_type())
    # Because rand prev has some additional options first
    if (analysis_type() == "optimise_random_prevalence") req(randPrev_valid())
    # Shared across all analysis types
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$span(
        tags$b("Costs"),
        shinyBS::tipify(
          icon("info-circle"),
          "The costs can be any type of measurement but needs to be used consistently across each input. Example costs per input include the monetary value, or the time required.",
          placement = "right"
        )
      ),
      tags$br(),
      tags$br(),
      tags$span("Enter the cost for each individual input. Use '.' for decimals (e.g. 10.5)."),
      tags$br(),
      tags$br(),
      # Custom module for handling data input and storage. See docs.
      boundNumericInput(costs, "unit", "Cost per unit", min = 1e-6, step = 1),
      boundNumericInput(costs, "pool", "Cost per pool", min = 1e-6, step = 1),
      # Conditional parameters to show if clustered or randPrev
      if (input$optsClustered) {
        boundNumericInput(costs, "cluster", "Cost per cluster", min = 1e-6, step = 1)
      },
      if (analysis_type() == "optimise_random_prevalence") {
        boundNumericInput(costs, "period", "Cost per period", min = 1e-6, step = 1)
      },
      textOutput("validCost")
    )
  })

  ### Server ----
  # Reactive object to store values
  costs <- reactiveValues(
    unit = NA,
    pool = NA,
    cluster = NA,
    period = NA
  )

  # Update values received from boundNumericInput(costs, ...)
  saveNumericInput("unit", costs)
  saveNumericInput("pool", costs)
  saveNumericInput("cluster", costs)
  saveNumericInput("period", costs)

  ### Validation UI ----
  output$validCost <- renderText({
    req(cost_exists())
    # Conditionally check each field is non-negative
    validate(
      need_ge0(costs$unit, "Unit cost"),
      need_ge0(costs$pool, "Pool cost"),
    )

    if (input$optsClustered) {
      validate(need_ge0(costs$cluster, "Cluster cost"))
    }

    if (analysis_type() == "optimise_random_prevalence") {
      validate(need_ge0(costs$period, "Period cost"))
    }

    # Conditionally check total is non-zero
    cluster_cost <- ifelse(input$optsClustered, costs$cluster, 0)
    period_cost <- ifelse(analysis_type() == "optimise_random_prevalence", costs$period, 0)
    validate(
      need(
        costs$unit + costs$pool + cluster_cost + period_cost > 0,
        "At least one of the costs must be > 0"
      )
    )
  })

  ### Validation server ----
  # Conditionally check all three cost variations are filled, If so, check that
  # inputs are valid.
  cost_exists <- reactive({
    required <- is_filled(costs$unit) && is_filled(costs$pool)
    clustered <- (!input$optsClustered || input$optsClustered && is_filled(costs$cluster))
    periodic <- (analysis_type() != "optimise_random_prevalence" || (analysis_type() == "optimise_random_prevalence" && is_filled(costs$period)))
    required && clustered && periodic
  })


  cost_valid <- reactive({
    req(cost_exists())
    # Conditionally check that individual costs are non-negative
    required <- costs$unit >= 0 && costs$pool >= 0
    clustered <- !input$optsClustered || (input$optsClustered && costs$cluster >= 0)
    periodic <- analysis_type() != "optimise_random_prevalence" || (analysis_type() == "optimise_random_prevalence" && costs$period >= 0)
    if (!(required && clustered && periodic)) {
      return(FALSE)
    }
    # Check the conditional total cost is > $0
    cluster_cost <- ifelse(input$optsClustered, costs$cluster, 0)
    period_cost <- ifelse(analysis_type() == "optimise_random_prevalence", costs$period, 0)

    total_cost <- costs$unit + costs$pool + cluster_cost + period_cost > 0
  })

  ## Params ----
  # For parameters prevalence and correlation. Dropdown selections with sensible
  # default, but an option to input own values manually.

  ### UI ----
  output$uiParams <- renderUI({
    req(cost_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$b("Design metrics"),
      tags$br(),
      tags$br(),
      selectInputTT("optsPrevalence", "Prevalence",
        tooltip = "The proportion of units that carry the marker of interest",
        choices = c(
          "Low (0.1%)" = 0.001,
          "Med. (0.5%)" = 0.005,
          "High (2%)" = 0.02,
          "Other %" = "other"
        ),
        # This allows the UI to be initialised with a default value from the
        # reactive data storage object. The use of isolate ensures that you
        # don't get stuck in an infinite loop.
        selected = isolate(design_opts$prev)
      ),
      conditionalPanel(
        condition = "input.optsPrevalence == 'other'",
        numericInput("optsPrevalenceOther", NULL, value = isolate(design_opts$prev) * 100, min = 1e-6, max = 50, step = 0.01)
      ),
      if (input$optsClustered) {
        tagList(
          selectInputTT("optsCorrelation", "Within-cluster correlation",
            tooltip = "The correlation between test results within a single cluster.",
            choices = c(
              "Low (1%)" = 0.01,
              "Med. (10%)" = 0.1,
              "High (30%)" = 0.3,
              "Other %" = "other"
            ),
            selected = isolate(design_opts$rho)
          ),
          conditionalPanel(
            condition = "input.optsCorrelation == 'other'",
            numericInput("optsCorrelationOther", NULL, value = isolate(design_opts$rho) * 100, min = 1e-6, max = 50, step = 0.01)
          )
        )
      }
    )
  })

  ### Server ----
  # For variables that are shared between sN and random.
  # These are default values which will populate the UI on session start.
  design_opts <- reactiveValues(
    # Design metrics
    prev = 0.005,
    rho = 0.1,
    # Advanced settings
    sens = 1,
    spec = 1,
    max_period = 10,
    max_s = 50,
    max_N = 20
  )

  # Corresponding data updaters for design_opts() storage
  observeEvent(input$optsPrevalence,
    {
      design_opts$prev <- processOther(input, "optsPrevalence")
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$optsCorrelation,
    {
      design_opts$rho <- processOther(input, "optsCorrelation")
    },
    ignoreNULL = TRUE
  )

  ## Advanced settings ----
  # Contains additional parameters hidden by default
  # e.g. sensitivity, specificity, and several ones related to how many times
  # things should be sampled by PoolPoweR functions.

  ### Server ----
  observeEvent(input$optsSensitivity,
    {
      # processOther divides by 100
      design_opts$sens <- processOther(input, "optsSensitivity")
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$optsSpecificity,
    {
      # processOther divides by 100
      design_opts$spec <- processOther(input, "optsSpecificity")
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$optsMaxPeriod,
    {
      design_opts$max_period <- as.numeric(input$optsMaxPeriod)
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$optsMaxS,
    {
      design_opts$max_s <- as.numeric(input$optsMaxS)
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$optsMaxN,
    {
      design_opts$max_N <- as.numeric(input$optsMaxN)
    },
    ignoreNULL = TRUE
  )

  ### UI ----
  output$uiDesignAdv <- renderUI({
    req(cost_valid())
    tagList(
      tags$hr(style = "border-top: 1px solid #CCC;"),
      tags$details(
        tags$br(),
        tags$summary("Advanced settings", style = "display: list-item;"),
        selectInputTT("optsSensitivity", "Sensitivity",
          tooltip = "The probability that the test correctly identifies a true positive.",
          choices = c(
            "Low (80%)" = 0.8,
            "Med. (90%)" = 0.9,
            "High (100%)" = 1,
            "Other %" = "other"
          ),
          # Ensures that either the default, or new input value is shown when
          # the UI changes.
          selected = isolate(design_opts$sens)
        ),
        conditionalPanel(
          condition = "input.optsSensitivity == 'other'",
          numericInput(
            "optsSensitivityOther",
            NULL,
            # *100 for percentage display purposes
            value = isolate(design_opts$sens) * 100,
            min = 50, max = 100, step = 0.01
          )
        ),
        selectInputTT("optsSpecificity", "Specificity",
          tooltip = "The probability that the test correctly identifies a true negative.",
          choices = c(
            "Low (95%)" = 0.95,
            "Med. (99%)" = 0.99,
            "High (100%)" = 1,
            "Other" = "other"
          ), # 0.5-1
          selected = isolate(design_opts$spec)
        ),
        conditionalPanel(
          condition = "input.optsSpecificity == 'other'",
          # *100 for percentage display purposes
          numericInput("optsSpecificityOther", NULL, value = isolate(design_opts$spec) * 100, min = 50, max = 100, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.optsTrapping == 'Fixed sampling period'",
          numericInput("optsMaxPeriod", "Max sampling period", value = isolate(design_opts$max_period), min = 1, step = 1)
        ),


        #### optimise_sN_prevalence ----
        if (analysis_type() == "optimise_sN_prevalence") {
          tagList(
            numericInput("optsMaxS", "Max units per pool", value = isolate(design_opts$max_s), min = 1, step = 1),
            if (input$optsClustered) {
              numericInput("optsMaxN", "Max pools per cluster", value = isolate(design_opts$max_N), min = 1, step = 1)
            }
          )
        }
      ), # End of tags$details()
      tags$br()
    ) # End of tagList()
  }) # End Adv settings

  ### Server ----
  # Values shared between sN an random are under design_opts
  sN_opts <- reactiveValues(
    max_s = 50,
    max_N = 20
  )

  observeEvent(input$optsSensitivity,
    {
      # processOther divides by 100
      design_opts$sens <- processOther(input, "optsSensitivity")
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$optsSpecificity,
    {
      # processOther divides by 100
      design_opts$spec <- processOther(input, "optsSpecificity")
    },
    ignoreNULL = TRUE
  )

  saveNumericInput("max_period", random_opts)
  saveNumericInput("max_s", sN_opts)
  saveNumericInput("max_N", sN_opts)

  ### Validation UI ----
  output$uiValidOther <- renderText({
    # Waits for advanced settings to populate before checking
    # req(!is.null(input$optsSensitivity) && !is.null(input$optsSpecificity))
    req(cost_valid())
    paste("Other valid", other_valid())
    # TODO: Refactor this mess
    if (is_filled(input$optsPrevalence) && input$optsPrevalence == "other" && is_filled(input$optsPrevalenceOther)) {
      validate(
        need(in_range(input, "optsPrevalence", c(0, 50), inc_lower = F), "Error: The recommended range for Prevalence is > 0% and <= 50%"),
      )
    }
    if (is_filled(input$optsSensitivity) && input$optsSensitivity == "other" && is_filled(input$optsSensitivityOther)) {
      validate(
        need(in_range(input, "optsSensitivity", c(50, 100), inc_lower = T), "Error: The recommended range for Sensitivity is >= 50% and <= 100%"),
      )
    }
    if (is_filled(input$optsSpecificity) && input$optsSpecificity == "other" && is_filled(input$optsSpecificityOther)) {
      validate(
        need(in_range(input, "optsSpecificity", c(50, 100), inc_lower = T), "Error: The recommended range for Specificity is >= 50% and <= 100%"),
      )
    }
    if (is_filled(input$optsClustered) && input$optsClustered && input$optsCorrelation == "other" && is_filled(input$optsCorrelationOther)) {
      validate(
        need(in_range(input, "optsCorrelation", c(1, 50), inc_lower = T), "Error: The recommended range for Correlation is > 0% and <= 50%"),
      )
    }
  })

  ### Validation server ----
  other_valid <- reactive({
    # Waits for advanced settings to populate before checking
    # req(!is.null(input$optsSensitivity) && !is.null(input$optsSpecificity))
    req(cost_valid())
    valid <- TRUE

    # TODO: Refactor this mess
    if (is_filled(input$optsPrevalence) && input$optsPrevalence == "other") {
      if (is_filled(input$optsPrevalenceOther) && !in_range(input, "optsPrevalence", c(0, 50), inc_lower = F) || !is_filled(input$optsPrevalenceOther)) valid <- FALSE
    }
    if (is_filled(input$optsSensitivity) && input$optsSensitivity == "other") {
      if (is_filled(input$optsSensitivityOther) && !in_range(input, "optsSensitivity", c(50, 100), inc_lower = T) || !is_filled(input$optsSensitivityOther)) valid <- FALSE
    }
    if (is_filled(input$optsSpecificity) && input$optsSpecificity == "other") {
      if (is_filled(input$optsSensitivityOther) && !in_range(input, "optsSpecificity", c(50, 100), inc_lower = T || !is_filled(input$optsSpecificityOther))) valid <- FALSE
    }
    if (is_filled(input$optsClustered) && input$optsClustered && is_filled(input$optsCorrelation) && input$optsCorrelation == "other") {
      if (is_filled(input$optsCorrelationOther) && !in_range(input, "optsCorrelation", c(0, 50), inc_lower = T) || !is_filled(input$optsCorrelationOther)) valid <- FALSE
    }
    return(valid)
  })

  ### Button ----
  output$btnDesign <- renderUI({
    req(cost_valid(), !is.null(other_valid()) && other_valid())
    actionButton("runDesign", "Run!")
  })

  ### Design output generation ----
  result_sN <- reactiveVal()
  result_randPrev <- reactiveVal()
  design_result <- reactiveVal()

  observeEvent(input$runDesign, {
    # These actions are run when the button is pressed
    req(cost_valid(), other_valid())
    shinybusy::show_modal_spinner(text = "Designing...")
    ### Parse input arguments ----
    if (input$optsClustered) {
      # replace with switch
      req(is_filled(input$optsClustered))
      rho <- design_opts$rho
      cc <- costs$cluster
    } else {
      rho <- NA
      cc <- NA
    }
    # End parse input arguments

    #### optimise_sN_prevalence ----
    # TODO: Once PoolPoweR::optimise_prevalence is implemented for
    # variable design, the `if` logic can be refactored on
    # input$optsTrapping instead, and same optimise_prevalence function reused!
    if (analysis_type() == "optimise_sN_prevalence") {
      out <- PoolPoweR::optimise_sN_prevalence(
        prevalence = design_opts$prev,
        cost_unit = costs$unit,
        cost_pool = costs$pool,
        cost_cluster = cc,
        correlation = rho,
        sensitivity = design_opts$sens,
        specificity = design_opts$spec,
        max_s = design_opts$max_s,
        max_N = design_opts$max_N,
        form = "logitnorm"
      )
      result_sN(out)
    }

    #### optimise_random_prevalence ----
    if (analysis_type() == "optimise_random_prevalence") {
      out <- PoolPoweR::optimise_random_prevalence(
        catch_mean = as.numeric(input$optsCatchMean),
        catch_variance = as.numeric(input$optsCatchVar),
        pool_strat_family = get(input$optsPoolStrat),
        prevalence = processOther(input, "optsPrevalence"),
        cost_unit = costs$unit,
        cost_pool = costs$pool,
        cost_period = costs$period,
        cost_cluster = cc,
        correlation = rho,
        sensitivity = processOther(input, "optsSensitivity"),
        specificity = processOther(input, "optsSpecificity"),
        max_period = as.numeric(input$optsMaxPeriod),
        form = "logitnorm",
        verbose = FALSE
      )
      result_randPrev(out)
    }


    ### Prepare text output ----
    if (analysis_type() == "optimise_sN_prevalence") {
      #### fixed sample size (clustered and unclustered) ----
      req(result_sN())
      design_result(
        sn_text(result_sN(), input$optsClustered)
      )
    } else if (analysis_type() == "optimise_random_prevalence") {
      #### Fixed sampling period ----
      req(result_randPrev())
      r <- result_randPrev()

      strat_txt <- strat_text(r, input$optsPoolStrat)

      if (input$optsClustered) {
        period_txt <- period_text(r) # only if clustered
        catch_txt <- catch_text(r, input$optsClustered)
        design_result(
          tagList(strat_txt, tags$br(), tags$br(), period_txt, tags$br(), tags$br(), catch_txt)
        )
      } else if (!input$optsClustered) {
        design_result(
          tagList(strat_txt)
        )
      }
    }

    shinybusy::remove_modal_spinner()
  })


  ### Output UI ----
  output$outDesign <- renderUI({
    req(design_result())
    design_result()
  })
} # End server()
