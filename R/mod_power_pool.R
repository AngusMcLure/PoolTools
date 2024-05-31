power_pool_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(

      tagList(
        numericInput(ns("s"), "Pool size", value = 0),
        numericInput(ns("N"), "Pool number", value = 0),
        numericInput(ns("clust_N"), "Number of clusters", value = 0), # if clustered
        numericInput(ns("prev_null"), "Prevalence (null)", value = 0),
        numericInput(ns("prev_alt"), "Prevalence (alternative)", value = 0),
        numericInput(ns("sig"), "Significance level", value = 0.05),
        selectInputOther(ns("corr_ui")), # if clustered
        selectInputOther(ns("sens_ui")),
        selectInputOther(ns("spec_ui")),
        uiOutput(ns("button_ui")),
      )

    ),

    mainPanel(
      textOutput(ns("result_ui"))
    )

  )
}

power_pool_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI ----
    corr <- selectInputOtherServer(
      "corr_ui", "Within-cluster correlation",
      tooltip = "The correlation between test results within a single cluster",
      choices = c(
        "Low (1%)" = 0.01,
        "Med. (10%)" = 0.1,
        "High (30%)" = 0.3,
        "Other %" = "other"
      ), selected = 0.1
    )

    sens <- selectInputOtherServer(
      "sens_ui", "Sensitivity",
      tooltip = "The probability that the test correctly identifies a true negative",
      choices = c(
        "Low (80%)" = 0.8,
        "Med. (90%)" = 0.9,
        "High (100%)" = 1,
        "Other %" = "other"
      ), selected = 1
    )

    spec <- selectInputOtherServer(
      "spec_ui", "Specificity",
      tooltip = "The probability that the test correctly identifies a true positive",
      choices = c(
        "Low (80%)" = 0.8,
        "Med. (90%)" = 0.9,
        "High (100%)" = 1,
        "Other %" = "other"
      ), selected = 1
    )

    output$button_ui <- renderUI({
      actionButton(ns("run"), "Run!")
    })

    # PoolPoweR functions ----
    result <- eventReactive(input$run, {
      PoolPoweR::power_pool(
        pool_size = input$s,
        pool_number = input$N,
        cluster_number = input$clust_N,
        prevalence_null = input$prev_null,
        prevalence_alt = input$prev_alt,
        correlation = corr(),
        sensitivity = sens(),
        specificity = spec(),
        sig_level = input$sig,
        alternative = "greater",
        form = "logitnorm",
        link = "logit"
      )
    })

    output$result_ui <- renderText({
      req(result())
      result()
    })

  })
}

