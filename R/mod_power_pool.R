power_pool_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(

      tagList(
        numericInput(ns("s"), "Pool size", value = 0),
        numericInput(ns("N"), "Pool number", value = 0),
        sizeOrPowerUI(ns("size_power_ui")),
        prevalencePairUI(ns("prev_pair_ui")),
        tags$hr(style = "border-top: 1px solid #CCC;"),
        selectInputOther(ns("corr_ui")), # if clustered
        advancedSettingsUI(ns("adv_ui")),
        tags$br(),
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
    sizeOrPowerServer("size_power_ui")

    prevalencePairServer("prev_pair_ui")

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

    advancedSettingsServer("adv_ui")

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

#' Module: sizeOrPower
#'
#' This controls whether the sample size OR power should be input. This decides
#' which `PowerPool` function should be used downstream. A "switch" button is
#' used to swap the inputs. This removes the need for a verbose and visually
#' separated dropdown button to change only a single input, and clearer that
#' only this input is the key difference between calculations.
sizeOrPowerUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$hr(style = "border-top: 1px solid #CCC;"),
    fluidRow(
      column(8, uiOutput(ns("num"))),
      # the style offsets the button so it is inline with the numericInput above
      column(4, actionButton(ns("switch"), icon("repeat"), style = "margin-top: 25px;"))
    )
  )
}

sizeOrPowerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    which_ui <- reactiveVal("size") # Show sample size input by default
    # Logic to trigger the "switching" of inputs between size and power
    observeEvent(input$switch, {
      if (which_ui() == "size") {
        which_ui("power")
      } else {
        which_ui("size")
      }
    })

    output$num <- renderUI({
      if (which_ui() == "size") {
        numericInput(ns("clust_N"), "Number of clusters", value = 0)
      } else {
        numericInput(ns("power"), "Power", value = 0)
      }
    })

    return(which_ui) # Use this downstream to determine which function to use
  })
}

prevalencePairUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$hr(style = "border-top: 1px solid #CCC;"),
    tags$h4("Prevalence"),
    fluidRow(
      column(6, numericInput(ns("prev_null"), "Null", value = 0)),
      column(6, numericInput(ns("prev_alt"), "Alternative", value = 0)),
    )
  )
}

prevalencePairServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    }
  )
}

advancedSettingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$hr(style = "border-top: 1px solid #CCC;"),
    tags$details(
      tags$summary("Advanced settings", style = "display: list-item;"),
        tags$br(),
        numericInput(ns("prev_sig"), "Prevalence significance level", value = 0.05),
        selectInputOther(ns("sens_ui")),
        selectInputOther(ns("spec_ui"))
    )
  )
}

advancedSettingsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

  })
}
