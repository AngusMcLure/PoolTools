ui <- fluidPage(
  ## Main navbar and pages
  navbarPage("PoolTools",
    id = "main_nav",
    tabPanel(
      "Home",
      fluidRow(
        column(
          width = 4,
          style = "text-align: center;",
          p(
            "To estimate marker prevalence from", tags$br(),
            "pooled test results, select:"
          ),
          actionButton("btnAnalysePage", "Analyse pooled data"),
        ),
        column(
          width = 4,
          style = "text-align: center;",
          p(
            "To design cost-effective tests, or evaluate", tags$br(),
            "the power of an existing design, select:"
          ),
          actionButton("btnDesignPage", "Design a pooled survey")
        )
      )
    ),
    tabPanel(
      "About",
      h2("About PoolTools"),
      h3("How to cite"),
      h3("Relevant papers"),
      h3("Contact"),
      h3("Credits and acknowledgements")
    ),
    tabPanel(
      "Analyse",
      h2("Analyse pooled data"),
      br(),
      sidebarLayout(
        sidebarPanel(
          style = "max-height: 75vh; overflow-y: auto;",
          fileInput(
            "fileAnalyse",
            accept = c(".csv", ".xlsx"),
            tags$span(
              "Upload data",
              tipify(icon("info-circle"), "Supported formats: .csv, .xlsx", placement = "right")
            )
          ),
          uiOutput("colSelectTestResults"),
          uiOutput("colSelectUnitNumber"),
          uiOutput("validColSelect"),
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
              uiOutput("outAnalyse"),
              uiOutput("btnDlAnalyse")
            ),
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
    tabPanel(
      "Design",
      h2("Design a pooled survey"),
      br(),
      sidebarLayout(
        sidebarPanel(
          style = "max-height: 75vh; overflow-y: auto;",

          # Survey options ------------------------------------
          selectInputTT("optsObjective", "Survey objective",
            tooltip = "tooltip",
            choices = c("Select" = "", "Estimate prevalence", "Detect pathogen (Coming soon...)")
          ),
          selectInputTT("optsMode", "Survey mode",
            tooltip = "tooltip",
            choices = c(
              "Select" = "",
              "Identify cost-effective designs",
              "Calculate power of existing designs (Coming soon...)"
            )
          ),
          selectInputTT("optsTrapping", "Collection strategy",
            tooltip = "tooltip",
            choices = c("Select" = "", "Fixed sample size", "Fixed sampling period")
          ),
          checkboxInputTT("optsClustered", "Clustered design?",
            tooltip = "tooltip",
            value = TRUE
          ),

          # Main settings -------------------------------------
          # UI are conditional based on survey options
          uiOutput("uiRandPrev"),
          uiOutput("uiCost"),
          uiOutput("uiParams"),
          uiOutput("uiAdvanced"),
          uiOutput("btnDesign")
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
