ui <- fluidPage(
  # Dropdown boxes appear over panels
  tags$head(tags$style(".selectize-control.single { width: 100%; z-index: 1; }")),
  ## Main navbar and pages
  navbarPage(paste0("PoolTools v", appVersion),
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
      includeMarkdown("inst/app/www/about.md")
    ),
    tabPanel(
      "Analyse",
      h2("Analyse pooled data"),
      br(),
      sidebarLayout(
        fluid = T,
        sidebarPanel(
          style = "max-height: 75vh; overflow-y: auto;",
          fileInput(
            "fileAnalyse",
            accept = c(".csv", ".xlsx"),
            tags$span(
              "Upload data (.xlsx or .csv)",
              shinyBS::tipify(icon("info-circle"), "See Help tab for formatting requirements", placement = "right")
            )
          ),
          uiOutput("colSelectTestResults"),
          uiOutput("colSelectUnitNumber"),
          uiOutput("validColSelect"),
          uiOutput("checkStratify"),
          uiOutput("colSelectStratify"),
          uiOutput("checkHierarchy"),
          uiOutput("colHierarchyOrder"),
          uiOutput("uiDisplay"),
          uiOutput("uiAnalyseAdv"),
          uiOutput("btnAnalyse")
        ),
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Results",
              br(),
              dataTableOutput("outAnalyse"),
              uiOutput("btnDlAnalyse")
            ),
            tabPanel(
              "Help",
              includeMarkdown("inst/app/www/help_analyse.md")
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
          selectInputTT(
            "optsMode",
            "Analysis mode",
            tooltip = "See Help tab for more information",
            choices = c(
              "Select" = "",
              "Identify cost-effective designs",
              "Calculate power of existing designs (Coming soon...)"
            )
          ),
          # TODO: Split Mode and the remaining below to Submode
          selectInputTT(
            "optsObjective",
            "Survey objective",
            tooltip = "tooltip",
            choices = c("Select" = "", "Estimate prevalence", "Detect pathogen (Coming soon...)")
          ),
          selectInputTT(
            "optsTrapping",
            "Collection strategy",
            tooltip = "tooltip",
            choices = c("Select" = "", "Fixed sample size", "Fixed sampling period")
          ),
          checkboxInputTT(
            "optsClustered",
            "Clustered design?",
            tooltip = "tooltip",
            value = TRUE
          ),

          # Main settings -------------------------------------
          # UI are conditional based on survey options
          uiOutput("uiRandPrev"),
          uiOutput("uiCost"),
          uiOutput("uiParams"),
          uiOutput("uiDesignAdv"),
          textOutput("uiValidOther"),
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
            tabPanel(
              "Help",
              includeMarkdown("inst/app/www/help_design.md")
            )
          )
        )
      ) # End of sidebarLayout -------------------------------
    )
  )
)
