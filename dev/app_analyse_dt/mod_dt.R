datatable_ui <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("datatable"))
}


datatable_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    output$datatable <- renderDataTable({
      req(df)
      df
    })
  })
}
