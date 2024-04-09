library(shiny)
library(DT)
source("mod_dt.R")

ui2 <- fluidPage(
  datatable_ui("analyse")
)

server2 <- function(input, output) {

  pooltestr_out <- read.csv("/home/fredjaya/GitHub/PoolTools/dev/prev_out.csv")
  datatable_server("analyse", pooltestr_out)

}

# Run the application
shinyApp(ui = ui2, server = server2)
