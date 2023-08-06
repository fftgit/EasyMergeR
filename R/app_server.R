#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  # Your application server logic
  df <- mod_load_xlsx_server("load_xlsx_1")
  mod_table_playground_server("table_playground_1",df)
}
