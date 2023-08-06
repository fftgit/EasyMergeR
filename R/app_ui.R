#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    titlePanel(
      fluidPage(
        fluidRow(column(3,
                        column(
                          h2(
                          HTML("<span style='color:#2C593B;'>Easy</span><span style='color:#AAB8B4;'>Merge</span><span style='color:#418BCA;'>R</span>"),
                          style = "font-style:italic;font-weight:bold;font-size:150%;text-indent:5%;"
                        ),
                        width = 5),
                       ),
                 column(1,
                   img(
                     src = "www/favicon.png",
                     width = 60,
                     height = 70,
                     style = "float:right;"
                   )
                 ),
                 column(8,
                        hr(style = "border: 0;border-top: 30px solid #DDDDDD;border-radius: 5px;")
                 )))
        ),

    fluidPage(
      waiter::autoWaiter(),
      waiter::use_waitress(),
      theme = shinythemes::shinytheme("simplex"),
      mod_load_xlsx_ui("load_xlsx_1"),
      fluidRow(
        column(4,hr(style = "border: 0;border-top: 10px solid #2B89FF;border-radius: 5px;")),
        column(8,hr(style = "border: 0;border-top: 10px solid #CF4A3B;border-radius: 5px;"))
      ),
      mod_table_playground_ui("table_playground_1")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "EasyMergeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()



  )
}
