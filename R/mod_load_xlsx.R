#' load_xlsx UI Function
#'
#' @description A Module for loading XLSX files.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_xlsx_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-size: +28px;
      }
    ")),

    sidebarLayout(

      sidebarPanel(
        h3("File upload："),
        fluidRow(
          column(8,
                 fileInput(ns("loadxlsx"),
                           label = "Upload your xlsx files",
                           buttonLabel = "Click to select",
                           multiple = T)),

          column(4,
                 fluidRow(
                   column(12,
                          shinyWidgets::dropdownButton(
                            selectInput(ns("example"),
                                        "Choose sample data", # Choose sample data here
                                        choices = c("beaver","MIMIC_demo"),
                                        multiple = F),
                            actionButton(ns("load_example_data"),
                                         "OK"),
                            status = "primary",
                            label = "Load example data",
                            circle = F,
                            icon = icon("table"),
                            width = 200),
                          style = "margin-top:23px"
                   )
                 )
          )),
        shinyWidgets::switchInput(ns("preview_switch"),label = "View table",size = "normal"),
        uiOutput(ns("select_1")),
        uiOutput(ns("select_2")),
        uiOutput(ns("select_3")),
        uiOutput(ns("select_4")),

        actionButton(ns("send2playground"),"Next")
      ),
      mainPanel(
        uiOutput(ns("table"))

      )

    )
  )
}

#' load_xlsx Server Functions
#'
#' @noRd
mod_load_xlsx_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    loading_screen <- tagList(
      waiter::spin_3(),
      h4("Loading xlsx files...")
    )

    processing_screen <- tagList(
      waiter::spin_3(),
      h4("Processing...")
    )

    load <- reactiveValues(
      datalist= NULL
    )

    # Load example data
    observe({
      req(input$example)
      waiter::waiter_show(html = loading_screen,color = "ivory2")
      if (input$example == "beaver") {
        load$datalist <- data_demo_beaver
      } else if (input$example == "MIMIC_demo") {
        load$datalist <- data_demo_mini_mimic
      }
      waiter::waiter_hide()
      showNotification("Loading complelte", type = "message")
    }) %>% bindEvent(input$load_example_data)

    observe({
      waiter::waiter_show(html = loading_screen,color = "ivory2")
      load$datalist <- loadexcelbook(input$loadxlsx$datapath)
      waiter::waiter_hide()
      showNotification("Loading complelte", type = "message")
    }) %>% bindEvent(input$loadxlsx$datapath)

    namelist <- reactive({as.list(names(load$datalist))}) # datalist()

    output$table <- renderUI({
      req(load$datalist)
      req(input$preview_switch == T)
      uniquenames <- length(unique(names(load$datalist))) == length(names(load$datalist))
      if (uniquenames == F) {
        validate("Please avoid duplicate sheetnames")
      }

      tabs <- map(namelist(),
                    ~tabPanel(title = .x,
                              DT::DTOutput(ns(paste0("table_",.x)))))
      do.call(tabsetPanel,tabs)
    })

    observe({
      req(load$datalist)
      req(namelist)
      map(namelist(), ~ {
        output[[paste0("table_",.x)]] <- DT::renderDT({
          DT::datatable(load$datalist[[.x]],
                        editable = F,
                        rownames = F)
        })
      })
    })

    # Select tables and columns
    output$select_1 <- renderUI({
      req(load$datalist)
      req(length(unique(names(load$datalist))) == length(names(load$datalist)))

      shinyWidgets::pickerInput(inputId = ns("select_table"),label = "Select sheet",multiple = T,
                                choices = names(load$datalist),
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count > 3",
                                  `live-search` = T
                                ))
    })

    output$select_2 <- renderUI({
      req(load$datalist)
      req(length(unique(names(load$datalist))) == length(names(load$datalist)))

      shinyWidgets::pickerInput(inputId = ns("select_column"),label = "Select column",multiple = T,
                                choices = unique(unlist(lapply(load$datalist[input$select_table], colnames),
                                                        use.names = F)),
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count > 3",
                                  `live-search` = T
                                )
      )
    })
    output$select_3 <- renderUI({
      req(load$datalist)
      req(length(unique(names(load$datalist))) == length(names(load$datalist)))

      shinyWidgets::pickerInput(inputId = ns("select_id"),label = "Select match column",multiple = T,
                                choices = unique(unlist(lapply(load$datalist[input$select_table], colnames),
                                                        use.names = F)),
                                options = list(
                                  `actions-box` = TRUE,
                                  size = 10,
                                  `selected-text-format` = "count > 3",
                                  `live-search` = T
                                )
      )
    })
    output$select_4 <- renderUI({
      tagList(
      selectInput(ns("bind_type"),label = "Select merge method", multiple = F,
                  choices = c("no bind","match column","row bind")),
      uiOutput(ns("select_antijoin"))
      )

    })

# Generate the subUI for choosing the anti_join()
    output$select_antijoin <- renderUI({
      req(input$bind_type == "match column")
      selectInput(ns("anti_join"),label = "Choose join method", multiple = F,
                  choices = c("left join","anti join"),
                  selected = "left join")
    })

    # Declare the output to return
    toreturn <- reactiveValues(
      playgrounddata = NULL
    )

    # Perform initial check
    observe({
      req(!purrr::is_null(input$match_column_alert))
      if(input$match_column_alert == T){
        waiter::waiter_show(html = processing_screen,color = "ivory2")
        playdata <- sendtoplayground(
          datalist = load$datalist,
          table = input$select_table,
          column = input$select_column,
          id = input$select_id,
          anti = input$anti_join
        )
        dplyr::glimpse(playdata)
        toreturn$playgrounddata <- playdata
        waiter::waiter_hide()
        showNotification("Send to playground", type = "message")
      }
    }) %>% bindEvent(input$match_column_alert)

    # Generate the table to work with
    observe({
      if (input$bind_type == "match column") {
        waiter::waiter_show(html = processing_screen,color = "ivory2")
        if ((all(
          unlist(
            map(load$datalist[input$select_table],function(x){
              print(input$select_id)
              print(colnames(x))
              input$select_id %in% colnames(x)
            })
            ,use.names = F)
        ) == F)) {
          waiter::waiter_hide()
          showNotification("Please make sure the match column exists in every table", type = "error")
          validate("Please make sure the match column exists in every table")
        } else if (
          all(
            unlist(
              map(load$datalist[input$select_table],function(x){
                print(input$select_id)
                purrr::is_empty(union(
                  setdiff(colnames(x),
                          input$select_id),
                  setdiff(input$select_id,
                          colnames(x))
                ))
              })
              ,use.names = F))
        ) {
          waiter::waiter_hide()
          showNotification("Wrong match method", type = "error")
          validate("Wrong match method")
        } else if ((length(input$select_table) > 1 &
                   any(
                     unlist(
                       map(load$datalist[input$select_table],function(x){
                         print(input$select_id)
                         purrr::is_empty(union(
                           setdiff(colnames(x),
                                   input$select_id),
                           setdiff(input$select_id,
                                   colnames(x))
                         ))
                       })
                       ,use.names = F))
                   )
                   ){
          waiter::waiter_hide()
          showNotification("Wrong match column", type = "error")
          validate("Wrong match column")
        } else if (
        !all(input$select_id %in% input$select_column)){
          waiter::waiter_hide()
          showNotification("Wrong match column", type = "error")
          validate("Wrong match column")
        } else if(
          !Reduce('&',
                 lapply(map(load$datalist[input$select_table],function(x){
                   sapply(x %>% dplyr::select(all_of(input$select_id)),class)
                 }),
                function(x) is_same(x,map(load$datalist[input$select_table],function(x){
                   sapply(x %>% dplyr::select(all_of(input$select_id)),class)
                 })[[1]]))
            )
        ){
          waiter::waiter_hide()
          showNotification("Different match column type", type = "error")
          validate("Wrong match column type")
        } else if(purrr::is_null(input$select_id) &
                  !(length(input$select_table) == 1)){
          waiter::waiter_hide()
          showNotification("Please select match column", type = "error")
        } else if(purrr::is_null(input$select_column)){
          waiter::waiter_hide()
          showNotification("Please select column", type = "error")
        }
        req(input$select_column)
        req(input$select_id)

        # perform initial check here
        playdata_check <- sendtoplayground_check(
          datalist = load$datalist,
          table = input$select_table,
          column = input$select_column,
          id = input$select_id,
          anti = input$anti_join
        )

        # print playdata_check here
        print(playdata_check)
        if (class(playdata_check) != "data.frame"){
          if (playdata_check == "stop"){
            waiter::waiter_hide()
            showNotification("No shared id detected, please choose the right match column or consider other megre options", type = "error")
            validate("No shared ids, can not perform anti join")
          } else if  (playdata_check == "small table"){
            showNotification("Send to playground", type = "message")
            playdata <- sendtoplayground(
              datalist = load$datalist,
              table = input$select_table,
              column = input$select_column,
              id = input$select_id,
              anti = input$anti_join
            )
            toreturn$playgrounddata <- playdata
            waiter::waiter_hide()
          }
        } else {
          shinyalert::shinyalert(
            inputId = "match_column_alert",
            title = "Estimated row number",
            text = paste0("This will generate about: ",
                          10*nrow(playdata_check),
                          " rows.",
                          " For large table, try transforming your table into a wide one using the 'spread rows to columns' function from the row opertaions"),
            showConfirmButton = T,
            showCancelButton = T,
            confirmButtonCol = "2B88FF",
            cancelButtonText = "Cancel"
          )
          waiter::waiter_hide()
        }
      } else if(input$bind_type == "row bind") {
        waiter::waiter_show(html = processing_screen,color = "ivory2")
        if (identical(
          unique(unlist(lapply(load$datalist[input$select_table], colnames),
                        use.names = F)),
          Reduce(dplyr::intersect,map(load$datalist[input$select_table], colnames))
        ) == F) {
          waiter::waiter_hide()
          showNotification("Please make sure column names are the same for each table", type = "error")
          validate("Please make sure column names are the same for each table")
        } else if(purrr::is_null(input$select_column)){
          waiter::waiter_hide()
          showNotification("Please select column", type = "error")
          validate("No column provided")
        }
        req(input$select_column)
        showNotification("Send to playground", type = "message")
        playdata <- sendtoplayground_rbind(
          datalist = load$datalist,
          table = input$select_table,
          column = input$select_column,
          id = input$select_id
        )
        dplyr::glimpse(playdata)
        toreturn$playgrounddata <- playdata
        waiter::waiter_hide()

      } else if(input$bind_type == "no bind") {
        waiter::waiter_show(html = processing_screen,color = "ivory2")
        if(length(input$select_table) > 1){
          waiter::waiter_hide()
          print(input$select_table)
          print(length(input$select_table))
          print(names(input$select_table))
          showNotification("Please select only one table when using 'no bind' method", type = "error")
          validate("Please select only one table when using 'no bind' method")

        } else if(purrr::is_null(input$select_column)){
          waiter::waiter_hide()
          showNotification("Please select column", type = "error")
          validate("No column provided")
        }
        req(input$select_column)
        showNotification("Send to playground", type = "message")
        playdata <- sendtoplayground_rbind(
          datalist = load$datalist,
          table = input$select_table,
          column = input$select_column,
          id = input$select_id
        )
        dplyr::glimpse(playdata)
        toreturn$playgrounddata <- playdata
        waiter::waiter_hide()
      }
    }) %>% bindEvent(input$send2playground)

    return(toreturn)

  })
}

## To be copied in the UI
# mod_load_xlsx_ui("load_xlsx_1")

## To be copied in the server
# mod_load_xlsx_server("load_xlsx_1")
