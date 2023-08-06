#' table_playground UI Function
#'
#' @description A Module for Data Manipulation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_playground_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
       h3("Table preview and preprocessing:"),
       shinyWidgets::dropdown(
         h3("Table summary"),
         verbatimTextOutput(ns("summary"), placeholder = TRUE),
         uiOutput(ns("col_type")),
         status = "primary",
         icon = icon("gear"),
         style = "float"
       )
       ,

    h3("Variable summary"),
    shinyWidgets::dropdown(
      tags$h3("Check single variable"),
      uiOutput(ns("summary_col")),
      status = "primary",
      icon = icon("search"),
      style = "float"
    ),

        # Add select
        h3("Data manipulation："),
        actionButton(ns("add_select_col"),"Column operation"),
        actionButton(ns("add_select_row"),"Row operation"),
        actionButton(ns("undo"),label = "Reload"),
        actionButton(ns("save_temp"),label = "Backup"),
        actionButton(ns("recovery"),label = "Recover"),
        # Function space
        uiOutput(ns("col_function")),
        uiOutput(ns("col_function_vars")),
        uiOutput(ns("row_function")),
        uiOutput(ns("row_function_vars")),
        # Export here
        uiOutput(ns("select_1")),
        downloadButton(ns("download_1"),"Export",
                       style = "background:#D82310;border:#A81C0C")
      ),
      mainPanel(
        DT::DTOutput(ns("preview_playground"))
      )
    )
  )
}

#' table_playground Server Functions
#'
#' @noRd
mod_table_playground_server <- function(id,df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Retrieving data from last module and renderDataTable
    toreturn <- reactiveValues()
    load_table_screen <- tagList(
      waiter::spin_3(),
      h4("Loading table...")
    )

    observe({
      waiter::waiter_show(html = load_table_screen,color = "ivory2")
      toreturn$inport <- df$playgrounddata
      toreturn$origin <- df$playgrounddata
      waiter::waiter_hide()
    })

    output$"preview_playground" <- DT::renderDT({
      DT::datatable(toreturn$inport, editable = F, rownames = F)
      })

    # Summary of table
    # customize output here
    output$summary <- renderPrint({
      req(toreturn$inport)
      skimr::skim(toreturn$inport)
    })

    # Summary of column
    output$summary_col <- renderUI({
      tagList(
        selectInput(ns("select_column_summary"),label = "select the column",multiple = F,
                    choices = colnames(toreturn$inport)),
        actionButton(ns("preview_column"), label = "preview"),
        verbatimTextOutput(ns("column_info"),placeholder = F)
      )
    })

    output$column_info <- renderPrint({
      req(input$select_column_summary)
      if (is.character(toreturn$inport %>% pull(.data[[input$select_column_summary]]) )) {
        toreturn$inport %>% janitor::tabyl(.data[[input$select_column_summary]])
      } else if (is.numeric(toreturn$inport %>% pull(.data[[input$select_column_summary]]))) {
        toreturn$inport %>% skimr::skim(.data[[input$select_column_summary]])
      } else if(lubridate::is.POSIXt(toreturn$inport %>% pull(.data[[input$select_column_summary]])))  {
        range(toreturn$inport %>% pull(.data[[input$select_column_summary]]))
      }
    }) %>% bindEvent(input$preview_column)

    # Column type
    output$col_type <- renderUI({
      tagList(
        h3("Transform column type："),
        shinyWidgets::pickerInput(ns("col_numeric"),label = "As numeric",multiple = T,
                    choices = colnames(toreturn$inport),
                    options = list(
                      `actions-box` = TRUE,
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = T
                    )),
        shinyWidgets::pickerInput(ns("col_character"),label = "As character",multiple = T,
                    choices = colnames(toreturn$inport),
                    options = list(
                      `actions-box` = TRUE,
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = T
                    )),
        shinyWidgets::pickerInput(ns("col_ymd"),label = "As ymd",multiple = T,
                    choices = colnames(toreturn$inport),
                    options = list(
                      `actions-box` = TRUE,
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = T
                    )),
        shinyWidgets::pickerInput(ns("col_ymdhms"),label = "As ymd_hms",multiple = T,
                    choices = colnames(toreturn$inport),
                    options = list(
                      `actions-box` = TRUE,
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = T
                    )),
        actionButton(ns("col_type_apply"), label = "Apply"),
      )
    })

    # Type convert
    observe({
      if (!is.null(input$col_numeric)) {
        toreturn$inport <- toreturn$inport %>% dplyr::mutate(across(input$col_numeric, as.numeric))
      } else if (!is.null(input$col_character)) {
        toreturn$inport <- toreturn$inport %>% dplyr::mutate(across(input$col_character, as.character))
      } else if (!is.null(input$col_ymd)) {
        toreturn$inport <- toreturn$inport %>% dplyr::mutate(across(input$col_ymd, lubridate::ymd))
      } else if(!is.null(input$col_ymdhms)) {
        toreturn$inport <- toreturn$inport %>% dplyr::mutate(across(input$col_ymdhms, lubridate::ymd_hms))
      }
    }) %>% bindEvent(input$col_type_apply)

    # Select_1 for export
    output$select_1 <- renderUI({
      tagList(
        h3("Export table："),
        shinyWidgets::pickerInput(ns("select_column_1"),label = "Select column",multiple = T,
                    choices = colnames(toreturn$inport),
                    options = list(
                      `actions-box` = TRUE,
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = T
                    ))
      )
    })

    # Reload
    undo_screen <- tagList(
      waiter::spin_3(),
      h4("Reloading...")
    )

    observe({shinyalert::shinyalert(
      inputId = "reload_alert",
      title = "Reload table",
      text = "This operation will bring the table back to the initial version",
      showConfirmButton = T,
      showCancelButton = T,
      confirmButtonCol = "2B88FF"
      )}) %>% bindEvent(input$undo)

    observe({
      req(input$reload_alert)
      if(input$reload_alert == T) {
        waiter::waiter_show(html = undo_screen,color = "ivory2")
        toreturn$inport <- toreturn$origin
        waiter::waiter_hide()
        showNotification("Reload complete", type = "message")
      }
    })

    # Save_temporarily
    savebackup_screen <- tagList(
      waiter::spin_3(),
      h4("Backingup current table...")
    )

    observe({shinyalert::shinyalert(
      inputId = "backup_alert",
      title = "Backup table",
      text = "Backup the current table and overwrite previous backup",
      showConfirmButton = T,
      showCancelButton = T,
      confirmButtonCol = "2B88FF"
    )}) %>% bindEvent(input$save_temp)


    recovery_screen <- tagList(
      waiter::spin_3(),
      h4("Recovering from backup...")
    )

    observe({shinyalert::shinyalert(
      inputId = "recover_alert",
      title = "Recover table",
      text = "Recover table to the last backup",
      showConfirmButton = T,
      showCancelButton = T,
      confirmButtonCol = "2B88FF"
    )}) %>% bindEvent(input$recovery)


    save_temp <- reactiveValues(
      backup = NULL
    )

    observe({
      req(input$backup_alert)
      if(input$backup_alert == T){
        waiter::waiter_show(html = savebackup_screen,color = "ivory2")
        save_temp$backup <- toreturn$inport
        waiter::waiter_hide()
        showNotification("Backup complete", type = "message")
      }
    }) %>% bindEvent(input$backup_alert)

    # Recovery from the backup
    observe({
      req(input$recover_alert)
      if(input$recover_alert == T){
        waiter::waiter_show(html = recovery_screen,color = "ivory2")
        toreturn$inport <- save_temp$backup
        waiter::waiter_hide()
        showNotification("Table restored", type = "message")
      }
    })

    # Download table
    exporttable_screen <- tagList(
      waiter::spin_3(),
      h4("Exporting table...")
    )

    observe({
      req(input$select_column_1)
      output$download_1 <- downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "_EasyMergeR_output", ".xlsx")
        },
        content = function(file) {
          toreturn$export_start_signal <- "start"
          waiter::waiter_show(html = exporttable_screen, color = "ivory2")
          toreturn$inport %>% dplyr::select(all_of(input$select_column_1)) %>% distinct() %>% openxlsx::write.xlsx(., file)
          toreturn$export_end_singal <- "exported"
          waiter::waiter_hide()
          showNotification("Exporting table complete", type = "message")
        }
      )
    })




    # col_Function here -- add option in UI--------------------------------------------------(Step1)
    observe({
      output$col_function <- renderUI({
        shinyWidgets::pickerInput(ns("select_col_function"), label = "select Column operation",multiple = F,
                    choices = c(
                      "bind columns",
                      "batch select columns",
                      "rename column",
                      "split column",
                      "gather column",
                      "check duplicate column"

                    ),
                    options = list(
                      `actions-box` = TRUE,
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = T
                    ))


      })
    })%>% bindEvent(input$add_select_col)

    # col_Function logic here--------------------------------------------------(Step3)
    observe({ ## bind columns -- function logic
      if(stringr::str_length(input$unite_name) == 0){t <- "New_column"} else {t <- input$unite_name}
      sep <- input$unite_sep
      toreturn$inport <- toreturn$inport %>% tidyr::unite(
        col = {{t}},
        all_of(input$unite_col),
        sep = {{sep}}
      )

    }) %>% bindEvent(input$unite_apply)

    observe({ ## batch select columns -- function logic
      t <- input$batch_select_match
      if (stringr::str_length(t) == 0) {
        toreturn$inport <- toreturn$inport %>% dplyr::select(input$batch_select_keep)
      } else if (input$batch_select_condition == "contain" &
                stringr::str_length(t) != 0) {
        toreturn$inport <- toreturn$inport %>% dplyr::select(input$batch_select_keep, contains(t))
      } else if (input$batch_select_condition == "not contain" &
                 stringr::str_length(t) != 0) {
        toreturn$inport <- toreturn$inport %>% dplyr::select(input$batch_select_keep, !contains(t))
      }

    }) %>% bindEvent(input$batch_select_apply)

    observe({ ## rename column -- function logic
      print(input$rename_name)
      print(input$rename_col)
      toreturn$inport <- toreturn$inport %>% dplyr::rename(
        (!!input$rename_name) := (!!input$rename_col)
      )
    }) %>% bindEvent(input$rename_apply)

    observe({ ## split column(UI) -- function logic
      if (input$splittype_col == "location") {
        output$splitcolUI <- renderUI({
          tagList(
          numericInput(ns("split_position_col"), label = "input split location",
                       min = 1,value = 1),
          selectInput(ns("split_position_fill"),choices = c("left","right"),label = "choose fill direction")
          )
        })
      } else if(input$splittype_col == "character")  {
        output$splitcolUI <- renderUI({
          tagList(
            textInput(ns("split_sym_col"), label = "input split character"),
            selectInput(ns("split_character_fill"),choices = c("left","right"),label = "choose fill direction")
          )
        })
      }
    }) %>% bindEvent(input$splittype_col)

    observe({ ## split column(Function) -- function logic
      if (input$splittype_col == "location") {
        toreturn$inport <- toreturn$inport %>% tidyr::separate(
          col = input$split_col,
          into = paste("New_col",1:2, sep = "_"),
          sep = input$split_position_col,
          fill = input$split_position_fill
        )
      } else if(input$splittype_col == "character")  {
        t <- (toreturn$inport %>% pull(input$split_col))[1]
        toreturn$inport <- toreturn$inport %>% tidyr::separate(
          col = input$split_col,
          into = paste("New_col",
                       1:(str_count(t,
                                    pattern = input$split_sym_col) + 1),
                       sep = "_"),
          sep = input$split_sym_col,
          fill = input$split_character_fill
        )
      }
    }) %>% bindEvent(input$colsplit_apply)

    observe({ ## gather column(Function) -- function logic
      print(input$longer_name)
      print(input$longer_value)
      toreturn$inport <- toreturn$inport %>% tidyr::pivot_longer(
        cols = input$longer_col,
        names_to = input$longer_name,
        values_to = input$longer_value
      )
    }) %>% bindEvent(input$longer_apply)

    observe({ ## check duplicate column(Function) -- function logic
      duplicate_column <- duplicated(as.list(
        toreturn$inport %>%
          select(all_of(c(input$check_duplicate_match_col,input$check_duplicate_col)))
      ))
      if (any(duplicate_column)){
        shinyalert::shinyalert("Duplicate detected",
                               c(input$check_duplicate_match_col,input$check_duplicate_col)[duplicate_column],
                               confirmButtonCol = "2B88FF",
                               type = "warning")
      } else {
        shinyalert::shinyalert("No duplicate detected",
                               "No duplicate column found, try filter and check",
                               confirmButtonCol = "2B88FF",
                               type = "info"

        )
      }
    }) %>% bindEvent(input$check_duplicate_col_apply)



    # row_Function here -- add option in UI--------------------------------------------------(Step1)
    observe({
      output$row_function <- renderUI({
        shinyWidgets::pickerInput(ns("select_row_function"), label = "select Row operation",multiple = F,
                    choices = c(
                      "judge value",
                      "match string",
                      "keep (remove) NA",
                      "remove duplicate",
                      "update row value",
                      "cut string length",
                      "spread rows to columns",
                      "replace strings"

                    ),
                    options = list(
                      `actions-box` = TRUE,
                      size = 10,
                      `selected-text-format` = "count > 3",
                      `live-search` = T
                    ))

      })
    })%>% bindEvent(input$add_select_row)

    # row_Function logic here--------------------------------------------------(Step3)

    observe({ ## judge value(UI) A-- function logic
      if(input$judge_value_condition == "equal to") {
        output$judge_value_UI <- renderUI({
          tagList(
            selectInput(ns("equals_to_row"),
                        label = "select column",
                        multiple = T,
                        choices = colnames(toreturn$inport)),
            selectInput(ns("equals_to_row_condition"),
                        label = "select method",
                        multiple = F,
                        choices = c("column vs. column",
                                    "column vs. input")),
            uiOutput(ns("equals_to_row_UI"))

          )
        })

      } else if(input$judge_value_condition == "larger than") {
        output$judge_value_UI <- renderUI({
          tagList(
            selectInput(ns("larger_than_row"),
                        label = "select column",
                        multiple = T,
                        choices = colnames(toreturn$inport)),
            selectInput(ns("larger_than_row_condition"),
                        label = "keep or drop?",
                        choices = c("keep","drop")),
            selectInput(ns("larger_than_row_any_all"),
                        label = "apply for any one or all?",
                        choices = c("any","all")),
            numericInput(ns("larger_than_row_value"),label = "input the value",value = "0")
          )
        })
      } else if(input$judge_value_condition == "less than") {
        output$judge_value_UI <- renderUI({
          tagList(
            selectInput(ns("less_than_row"),
                        label = "select column",
                        multiple = T,
                        choices = colnames(toreturn$inport)),
            selectInput(ns("less_than_row_condition"),
                        label = "keep or drop?",
                        choices = c("keep","drop")),
            selectInput(ns("less_than_row_any_all"),
                        label = "apply for any one or all?",
                        choices = c("any","all")),
            numericInput(ns("less_than_row_value"),label = "input the value",value = "0")
          )
        })
      }
    }) %>% bindEvent(input$judge_value_condition)

     observe({ ## judge value(UI) B-- function logic

          if(input$equals_to_row_condition == "column vs. column") {
            output$equals_to_row_UI <- renderUI({
              tagList(
                selectInput(ns("equals_to_row_by_column"),label = "select the source column",multiple = F,
                            choices = colnames(toreturn$inport)),
                selectInput(ns("equals_to_row_by_column_keepORdrop"),label = "Keep or drop the match?",multiple = F,
                            choices = c("keep","drop"))

              )
            })
          } else if(input$equals_to_row_condition == "column vs. input")  {
            output$equals_to_row_UI <- renderUI({
              tagList(
                textInput(ns("equals_to_row_name"),label = "input match value"),
                selectInput(ns("equals_to_row_by_column_keepORdrop"),label = "Keep or drop the match?",multiple = F,
                            choices = c("keep","drop"))
              )
            })
          }


     }) %>% bindEvent(input$equals_to_row_condition)

    observe({ ## judge value   -- function logic
      if (input$judge_value_condition == "equal to") {
      if(input$equals_to_row_condition == "column vs. input"){
        if(input$equals_to_row_by_column_keepORdrop == "keep") {
          toreturn$inport <- filter_multiple_byString(toreturn$inport,
                                                      input$equals_to_row,
                                                      input$equals_to_row_name,
                                                      keep = T)
        } else {
          toreturn$inport <- filter_multiple_byString(toreturn$inport,
                                                      input$equals_to_row,
                                                      input$equals_to_row_name,
                                                      keep = F)
        }
      } else if(input$equals_to_row_condition == "column vs. column"){

        if(input$equals_to_row_by_column %in% input$equals_to_row){
          showNotification("Please exlcude source column from the 'select column'",type = "warning")
          validate("Source column in the targets")
        } else {
          if(input$equals_to_row_by_column_keepORdrop == "keep") {
            toreturn$inport <- filter_multiple_byColumn(toreturn$inport,
                                                        input$equals_to_row,
                                                        input$equals_to_row_by_column,
                                                        keep = T)
          } else {
            toreturn$inport <- filter_multiple_byColumn(toreturn$inport,
                                                        input$equals_to_row,
                                                        input$equals_to_row_by_column,
                                                        keep = F)
          }
        }
      }
      } else if(input$judge_value_condition == "larger than") {
        if(input$larger_than_row_condition == "keep"&
           input$larger_than_row_any_all == "any"){
        toreturn$inport <- compare_multiple_larger_than(df = toreturn$inport,
                                                    columns = input$larger_than_row,
                                                    value = input$larger_than_row_value,
                                                    keep = T,
                                                    any = T)}
        else if(input$larger_than_row_condition == "keep"&
                input$larger_than_row_any_all == "all"){
          toreturn$inport <- compare_multiple_larger_than(df = toreturn$inport,
                                                          columns = input$larger_than_row,
                                                          value = input$larger_than_row_value,
                                                          keep = T,
                                                          any = F)
        } else if(input$larger_than_row_condition == "drop"&
                          input$larger_than_row_any_all == "any"){
          toreturn$inport <- compare_multiple_larger_than(df = toreturn$inport,
                                                          columns = input$larger_than_row,
                                                          value = input$larger_than_row_value,
                                                          keep = F,
                                                          any = T)
        } else if(input$larger_than_row_condition == "drop"&
                          input$larger_than_row_any_all == "all"){
          toreturn$inport <- compare_multiple_larger_than(df = toreturn$inport,
                                                          columns = input$larger_than_row,
                                                          value = input$larger_than_row_value,
                                                          keep = F,
                                                          any = F)
        }

      } else if(input$judge_value_condition == "less than"){
        if(input$less_than_row_condition == "keep"&
           input$less_than_row_any_all == "any"){
          toreturn$inport <- compare_multiple_less_than(df = toreturn$inport,
                                                          columns = input$less_than_row,
                                                          value = input$less_than_row_value,
                                                          keep = T,
                                                          any = T)}
        else if(input$less_than_row_condition == "keep"&
                input$less_than_row_any_all == "all"){
          toreturn$inport <- compare_multiple_less_than(df = toreturn$inport,
                                                          columns = input$less_than_row,
                                                          value = input$less_than_row_value,
                                                          keep = T,
                                                          any = F)
        } else if(input$less_than_row_condition == "drop"&
                  input$less_than_row_any_all == "any"){
          toreturn$inport <- compare_multiple_less_than(df = toreturn$inport,
                                                          columns = input$less_than_row,
                                                          value = input$less_than_row_value,
                                                          keep = F,
                                                          any = T)
        } else if(input$less_than_row_condition == "drop"&
                  input$less_than_row_any_all == "all"){
          toreturn$inport <- compare_multiple_less_than(df = toreturn$inport,
                                                          columns = input$less_than_row,
                                                          value = input$less_than_row_value,
                                                          keep = F,
                                                          any = F)
        }
      }


    }) %>% bindEvent(input$judge_value_apply)

    observe({ ## match string-- function logic
      t <- input$row_match_text
      if (input$row_match_condition == "contain") {
        toreturn$inport <- multiple_col_match(toreturn$inport,input$row_match_column,t,contain = T)
      } else if (input$row_match_condition == "not contain") {
        toreturn$inport <- multiple_col_match(toreturn$inport,input$row_match_column,t,contain = F)
      }

    }) %>% bindEvent(input$row_match_apply)

    observe({ ## keep (remove) NA-- function logic
      if (input$row_NA_condition == "keep NA") {
        print(input$row_NA_columns)
        print(paste0("'",input$row_NA_columns,"'","%in% NA ",collapse = "& "))
        toreturn$inport <- toreturn$inport %>% dplyr::filter(eval(parse(text = paste0("`",input$row_NA_columns,"`","%in% NA ",collapse = "& ") )))
      } else if (input$row_NA_condition == "remove NA")  {
        print(input$row_NA_columns)
        print(paste0("!","'",input$row_NA_columns,"'","%in% NA ",collapse = "& "))
        toreturn$inport <- toreturn$inport %>% dplyr::filter(eval(parse(text = paste0("!","`",input$row_NA_columns,"`","%in% NA ",collapse = "& ") )))
      }
    }) %>% bindEvent(input$row_NA_apply)

    observe({ ## remove duplicate-- function logic
      print(input$row_rm_duplicate_col)
      toreturn$inport <- toreturn$inport %>% dplyr::distinct(across(input$row_rm_duplicate_col))
      showNotification("Duplicate removed", type = "message")
    }) %>% bindEvent(input$row_rm_duplicate_apply)

    observe({ ## update row value(UI)-- function logic
      if(input$row_coalesce_condition == "update NAs") {
        output$row_coalesce_UI <- renderUI({
          tagList(
            selectInput(ns("row_coalesce_NA_a"),label = "select column a",multiple = F,
                        choices = colnames(toreturn$inport)),
            selectInput(ns("row_coalesce_NA_b"),label = "select column b",multiple = F,
                        choices = colnames(toreturn$inport))
          )
        })
      } else if(input$row_coalesce_condition == "update with priority")  {
        output$row_coalesce_UI <- renderUI({
          tagList(
            selectInput(ns("row_coalesce_target_multi"),label = "select column",multiple = T, # name clash?
                        choices = colnames(toreturn$inport)),
            uiOutput(ns("row_coalesce_sub_UI_multi"))
          )
        })
      }
    }) %>% bindEvent(input$row_coalesce_condition)

    observe({ ## update row value(Sub-UI)-- function logic
      output$row_coalesce_sub_UI_multi <- renderUI({
        print(input$row_coalesce_target_multi)
        print(length(input$row_coalesce_target_multi))

        row_coalesce_target_list <- input$row_coalesce_target_multi
        # generate sub-UI here
        map(input$row_coalesce_target_multi,
            ~ selectInput(inputId =  ns(paste0(.x,"_row_coalesce_sub_UI_multi_", # ns() here is important for accessing the value
                                               which(input$row_coalesce_target_multi == .x))),
                          label = paste0("select the level: ",
                                         which(input$row_coalesce_target_multi == .x)," priority"),
                          multiple = F,
                          choices = input$row_coalesce_target_multi))
      })
    }) %>% bindEvent(input$row_coalesce_target_multi)

    updatecols <- reactiveValues(
      cols = NULL
    )

    observe({
      updatecols$cols <- unlist(map(input$row_coalesce_target_multi,
                                    ~ paste0("input$",
                                             .x,
                                             "_row_coalesce_sub_UI_multi_",
                                             which(input$row_coalesce_target_multi == .x)
                                    ))# the map function doesn't return valid values
                                ,use.names = F
      )

    })


    observe({ ## update row value(Function)-- function logic
      if(input$row_coalesce_condition == "keep (remove) NA") {
        toreturn$inport <- toreturn$inport %>% mutate("update NAs" = coalesce(.data[[input$row_coalesce_NA_a]],.data[[input$row_coalesce_NA_b]]))
      } else if(input$row_coalesce_condition == "update with priority"){
        print(unlist(map(input$row_coalesce_target_multi,
                         ~ paste0("input$",
                                  .x,
                                  "_row_coalesce_sub_UI_multi_",
                                  which(input$row_coalesce_target_multi == .x)
                         ))
                     ,use.names = F
        ))

        print("str below")
        toreturn$inport <- update_dataframe_mulitple(toreturn$inport,
                                                     c(unlist(map(
                                                       input$row_coalesce_target_multi,
                                                       ~ eval(parse(
                                                         text = paste0(
                                                           "input$",
                                                           .x,
                                                           "_row_coalesce_sub_UI_multi_",
                                                           which(input$row_coalesce_target_multi == .x)
                                                         )
                                                       ))
                                                     )# the map function doesn't return valid values
                                                     , use.names = F))
        )
      }
    })%>% bindEvent(input$row_coalesce_apply)

    #
    observe({ ## cut string length(UI)-- function logic
      if(input$row_length_condition == "location") {
        output$row_length_UI <- renderUI({
          tagList(
            selectInput(ns("row_length_target"),label = "select column",multiple = F,
                        choices = colnames(toreturn$inport)),
            numericInput(ns("row_length_anchor"),label = "input anchor location",1,min = 1,max = 999
            ),
            numericInput(ns("row_length_num"),label = "input length",1,min = 0,max = 999
            )
          )
        })
      } else if(input$row_length_condition == "character")  {
        output$row_length_UI <- renderUI({
          tagList(
            selectInput(ns("row_length_target"),label = "select column",multiple = F,
                        choices = colnames(toreturn$inport)),
            textInput(ns("row_length_text"),label = "input anchor character"),
            numericInput(ns("row_length_num"),label = "input length",1,min = 0,max = 999
            )
          )
        })
      }
    }) %>% bindEvent(input$row_length_condition)

    observe({ ## cut string length(Function)-- function logic
      if(input$row_length_condition == "location") {
        print(c(input$row_length_target))
        print(input$row_length_anchor)
        print(input$row_length_num)

        toreturn$inport <- toreturn$inport %>% mutate("New_col" = stringr::str_sub(
          .data[[input$row_length_target]],
          start = as.numeric(input$row_length_anchor),
          end = sum(as.numeric(input$row_length_anchor),
                    as.numeric(input$row_length_num))
        ))

      } else if(input$row_length_condition == "character"){
        print(c(input$row_length_target))
        print(input$row_length_text)
        print(input$row_length_num)

        toreturn$inport <- extract_wrap(toreturn$inport,
                                        input$row_length_target,
                                        input$row_length_text,
                                        input$row_length_num)
      }
    })%>% bindEvent(input$row_length_apply)

    observe({#spread rows to columns(Function)-- function logic
      print(input$row_wider_name)
      print(input$row_wider_value)
      toreturn$inport <- toreturn$inport %>% tidyr::pivot_wider(
        names_from = input$row_wider_name,
        values_from = input$row_wider_value
      )
    }) %>% bindEvent(input$row_wider_apply)

    observe({#replace strings(Function)-- function logic
      toreturn$inport <- toreturn$inport %>% mutate(across(
        input$row_replace_target,
        ~ stringr::str_replace_all(.x,
                                   pattern = input$row_replace_match,
                                   replacement = input$row_replace_replace)
      ))
    }) %>% bindEvent(input$row_replace_apply)


    # Generate Function variables input (UI)--------------------------------------------------(Step2)

    # Column
    output$col_function_vars <- renderUI({

      req(input$add_select_col)
      if (input$select_col_function == "bind columns") { # col_Function_1: bind columns
        tagList(
          selectInput(ns("unite_col"),label = "select column" ,multiple = T,
                      choices = colnames(toreturn$inport)),
          textInput(ns("unite_sep"),label = "input separation character"),
          textInput(ns("unite_name"),label = "input new column name"),
          actionButton(ns("unite_apply"),label = "Apply")
        )

      } else if(input$select_col_function == "batch select columns") { # col_Function_2: batch select columns
        tagList(
          selectInput(ns("batch_select_condition"), label = "select condition", multiple = F,
                      choices = c("contain","not contain")),
          textInput(ns("batch_select_match"),label = "input match string"),
          selectInput(ns("batch_select_keep"),label = "select exception column" ,multiple = T,
                      choices = colnames(toreturn$inport)),
          actionButton(ns("batch_select_apply"),label = "Apply")
        )
      } else if(input$select_col_function == "rename column") { # col_Function_3: rename column
        tagList(
          selectInput(ns("rename_col"),label = "select column" ,multiple = F,
                      choices = colnames(toreturn$inport)),
          textInput(ns("rename_name"),label = "input new column name"),
          actionButton(ns("rename_apply"),label = "Apply")
        )
      } else if(input$select_col_function == "split column") { # col_Function_4: split column
        tagList(
          selectInput(ns("split_col"),label = "select column" ,multiple = F,
                      choices = colnames(toreturn$inport)),
          selectInput(ns("splittype_col"),label = "select split type",multiple = F,
                      choices = c("location","character")),
          uiOutput(ns("splitcolUI")),
          actionButton(ns("colsplit_apply"),label = "Apply")
        )
      } else if(input$select_col_function == "gather column"){ # col_Function_5: gather column
        tagList(
          selectInput(ns("longer_col"),label = "select column" ,multiple = T,
                      choices = colnames(toreturn$inport)),
          textInput(ns("longer_name"),label = "input new name column name"),
          textInput(ns("longer_value"),label = "input new value column name"),
          actionButton(ns("longer_apply"),label = "Apply")
        )
      } else if(input$select_col_function == "check duplicate column"){ # col_Function_6: check duplicate column
        tagList(
          selectInput(ns("check_duplicate_col"),label = "select column" ,multiple = T,
                      choices = colnames(toreturn$inport)),
          selectInput(ns("check_duplicate_match_col"),label = "select identifier column" ,multiple = T,
                      choices = colnames(toreturn$inport)),
          actionButton(ns("check_duplicate_col_apply"),label = "Apply")
        )
      }

    })

    # Row
    output$row_function_vars <- renderUI({

      req(input$add_select_row)
      if (input$select_row_function == "judge value") {  # row_Function_1: judge value
        tagList(
          selectInput(ns("judge_value_condition"),label = "choose row value judging method",choices = c("equal to","larger than","less than")),
          # Write a rendUI wrapper here!
          uiOutput(ns("judge_value_UI")),
          # Wrap the code below

          # selectInput(ns("equals_to_row"),label = "select column" ,multiple = T,
          #             choices = colnames(toreturn$inport)),
          # selectInput(ns("equals_to_row_condition"),label = "select method" ,multiple = F,
          #             choices = c("column vs. column","column vs. input")),
          # uiOutput(ns("equals_to_row_UI")),

          actionButton(ns("judge_value_apply"),label = "Apply")
        )

      } else if(input$select_row_function == "match string") { # row_Function_2: match string
        tagList(
          selectInput(ns("row_match_condition"), label = "select condition", multiple = F,
                      choices = c("contain","not contain")),
          textInput(ns("row_match_text"),label = "input match string"),
          selectInput(ns("row_match_column"),label = "select columns" ,multiple = T,
                      choices = colnames(toreturn$inport)),
          actionButton(ns("row_match_apply"),label = "Apply")
        )

      } else if(input$select_row_function == "keep (remove) NA") { # row_Function_3: keep (remove) NA
        tagList(
          selectInput(ns("row_NA_condition"), label = "select condition", multiple = F,
                      choices = c("remove NA","keep NA")),
          selectInput(ns("row_NA_columns"),label = "select column" ,multiple = T,
                      choices = colnames(toreturn$inport)),
          actionButton(ns("row_NA_apply"),label = "Apply")
        )
      } else if (input$select_row_function == "remove duplicate") {# row_Function_5: remove duplicate
        tagList(
          shinyWidgets::pickerInput(ns("row_rm_duplicate_col"),label = "select columns",multiple = T,
                                    choices = colnames(toreturn$inport),
                                    options = list(
                                      `actions-box` = TRUE,
                                      size = 10,
                                      `selected-text-format` = "count > 3",
                                      `live-search` = T
                                    )),
          actionButton(ns("row_rm_duplicate_apply"),label = "Apply")
        )
      } else if(input$select_row_function == "update row value") {# row_Function_6: update row value
        tagList(
          selectInput(ns("row_coalesce_condition"),label = "select update condition",multiple = F,
                      choices = c("update NAs",
                                  "update with priority")),
          uiOutput(ns("row_coalesce_UI")),
          actionButton(ns("row_coalesce_apply"),label = "Apply")
        )
      } else if(input$select_row_function == "cut string length"){# row_Function_6: cut string length
        tagList(
          selectInput(ns("row_length_condition"),label = "select condition",multiple = F,
                      choices = c("location","character")),
          uiOutput(ns("row_length_UI")),
          actionButton(ns("row_length_apply"),label = "Apply")
        )
      } else if (input$select_row_function == "spread rows to columns") {# row_Function_7: spread rows to columns
        tagList(
          selectInput(ns("row_wider_name"),label = "select column",multiple = F,
                      choices = colnames(toreturn$inport)),
          selectInput(ns("row_wider_value"),label = "select value column",multiple = T,
                      choices = colnames(toreturn$inport)),
          actionButton(ns("row_wider_apply"),label = "Apply")
        )
      } else if(input$select_row_function == "replace strings"){# row_Function_8: replace strings
        tagList(
          selectInput(ns("row_replace_target"),label = "select column",multiple = T,
                      choices = colnames(toreturn$inport)),
          textInput(ns("row_replace_match"),"input match string"),
          textInput(ns("row_replace_replace"),"input replace string"),
          actionButton(ns("row_replace_apply"),label = "Apply")
        )
      }


    })






  })
}

## To be copied in the UI
# mod_table_playground_ui("table_playground_1")

## To be copied in the server
# mod_table_playground_server("table_playground_1")
