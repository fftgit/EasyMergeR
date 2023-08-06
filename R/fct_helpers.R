#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# A function to load multiple xlsx files with multiple sheets
loadexcelbook <- function(x) {
  x <- as.list(x)
  t <- lapply(x, openxlsx::loadWorkbook)
  namels <- lapply(t, function(x){paste(openxlsx::sheets(x))})
  n <- length(t)
  sn <- length(unlist(namels))
  container <- list()

  for (i in 1:n) {

    names <- unlist(lapply(t[i], function(x){paste(openxlsx::sheets(x))}))
    end <- length(names)
    eval(rlang::call2("<-", rlang::sym(paste0("t_", i, "_list")), list()))
    for (k in 1:end) {
      eval(rlang::call2("<-",
                        rlang::parse_expr(paste0("t_", i, "_list", "[[", k, "]]")),
                        rlang::call2(
                          rlang::parse_expr("openxlsx::readWorkbook"),
                          rlang::parse_expr(paste0("t[[", i, "]]")),
                   sheet = k
                 )))
    }
    eval(rlang::call2("<-", rlang::parse_expr(paste0("container[[",i,"]]")),rlang::sym(paste0("t_",i,"_list"))))
  }

  datalist <- unlist(container,recursive = F)
  names(datalist) <- unlist(namels)
  return(datalist)
}

# A function to generate new dataframe given selected tables, columns, and id column
sendtoplayground <- function(datalist, table,column,id,anti="left join")  {
  datalist <- datalist
  playdatalist <- datalist[table]
  playdatalist <- lapply(playdatalist, function(x) {
    return(x %>% dplyr::select(any_of(column)) %>% distinct())
  })

  ifelse(anti == "left join",
         return(
           Reduce(
             function(x,y)  {x %>% dplyr::left_join(y %>% distinct(),by = id) %>% distinct()},
             playdatalist
           )),
         return(
           Reduce(
             function(x,y)  {x %>% dplyr::anti_join(y %>% distinct(),by = id) %>% distinct()},
             playdatalist
           )
         )
    )
}

# A minor change version of sendtoplayground
sendtoplayground_rbind <- function(datalist, table,column,id)  {
  datalist <- datalist
  playdatalist <- datalist[table]
  playdatalist <- map(playdatalist, .f = function(x){
    return(
      x %>% dplyr::select(any_of({{column}})) %>% distinct()
    )})
  return(
    Reduce(
      function(x,y)  {x %>% dplyr::bind_rows(y) %>% distinct()},
      playdatalist
    )
  )
}

# a function to extract samples from a table
sample_table <- function(x,rate,replace) {
  return(
    x %>% dplyr::slice_sample(
      prop = rate,
      replace = replace
    )
  )
}

# a function to judge the proper sample rate
judge_rate <- function(x){
  if (x < 1000){
    return(0.1)
  } else if (1000 <= x & x < 10000){
    return(0.01)
  } else {
    return(0.001)
  }
}


# a function to check columns before sending to background
sendtoplayground_check <- function(datalist, table,column,id,anti="left join")  {
  datalist <- datalist
  playdatalist <- datalist[table]
  playdatalist <- lapply(playdatalist, function(x) {
    return(x %>% dplyr::select(any_of(column)) %>% distinct())
  })
  if (anti == "left join") {  shared_ids <- Reduce(
    function(x,y)  {x %>% dplyr::semi_join(y, by = id) %>% distinct()},
    lapply(playdatalist,
           function(x){x %>% dplyr::select(id) %>%
               distinct()})
  )} else {
    shared_ids <- Reduce(
      function(x,y)  {x %>% dplyr::anti_join(y, by = id) %>% distinct()},
      lapply(playdatalist,
             function(x){x %>% dplyr::select(id) %>%
                 distinct()})
    )
  }
  n <- nrow(shared_ids)

  if(n == 0) {
    return("stop")
  } else if (n >= 10) {
    shared_ids_sample <- sample_table(shared_ids,judge_rate(n),F)
    playdatalist_sample <- lapply(playdatalist, function(x) {
      return(x %>% semi_join(shared_ids_sample) %>% distinct())
    })
    return(
      data_sample <- Reduce(
        function(x,y)  {x %>% dplyr::left_join(y %>% distinct(),by = id) %>% distinct()},
        playdatalist_sample
      )
    )
  } else {
    return("small table")
  }
}


# a function to update one column using data from another column
case_when_generator <- function(...){
  exp <- c()
  orders <- c(...)
  if(length(orders) == 1) {
    exp <- c(exp,paste0("!is.na(",list(...)[1],")~",list(...)[1]))
    return(exp)
  }
  else if (length(orders) >= 2 ) {

    for (x in seq_along(orders)) {
      if(x == 1) {
        exp <- c(exp,
                 paste0("!is.na(",orders[x],")~",orders[x])
        )
      } else if (x > 1){
        exp <- c(exp,
                 paste0("is.na(",orders[x-1],")","&",
                        "!is.na(",orders[x],")","~",orders[x]))
      }
    }
    return(exp)
  }
}

# a function to update row value given a predefined order of multiple columns
update_dataframe_mulitple <- function(df,...){
  print(paste(case_when_generator(...),
              collapse = ","))
  eval(parse(
    text = paste0("df%>%mutate('New_col'=","case_when(",
                  paste(case_when_generator(...), collapse = ","),
                  "))"
    )))

}

# a function to extract patterns and collapse them into comma separated character
extract_wrap <- function(df,target,text,num){
  # target = target column
  # text = the anchor text
  # num = the length
  df <- df %>% mutate("New_col" = str_extract_all(
    .data[[target]],
    pattern = paste0(text,".","{",num,"}")
  )) %>% rowwise() %>% mutate(
    "New_col" = paste(unlist(New_col,use.names = F),
                      collapse = ",")
  )
}

# a function to match strings in multiple columns
multiple_col_match <- function(df,columns,string,contain = T){
  if (contain == T) {
    df %>% filter(if_all(
      {{columns}},
      function(x){
        str_detect(x,
                   pattern = string)
      }
    ))
  } else if (contain == F) {
    df %>% filter(if_all(
      {{columns}},
      function(x){
        !str_detect(x,
                    pattern = string)
      }
    ))
  }
}

# Two functions for "equals to" with multiple columns()
filter_multiple_byString <- function(df,columns,string,keep = T){
  if (keep == T) {
    df %>% filter(if_all(
      {{columns}},
      function(x){
        x == string
      }
    ))
  } else if (keep == F) {
    df %>% filter(if_all(
      {{columns}},
      function(x){
        x != string
      }
    ))
  }
}


filter_multiple_byColumn <- function(df,targets,source,keep = T){
  if (keep == T) {
    df %>% filter(if_all(
      {{targets}},
      function(x){
        x == .data[[source]]
      }
    ))
  } else if (keep == F) {
    df %>% filter(if_all(
      {{targets}},
      function(x){
        x != .data[[source]]
      }
    ))
  }
}

# A function compare multiple columns with user input
compare_multiple_larger_than <- function(df,columns,value,keep = T, any = T){
  if(keep == T & any == T){
    df %>% filter(if_any(
      .cols = {{columns}},
      .fns = function(x){x>value}
    ))
  } else if(keep == T & any == F){
    df %>% filter(if_all(
      .cols = {{columns}},
      .fns = function(x){x>value}
    ))
  } else if(keep == F & any == T){
    df %>% filter(!if_all(
      .cols = {{columns}},
      .fns = function(x){x>value}
    ))
  } else if(keep == F & any == F){
    df %>% filter(!if_all(
      .cols = {{columns}},
      .fns = function(x){x>value}
    ))
  }
}

compare_multiple_less_than <- function(df,columns,value,keep = T, any = T){
  if(keep == T & any == T){
    df %>% filter(if_any(
      .cols = {{columns}},
      .fns = function(x){x<value}
    ))
  } else if(keep == T & any == F){
    df %>% filter(if_all(
      .cols = {{columns}},
      .fns = function(x){x<value}
    ))
  } else if(keep == F & any == T){
    df %>% filter(!if_all(
      .cols = {{columns}},
      .fns = function(x){x<value}
    ))
  } else if(keep == F & any == F){
    df %>% filter(!if_all(
      .cols = {{columns}},
      .fns = function(x){x<value}
    ))
  }
}

# a function to compare two value is identical
is_same <-function(x,y){
  ifelse(identical(x,y),T,F)
}

# TODO Try build a small gadget for table summary


































