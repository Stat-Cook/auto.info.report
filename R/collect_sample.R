get_sql_table <- function(query){
  UseMethod("get_sql_table")
}

get_sql_table.tbl_lazy <- function(query){
  get_sql_table(query$lazy_query$x)
}

get_sql_table.lazy_query <- function(query){
  get_sql_table(query$x)
}

get_sql_table.ident <- function(query){
  query
}

make_sample_text <- function(rows = NULL, percent = NULL){
  if (!is.null(rows)){
    return(glue("{as.integer(rows)} ROWS"))
  }

  if (!is.null(percent)){
    return(glue("{percent} PERCENT"))
  }

  "1000 ROWS"
}

collect_sample <- function(query, rows = NULL, percent = NULL){

  #' @export

  sql_tbl <- get_sql_table(query)
  sql.str <- as.character(dbplyr:::db_sql_render(query$src$con, query))

  sample_text <- make_sample_text(rows=rows, percent=percent)

  # new.sql.str <- glue("Select * from ({sql.str}) as table TABLESAMPLE({table_sample})")
  new.sql.str <- stringr::str_replace(
    sql.str, glue('"{sql_tbl}"'), glue('"{sql_tbl}" TABLESAMPLE({sample_text})')
  )

  cat(new.sql.str)

  send <- DBI::dbSendQuery(q$src$con, new.sql.str)
  result <- DBI::dbFetch(send)

  if (nrow(result) == 0){
    warning("No rows of data returned")
  }
  result
}
