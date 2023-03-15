pkg.env <- new.env()

InfoSchema <- R6Class("InfoSchema", list(
  data = NA,
  con = NA,
  initialize = function(con){
    self$con <- con
    query <- DBI::dbSendQuery(con, "Select * from information_schema.columns")
    self$data = DBI::dbFetch(query)
  },

  set_con = function(con){
    self$con <- con
  },

  get_cols = function(sql_table){
    self$data %>%
      filter(TABLE_NAME == sql_table)
  },

  ordered_cols = function(sql_table){

    all.cols <- self$get_cols(sql_table)

    octet.negative <- all.cols %>%
      filter(CHARACTER_OCTET_LENGTH < 0)

    end.cols <- octet.negative$COLUMN_NAME

    reordered.cols <- c(
      setdiff(all.cols$COLUMN_NAME, end.cols),
      end.cols
    )
  },

  tbl = function(table_name, con=NULL){
    ordered_cols <- self$ordered_cols(table_name)

    if (is.null(con)){
      con <- self$con
    }

    tbl(con, table_name) %>% select(all_of(ordered_cols))
  }
))

init_info_schema <- function(con){
  pkg.env$InformationSchema <- InfoSchema$new(con)
}

reset_info_schema_con <- function(con, env=pkg.env){
  env$InformationSchema$set_con(con)
}

tbl_infoschema <- function(con, sql_table){
  UseMethod("tbl_infoschema")
}

tbl_infoschema.character <- function(sql_table, env=pkg.env){
  #' @exportS3Method
  if (is.null(env$InformationSchema)){
    stop(glue("Package level `InfoSchema` not yet declared. \\
         Run `init_info_schema` to rectify."))
  }

  env$InformationSchema$tbl(sql_table)
}

tbl_infoschema.DBIConnection <- function(con, sql_table){
  #' @exportS3Method
  if (is.null(pkg.env$InformationSchema)){
    init_info_schema(con)
  }

  pkg.env$InformationSchema$tbl(sql_table, con)
}

tbl_infoschema.InfoSchema <- function(infoschema, sql_table){
  #' @exportS3Method
  infoschema$tbl(sql_table)
}

