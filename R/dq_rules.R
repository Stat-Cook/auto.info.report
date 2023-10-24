new_rule <- function(name, .rule, .sel=everything()){
  #' Create a 'data quality rule' for value level measures.
  #'
  #' For consistency with other code in the package we recommend writing rules
  #' to identify the desired behavior i.e. 'True'  reflecting a pass and 'false'
  #' a failure.
  #'
  #' @examples
  #' new_rule("Example rule", function(vec) !is.na(vec))
  #'
  #' @export
  #' @importFrom rlang enexpr

  # if (is.null(.sel)){
  #   .sel <- quote(everything())
  # } else {
  #   .sel <- enexpr(.sel)
  # }

  .sel <- substitute(.sel)

  rule <- c(
    Name = name,
    Sel = .sel,
    Test = .rule
  )

  class(rule) <- c("dq_rule", "list")

  rule
}


new_variable_rule <- function(...){

  #' Create a 'data quality rule' for variable level measures.
  #'
  #' @examples
  #' new_variable_rule("Example rule", function(vec) mean(is.na(vec)) < 0.1)
  #' @export
  .rule <- new_rule(...)

  class(.rule) <- c("dq_variable_rule", "dq_rule", "list")

  .rule
}

append_rule <- function(.list, .rule, .sel=everything()){
  append(.list, list(new_rule(.rule, {{.sel}})))
}


apply_rule <- function(data, rule){
  #' Apply 'data quality rules' to a given data set.
  #'
  #' @examples
  #' .rule <- new_rule("Example", function(i) !is.na(i))
  #' apply_rule(iris, .rule)
  #'
  #' @export
  UseMethod("apply_rule", rule)
}

apply_rule.dq_rule <-  function(data, rule){
  #' @exportS3Method
  #'
  #' @importFrom dplyr select mutate everything
  .result <- data %>%
    select(eval(rule$Sel)) %>%
    mutate(across(everything(), rule$Test))

  class(.result) <- c("dq_rule_result", class(.result))
  .result
}

apply_rule.dq_variable_rule <-  function(data, rule){
  #' @exportS3Method
  #'
  #' @importFrom dplyr summarize
  .result <- data %>%
    summarize(across(eval(rule$Sel), rule$Test))

  class(.result) <- c("dq_variable_rule_result", class(.result))
  .result
}


apply_rules <- function(data, .rules, ...){
  #' @export
  UseMethod("apply_rules", .rules)
}

apply_rules.dq_rule <- function(data, .rules, ...){
  #' @exportS3Method
  .rules <- list(.rules, ...)
  names(.rules) <- map(.rules, "Name")

  map(.rules, apply_rule, data=data)
}

apply_rules.list <- function(data, .rules, ...){
  #' @exportS3Method
  .rules <- append(.rules, list(...))
  # .rules <- list(.rules, ...)
  names(.rules) <- map(.rules, "Name")

  map(.rules, apply_rule, data=data)
}


greater_than.f <- function(limit=0){
  function(vec) vec > limit
}

greater_than_zero <- greater_than.f()


less_than.f <- function(limit=0){
  function(vec){
    all(vec < limit)
  }
}

less_than_zero <- less_than.f(0)


not.na <- function(vec) !is.na(vec)


one_of.f <- function(allowed_values){
  function(vec){
    all(vec %in% allowed_values)
  }
}


limited_cardinality <- function(vec){
  carinality <- length(unique(vec))
  carinality <= sqrt(length(vec))
}


within_interval <- function(object, ...){
  UseMethod("within_interval")
}


within_interval.default <- function(object, end, ...){
  #' @exportS3Method
  function(vec) {
    (vec - object) < end
  }
}

within_interval.Interval <- function(object, ...){
  #' @exportS3Method
  function(vec){
    .vec <- as.numeric(as.POSIXct(vec))
    (.vec - as.numeric(object@start)) < object@.Data
  }
}



default_rule_list <- function(){
  list(
    new_rule("Not NA", not.na),
    new_rule("<0", less_than_zero, select_numerics),
    new_rule("Cardinality Limit ", limited_cardinality, select_categorical)
  )
}





