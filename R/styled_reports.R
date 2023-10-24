red <- scales::hue_pal()(1)

styled_variable_report <- function(report){
  #' Apply `datatable` styling to the variable rules from a `dq_rule_report`
  #'
  #' @param report the output of `dq_rule_report`
  #'
  #' @examples
  #' rule <- new_rule("A", function(vec) !is.na(vec))
  #' variable_rule <- new_variable_rule("A", function(vec) mean(is.na(vec)) < 0.1)
  #'
  #' applied.rules <- apply_rules(iris, rule, variable_rule)
  #'
  #' report <- dq_rule_report(applied.rules)
  #'
  #' styled_variable_report(report)
  #'
  #'
  #' @importFrom DT datatable formatStyle styleEqual
  #' @export
  report$VariableRules %>% datatable() %>%
    formatStyle(
      colnames(report$VariableRules),
      backgroundColor = styleEqual(c(T, F),  c(NA, red))
    )
}

styled_value_report <- function(report, limit=80){
  #' Apply `datatable` styling to the value rules from a `dq_rule_report`
  #'
  #' @param report the output of `dq_rule_report`
  #'
  #' @examples
  #' rule <- new_rule("A", function(vec) !is.na(vec))
  #' variable_rule <- new_variable_rule("A", function(vec) mean(is.na(vec)) < 0.1)
  #'
  #' applied.rules <- apply_rules(iris, rule, variable_rule)
  #'
  #' report <- dq_rule_report(applied.rules)
  #'
  #' styled_value_report(report)
  #'
  #' @importFrom DT datatable formatStyle styleInterval
  #' @export
  report$ValueRules %>% datatable() %>%
    formatStyle(
      colnames(report$ValueRules),
      backgroundColor = styleInterval(limit,  c(red, NA))
    )
}
