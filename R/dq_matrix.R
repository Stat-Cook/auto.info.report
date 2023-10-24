proportion <- function(vec){
  round(100*mean(vec), 1)
}



# apply_rules <- function(data, .rules, ...){
#   UseMethod("apply_rules")
# }
#
# apply_rules.list <- function(data, .rules, ...){
#   names(.rules) <- map(.rules, "Name")
#
#   rule.results <- map(.rules, apply_rule, data=iris)
#
#   all_rules <- reduce(rule.results, merge_with_and) %>%
#     summarize_proportions()
#
#   proportion.list <- map(rule.results, summarize_proportions) %>%
#     append(list("All" = all_rules), after=0) # %>%
#
#   do.call(plyr::rbind.fill, proportion.list)  %>%
#     data.frame() %>%
#     t() %>%
#     set_colnames(names(proportion.list))
# }


select_or_create <- function(frm, cols, fill.value=NA){
  frm_cols <- colnames(frm)

  to.create <- cols[!cols %in% frm_cols]

  for (i in to.create){
    frm[[i]] <- NA
  }

  frm %>%
    select(cols)

}

melt_and_rename <- function(data, .name){

  data %>%
    mutate(ID = 1:nrow(data)) %>%
    pivot_longer(cols=-ID) %>%
    mutate(
      name = glue::glue("{name} [Rule: {.name}]")
    )
}

row_summary <- function(rule.results){
  map2(rule.results, names(rule.results),
       melt_and_rename) %>%
    do.call(rbind, .) %>%
    reshape2::dcast(ID ~ name)
}


dq_rule_report <- function(rule.results){
  #' @export
  are_variable_results <- rule.results %>%
    map(class) %>%
    map(\(x) "dq_variable_rule_result" %in% x) %>%
    unlist()

  variable_results <- map(which(are_variable_results), \(x) rule.results[[x]])
  value_results <- map(which(!are_variable_results), \(x) rule.results[[x]])

  all_value_results <- reduce(value_results, merge_with_and) %>%
    summarize_proportions()

  value_proportion.list <- map(value_results, summarize_proportions) %>%
    append(list("All Value Rules" = all_value_results), after=0)

  all_variable_results <- reduce(variable_results, merge_with_and)

  variable_proportion.list <- variable_results %>%
    append(list("All Variable Rules" = all_variable_results), after=0)


  value_proportion_frame <- do.call(plyr::rbind.fill, value_proportion.list)  %>%
    data.frame() %>%
    t() %>%
    set_colnames(names(value_proportion.list))

  variable_proportion_frame <- do.call(plyr::rbind.fill, variable_proportion.list)  %>%
    data.frame() %>%
    t() %>%
    set_colnames(names(variable_proportion.list))

  list(
    ValueRules = value_proportion_frame,
    VariableRules = variable_proportion_frame
  )
}












#
#
#
# r1 <- new_rule(not.na)
# r2 <- new_rule(not.na)



# {
#   .job <- append(.rule.list, list(iris), after=0)
#   reduce(.job, apply_rule)
# }
#
# .job[[1]]
# .job[[2]]
#
# eval("everything()")
#
# c(
#   where(is.numeric),
#   greater_than.f()
# )
#
# .f(where(is.numeric), greater_than.f())
#
