get_aurum_version <- function() {
  "202309"
}
# According group discussion on Project chat (5th Dec, 2023), Jen said study end date will be 2020.
get_study_dates <- function() {
  as.Date(c("start" = "2008-01-01", "end" = "2020-12-31"))
}


format_int <- function(x, cost = FALSE) {
  if (cost) {
    # Careful of recursion
    format_cost(x)
  } else {
    x <- as.integer(x)
    format(x, big.mark = ",", trim = TRUE)
  }
}

format_cost <- function(x) {
  x <- round(as.integer(x))
  glue::glue("\u00A3{format_int(x)}")
}

format_dec <- function(x, digits = 1) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

format_n_pct <- function(n, total) {
  glue::glue(
    "{format_int(n)} ",
    "({format_dec(100 * n / total)}%)"
  )
}

get_cohort_order <- function() {
  c("case", "control")
}

factorise_with_unknown <- function(x, levels, labels = NULL) {
  if (is.null(labels)) {
    labels <- levels
  }
  forcats::fct_na_value_to_level(
    factor(x, levels, labels),
    level = "Unknown"
  )
}

fill_n <- function(df, ...) {
  tidyr::complete(
    df,
    ...,
    fill = list(n = 0)
  )
}


pivot_stat <- function(df, col) {
  tidyr::pivot_wider(
    df,
    names_from = {{ col }},
    values_from = "stat",
    names_expand = TRUE,
    names_sort = TRUE
  )
}

format_mean_sd <- function(x, cost = FALSE) {
  if (cost) {
    glue::glue(
      "{format_cost(mean(x))} ({format_cost(sd(x))})"
    )
  } else {
    glue::glue(
      "{format_dec(mean(x))} ({format_dec(sd(x))})"
    )
  }
}

format_median_iqr <- function(x, cost = FALSE) {
  if (cost) {
    glue::glue(
      "{format_cost(median(x))} ",
      "({format_cost(quantile(x, 0.25))} to ",
      "{format_cost(quantile(x, 0.75))})"
    )
  } else {
    glue::glue(
      "{format_dec(median(x))} ",
      "({format_dec(quantile(x, 0.25))} to ",
      "{format_dec(quantile(x, 0.75))})"
    )
  }
}

format_range <- function(x, cost = FALSE) {
  if (cost) {
    glue::glue(
      "{format_cost(min(x))} to {format_cost(max(x))}"
    )
  } else {
    glue::glue(
      "{format_dec(min(x))} to {format_dec(max(x))}"
    )
  }
}

apply_cost_imputation <- function(df, cost) {
  # Must be ungrouped
  if (dplyr::is.grouped_df(df)) {
    stop("Must be ungrouped")
  }
  # Set zero costs to NA
  df <- dplyr::mutate(df, {{ cost }} := dplyr::na_if({{ cost }}, 0))
  # Get median non-zero cost
  med_cost <- df %>%
    dplyr::summarise(median_cost = median({{ cost }}, na.rm = TRUE))

  df %>%
    dplyr::cross_join(med_cost) %>%
    dplyr::mutate({{ cost }} := dplyr::coalesce({{ cost }}, median_cost)) %>%
    dplyr::select(-"median_cost")
}


#' Get code lists from Shiny Code Browser
get_code_lists <- function(db) {
  stopifnot(inherits(db, "AgiosDb"))

  db$table("all_project_codes", "code_lists") %>%
    filter(project_id == "agios_23_003300_dr_11410") %>%
    select(section, code, code_type, code_list_label) %>%
    distinct() #%>%
    # rename_code_lists(code_list_label)
}

#' dB <- AgiosDb$new()
#' get_code_lists(dB)
