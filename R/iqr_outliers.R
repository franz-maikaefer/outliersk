#' @import dplyr
NULL

#' Get upper and lower thresholds for considering a value in a data.frame column an outlier according IQR method.
#'
#' @param df a data.frame.
#'
#' @param col a symbol for the column where to search for ouliers.
#'
#' @param scale how much to scale the IQR, beyond what values are considered outliers.
#'
#' @return a list with following elements:
#' (1) less_than: lower bound, bellow what values are considered outliers;
#' (2) greater_than: upper bound, above what values are considered outliers.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(label = paste0("a_", 1:10), let_be_outlier_column = c(rep(1, 7), 8, 1, 1))
#' outliers_list <-  get_iqr_based_outlier_detection_thresholds_for(df, let_be_outlier_column)
#' }
get_iqr_based_outlier_detection_thresholds_for <- function(df, col, scale = 1.5, na.rm = TRUE) {
  col_qt <- rlang::enquo(col)
  values <- df %>% pull(!! col_qt)
  iqr_values <- IQR(values, na.rm = na.rm)
  fix_names_for <- function(bound, signal) {
    iqr_label <- paste0(scale, "*IQR")
    names(bound) <- paste0(names(bound), " ", signal, " ", iqr_label)
    bound
  }
  lower_bound <- quantile(values, probs = 0.25, na.rm = na.rm) - scale*iqr_values
  lower_bound <- fix_names_for(lower_bound, "-")
  upper_bound <- quantile(values, probs = 0.75, na.rm = na.rm) + scale*iqr_values
  upper_bound <- fix_names_for(upper_bound, "+")
  list(less_than = lower_bound, greater_than = upper_bound)
}


#' Flag outliers in a data.frame column according IQR method.
#'
#' @param df a data.frame.
#'
#' @param col a symbol for the column where to search for ouliers.
#'
#' @param scale how much to scale the IQR, beyond what values are considered outliers.
#'
#' @return original data.frame plus an aditional column of form 'col_is_outlier', after 'col'.
#'
#' @examples
#' @export
#' \dontrun{
#' df <- data.frame(label = paste0("a_", 1:10), let_be_outlier_column = c(rep(1, 7), 8, 1, 1))
#' new_df <-  flag_iqr_outliers_in(df, let_be_outlier_column)
#' }
flag_iqr_outliers_in <- function(df, col, scale = 1.5) {
  flag_col_nm <- paste0(rlang::as_name(rlang::enquo(col)), "_is_outlier")
  outlier_if <-
    df %>%
    get_iqr_based_outlier_detection_thresholds_for({{col}}, scale)
  df %>%
    mutate(
      "{{col}}_is_outlier" := {{col}} < outlier_if$less_than | {{col}} > outlier_if$greater_than
    ) %>%
    relocate(all_of(flag_col_nm), .after = {{col}})
}


#' Flag outliers in data.frame column(s) according IQR method.
#'
#' @param df a data.frame.
#'
#' @param ... <[`tidy-select`][dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas, for the column where to search for
#'   ouliers. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#'
#' @param scale how much to scale the IQR, beyond what values are considered outliers.
#'
#' @return original data.frame plus aditional column(s) of form 'col_is_outlier', after each 'col' to flag.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(label = paste0("a_", 1:10),
#'                  A_flag = c(rep(1, 7), 8, 1, 1),
#'                  B_flag = c(rep(1, 5), 3, 2, 2, 1, 1))
#' new_df <-  flag_iqr_outliers_in_cols(df, ends_with("_flag"))
#' }
flag_iqr_outliers_in_cols <- function(.data, ..., scale = 1.5) {
  loc <- tidyselect::eval_select(
    expr(c(...)),
    data = .data
  )
  loc <- vctrs::vec_as_location(loc, n = ncol(.data), names = names(.data))
  cols <- rlang::syms(names(.data)[loc])
  purrr::reduce(cols, function(df_i, col) {
    df_i %>% flag_iqr_outliers_in(!! col, scale = scale)
  },
  .init = .data
  )
}
