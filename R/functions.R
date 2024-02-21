
#' Negated value matching
#'
#' `%nin%` is a negated version of `%in%`;.
#'
#' @param x vector or `NULL`; the values to be matched.
#' @param table vector or `NULL`; the values to be matched against.
#'
#' @return a vector of the same length as x
#' @export
#'
`%nin%` <- function(x, table) {
  !x %in% table
}

#

#' Augment a dataframe with columns to allow plotting of conditioned box plots.
#'
#' @param df a dataframe
#' @param var a tidy-select column specification
#' @param nbins the number of bins to generate
#'
#' @return a tibble with two new columns, for the var's bin number and the var's
#'          median
#' @export
#'
augment_bins <- function(df, var, nbins=25)
{
  # Capture the argument as a quosure
  v <- rlang::enquo(var)
  # Get the string representation of the variable
  vname <- rlang::as_string(rlang::quo_get_expr(v))
  # add a new column named bin_column_name carrying the bin number for each row.
  tmp <- dplyr::mutate(df, ibin = ggplot2::cut_number(!!v, nbins, labels=FALSE))
  # Determine the median of the variable var in each bin.
  tmp2 <- dplyr::group_by(tmp, .data$ibin) |>
    dplyr::summarize(xxx = median(!!v), .groups="drop")
  # Give the new column a more appropriate name.
  names(tmp2)[length(tmp2)] <- paste0(vname, "_median")
  # Now augment the original dataframe with the new column
  tmp <- dplyr::left_join(tmp, tmp2, by="ibin")
  names(tmp)[length(tmp)-1] <- paste0(vname, "_bin")
  tmp
}
