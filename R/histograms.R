#' Histogram drawn with points
#'
#' `geom_hist()` is a variant of `geom_histogram()` that displays
#' histogram bin counts using points instead of bars.
#'
#' Internally it uses `stat_bin()` to perform the binning and
#' `GeomPoint` to render the bins.
#'
#' @inheritParams ggplot2::geom_histogram
#'
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of observations in the bin}
#'   \item{density}{density of observations in the bin}
#' }
#'
#' @examples
#' ggplot(mtcars, aes(mpg)) +
#'   geom_hist(bins = 20)
#'
#' ggplot(mtcars, aes(mpg)) +
#'   geom_hist(aes(y = after_stat(count)), bins = 20) +
#'   scale_y_log10()
#'
#' @export
geom_hist <- function(mapping = NULL,
                      data = NULL,
                      stat = "bin",
                      position = "identity",
                      ...,
                      bins = NULL,
                      binwidth = NULL,
                      drop.zeros = TRUE,
                      na.rm = TRUE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
    # Default y aesthetic
    default_y <- ggplot2::aes(y = after_stat(count))

    # Merge user mapping with default
    if (is.null(mapping)) {
        mapping <- default_y
    } else {
        mapping <- utils::modifyList(default_y, mapping)
    }

    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = ggplot2::GeomPoint,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            bins = bins,
            binwidth = binwidth,
            na.rm = na.rm,
            ...
        )
    )
}
