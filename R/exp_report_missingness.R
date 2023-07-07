
#' Report missingness of columns in a dataframe
#'
#' @param df (Dataframe) A dataframe.
#' @param pattern (Character) A regular expression that will be used to select columns of `df`, passed to [agrep()].
#' @param ... (Dots) Other arguments that will be passed to [agrep()].
#'
#' @return Prints to the console.
#' @export
#'
#' @examples
#' report_missingness(iris, "^Petal")
#' report_missingness(iris, "Length$")
report_missingness <- function(df, pattern, ...) {
    out_names <- agrep(pattern = pattern, x = colnames(df), value = TRUE, ...)

    result_format <- sprintf("# %%-%is: %%.2f%%%% missing", max(nchar(out_names)))

    result <-
        Map(function(x) 100 * (sum(is.na(x)) / length(x)), df[out_names])

    styled <- sprintf(result_format, out_names, result)

    cat(styled, sep = "\n")
}
