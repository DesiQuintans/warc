
#' Report missingness of columns in a dataframe
#'
#' @param df (Dataframe) A dataframe.
#' @param pattern (Character) A regular expression that will be used to select columns of `df`, passed to [agrep()].
#' @param ignore.case (Logical) If `TRUE` (default), ignore case when matching.
#' @param ... (Dots) Other arguments that will be passed to [agrep()].
#'
#' @return Prints to the console.
#' @export
#'
#' @examples
#' report_missingness(iris, "^petal")   # Starts with "petal"
#' report_missingness(iris, "length$")  # Ends with "length"
#' report_missingness(iris, ".")        # Matches anything (returns all cols)
report_missingness <- function(df, pattern, ignore.case = TRUE, ...) {
    out_names <- agrep(pattern = pattern,
                       x = colnames(df),
                       ignore.case = ignore.case,
                       value = TRUE,
                       ...)

    result_format <- sprintf("# %%-%is: %%.2f%%%% missing", max(nchar(out_names)))

    result <-
        Map(function(x) 100 * (sum(is.na(x)) / length(x)), df[out_names])

    styled <- sprintf(result_format, out_names, result)

    cat(styled, sep = "\n")
}
