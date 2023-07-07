
#' Compare sets of values between two vectors or columns
#'
#' This is a wrapper around [setdiff()] that a) performs the set difference in
#' both directions, b) reports the differences in a way that's easy to understand
#' ("x has 4 values not in y", and vice versa), and c) returns the vectors of
#' differences.
#'
#' @param x (Vector) Vector to compare.
#' @param y (Vector) Vector to compare.
#'
#' @return Prints to the console. Invisibly returns a named list (`x_diff_from_y`
#'     and `y_diff_from_x`) of differing values.
#' @export
#'
#' @examples
#' set.seed(12345)
#' df1 <- data.frame(patient    = letters[1:10],
#'                   hospital   = sample(c("WH", "NH", "CH"), 10, replace = TRUE))
#'
#' df2 <- data.frame(patient_id = letters[5:26],
#'                   hospital   = sample(c("WH", "NH", "CH"), 22, replace = TRUE))
#'
#' report_setdiff(df1$hospital, df2$hospital)
#' report_setdiff(df1$patient,  df2$patient_id)
report_setdiff <- function(x, y) {
    x_name <- deparse1(substitute(x))
    y_name <- deparse1(substitute(y))

    # setdiff() is asymmetrical. It asks: Who in `x` is not in `y`?
    # setdiff(x = 1:10, y = 3:5)
    # [1]  1  2  6  7  8  9 10

    x_not_in_y <- setdiff(x = x, y = y)
    y_not_in_x <- setdiff(x = y, y = x)

    if (setequal(x, y)) {
        msg <- sprintf("# '%s' and '%s' are equal sets.", x_name, y_name)
    } else {
        msg_format <- sprintf("# '%%-%is' has %%-%ii values not in '%%s'.",
                          max(nchar(c(x_name, y_name))),
                          max(nchar(c(length(x_not_in_y), length(y_not_in_x)))))

        msg <- c(sprintf(msg_format, x_name, length(x_not_in_y), y_name),
                 sprintf(msg_format, y_name, length(y_not_in_x), x_name))
    }

    cat(msg, sep = "\n")

    return(invisible(list(x_diff_from_y = x_not_in_y,
                          y_diff_from_x = y_not_in_x)))
}
