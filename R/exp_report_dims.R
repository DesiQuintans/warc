
#' Report the difference between the number of rows in two dataframes
#'
#' @details
#' This is a symmetric comparison; the order of `x` and `y` doesn't matter.
#'
#' @param x (Dataframe) A dataframe to compare.
#' @param y (Dataframe) A dataframe to compare.
#'
#' @return Prints to the console.
#' @export
#'
#' @examples
#' report_dims(head(iris, 10), iris)
report_dims <- function(x, y) {
    x_df <- deparse1(substitute(x))
    y_df <- deparse1(substitute(y))

    dx <- dim(x)
    dy <- dim(y)
    dd <- abs(dx - dy)

    name_width <-
    num_width <- max(nchar(c(dx, dy, dd)))

    msg_format <- sprintf("# %%%is \t %%%is \t %%%is",
                          max(nchar(c(x_df, y_df, "Difference"))),
                          max(nchar(c(dx[1], dy[1], dd[1], "Rows"))),
                          max(nchar(c(dx[2], dy[2], dd[2], "Cols"))))

    result <- c(sprintf(msg_format, "", "Rows", "Cols"),
                sprintf(msg_format, x_df, dx[1], dx[2]),
                sprintf(msg_format, y_df, dy[1], dy[2]),
                sprintf(msg_format, "Difference", dd[1], dd[2])
                )

    cat(result, sep = "\n")
}
