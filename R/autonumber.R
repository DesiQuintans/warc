# Auto-numbering tables, figures, and maps.
# Based on <https://medium.com/@hshihab/emulating-c-like-static-variables-in-r-c3cde0334f1b>
#
# Creates an enclosing environment (closure.___number) that stores a `running_count`
# variable, which is available to the function that

closure.tblnumber <- function() {
    running_count <- 0

    f <- function(i = NULL) {
        if (is.null(i) | sum(i %in% c("prev", "last", "next", "curr", "this")) == 0) {
            running_count <<- running_count + 1
        } else {
            running_count <-
                switch (i,
                        "prev" = running_count - 1,
                        "last" = running_count - 1,
                        "next" = running_count + 1,
                        "curr" = running_count,
                        "this" = running_count
                )
        }

        return(paste0("Table ", running_count, "."))
    }

    return(f)
}

closure.fignumber <- function() {
    running_count <- 0

    f <- function(i = NULL) {
        if (is.null(i) | sum(i %in% c("prev", "last", "next", "curr", "this")) == 0) {
            running_count <<- running_count + 1
        } else {
            running_count <-
                switch (i,
                        "prev" = running_count - 1,
                        "last" = running_count - 1,
                        "next" = running_count + 1,
                        "curr" = running_count,
                        "this" = running_count
                )
        }

        return(paste0("Figure ", running_count, "."))
    }

    return(f)
}

closure.mapnumber <- function() {
    running_count <- 0

    f <- function(i = NULL) {
        if (is.null(i) | sum(i %in% c("prev", "last", "next", "curr", "this")) == 0) {
            running_count <<- running_count + 1
        } else {
            running_count <-
                switch (i,
                        "prev" = running_count - 1,
                        "last" = running_count - 1,
                        "next" = running_count + 1,
                        "curr" = running_count,
                        "this" = running_count
                )
        }

        return(paste0("Map ", running_count, "."))
    }

    return(f)
}


#' Auto-numbering of tables, figures, and maps
#'
#' These functions insert caption numbers ("Table 1", "Figure 1", etc.) that automatically
#' increment every time the function is called.
#'
#' @param i (Character) Keywords that control which number is printed.
#' * An empty argument or anything **not** in this keyword list will add 1 to the number.
#' * To return the current item number, use `"curr"` or `"this"`.
#' * To return the last item number, use `"prev"` or `"last"`.
#' * To return the next item number, use `"next"`.
#'
#' @details Each function stores its own count, so use the correct function for the
#'     correct item type.
#'
#'
#' @return A Character vector of length one.
#'
#' @describeIn tbl_number
#' Auto-numbering of Tables.
#'
#' @export
#'
#' @examples
#' my_table_caption <- paste(tbl_number(), "Summary statistics for iris petal lengths.")
#' my_table_caption
#' #> [1] "Table 1. Summary statistics for iris petal lengths."
#'
#' another_table <- paste(tbl_number(), "Another table.")
#' another_table
#' #> [1] "Table 2. Another table."
#'
#' # What is the current table number?
#' tbl_number("curr")
#' #> [1] "Table 2."
#'
#' # What was the table number before this one?
#' tbl_number("prev")
#' #> [1] "Table 1."
#'
#' # What is the number of the next table?
#' tbl_number("next")
#' #> [1] "Table 3."
#'
#' # Each autonumber function tracks its own count.
#' fig_number()
#' #> [1] "Figure 1."
#'
#' map_number()
#' map_number()
#' #> [1] "Map 2."
#'
tbl_number <- closure.tblnumber()

#' @describeIn tbl_number
#' Auto-numbering of Figures.
#' @export
fig_number <- closure.fignumber()

#' @describeIn tbl_number
#' Auto-numbering of Maps.
#' @export
map_number <- closure.mapnumber()

