# Auto-numbering tables, figures, and maps.
# Based on <https://medium.com/@hshihab/emulating-c-like-static-variables-in-r-c3cde0334f1b>
#
# Creates an enclosing environment (closure.___number) that stores a `running_count`
# variable, which is available to the function that is enclosed inside.

closure.tblnumber <- function() {
    running_count <- 0

    f <- function(i = NULL) {
        if (is.numeric(i)) {
            # Passing a number doesn't increment the internal counter.
            running_count <- running_count + round(i, 0)
        } else {
            # Any non-numeric value will increment the counter.
            running_count <<- running_count + 1
        }

        return(paste0("Table ", running_count))
    }

    return(f)
}

closure.fignumber <- function() {
    running_count <- 0

    f <- function(i = NULL) {
        if (is.numeric(i)) {
            # Passing a number doesn't increment the internal counter.
            running_count <- running_count + round(i, 0)
        } else {
            # Any non-numeric value will increment the counter.
            running_count <<- running_count + 1
        }

        return(paste0("Figure ", running_count))
    }

    return(f)
}

closure.mapnumber <- function() {
    running_count <- 0

    f <- function(i = NULL) {
        if (is.numeric(i)) {
            # Passing a number doesn't increment the internal counter.
            running_count <- running_count + round(i, 0)
        } else {
            # Any non-numeric value will increment the counter.
            running_count <<- running_count + 1
        }

        return(paste0("Map ", running_count))
    }

    return(f)
}


#' Auto-numbering of tables, figures, and maps
#'
#' These functions insert caption numbers ("Table 1", "Figure 1", etc.) that automatically
#' increment every time the function is called. Note that repeated runs of the same code
#' will not have the same number; if you're troubleshooting your table and keep running
#' its code, then the table number will keep ticking up. **That's perfectly okay!** When
#' you restart R and source the code (or if you're knitting a RMarkdown document that uses
#' this auto-numbering), then the numbers will be correct in the output.
#'
#' @param i (`NULL` or Numeric)
#' - To increment the internal counter, pass `NULL` or skip `i` (default).
#' - Otherwise, to 'look around' at the item numbers **without** incrementing the counter,
#'   pass a number. For example:
#'   - `i = 0` returns the current item number,
#'   - `i = -1` returns the previous one, and
#'   - `i = +3` returns the number 3 items ahead.
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
#' # glue::glue() is strongly suggested.
#' my_table_caption <- glue::glue("{tbl_number()}. Summary statistics for iris petal lengths.")
#' my_table_caption
#' #> "Table 1. Summary statistics for iris petal lengths."
#'
#' # paste0() is fine too, though.
#' another_table <- paste0(tbl_number(), ". Another table.")
#' another_table
#' #> [1] "Table 2. Another table."
#'
#' # What is the current table number?
#' tbl_number(0)
#' #> [1] "Table 2"
#'
#' # What was the table number before this one?
#' tbl_number(-1)
#' #> [1] "Table 1"
#'
#' # What is the number of the next table?
#' tbl_number(+1)
#' #> [1] "Table 3"
#'
#' # What is the number of the table 10 items from now?
#' tbl_number(+10)
#' #> [1] "Table 12"
#'
#' # Each autonumber function tracks its own count.
#' fig_number()
#' #> [1] "Figure 1"
#'
#' map_number()
#' #> [1] "Map 1"
#'
#' map_number()
#' #> [1] "Map 2"
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

