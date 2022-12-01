# Functions that are 'exported' by the package (i.e. visible to package users).

#' Mode of a vector (numeric/character/factor)
#'
#' There is no built-in function to find the mode of something. This function
#' can find the mode of a numeric, character, or factor vector. By default it
#' will return multiple values in a multi-modal dataset, but there are several
#' methods of tie-breaking included.
#'
#' If all values are unique, it will return **all** of the values unless you
#' choose to break the tie.
#'
#' @param x (Char/Numeric/Factor) A vector.
#' @param break_ties (Character) If more than one mode is found, how should the
#'    tie be broken?
#'    - `"no"` or `FALSE`: Return a vector of all of the modes that were found.
#'    - `"random"`: Randomly choose one of the modes to return.
#'    - `"mean"`: Return the average of all of the modes (for numeric vectors).
#'    - `"first"`: Return the first mode found.
#'    - `"last"`: Return the last mode found.
#'    - `"median"`: Return the median value of all of the modes.
#'    - `"median l"` or `"median r"`: Return the mode to the left or right of the median.
#'    - `"NA"`: Return NA. Useful if you only want one clear winner.
#' @param na.rm (Logical) If `TRUE`, NAs will be silently removed.
#'
#' @return A vector of the mode value(s).
#' @export
#'
#' @examples
#' vec <- c(1, 2, 3, 4, 4, 4, 3, 3, NA, NA, NA)
#'
#' Mode(vec, break_ties = "no")
#' #> [1]  3  4 NA
#'
#' Mode(vec, break_ties = "no", na.rm = TRUE)
#' #> [1] 3 4
#'
#' Mode(vec, break_ties = "mean", na.rm = FALSE)
#' #> [1] NA
#'
#' Mode(vec, break_ties = "mean", na.rm = TRUE)
#' #> [1] 3.5
#'
#' Mode(vec, break_ties = "median", na.rm = TRUE)
#' #> [1] 3
#'
#' Mode(letters[1:4], break_ties = "no")
#' #> [1] "a" "b" "c" "d"
#'
#' Mode(letters[1:4], break_ties = "median l")
#' #> "b"
#'
#' Mode(letters[1:4], break_ties = "median r")
#' #> "c"
#'
#' Mode(letters[1:4], break_ties = "random")
#' #> [1] "a"
#'
#' @section Authors:
#' - Ken Williams (<https://stackoverflow.com/users/169947/ken-williams>)
#' - jprockbelly (<https://stackoverflow.com/users/1502898/jprockbelly>)
#' - digEmAll (<https://stackoverflow.com/users/316644/digemall>)
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/8189441/5578429>
#'
#' @md
Mode <- function(x, break_ties = "no", na.rm = FALSE) {
    if (na.rm) {
        x = x[!is.na(x)]
    }

    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    result <- ux[tab == max(tab)]

    if (length(result) > 1) {
        switch(break_ties,
               "first"    = return(result[1]),
               "last"     = return(result[length(result)]),
               "random"   = return(sample(result, 1)),
               "mean"     = return(mean(result, na.rm = na.rm)),
               "NA"       = return(methods::as(NA, class(x))),
               "median"   = return(result[        stats::median(seq_along(result))]),
               "median l" = return(result[floor(  stats::median(seq_along(result)))]),
               "median r" = return(result[ceiling(stats::median(seq_along(result)))]),
               "no"       = return(result)
        )
    } else {
        return(result)
    }
}



#' Fuzzily search column names and labels
#'
#' When working with dataframes hundreds of columns wide, it can be hard to find which
#' variable contains a particular measurement. This function lets you fuzzily search a
#' dataframe's column names and labels (the `attr()` named `"label"`) for a match.
#'
#' @param df (Dataframe) The dataframe
#' @param query (Character) A string (or regular expression) to search for.
#' @param dist (Numeric) Maximum string distance. Either an integer or a double; see
#'     `max.distance` in `base::agrep()`. Set to `0` to force an exact match.
#'
#' @return Prints out matching name & label combinations, and invisibly returns the same.
#' @export
#'
#' @examples
#'
#' sift(iris_labelled, "col")
#'
#' #> eye_color
#' #> hair_color
#' #> skin_color
#' #> vehicles
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
#' @importFrom magrittr %>%
sift <- function(df, query, dist = 0.1) {
    get_label <- function(item) {
        result <- attr(item, "label")

        if (is.null(result)) {
            return("")
        } else {
            return(stringr::str_trim(result))
        }
    }

    attr_name  <- stringr::str_trim(colnames(df))
    attr_label <- sapply(df, get_label, USE.NAMES = FALSE)
    names(attr_label) <- NULL

    # This is what gets searched to find matching columns
    attr_joined <- stringr::str_trim(paste(attr_name, attr_label))

    out <- agrep(query, attr_joined, ignore.case = TRUE, value = FALSE, fixed = FALSE,
                 max.distance = dist)

    purrr::pwalk(list(attr_name[out], attr_label[out], df[out]),
                 function(colname, lab, vec) {


                     cli::cli({
                         cli::cli_text(colname,
                                       cli::col_silver(
                                           glue::glue(
                                               "",
                                               "type: {fold(class(vec), 2)}",
                                               "Uniq: {length(unique(vec))}",
                                               "NA %: {dp(sum(is.na(vec))/length(vec)*100)}",
                                               .sep = " | "
                                           )
                                       )
                         )

                         cli::cli_div(class = "tmp",
                                      theme = list(.tmp = list("margin-left" = 4,
                                                               "margin-right" = 4)))
                         if (lab != "") {
                             cli::cli_text(cli::col_silver(lab))
                         }

                         if (is.numeric(vec)) {
                             cli::cli_text(cli::col_silver(
                                 glue::glue("range: {paste(dp(range(vec, na.rm = TRUE)), collapse = '-')}",
                                            "mode: {fold(warc::Mode(vec, na.rm = TRUE))}",
                                            "median: {dp(median(vec, na.rm = TRUE))}",
                                            .sep = " | "
                                 )
                             ))
                         } else {
                             cli::cli_text(cli::col_silver(
                                 glue::glue("mode: {fold(warc::Mode(vec, na.rm = TRUE))}",
                                            .sep = " | "
                                 )
                             ))
                         }
                     })
                 }
    )

    return(invisible(list(cols = attr_name[out],
                          labels = attr_label[out])))
}



#' Bootstrapped confidence interval of the mean
#'
#' Quick confidence interval calculation using bootstrapping, which makes no assumptions
#' about the distribution of the data.
#'
#' By default, this is calculated using the percentile method, but it also supports the
#' Bias-Corrected and Accelerated (BCA) method (DOI: 10.1080/01621459.1987.10478410), which
#' is useful if you want to show that the lower and upper bounds of a statistic are
#' asymmetric around the point estimate.
#'
#' @param vec (Numeric) The numeric vector to calculate a CI for.
#' @param conf (Double) The confidence level, by default `0.95` (95% CI).
#' @param R (Integer) Number of bootstrap repetitions.
#' @param type (Character) Either `"perc"` (percentile method, default) or `"bca"`
#'    (bias-corrected and accelerated method).
#'
#' @return A list with three items: `ci_lwr` contains the lower bound, `ci_est` contains
#'     the point estimate, and `ci_upr` contains the upper bound. If you are using this
#'     inside a dataframe, look at `dplyr::unnest()` to split this list out into columns.
#' @export
#'
#' @examples
#' set.seed(12345)
#' nums <- c(rnorm(10), NA_real_)
#'
#' nums
#'
#' ## [1]  0.5855288  0.7094660 -0.1093033 -0.4534972  0.6058875 -1.8179560  0.6300986
#' ## [8] -0.2761841 -0.2841597 -0.9193220         NA
#'
#' ci_mean(nums)
#'
#' ## $ci_lwr
#' ## [1] -0.6455236
#' ##
#' ## $ci_est
#' ## [1] -0.1329441
#' ##
#' ## $ci_upr
#' ## [1] 0.3430405
#'
#' # Demonstration of unnesting into a dataframe
#' # library(dplyr)
#' # library(tidyr)
#' #
#' # iris %>%
#' #     group_by(Species) %>%
#' #     summarise(mean_boot = ci_mean(Petal.Length)) %>%
#' #     unnest(mean_boot, names_sep = ".")
#'
#' ## # A tibble: 3 × 4
#' ##   Species    mean_boot.ci_lwr mean_boot.ci_est mean_boot.ci_upr
#' ##   <fct>                 <dbl>            <dbl>            <dbl>
#' ## 1 setosa                 1.41             1.46             1.51
#' ## 2 versicolor             4.13             4.26             4.39
#' ## 3 virginica              5.40             5.55             5.7
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
ci_mean <- function(vec, conf = 0.95, R = 999, type = "perc") {
    if (length(type) != 1 | sum(type %in% c("bca", "perc")) != 1) {
        stop('`type` argument must be one of "bca" or "perc".')
    }

    boot_mean <- function(v, i) {
        mean(v[i], na.rm = TRUE)
    }

    boot_out <- boot::boot(data = vec, statistic = boot_mean, R = R)
    cis <- boot::boot.ci(boot_out, conf = conf, type = type)

    if (is.null(cis)) {
        return(data.frame(
            ci_lwr = NA_real_,
            ci_est = NA_real_,
            ci_upr = NA_real_
        ))
    }

    return(data.frame(
        ci_lwr = cis[[4]][4],
        ci_est = cis$t0,
        ci_upr = cis[[4]][5]
    ))
}



#' Columns that appear in two dataframes
#'
#' @param df1 (Dataframe) A dataframe whose column names to compare.
#' @param df2 (Dataframe) A dataframe whose column names to compare.
#'
#' @return A Character vector with the names of the columns that appear
#'     in both `df1` and `df2`.
#' @export
#'
#' @examples
#' iris1 <- iris[, 1:3]
#' colnames(iris1)
#' ## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length"
#'
#' iris2 <- iris[, 2:5]
#' colnames(iris2)
#' ## [1]                "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#'
#' same_cols(iris1, iris2)
#' #> [1] "Sepal.Width"  "Petal.Length"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
same_cols <- function(df1, df2) {
    base::intersect(colnames(df1), colnames(df2))
}



#' Columns that don't appear in two dataframes
#'
#' @param df1 (Dataframe) A dataframe whose column names to compare.
#' @param df2 (Dataframe) A dataframe whose column names to compare.
#' @param side (Character) `"both"` or `"b"` (default) finds columns that are missing from
#'     both dataframes. `"left"` or `"l"` finds cols in `df1` that are not in `df2`.
#'     `"right"` or `"r"` finds cols in `df2` that are not in `df1`.
#'
#' @return A Character vector with the names of missing columns.
#' @export
#'
#' @examples
#' iris1 <- iris[, 1:3]
#' colnames(iris1)
#' ## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length"
#'
#' iris2 <- iris[, 2:5]
#' colnames(iris2)
#' ## [1]                "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#'
#' diff_cols(iris1, iris2)
#' #> [1] "Sepal.Length" "Petal.Width"  "Species"
#'
#' diff_cols(iris1, iris2, side = "l")
#' #> [1] "Sepal.Length"
#'
#' diff_cols(iris1, iris2, side = "r")
#' #> [1] "Petal.Width"  "Species"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
diff_cols <- function(df1, df2, side = "both") {
    # Both directions need to be compared.
    set1 <- base::setdiff(colnames(df1), colnames(df2))
    set2 <- base::setdiff(colnames(df2), colnames(df1))

    if (grepl("^b", side))
        return(unique(c(set1, set2)))

    if (grepl("^l", side))
        return(set1)

    if (grepl("^r", side))
        return(set2)
}



#' Friendly printing of a multi-element vector
#'
#' Produces a readout like `"January, February, and 10 others"`.
#'
#' @param vec (Vector) The vector you want to print.
#' @param n (Integer) The maximum number of vector elements to show.
#'
#' @return A character vector of length 1 containing the readout (or `NA`).
#' @export
#'
#' @examples
#' month.name
#' #> [1] "January"   "February"  "March"     "April"     "May"       "June"
#' #> [7] "July"      "August"    "September" "October"   "November"  "December"
#'
#' fold(month.name)
#' #> January, February, and 10 others
#'
#' fold(1:20, n = 5)
#' #> 1, 2, 3, 4, 5, and 15 others
#'
#' fold(character(0), n = 1)
#' #> [1] NA
#'
#' fold(1:10, n = 0)
#' #> [1] NA
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
fold <- function(vec, n = 2) {
    if (length(vec) == 0 | n < 1) {
        return(NA_character_)
    }

    if (length(vec) <= n) {
        return(paste(vec, collapse = ", "))
    }

    return(glue::glue(
        "{items}, and {remaining} {others}",
        items = paste(vec[1:n], collapse = ", "),
        remaining = length(vec) - n,
        others = ifelse(length(vec) - n > 1, "others", "other")
    ))
}



#' Alias for `any(!(...))`
#'
#' `any(...)` returns `TRUE` if anything in `...` is `TRUE`, so `any_false(...)` returns
#' `TRUE` if anything in `...` is `FALSE`.
#'
#' This function exists to avoid confusion when negating `any()`;
#' `any(!c(TRUE, FALSE)) == TRUE`, but `!any(c(TRUE, FALSE)) == FALSE`, and the difference
#' is which part of the expression you negate. In many cases, you want the former because
#' you are testing whether anything in the vector is `FALSE`, and that's what this
#' function does.
#'
#' @param ... (Vectors) Zero or more logical vectors.
#' @param na.rm (Logical) If `TRUE`, `NA` values are removed before the result is computed.
#'
#' @return Logical vector.
#' @export
#'
#' @examples
#' any_false(c(TRUE, TRUE))
#' #> [1] FALSE
#'
#' any_false(c(FALSE, FALSE))
#' #> [1] TRUE
#'
#' any_false(c(TRUE, FALSE))
#' #> [1] TRUE
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
any_false <- function(..., na.rm = FALSE) {
    base::any(!(...), na.rm = na.rm)
}



#' Report pairwise differences between two vectors
#'
#' Compares each element of two vectors to each other and reports the number of total
#' matches and mismatches, as well as a count for each unique mismatch.
#'
#' @param l (Vector) Any vector.
#' @param r (Vector) Any vector.
#' @param full (Logical) If `TRUE` (default), adds counts of each specific mismatch to the
#' dataframe output. If `FALSE`, only shows the first two rows of the output (the overall
#' count of matches and mismatches).
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' v1 <- c(1, 2, 3, 4, 5, NA, NA)
#' v2 <- c(1, 2, 5, 4, 3, NA, 7)
#'
#' report_diff(v1, v2)
#'
#' #>     pairs count
#' #> 1  l == r     4
#' #> 2  l != r     3
#' #> 3  3 != 5     1
#' #> 4  5 != 3     1
#' #> 5 NA != 7     1
#'
#' report_diff(v1, v2, full = FALSE)
#'
#' #>    pairs count
#' #> 1 l == r     4
#' #> 2 l != r     3
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
report_diff <- function(l, r, full = TRUE) {
    v_identical <- Vectorize(identical)

    # Count how many elements are the same
    same <- v_identical(l, r)
    n_same <- sum(same)

    # Grab the elements that are not the same
    diff <- !v_identical(l, r)
    n_diff <- sum(diff)

    overall <-
        data.frame(pairs = c("l == r", "l != r"),
                   count = c(n_same, n_diff))

    if (full == FALSE) {
        return(overall)
    } else {
        pairs <- paste(l[diff], r[diff], sep = " != ")
        result <- as.data.frame(table(pairs), responseName = "count")

        full_output <- rbind(overall, result)

        return(full_output)
    }
}
