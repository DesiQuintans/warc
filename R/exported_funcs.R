# Functions that are 'exported' by the package (i.e. visible to package users).

#' Fuzzily search dataframe columns and labels with regex
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
#' sift(dplyr::starwars, "col")
#'
#' #> eye_color
#' #> hair_color
#' #> skin_color
#' #> vehicles
#'
#' sift(dplyr::starwars, "col", dist = 0)
#'
#' #> eye_color
#' #> hair_color
#' #> skin_color
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
                                               "type: {fold(class(vec))}",
                                               "Uniq: {howmany(vec)}",
                                               "NA %: {dp2(sum(is.na(vec))/length(vec)*100)}",
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
                                 glue::glue("range: {paste(dp2(range(vec, na.rm = TRUE)), collapse = '-')}",
                                            "mode: {Mode(vec, 'NA', na.rm = TRUE)}",
                                            "median: {dp2(median(vec, na.rm = TRUE))}",
                                            .sep = " | "
                                 )
                             ))
                         } else {
                             cli::cli_text(cli::col_silver(
                                 glue::glue("mode: {fold(Mode(vec, na.rm = TRUE))}",
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
