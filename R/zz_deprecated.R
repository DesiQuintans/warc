# Deprecated functions.

#' Deprecated: Generate shortcuts to folders in WARC's standardised folder structure
#'
#' @description
#' This function is removed and should not be used. Use relative paths to
#' navigate the WARC folder structure instead.
#'
#' For example, `"../../../03_Data/raw.csv"` navigates 3 directory levels
#' upwards/backwards (`../` 3 times), then enters the folder `03_Data` to access
#' the file `raw.csv`.
#'
#' @param rds_folder (Character) Path to working directory.
#' @param quiet (Character) If `FALSE` (default), report shortcuts.
#'
#' @return A named list of Character vectors, which are paths to folders.
#'
#' @export
warc_dirs <- function(rds_folder, quiet = FALSE) {
    .Defunct(msg = c(
        "warc::warc_dirs() is defunct and should not be used.\n\n",
        "Use relative paths to navigate the WARC folder structure instead.\n\n",
        "For example, \"../../../03 Data/raw.csv\" navigates 3 levels up and \n",
        "into the raw data folder."
    ))
}


#' Deprecated: One-step evaluation and labelling
#'
#' This function is being deprecated because applying labels inside a
#' dplyr pipeline works inconsistently. Mostly it works fine, but
#' sometimes the labels don't get applied at all, and I can't discern
#' a reason why. Use [labelled::set_variable_labels()] instead.
#'
#' @param expr (Expression) An expression to evaluate.
#' @param label (Character) The label to apply to the output.
#'
#' @return The evaluated result of `expr`, labelled with `label`.
#'
#' @export
with_label <- function(expr, label) {
    # Deprecating this function because applying labels within
    # a pipeline is not reliable. Sometimes the labels are
    # applied, and sometimes they aren't!
    .Deprecated(new = "labelled::set_variable_labels()",
                old = "with_label()")

    out <- expr

    attr(out, "label") <- label
    # Hmisc::label(out) <- label

    return(out)
}





#' Deprecated: Set labels on many dataframe columns at once
#'
#' So that we can work with very large dataframes, this function modifies the
#' dataframe in-place (i.e. has the side-effect of mutating the dataframe's
#' state).
#'
#' @param df (Dateframe) The dataframe.
#' @param ... (dots) Column names and labels in the form `colname = "label"`.
#'
#' @return Nothing. Changes the dataframe in-place.
#'
#' @export
set_labels <- function(df, ...) {
    .Deprecated(new = "labelled::set_variable_labels()",
                old = "warc::set_labels()")

    df_name <- deparse(substitute(df))
    col <- names(list(...))
    lab <- stringr::str_squish(as.character(list(...)))

    for (i in seq_along(col)) {
        # Using attr(), not attributes(). The latter undoes factors for some reason.
        code <- sprintf('attr(%s$%s, "label") <- "%s"',
                        df_name, col[[i]], lab[[i]])

        eval(parse(text = code), envir = .GlobalEnv)
    }
}
