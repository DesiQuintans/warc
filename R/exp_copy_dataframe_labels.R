#' Copy labels between dataframes
#'
#' This uses [Hmisc::label<-()] to apply labels to the dataframe. Dataframes that were
#' labelled by [Hmisc::label()], [haven::labelled()], or [labelled::var_label()] are
#' accepted.
#'
#' Value labels are also copied over.
#'
#' @param to (Dataframe) The dataframe whose columns will be labelled.
#' @param from (Dataframe) The dataframe whose labels will be used.
#'
#' @details Column names must exactly match between `to` and `from`. Columns in `to` that
#' are not in `from` will not be modified.
#'
#' @return A version of `to` with variables labelled. Since `Hmisc` is used
#' to do the labelling, it also adds the `labelled` class to the vector.
#'
#' @export
#'
#' @examples
#' x <- copy_dataframe_labels(iris, iris_labelled_hmisc)
#' str(x)
#'
#' ## data.frame':	150 obs. of  5 variables:
#' ##     $ Sepal.Length: 'labelled' num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#' ## ..- attr(*, "label")= chr "Length of sepals."
#' ## $ Sepal.Width : 'labelled' num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#' ## ..- attr(*, "label")= chr "Width of sepals."
#' ## $ Petal.Length: 'labelled' num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#' ## ..- attr(*, "label")= chr "Length of petals."
#' ## $ Petal.Width : 'labelled' num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#' ## ..- attr(*, "label")= chr "Width of petals."
#' ## $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1  ...
#' ## ..- attr(*, "label")= chr "Species of iris: setosa, versicolor, virginica"
#'
copy_dataframe_labels <- function(to, from) {
    cols_to_label <- base::intersect(colnames(to), colnames(from))

    # Code writing code! :3
    for (i in cols_to_label) {
        # The variable's label
        has_label <-
            eval(parse(text = glue::glue("is.null(attr(from${i}, 'label')) == FALSE")))

        # Labels applied to the variable's values (by haven, usually).
        has_value_labels <-
            eval(parse(text = glue::glue("is.null(attr(from${i}, 'labels')) == FALSE")))

        # "Evaluate expression given as a string"
        # https://stackoverflow.com/a/1743796/5578429

        if (has_label) {
            # Hmisc::label(to$colname) <- attr(from$colname, 'label')
            code_label <-
                glue::glue("Hmisc::label(to${i}) <- attr(from${i}, 'label')")

            eval(parse(text = code_label))
        }

        if (has_value_labels) {
            # attr(to$colname, 'labels') <- attr(from$colname, 'labels')
            code_value_labels <-
                glue::glue("attr(to${i}, 'labels') <- attr(from${i}, 'labels')")

            eval(parse(text = code_value_labels))
        }
    }

    return(to)
}
