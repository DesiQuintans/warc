# Example datasets


# Run this function to build the iris_labelled dataset.
make_iris_labelled <- function() {
    iris_labelled <- datasets::iris
    labelled::var_label(iris_labelled) <- list(
        Sepal.Length = "Length of sepals.",
        Sepal.Width = "Width of sepals.",
        Petal.Length = "Length of petals.",
        Petal.Width = "Width of petals.",
        Species = paste("Species of iris:",
                        paste(unique(iris_labelled$Species), collapse = ", "))
    )

    usethis::use_data(iris_labelled, overwrite = TRUE)
}


#' The classic Iris dataset, with variable labels
#'
#' @format ## `iris_labelled`
#' A data frame with 150 rows and 5 columns:
#' \describe{
#'   \item{Sepal.Length}{Length of sepals.}
#'   \item{Sepal.Width}{Width of sepals.}
#'   \item{Petal.Length}{Length of petals.}
#'   \item{Petal.Width}{Width of petals.}
#'   \item{Species}{Species of iris: setosa, versicolor, virginica}
#'   ...
#' }
#' @source R `datasets` package
"iris_labelled"
