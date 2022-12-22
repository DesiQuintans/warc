# Example datasets


# Run this function to build the iris_labelled dataset.
make_iris_labelled_hmisc <- function() {
    iris_labelled_hmisc <- datasets::iris

    Hmisc::label(iris_labelled_hmisc$Sepal.Length) <- "Length of sepals."
    Hmisc::label(iris_labelled_hmisc$Sepal.Width)  <- "Width of sepals."
    Hmisc::label(iris_labelled_hmisc$Petal.Length) <- "Length of petals."
    Hmisc::label(iris_labelled_hmisc$Petal.Width)  <- "Width of petals."
    Hmisc::label(iris_labelled_hmisc$Species)      <-
        paste("Species of iris:", paste(unique(datasets::iris$Species), collapse = ", "))

    usethis::use_data(iris_labelled_hmisc, overwrite = TRUE)
}

make_iris_labelled_haven <- function() {
    iris_labelled_haven <- datasets::iris

    iris_labelled_haven$Sepal.Length  <-
        haven::labelled(iris_labelled_haven$Sepal.Length,
                        label = "Length of sepals.")

    iris_labelled_haven$Sepal.Width <-
        haven::labelled(iris_labelled_haven$Sepal.Width,
                        label = "Width of sepals.")

    iris_labelled_haven$Petal.Length <-
        haven::labelled(iris_labelled_haven$Petal.Length,
                        label = "Length of petals.")

    iris_labelled_haven$Petal.Width <-
        haven::labelled(iris_labelled_haven$Petal.Width,
                        label = "Width of petals.")

    iris_labelled_haven$Species <-
        haven::labelled(iris_labelled_haven$Species,
                        label = paste("Species of iris:",
                                      paste(unique(datasets::iris$Species),
                                            collapse = ", ")))

    usethis::use_data(iris_labelled_haven, overwrite = TRUE)
}


make_iris_labelled_labelled <- function() {
    iris_labelled_labelled <- datasets::iris

    labelled::var_label(iris_labelled_labelled) <- list(
        Sepal.Length = "Length of sepals.",
        Sepal.Width = "Width of sepals.",
        Petal.Length = "Length of petals.",
        Petal.Width = "Width of petals.",
        Species = paste("Species of iris:",
                        paste(unique(iris_labelled_labelled$Species), collapse = ", "))
    )

    usethis::use_data(iris_labelled_labelled, overwrite = TRUE)
}


#' The classic Iris dataset, labelled with 'Hmisc'
#'
#' This was labelled with [Hmisc::label()].
#'
#' @format ## `iris_labelled_hmisc`
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
"iris_labelled_hmisc"


#' The classic Iris dataset, labelled with 'haven'
#'
#' This was labelled with [haven::labelled()].
#'
#' @format ## `iris_labelled_haven`
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
"iris_labelled_haven"


#' The classic Iris dataset, labelled with 'labelled'
#'
#' This was labelled with [labelled::var_label()].
#'
#' @format ## `iris_labelled_labelled`
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
"iris_labelled_labelled"
