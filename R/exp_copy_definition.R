#' Copy factor levels, ordering, and labels from one vector to another
#'
#' This function supports vectors that were labelled with either [Hmisc::label()] or
#' [haven::labelled()].
#'
#' @param to (Vector) The vector to be factored and labelled.
#' @param from (Vector) The vector whose factor levels and labels will be used.
#' @param ... (...) Additional arguments that will be passed to [factor()].
#'
#' @details Labels will only be applied to `to` if they existed in `from`.
#'
#' @return A factorised version of `to` with levels, level ordering, and labels (if any).
#' @export
#'
#' @examples
#' # ---- Labelled with Hmisc ----
#'
#' fac_old_ord <-
#'     ordered(c("Always", "Sometimes", "Always", "Rarely",
#'               "Never", "Sometimes", "Rarely", "Never"),
#'             levels = c("Always", "Sometimes", "Rarely", "Never"))
#'
#' Hmisc::label(fac_old_ord) <- "This label is for my ordered and labelled factor."
#'
#' fac_new_ord <- c("Rarely", "Always", "Sometimes", "Never",
#'                  "Never", "Always", "Rarely")
#'
#'
#' # This copies the levels, ordering, and label from `fac_old_ord` to `fac_new_ord`.
#' copy_definition(fac_new_ord, fac_old_ord)
#'
#' #> An example of an ordered and labelled factor.
#' #> [1] Rarely    Always    Sometimes Never     Never     Always    Rarely
#' #> [8] Sometimes
#' #> Levels: Always < Sometimes < Rarely < Never
#'
#'
#'
#' # ---- Labelled with haven ----
#'
#' haven_old <- haven::labelled(c("M", "M", "F"),
#'                              c(Male = "M", Female = "F"),
#'                              "This label is for my haven-labelled factor.")
#'
#' haven_new <- c("M", "F", "F", "F", "M")
#'
#'
#' copy_definition(haven_new, haven_old)
#'
#' #> <labelled<character>[5]>: This label is for my haven-labelled factor.
#' #> [1] M F F F M
#' #>
#' #> Labels:
#' #>     value  label
#' #> M   Male
#' #> F Female
#'
#'
#'
#' # ---- Unlabelled ----
#'
#' fac_old_unord <-
#'     factor(c("Curtain", "Cat", "Keyboard",
#'              "Keyboard", "Cat", "Curtain"))
#'
#' fac_new_unord <- c("Curtain", "Keyboard", "Curtain",
#'                    "Cat", "Cat", "Keyboard")
#'
#'
#' # Since `fac_old_unord` was unlabelled, the output is also unlabelled.
#' copy_definition(fac_new_unord, fac_old_unord)
#'
#' #> [1] Curtain  Keyboard Curtain  Cat      Cat      Keyboard
#' #> Levels: Cat Curtain Keyboard
#'
copy_definition <- function(to, from, ...) {
    # Haven labelling. It's handled first because haven::labelled() expects x and
    # labels to be of the same type. But factors in R are actually Integers, so trying
    # to label a factor vector with a Character vector gives the error:
    # #> Can't convert `labels` <character> to match type of `x` <integer>.
    if ("haven_labelled" %in% class(from)) {
        out <- haven::labelled(x = to,
                               labels = attr(from, "labels"),
                               label = attr(from, "label"))

        return(out)
    }

    # Only turn `to` into a factor if `from` is one to begin with.
    if (is.factor(from)) {
        out <- factor(x = to,
                      levels = levels(from),
                      ordered = is.ordered(from),
                      ...)
    } else {
        out <- to
    }


    # Only apply a label to the new vector if one existed in the old one.
    # This is because labelling adds the "labelled" class to the vector as well,
    # which some functions (e.g. skimr::skim()) do not know how to handle.
    if ("labelled" %in% class(from)) {
        Hmisc::label(out) <- Hmisc::label(from)
    } else if (is.null(attributes(from)$label) == FALSE) {
        # labelled package does not change the vector's class, but it does add a 'label'
        # attribute to the vector.
        attr(out, "label") <-  attr(from, "label")
    }

    return(out)
}
