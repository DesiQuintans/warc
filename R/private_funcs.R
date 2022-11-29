# Functions that are hidden from package users; they're used internally in other functions.
# These packages don't need a full Roxygen documentation block, since a Help file is never
# generated for them.



# Short alias for rounding to fixed decimal places (2 by default)
# dp(rnorm(10))
dp <- function(num, fun, p = 2) {
    round(num, p)
}

# For vectors with > `max` elements, print the first `max` elements and tell the user how
# many others are unprinted.
# fold(month.name)
fold <- function(vec, max = 2) {
    if (length(vec) == 0) {
        return(NA)
    }

    if (length(vec) <= max) {
        return(paste(vec, collapse = ", "))
    }

    return(glue::glue(
        "{items}, and {remaining} {others}",
        items = paste(vec[1:max], collapse = ", "),
        remaining = length(vec) - max,
        others = ifelse(length(vec) - max > 1, "others", "other")
    ))
}
