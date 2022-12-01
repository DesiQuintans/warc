# Functions that are hidden from package users; they're used internally in other functions.
# These packages don't need a full Roxygen documentation block, since a Help file is never
# generated for them.



# Short alias for rounding to fixed decimal places (2 by default)
# dp(rnorm(10))
dp <- function(num, fun, p = 2) {
    round(num, p)
}
