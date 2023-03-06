library(dplyr)

char_iris <-
    iris %>%
    mutate(across(everything(), as.character)) %>%
    glimpse()


type_continuous <- function(.df, ..., .args = list()) {
    parser <- function(col, arglist) {
        rlang::exec(readr::parse_number, as.character(col), !!!arglist)
    }

    # c(...) is needed for programming. https://stackoverflow.com/a/70749748/5578429
    dplyr:: mutate(.df, across(c(...), ~ parser(., .args)))
}


type_discrete <- function(.df, ..., .args = list()) {
    parser <- function(col, arglist) {
        rlang::exec(readr::parse_integer, as.character(col), !!!arglist)
    }

    dplyr:: mutate(.df, across(c(...), ~ parser(., .args)))
}


type_factor <- function(.df, ..., .args = list()) {
    parser <- function(col, arglist) {
        rlang::exec(readr::parse_factor, as.character(col), !!!arglist)
    }

    dplyr:: mutate(.df, across(c(...), ~ parser(., .args)))
}

type_continuous(char_iris, matches("Length|Width"), .args = list(na = "5.1")) %>% glimpse()
type_discrete(char_iris, matches("Length|Width")) %>% glimpse()
type_factor(char_iris, Species) %>% glimpse()




type_factor <- function(vec, ...) {
    readr::parse_factor(x = as.character(vec), ...)
}


type_character <- function(vec, ...) {
    readr::parse_character(x = as.character(vec), ...)
}


type_binary <- function(vec, .trueif = NULL, ...) {
    if (is.null(.trueif) == FALSE) {
        fn <- rlang::as_function(.trueif)
        vec <- fn(vec)
    }

    readr::parse_logical(x = as.character(vec), ...)
}
