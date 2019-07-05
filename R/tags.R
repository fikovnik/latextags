#' Create a new tags container
#'
#' It creates a new tags container that will be associated with the given
#' `filename` and the tags will be prefixed with `prefix` string.
#'
#' @param filename the name of the file where the tags will be stored.
#' @param prefix the string to prepend to all tag names.
#' @param default if `TRUE` make this the default tags container.
#'
#' @export
create_tags <- function(filename, prefix="", default=TRUE) {
    tags <- new.env(parent=emptyenv())
    tags$filename <- filename
    tags$prefix <- prefix
    tags$values <- tibble::tibble(name=character(), value=character(), latex=character())

    if (default) {
        set_default_tags(tags)
    }

    writeLines("", filename)

    class(tags) <- "latextags"

    tags
}

tags_filename <- function(tags) {
    tags$filename
}

tags_prefix <- function(tags) {
    tags$prefix
}

tags <- function(tags) {
    tags$values
}

tags_latex <- function(tags) {
    tags(tags)$latex
}

#' Create a new tag
#'
#' Creates a new tag named `name` with value `value` in the `tags` tags
#' container.
#'
#' @param name the tag name.
#' @param value the tag value.
#' @param tags the tag container, using the default container by default.
#'
#' @export
tag <- function(name, value, tags=get_default_tags()) {
    stopifnot(!is.null(tags))
    stopifnot(is.character(name))
    stopifnot(length(name)==1)

    value <- as.character(value)
    latex <- generate_latex_command(name, value, prefix=tags_prefix(tags))

    existing <- which(tags$values$name == name)

    if (length(existing) > 0) {
        tags$values[existing, "value"] <- value
        tags$values[existing, "latex"] <- latex
    } else {
        tags$values <- tibble::add_row(
            tags$values,
            name,
            value,
            latex
        )
    }

    save_tag_file(tags)

    tags
}

latex_command_name <- function(s) {
    s <- stringr::str_split_fixed(s, "\\(", 2)[, 1]
    s <- stringr::str_to_title(s)
    stringr::str_replace_all(s, "\\s", "")
}

generate_latex_command <- function(name, value, prefix = "") {
    stopifnot(length(names) == length(value))

    name <- stringr::str_c(prefix, name)
    name <- latex_command_name(name)

    stringr::str_c(
        "\\newcommand{\\", name, "}{", value, "\\xspace}"
    )
}

save_tag_file <- function(tags) {
    out <- stringr::str_c(tags(tags)$latex, collapse="\n")
    writeLines(out, tags_filename(tags))
}

get_default_tags <- function() {
    getOption("latextags_default")
}

set_default_tags <- function(tags) {
    options(latextags_default=tags)
}

#' @export
as.list.latextags <- function(x, ...) {
    list(
        filename=tags_filename(x),
        prefix=tags_prefix(x),
        tags=tags(x)
    )
}

#' @export
format.latextags <- function(x, ...) {
    format(as.list(x), ...)
}

#' @export
print.latextags <- function(x, ...) {
    print(as.list(x), ...)
}
