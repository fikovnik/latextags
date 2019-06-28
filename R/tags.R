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
    tgs <- new.env(parent=emptyenv())
    tgs$filename <- filename
    tgs$prefix <- prefix
    tgs$tags <- tibble::tibble(name=character(), value=character())

    if (default) {
        set_default_tags(tgs)
    }

    writeLines("", filename)

    class(tgs) <- "latextags"
    tgs
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

    existing <- which(tags$tags$name == name)

    if (length(existing) > 0) {
        tags$tags[existing, "value"] <- value
    } else {
        tags$tags <- tibble::add_row(
            tags$tags,
            name,
            value
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

generate_latex_commands <- function(tags, prefix = "") {
    names <- stringr::str_c(prefix, tags$name)
    names <- latex_command_name(names)

    stringr::str_c(
        "\\newcommand{\\", names, "}{", tags$value, "\\xspace}", collapse="\n"
    )
}

save_tag_file <- function(tags) {
    out <- generate_latex_commands(tags$tags)
    writeLines(out, tags$filename)
}

get_default_tags <- function() {
    getOption("latextags_default")
}

set_default_tags <- function(tgs) {
    options(latextags_default=tgs)
}

#' @export
as.list.latextags <- function(x, ...) {
    list(
        filename=x$filename,
        prefix=x$prefix,
        tags=x$tags
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
