#' Formats given value.
#'
#' The `x` value is formatted according to its class with an additional
#' `prefix` and `suffix` prepended and appended respectively.
#'
#' @param x the value to be formatted
#' @param prefix the string to be prepended
#' @param suffix the string to be appended
#' @param ... parameters to be passed to the actual formatter. The number
#'     formatter accepts `floor` and `ceiling` to indicate that given value
#'     should be rounded down or up to a full integer before formatting or
#'     `digits` to indicate the number of digits the number should be rounded
#'     to.
#'
#' @export
fmt <- function(x, prefix="", suffix="", ...) {
  if (is.null(x)) {
    NULL
  } else {
      v <- as.character(.fmt(x, ...))

    stringr::str_c(prefix, v, suffix)
  }
}

.fmt <- function(x, ...) {
  UseMethod(".fmt", x)
}

.fmt.default <- function(x) {
  x
}

.fmt.integer <- function(x) {
  format(x, big.mark=",", trim=TRUE)
}

.fmt.double <- function(x, digits=1, floor=FALSE, ceiling=FALSE) {
  if (floor) x <- floor(x)
  if (ceiling) x <- ceiling(x)

  prettyNum(round(x, digits), big.mark=",")
}

.fmt.percent <- function(x, floor=FALSE, ceiling=FALSE, digits=0) {
  fmt(as.double(x), suffix="%", floor=floor, ceiling=ceiling, digits=digits)
}

.fmt.num_with_suffix <- function(x, floor=FALSE, ceiling=FALSE, digits=1) {
  suffix <- attr(x, "suffix")
  fmt(as.double(x), suffix=suffix, floor=floor, ceiling=ceiling, digits=digits)
}

#' Converts given value to a percent representation.
#'
#' It multiples the value by 100 and adds the `%` suffix attribute.
#'
#' @param x the value to convert
#'
#' @export
percent <- function(x) {
    x <- x*100
    class(x) <- "num_with_suffix"
    attr(x, "suffix") <- "%"
    x
}

scale_with_suffix <- function(x, base, suffixes) {
    stopifnot(all(base > 0))
    stopifnot(length(suffixes) > 0)
    stopifnot(is.character(suffixes))

    scale <- if (length(base) == 1) {
        base^(1:length(suffixes))
    } else {
        stopifnot(length(base) == length(x))
        base
    }

    suffixes <- suffixes
    magnitute <- floor(log(abs(x), base))
    magnitute[magnitute > max(scale)] <- max(scale)

    stopifnot(which.max(scale) <= length(suffixes))

    magnitute <- as.integer(magnitute)
    suffix <- sapply(magnitute, function(x) suffixes[x + 1])

    d <- base^magnitute
    v <- x / d
    class(v) <- "num_with_suffix"
    attr(v, "suffix") <- suffix
    v
}

#' Scale the value to its closest order of magnitude
#'
#' It uses the base of 1000.
#'
#' @param x the value to scale
#' @param suffixes are the suffixes to be add. Each corresponds to the exponent
#'     of the base.
#'
#' @export
oom <- function(x, suffixes=c("", "K", "M", "G", "T")) {
    scale_with_suffix(x, base=1000, suffixes=suffixes)
}

#' Scale the value to its closest order of magnitude in the size of bytes.
#'
#' It uses the base of 1024.
#'
#' @param x the value to scale
#' @param units are the suffixes to be add. Each corresponds to the exponent
#'     of the base.
#'
#' @export
size <- function(x, units=c("B", "kB", "MB", "GB", "TB")) {
    scale_with_suffix(x, base=1024, suffixes=units)
}
