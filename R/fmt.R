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

.fmt.double <- function(x, floor=FALSE, ceiling=FALSE, digits=1) {
  if (floor) x <- floor(x)
  if (ceiling) x <- ceiling(x)
  format(round(x, digits), big.mark=",", trim=TRUE)
}

.fmt.percent <- function(x, floor=FALSE, ceiling=FALSE, digits=0) {
  fmt(as.double(x), suffix="%", floor=floor, ceiling=ceiling, digits=digits)
}

.fmt.approx <- function(x, floor=FALSE, ceiling=FALSE, digits=1) {
  symbol <- attr(x, "symbol")
  fmt(as.double(x), suffix=symbol, floor=floor, ceiling=ceiling, digits=digits)
}

percent <- function(x) {
  class(x) <- "percent"
  x*100
}

approx <- function(x, magnitute=NULL, symbol=NULL) {
    OMV <- c(0, 3, 6, 9, 12)
    OMN <- c("", "K", "M", "G", "T")

    if (is.null(magnitute)) {
        magnitute <- floor(log10(abs(x)))
        magnitute <- floor(magnitute / 3) * 3
        magnitute[magnitute > OMV[length(OMV)]] <- OMV[length(OMV)]
    }

    magnitute <- as.integer(magnitute)

    if (is.null(symbol)) {
        symbol <- sapply(magnitute / 3 + 1, function(x) OMN[x])
    } else {
        stopifnot(length(symbol) %in% c(1, length(magnitute)))
    }

    d <- 10^magnitute
    v <- x/d
    class(v) <- "approx"
    attr(v, "symbol") <- symbol
    v
}
