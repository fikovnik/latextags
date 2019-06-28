context("tags")

test_with_tmp_tag <- function(desc, f) {
    tmp <- tempfile()
    tgs <- create_tags(tmp, default=FALSE)
    tryCatch({
        test_that(desc, f(tgs))
    }, finally=file.remove(tmp))
}

test_that("create_tag_file", {
    tmp <- tempfile()
    tgs <- create_tags("A", prefix="abc", default=FALSE)

    expect_equal(tgs$filename, "A")
    expect_equal(tgs$prefix, "abc")
    expect_equal(nrow(tgs$tags), 0)
})


test_with_tmp_tag("tag", function(tgs) {
    tag("X", 1+1, tags=tgs)
    expect_equal(nrow(tgs$tags), 1)
    expect_equal(tgs$tags$value[1], "2")

    tag("Y", 2+2, tags=tgs)
    expect_equal(nrow(tgs$tags), 2)
    expect_equal(tgs$tags$value[1], "2")
    expect_equal(tgs$tags$value[2], "4")

    tag("X", 5+5, tags=tgs)
    expect_equal(nrow(tgs$tags), 2)
    expect_equal(tgs$tags$value[1], "10")
    expect_equal(tgs$tags$value[2], "4")
})

test_with_tmp_tag("generate_latex_commands", function(tgs) {
    tag("X", 1, tags=tgs)
    tag("Y", 2, tags=tgs)

    out <- generate_latex_commands(tgs$tags)
    expect_equal(out, "\\newcommand{\\X}{1\\xspace}\n\\newcommand{\\Y}{2\\xspace}")
})

test_with_tmp_tag("generate_latex_commands escapes text", function(tgs) {
    tag("My name", 1, tags=tgs)
    tag("some other name (detail to be ignored (even more details)", 2, tags=tgs)

    out <- generate_latex_commands(tgs$tags)
    expect_equal(out, "\\newcommand{\\MyName}{1\\xspace}\n\\newcommand{\\SomeOtherName}{2\\xspace}")
})


test_with_tmp_tag("tag immediately saves the file", function(tgs) {
    expect_equal(readLines(tgs$filename), "")
    tag("X", 1, tags=tgs)
    expect_equal(readLines(tgs$filename), "\\newcommand{\\X}{1\\xspace}")

    tag("Y", 2, tags=tgs)
    expect_equal(readLines(tgs$filename), c("\\newcommand{\\X}{1\\xspace}", "\\newcommand{\\Y}{2\\xspace}"))

    tag("X", 3, tags=tgs)
    expect_equal(readLines(tgs$filename), c("\\newcommand{\\X}{3\\xspace}", "\\newcommand{\\Y}{2\\xspace}"))
})
