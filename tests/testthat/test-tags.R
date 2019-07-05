context("tags")

test_with_tmp_tag <- function(desc, f) {
    tmp <- tempfile()
    tags <- create_tags(tmp, default=FALSE)
    tryCatch({
        test_that(desc, f(tags))
    }, finally=file.remove(tmp))
}

test_that("create_tag_file", {
    tmp <- tempfile()
    tags <- create_tags(tmp, prefix="abc", default=FALSE)

    tryCatch({
        expect_equal(tags_filename(tags), tmp)
    }, finally= file.remove(tmp))

    expect_equal(tags_prefix(tags), "abc")
    expect_equal(nrow(tags(tags)), 0)
    expect_equal(tags_latex(tags), character())
})


test_with_tmp_tag("tag", function(tags) {
    tag("X", 1+1, tags=tags)
    expect_equal(nrow(tags(tags)), 1)
    expect_equal(tags(tags)$value[1], "2")

    tag("Y", 2+2, tags=tags)
    expect_equal(nrow(tags(tags)), 2)
    expect_equal(tags(tags)$value[1], "2")
    expect_equal(tags(tags)$value[2], "4")

    tag("X", 5+5, tags=tags)
    expect_equal(nrow(tags(tags)), 2)
    expect_equal(tags(tags)$value[1], "10")
    expect_equal(tags(tags)$value[2], "4")
})

test_with_tmp_tag("generate_latex_commands", function(tags) {
    tag("X", 1, tags=tags)
    tag("Y", 2, tags=tags)

    expect_equal(tags_latex(tags), c("\\newcommand{\\X}{1\\xspace}", "\\newcommand{\\Y}{2\\xspace}"))
})

test_with_tmp_tag("generate_latex_commands escapes text", function(tags) {
    tag("My name", 1, tags=tags)
    tag("some other name (detail to be ignored (even more details)", 2, tags=tags)

    expect_equal(tags_latex(tags), c("\\newcommand{\\MyName}{1\\xspace}", "\\newcommand{\\SomeOtherName}{2\\xspace}"))
})


test_with_tmp_tag("tag immediately saves the file", function(tags) {
    expect_equal(readLines(tags_filename(tags)), "")
    tag("X", 1, tags=tags)
    expect_equal(readLines(tags_filename(tags)), "\\newcommand{\\X}{1\\xspace}")

    tag("Y", 2, tags=tags)
    expect_equal(readLines(tags_filename(tags)), c("\\newcommand{\\X}{1\\xspace}", "\\newcommand{\\Y}{2\\xspace}"))

    tag("X", 3, tags=tags)
    expect_equal(readLines(tags_filename(tags)), c("\\newcommand{\\X}{3\\xspace}", "\\newcommand{\\Y}{2\\xspace}"))
})
