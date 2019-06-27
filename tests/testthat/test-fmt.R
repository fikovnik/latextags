test_that("fmt formats string", {
  expect_equal(fmt("A"), "A")
  expect_equal(fmt("A", prefix="1"), "1A")
  expect_equal(fmt("A", suffix="1"), "A1")
  expect_equal(fmt("A", prefix="1", suffix="2"), "1A2")
})


test_that("fmt formats a number", {
    expect_equal(fmt(1), "1")
    expect_equal(fmt(1.234), "1.2")
    expect_equal(fmt(1.234, digits=2), "1.23")
    expect_equal(fmt(1.238, digits=2), "1.24")
    expect_equal(fmt(1.234, floor=T), "1")
    expect_equal(fmt(1.234, ceiling=T), "2")
    expect_equal(fmt(c(123, 123.456), digits=1), c("123", "123.5"))
})

test_that("fmt formats vector", {
    expect_equal(fmt(c(1.234, 2.345), prefix="$", digits=1), c("$1.2", "$2.3"))
})

test_that("fmt formats percent", {
    expect_equal(fmt(percent(c(1.1234, 0.9959)), digits=1), c("112.3%", "99.6%"))
})

test_that("fmt formats orders of magnitude", {
    expect_equal(fmt(oom(c(1234, 123, 1234567, 123456))), c("1.2K", "123", "1.2M", "123.5K"))
})

test_that("fmt formats negative orders of magnitude", {
    expect_equal(fmt(oom(-1 * c(1234, 123, 1234567, 123456))), c("-1.2K", "-123", "-1.2M", "-123.5K"))
})

test_that("fmt formats size", {
    expect_equal(fmt(size(c(123, 1024*123, 1024*1024*123, 1024*1024, 12354))), c("123B", "123kB", "123MB", "1MB", "12.1kB"))
})
