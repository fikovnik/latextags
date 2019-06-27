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
})

test_that("fmt formats vector", {
    expect_equal(fmt(c(1.234, 2.345), prefix="$", digits=1), c("$1.2", "$2.3"))
})

test_that("fmt formats percent", {
    expect_equal(fmt(percent(c(1.1234, 0.9959)), digits=1), c("112.3%", "99.6%"))
})

test_that("fmt formats approximations", {
    expect_equal(fmt(approx(c(1234, 123, 1234567, 123456))), c("1.2K", "123.0", "1.2M", "123.5K"))
})

test_that("fmt formats negative approximations", {
    expect_equal(fmt(approx(-1 * c(1234, 123, 1234567, 123456))), c("-1.2K", "-123.0", "-1.2M", "-123.5K"))
})
