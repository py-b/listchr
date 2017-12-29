context("Test chr_to_list")

test_that("separators are recognized", {
  expect_equal(
    chr_to_list("1;2;3#a;b",  list_sep = "#"),
    list(list(c("1", "2", "3"), c("a", "b")))
  )
  expect_equal(
    chr_to_list("1,2,3|a,b",  atom_sep = ","),
    list(list(c("1", "2", "3"), c("a", "b")))
  )
})

test_that("length 1 examples gives expected results", {
  expect_equal(
    chr_to_list("1;2;3|a;b"),
    list(list(c("1", "2", "3"), c("a", "b")))
  )
  expect_equal(
    chr_to_list("1;2;3"),
    list(c("1", "2", "3"))
  )
  expect_equal(
    chr_to_list("1|2|3"),
    list(list("1", "2", "3"))
  )
})

test_that("length > 1 examples gives expected results", {
  expect_equal(
    chr_to_list(c("1;3", "2|4")),
    list(list(c("1", "3")), list("2", "4"))
  )
  expect_equal(
    chr_to_list(c("1;3", "2;4")),
    list(c("1", "3"), c("2", "4"))
  )
})
