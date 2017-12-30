context("Test list_to_chr")

test_that("length 1 examples gives expected results", {
  expect_equal(
    list_to_chr(list(c("a", "b"), 1:3, 4)),
    "a;b|1;2;3|4"
  )
  expect_equal(
    list_to_chr(list(letters, LETTERS), simplify_threshold = 10),
    "a->z|A->Z"
  )
})

test_that("separators are recognized", {
  expect_equal(
    list_to_chr(list(1:3, 4), atom_sep = ","),
    "1,2,3|4"
  )
  expect_equal(
    list_to_chr(list(1:3, 4), list_sep = "#"),
    "1;2;3#4"
  )
  expect_equal(
    list_to_chr(list(1:3, 4), atom_sep = " ", list_sep = " # "),
    "1 2 3 # 4"
  )
})

test_that("simplify works", {
  expect_equal(
    list_to_chr(list(1:3, 4:5), simplify_threshold = 2),
    "1->3|4->5"
  )
  expect_equal(
    list_to_chr(list(1:3, 4:5), simplify_threshold = 3),
    "1->3|4;5"
  )
  expect_equal(
    list_to_chr(list(1, 2), simplify_threshold = 1),
    "1|2"
  )
  expect_equal(
    list_to_chr(
      list(1:3, 4:5),
      simplify_threshold = 2,
      simplify_sep = ":"
    ),
    "1:3|4:5"
  )
  expect_warning(
    list_to_chr(list(1:3), simplify_sep = ":")
  )
  expect_error(
    list_to_chr(list(1:3), simplify_threshold = -1)
  )
})

