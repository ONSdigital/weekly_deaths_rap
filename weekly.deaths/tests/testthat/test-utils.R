test_that("Group by and summarise dataframe", {

  abc <- c("a", "b", "c")

  data <- data.frame(
    col_1 = c(rep("Female", 14), rep("Male", 7)),
    col_2 = factor(c(rep(c("a", "b"), 10), "c"), abc),
    col_3 = 1:21
  )

  expected <- data.frame(
    col_1 = c(rep("Female", 3), rep("Male", 3)),
    col_2 = factor(rep(abc, 2), abc),
    count = c(7, 7, 0, 3, 3, 1)
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  expect_equal(group_by_summarise(data, c("col_1", "col_2")), expected)

})
