test_that("cross_df_safe works", {
  to_cross_list <- list(
    v1 = c("a", "b"),
    v2 = list(3, 4:5)
  )
  expect_identical(
    get_cols_to_protect(to_cross_list),
    2L
  )

  to_cross_list_fn <- list(
    v1 = c("a", "b"),
    v3 = list("1", function(x) x)
  )
  expect_identical(
    get_cols_to_protect(to_cross_list_fn),
    integer(0)
  )
  expect_identical(
    cross_df_safe(to_cross_list),
    structure(list(v1 = c("a", "b", "a", "b"),
                   v2 = list(3, 3, 4:5,
                             4:5)),
              row.names = c(NA, -4L),
              class = c("tbl_df", "tbl", "data.frame"))
  )

  to_cross_list_list <- list(
    v1 = c("a", "b"),
    v3 = list("1", list("2"))
  )
  expect_identical(
    cross_df_safe(to_cross_list_list),
    structure(list(v1 = c("a", "b", "a", "b"),
                   v3 = list("1", "1",
                             list("2"), list("2"))),
              row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))
  )
})
