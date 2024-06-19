test_that("get_task_id_cols works", {
  ex_qs <- seq(0, 1, length.out = 5)
  model_outputs <- expand.grid(
    grp1 = 1:2,
    grp2 = 1:3,
    model_id = LETTERS[1:2],
    output_type = "quantile",
    output_type_id = ex_qs,
    stringsAsFactors = FALSE
  )
  model_outputs$value <- runif(nrow(model_outputs))
  expected <- c("grp1", "grp2")
  test <- get_task_id_cols(model_outputs)
  expect_equal(expected, test)
})
