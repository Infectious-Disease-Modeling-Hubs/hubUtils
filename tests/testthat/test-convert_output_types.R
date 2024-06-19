### test convert_output_type()
test_that("convert_output_type works (quantile >> mean)", {
  ex_qs <- seq(0, 1, length.out = 500)[2:499]
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "quantile",
    output_type_id <- ex_qs,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = qnorm(ex_qs, mean)) %>%
    dplyr::select(-mean)
  new_output_type <- "mean"
  new_output_type_id <- NA
  expected <- tibble::tibble(
    grp1 = rep(1:2, 2), model_id = sort(rep(LETTERS[1:2], 2))
  ) %>%
    dplyr:: mutate(value = grp1 * ifelse(model_id == "A", 1, 3)) %>%
    dplyr::mutate(output_type <- new_output_type,
                  output_type_id <- new_output_type_id) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type works (quantile >> median)", {
  ex_qs <- seq(0, 1, length.out = 500)[2:499]
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "quantile",
    output_type_id <- ex_qs,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = qnorm(ex_qs, mean)) %>%
    dplyr::select(-mean)
  new_output_type <- "median"
  new_output_type_id <- NA
  expected <- tibble::tibble(
    grp1 = rep(1:2, 2), model_id = sort(rep(LETTERS[1:2], 2))
  ) %>%
    dplyr:: mutate(value = grp1 * ifelse(model_id == "A", 1, 3)) %>%
    dplyr::mutate(output_type <- new_output_type,
                  output_type_id <- new_output_type_id) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type works (quantile >> cdf)", {
  ex_qs <- seq(0, 1, length.out = 500)[2:499]
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "quantile",
    output_type_id <- ex_qs,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = qnorm(ex_qs, mean)) %>%
    dplyr::select(-mean)
  new_output_type <- "cdf"
  new_output_type_id <- seq(-2, 2, 0.5)
  expected <- tibble::as_tibble(expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- new_output_type,
    output_type_id <- new_output_type_id,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )) %>%
    dplyr:: mutate(value = pnorm(output_type_id, grp1 * ifelse(model_id == "A", 1, 3))) %>%
    dplyr::arrange(model_id, grp1) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type works (cdf >> mean)", {
  ex_ps <- seq(-2, 10, length.out = 500)[2:499]
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- ex_ps,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = pnorm(output_type_id, mean)) %>%
    dplyr::select(-mean)
  new_output_type <- "mean"
  new_output_type_id <- NA
  expected <- tibble::tibble(
    grp1 = rep(1:2, 2), model_id = sort(rep(LETTERS[1:2], 2))
  ) %>%
    dplyr:: mutate(value = grp1 * ifelse(model_id == "A", 1, 3)) %>%
    dplyr::mutate(output_type <- new_output_type,
                  output_type_id <- new_output_type_id) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type works (cdf >> median)", {
  ex_ps <- seq(-2, 10, length.out = 500)[2:499]
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- ex_ps,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = pnorm(output_type_id, mean)) %>%
    dplyr::select(-mean)
  new_output_type <- "median"
  new_output_type_id <- NA
  expected <- tibble::tibble(
    grp1 = rep(1:2, 2), model_id = sort(rep(LETTERS[1:2], 2))
  ) %>%
    dplyr:: mutate(value = grp1 * ifelse(model_id == "A", 1, 3)) %>%
    dplyr::mutate(output_type <- new_output_type,
                  output_type_id <- new_output_type_id) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_output_type(model_out_tbl,
                              new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type works (cdf >> quantile)", {
  ex_ps <- seq(-2, 10, length.out = 500)[2:499]
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- ex_ps,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = pnorm(output_type_id, mean)) %>%
    dplyr::select(-mean)
  new_output_type <- "quantile"
  new_output_type_id <- c(0.25, 0.5, 0.75)
  expected <- tibble::as_tibble(expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- new_output_type,
    output_type_id <- new_output_type_id,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )) %>%
    dplyr:: mutate(value = qnorm(output_type_id, grp1 * ifelse(model_id == "A", 1, 3))) %>%
    dplyr::arrange(model_id, grp1) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_output_type(model_out_tbl,
                              new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type works (sample >> quantile, cdf, mean)", {
  ex_bins <- seq(-2, 2, 1)
  ex_quantiles <- c(0.25, 0.5, 0.75)
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "sample",
    output_type_id <- 1:1e5,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = rnorm(dplyr::n(), mean)) %>%
    dplyr::select(-mean)
  new_output_type <- c("mean", "quantile", "cdf")
  new_output_type_id <- list("quantile" = ex_quantiles, "cdf" = ex_bins)
  expected_quantile <- tibble::as_tibble(expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "quantile",
    output_type_id <- ex_quantiles,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )) %>%
    dplyr:: mutate(value = qnorm(output_type_id, grp1 * ifelse(model_id == "A", 1, 3))) %>%
    dplyr::arrange(model_id, grp1) %>%
    hubUtils::as_model_out_tbl()
  expected_cdf <- tibble::as_tibble(expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- ex_bins,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )) %>%
    dplyr:: mutate(value = pnorm(output_type_id, grp1 * ifelse(model_id == "A", 1, 3))) %>%
    dplyr::arrange(model_id, grp1) %>%
    hubUtils::as_model_out_tbl()
  expected_mean <- tibble::as_tibble(expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "mean",
    output_type_id <- NA,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )) %>%
    dplyr:: mutate(value = grp1 * ifelse(model_id == "A", 1, 3)) %>%
    dplyr::arrange(model_id, grp1) %>%
    hubUtils::as_model_out_tbl()
  expected <- dplyr::bind_rows(expected_mean, expected_quantile, expected_cdf)
  test <- convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type fails correctly (quantile)", {
  ex_ps <- seq(-2, 10, length.out = 500)[2:499]
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- ex_ps,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(mean = grp1 * ifelse(model_id == "A", 1, 3),
                  value = pnorm(output_type_id, mean)) %>%
    dplyr::select(-mean)
  new_output_type <- "quantile"
  new_output_type_id <- c(0.25, 0.5, 0.75)
  expected <- tibble::as_tibble(expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- new_output_type,
    output_type_id <- new_output_type_id,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )) %>%
    dplyr:: mutate(value = qnorm(output_type_id, grp1 * ifelse(model_id == "A", 1, 3))) %>%
    dplyr::arrange(model_id, grp1) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("convert_output_type fails correctly: wrong starting output_type", {
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "pmf",
    output_type_id <- c("bin1", "bin2"),
    stringsAsFactors = FALSE
  )
  new_output_type <- "mean"
  expect_error(convert_output_type(model_out_tbl, new_output_type))
})

test_that("convert_output_type fails correctly: wrong new_output_type (quantile >> pmf)", {
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "quantile",
    output_type_id <- c(0.25, 0.5, 0.75),
    stringsAsFactors = FALSE
  )
  new_output_type <- "pmf"
  expect_error(convert_output_type(model_out_tbl, new_output_type))
})

test_that("convert_output_type fails correctly: wrong new_output_type (cdf >> sample)", {
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- -1:1,
    stringsAsFactors = FALSE
  )
  new_output_type <- "sample"
  expect_error(convert_output_type(model_out_tbl, new_output_type))
})

test_that("convert_output_type fails correctly: wrong new_output_type_id (mean)", {
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- -1:1,
    stringsAsFactors = FALSE
  )
  new_output_type <- "mean"
  new_output_type_id <- c("A", "B")
  expect_error(convert_output_type(model_out_tbl, new_output_type, new_output_type_id))
})

test_that("convert_output_type fails correctly: wrong new_output_type_id (quantile)", {
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "cdf",
    output_type_id <- -1:1,
    stringsAsFactors = FALSE
  )
  new_output_type <- "quantile"
  new_output_type_id <- c(-1, 0, 1)
  expect_error(
    convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  )
})

test_that("convert_output_type fails correctly: wrong new_output_type_id (cdf)", {
  model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type <- "quantile",
    output_type_id <- seq(0, 1, 0.5),
    stringsAsFactors = FALSE
  )
  new_output_type <- "cdf"
  new_output_type_id <- c("A", "B")
  expect_error(
    convert_output_type(model_out_tbl, new_output_type, new_output_type_id)
  )
})

### test convert_from_sample()
test_that("convert_from_sample works (return mean)", {
  grouped_model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type_id <- 1:5
  ) %>%
    dplyr::mutate(value = grp1 * ifelse(model_id == "A", 1, 3) * output_type_id) %>%
    dplyr::group_by(grp1, model_id)
  new_output_type <- "mean"
  new_output_type_id <- NA
  expected <- grouped_model_out_tbl %>%
    dplyr::reframe(value = mean(value)) %>%
    dplyr::mutate(output_type <- new_output_type,
                  output_type_id <- new_output_type_id) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_from_sample(grouped_model_out_tbl, new_output_type,
                              new_output_type_id)
  expect_equal(test, expected)
})

test_that("convert_from_sample works (return median)", {
  grouped_model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type_id <- 1:5
  ) %>%
    dplyr::mutate(value = grp1 * ifelse(model_id == "A", 1, 3) * output_type_id) %>%
    dplyr::group_by(grp1, model_id)
  new_output_type <- "median"
  new_output_type_id <- NA
  expected <- grouped_model_out_tbl %>%
    dplyr::reframe(value = median(value)) %>%
    dplyr::mutate(output_type <- new_output_type,
                  output_type_id <- new_output_type_id) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_from_sample(grouped_model_out_tbl, new_output_type,
                              new_output_type_id)
  expect_equal(test, expected)
})

test_that("convert_from_sample works (return quantile)", {
  grouped_model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type_id <- 1:5
  ) %>%
    dplyr::mutate(value = grp1 * ifelse(model_id == "A", 1, 3) * output_type_id) %>%
    dplyr::group_by(grp1, model_id)
  new_output_type <- "quantile"
  new_output_type_id <- c(0.25, 0.75)
  expected <- grouped_model_out_tbl %>%
    dplyr::reframe(value = quantile(value, new_output_type_id, names = FALSE),
                   output_type_id <- new_output_type_id) %>%
    dplyr::mutate(output_type <- new_output_type) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_from_sample(grouped_model_out_tbl, new_output_type,
                              new_output_type_id)
  expect_equal(test, expected)
})

test_that("convert_from_sample works (return cdf)", {
  grouped_model_out_tbl <- expand.grid(
    grp1 = 1:2,
    model_id = LETTERS[1:2],
    output_type_id <- 1:5
  ) %>%
    dplyr::mutate(value = grp1 * ifelse(model_id == "A", 1, 3) * output_type_id) %>%
    dplyr::group_by(grp1, model_id)
  new_output_type <- "cdf"
  new_output_type_id <- seq(0, 30, 5)
  expected <- grouped_model_out_tbl %>%
    dplyr::reframe(value = ecdf(value)(new_output_type_id),
                   output_type_id <- new_output_type_id) %>%
    dplyr::mutate(output_type <- new_output_type) %>%
    hubUtils::as_model_out_tbl()
  test <- convert_from_sample(grouped_model_out_tbl, new_output_type,
                              new_output_type_id)
  expect_equal(test, expected)
})
