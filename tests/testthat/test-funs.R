test_that("make_rank works in a standard environment", {
  # setup

  # execution
  ranked <- make_rank(test_df, "snp_exposure", "r2_proxy")
  res <- ranked |>
    dplyr::filter(snp_exposure == "rs14658") |>
    dplyr::pull(proxy_rank)

  # test
  expect_equal(res, c(1, 1, 3, 4, 5))
})

test_that("make_rank works with possible var name clashes", {
  # setup
  r2_proxy <- 1

  # execution
  ranked <- make_rank(test_df, "snp_exposure", "r2_proxy")
  res <- ranked |>
    dplyr::filter(snp_exposure == "rs14658") |>
    dplyr::pull(proxy_rank)

  # test
  expect_equal(res, c(1, 1, 3, 4, 5))
})




test_that("filter_optimal_proxy works", {
  # setup
  ranked <- make_rank(test_df, "snp_exposure", "r2_proxy")

  # execution
  filtered <- filter_optimal_proxies(
    ranked, "snp_proxy", "pval_exposure"
  )
  proxy_rank <- filtered[["proxy_rank"]]

  # test
  expect_equal(proxy_rank, rep(1, 3))
})










test_that("pick_unique_proxy works", {
  # setup
  filtered <- test_df |>
    make_rank("snp_exposure", "r2_proxy") |>
    filter_optimal_proxies("snp_proxy", "pval_exposure")

  # execution
  distinct_proxy <- pick_unique_proxy(filtered, "snp_exposure")
  snp_exposure_extracted <- distinct_proxy[["snp_exposure"]]

  # test
  expect_equal(snp_exposure_extracted, unique(snp_exposure_extracted))
})


test_that("extract_unique_optimal_proxy works as a shortcut", {
  # setup
  ranked <- make_rank(test_df, "snp_exposure", "r2_proxy")
  expected <-  ranked |>
    filter_optimal_proxy("snp_proxy", "pval_exposure") |>
    pick_unique_proxy()

  # execution
  obtained <- ranked |>
    extract_unique_optimal_proxy(
      "snp_proxy", "pval_exposure", "snp_exposure"
    )

  # test
  expect_equal(expected, obtained)
})



test_that("select_only_proxies works", {
  # setup
  unique_proxy_df <- make_rank(test_df, "snp_exposure", "r2_proxy") |>
    extract_unique_optimal_proxy(
      "snp_proxy", "pval_exposure", "snp_exposure"
    )

  # execution
  proxy_alone <- select_snps_and_proxies(unique_proxy_df)

  # test
  expect_named(
    proxy_alone,
    c("snp_exposure", "snp_outcome_proxy", "proxy_rank")
  )
})



test_that("remove_optimal_proxies works", {
  # setup
  ranked <- make_rank(test_df, "snp_exposure", "r2_proxy")
  proxy_alone <- ranked |>
    extract_unique_optimal_proxy(
      "snp_proxy", "pval_exposure", "snp_exposure"
    ) |>
    select_snps_and_proxies()

  # exectution
  res <- remove_optimal_proxies(ranked, proxy_alone)

  # test
  expect_s3_class(res, "tbl_df")

  expect_named(res, names(ranked))

  intersect(
    proxy_alone[["snp_exposure"]],
    res[["snp_exposure"]]
  ) |>
    expect_length(0)

  intersect(
    proxy_alone[["snp_outcome_proxy"]],
    res[["snp_outcome_proxy"]]
  ) |>
    expect_length(0)
})


test_that("filter_optimal_proxy works on the second iteration", {
  # setup
  base_df <- make_rank(test_df, "snp_exposure", "r2_proxy")

  first_optimals <- base_df |>
    extract_unique_optimal_proxy(
      "snp_proxy", "pval_exposure", "snp_exposure"
    ) |>
    select_snps_and_proxies()
  first_df <- remove_optimal_proxies(base_df, first_optimals)


  # execution
  second_optimals <- first_df |>
    extract_unique_optimal_proxy(
      "snp_proxy", "pval_exposure", "snp_exposure"
    ) |>
    select_snps_and_proxies()
  second_df <- remove_optimal_proxies(first_df, second_optimals)

  # test
  expect_s3_class(second_df, "tbl_df")
  expect_named(second_df, names(base_df))

  expect_equal(second_df[["snp_exposure"]], character())
})
