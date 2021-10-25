## code to prepare `DATASET` dataset goes here

test_df <- tibble(
  snp_exposure = c(
    rep("rs10356", 3L),
    rep("rs675890", 4L),
    rep("rs4956867", 2L),
    rep("rs14658", 5L)
  ),
  pval_exposure = c(
    rep(0.005, 3L),
    rep(0.00004, 4L),
    rep(0.10, 2L),
    rep(0.001, 5L)
  ),
  snp_outcome_proxy = c(
    "rs1084", "rs987", "rs76544",
    "rs1084", "rs987", "rs9083", "rs8754",
    "rs1084", "rs987",
    "rs6345239", "rs908237", "rs54984986", "rs948864", "rs44455"
  ),
  r2_proxy = c(
    1, 0.99, 0.94,
    1, 0.99, 0.89, 0.87,
    1, 0.98,
    1, 1, 0.99, 0.95, 0.86
  )
)

usethis::use_data(test_df, overwrite = TRUE, internal = TRUE)
