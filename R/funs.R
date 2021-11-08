#' Rank the proxies based on their correlations
#'
#' For each SNP, rank the proxies based on their correlations measured
#' with r2
#'
#' @param data (data.frame) the data containing SNPs and proxies
#' @param snp_exposure (character) the name of the column in `data` that
#'                                 contains the SNP (either rs number or
#'                                 chromosome:position)
#' @param r2_proxy (double) the name of the column in `data` that
#'                          contains the correlation between the SNP
#'                          and its proxy measure with r2
#'
#' @return an object of class tibble
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_df |>
#'   make_rank("snp_exposure", "r2_proxy")
#'
#' }
#'
#'
#'
make_rank <- function(data, snp_exposure, r2_proxy) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_character(snp_exposure)
  assertive::assert_is_character(r2_proxy)

  data |>
    dplyr::with_groups(
      .data[[snp_exposure]],
      dplyr::mutate,
      proxy_rank = dplyr::min_rank(1 - .data[[r2_proxy]]))

}


#' Filter optimal proxies
#'
#' For each SNP, filter the proxies with the highest correlations
#' measured with r2
#'
#' @param data (data.frame) the data containing SNPs and proxies obtained
#'                          after `make_rank`
#' @param snp_proxy (character) the name of the column in `data` that
#'                              contains the names of the proxies (
#'                              either rs number of chromosome:position)
#' @param pval_exposure (character) the name of the column in `data` that
#'                                  contains the p-value for the
#'                                  association between the SNP and the
#'                                  exposure
#'
#' @return an object of class tibble
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_df |>
#'   make_rank("snp_exposure", "r2_proxy") |>
#'   filter_optimal_proxies("snp_proxy", "pval_exposure")
#'
#' }
#'
filter_optimal_proxies <- function(
  data, snp_proxy, pval_exposure
) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_character(snp_proxy)
  assertive::assert_is_character(pval_exposure)

  message("!!! RESTART FROM HERE !!!")
  # current_min_rank <- min(x[["proxy_rank"]])

  data |>
    dplyr::with_groups(
      .data[[snp_proxy]],
      dplyr::filter,
      proxy_rank == 1, # current_min_rank,
      pval_exposure == min(.data[[pval_exposure]])
    )
}



#' Pick unique proxy
#'
#' For each SNP, retrieved just one proxy among the most correlated
#'
#' @param data (data.frame) the data containing SNPs and proxies obtained
#'                          after `filter_optimal_proxies`
#' @param snp_exposure (character) the name of the column in `data` that
#'                                 contains the SNP (either rs number or
#'                                 chromosome:position)
#'
#' @return an object of class tibble
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_df |>
#'   make_rank("snp_exposure", "r2_proxy") |>
#'   filter_optimal_proxies("snp_proxy", "pval_exposure") |>
#'   pick_unique_proxz("snp_exposure")
#'
#' }
#'
pick_unique_proxy <- function(data, snp_exposure) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_character(snp_exposure)

  data |>
    dplyr::distinct(snp_exposure, .keep_all = TRUE)
}


#' Extract the optimal proxy
#'
#' For each SNP, collect the most correlated proxy
#'
#' @param data (data.frame) the data containing SNPs and proxies
#'                          obtained after `make_rank`
#' @param snp_proxy (character) the name of the column in `data` that
#'                              contains the names of the proxies (
#'                              either rs number of chromosome:position)
#' @param pval_exposure (character) the name of the column in `data` that
#'                                  contains the p-value for the
#'                                  association between the SNP and the
#'                                  exposure
#' @param snp_exposure (character) the name of the column in `data` that
#'                                 contains the SNP (either rs number or
#'                                 chromosome:position)
#'
#' @return an object of class tibble
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_df |>
#'   make_rank("snp_exposure", "r2_proxy") |>
#'   extract_unique_optimal_proxy(
#'   "snp_proxy", "pval_exposure", "snp_exposure"
#'   )
#'
#' }
#'
extract_unique_optimal_proxy <- function(
  data, snp_proxy, pval_exposure, snp_exposure
) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_character(snp_proxy)
  assertive::assert_is_character(pval_exposure)
  assertive::assert_is_character(snp_exposure)

  data |>
    filter_optimal_proxies(snp_proxy, pval_exposure) |>
    pick_unique_proxy(snp_exposure)
}


#' Select SNPs and proxies
#'
#' Select the SNPs and their proxies along with the rank based on
#' the correlation
#'
#' @param data (data.frame) the data containing SNPs and proxies obtained
#'                          after `extract_unique_optimal_proxy`
#'
#' @return an object of class tibble
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' test_df |>
#'   make_rank("snp_exposure", "r2_proxy") |>
#'   extract_unique_optimal_proxy(
#'   "snp_proxy", "pval_exposure", "snp_exposure"
#'   ) |>
#'   select_snps_and_proxies()
#'
#' }
#'
select_snps_and_proxies <- function(data) {

  assertive::assert_is_data.frame(data)

  data |>
    dplyr::select(dplyr::starts_with("snp"), proxy_rank)
}


#' Remove SNPs with proxies from the initial data.frame
#'
#' Remove from the initial data.frame the SNPs that received a proxy
#' (along with their proxies)
#'
#' @param .from (data.frame) the initial data containing all the SNPs
#'                           and their proxies
#' @param .optimal (data.frame) the data.frame obtained after
#'                             `select_snps_and_proxies` that contains
#'                             the SNPs and their associated proxies
#' @param snp_exposure (character) the name of the column in `.from`
#'                                 that contains the SNP (either rs
#'                                 number or chromosome:position)
#' @param snp_proxy (character) the name of the column in `.optimal` that
#'                              contains the names of the proxies (
#'                              either rs number of chromosome:position)
#' @return an object of class tibble
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' opt_df <- test_df |>
#'   make_rank("snp_exposure", "r2_proxy") |>
#'   extract_unique_optimal_proxy(
#'   "snp_proxy", "pval_exposure", "snp_exposure"
#'   ) |>
#'   select_snps_and_proxies()
#'
#' final_df <- remove_optimal_proxies(
#'   test_df, opt_df, "snp_exposure", "snp_proxy"
#' )
#'
#' }
#'
remove_optimal_proxies <- function(
  .from, .optimal, snp_exposure, snp_proxy
) {

  assertive::assert_is_data.frame(.from)
  assertive::assert_is_data.frame(.optimal)
  assertive::assert_is_character(snp_exposure)
  assertive::assert_is_character(snp_proxy)

  .from |>
    dplyr::anti_join(
      .optimal[snp_exposure],
      by = snp_exposure
    ) |>
    dplyr::anti_join(
      .optimal[snp_proxy],
      by = snp_proxy
    )
}
