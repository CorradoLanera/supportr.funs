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
#'   make_rank()
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


#' Filter best proxy
#'
#' For each SNP, filter the proxies with the highest correlations
#' measured with r2
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
filter_optimal_proxy <- function(
  data, snp_proxy, pval_exposure
) {

  assertive::assert_is_data.frame(data)
  assertive::assert_is_character(snp_proxy)
  assertive::assert_is_character(pval_exposure)

  message("!!! RESTART FROM HERE !!!")
  # current_min_rank <- min(x[["proxy_rank"]])

  x |>
    dplyr::with_groups(
      snp_outcome_proxy,
      dplyr::filter,
      proxy_rank == 1, # current_min_rank,
      pval_exposure == min(pval_exposure)
    )
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
pick_unique_proxy <- function(x) {
  x |>
    dplyr::distinct(snp_exposure, .keep_all = TRUE)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extract_unique_optimal_proxy <- function(x) {
  x |>
    filter_optimal_proxy() |>
    pick_unique_proxy()
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
select_only_proxies <- function(x) {
  x |>
    dplyr::select(dplyr::starts_with("snp"), proxy_rank)
}



remove_optimal_proxies <- function(.from, .optimal) {
  .from |>
    dplyr::anti_join(
      .optimal["snp_exposure"],
      by = "snp_exposure"
    ) |>
    dplyr::anti_join(
      .optimal["snp_outcome_proxy"],
      by = "snp_outcome_proxy"
    )
}
