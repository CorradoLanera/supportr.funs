#' Ranking
#'
#' @return
#' @export
#'
#' @examples
make_rank <- function(x) {
  x |>
    dplyr::with_groups(
      snp_exposure,
      dplyr::mutate,
      proxy_rank = dplyr::min_rank(1 - r2_proxy))
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
filter_optimal_proxy <- function(x) {
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
