#' ---
#' title: "Untitled"
#' author: "Corrado Lanera"
#' date: "10/25/2021"
#' output: html_document
#' ---
#'
#' ```{r setup, include=FALSE}
#' knitr::opts_chunk$set(echo = TRUE)
#' ```
#'

#'
#' ## Packages
#'
library(tidyverse)
devtools::load_all()


#'
#' ## Data
#'

#'
#' ## Rank
#'
ranked <- test_df |>
  make_rank()

rank_1 <- ranked |>
  extract_unique_optimal_proxy() |>
  select_snps_and_proxies()


ranked |>
  remove_optimal_proxies(rank_1)
