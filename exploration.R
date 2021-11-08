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


## passaggio iniziale
ranked <- test_df |>
  make_rank()

rank_1 <- ranked |>
  extract_unique_optimal_proxy() |>
  select_snps_and_proxies()

data_2 <- remove_optimal_proxies(ranked, rank_1)







rank_2 <- data_2 |>
  extract_unique_optimal_proxy() |>
  select_snps_and_proxies()

data_3 <- remove_optimal_proxies(data_2, rank_2)




rank_3 <- data_3 |>
  extract_unique_optimal_proxy() |>
  select_snps_and_proxies()

data_4 <- remove_optimal_proxies(data_3, rank_3)



