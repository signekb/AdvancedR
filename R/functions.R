#' Calculate descriptive stats (mean and standard deviation) of each metabolite
#'
#' @param data Lipidomics dataset (loaded from csv)
#'
#' @return A data.frame/tibble
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(
      mean = mean,
      sd = sd,
      median = median,
      iqr = IQR
    ))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}

#' Figure showing distributions of metabolites
#'
#' @param data Lipidomics data (long format)
#'
#' @return A plot object
plot_distributions <- function(data) {
  ggplot2::ggplot(data, aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(vars(metabolite), scales = "free")
}
