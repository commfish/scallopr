#' Plot Aundance or Biomass Estimates
#'
#' Plots abundance or round weight biomass estimates with 95% confidence intervals.
#' @param scal_est Output of `scallop_est()`. See function help file.
#' @param abundance Logical, default = TRUE. If TRUE, plots as abundance. If FALSE, plots as round weight biomass.
#' @param YEAR Survey year. Default is defined in global options.
#'
#' @return
#' @details Plots point estimates and confidence intervals of abundance (abundance = TRUE) or biomass (abundance = FALSE). Saves output to the appropriate 'figs' folder.
#' @export plot_scal_est
#'
#' @examples
#' plot_scal_est(scal_est, abundance = TRUE, YEAR)
#' plot_scal_est(scal_est, abundance = FALSE, YEAR)

plot_scal_est <- function(scal_est, abundance = TRUE, YEAR){

  scal_est %>%
    ungroup %>%
    filter(!(Size %in% c("clapper", "all")))	%>%
    mutate(Bed = factor(Bed, levels = Bed_levels)) %>%
    ggplot(aes(Bed, est / 1000000, color = Size)) +
    geom_point(size = 1.5, position = position_dodge(0.5)) +
    geom_errorbar(aes(ymin = l95 / 1000000, ymax = u95 / 1000000),
                  size = 0.5,
                  width = 0.4,
                  position = position_dodge(.5)) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("small" = "grey70", "large" = "black"),
                       name = "Size class") +
    labs(y = ifelse(abundance, "Abundance (millions)\n",
                    "Round weight Biomass (million lb)\n")) -> x

  if(abundance){
    ggsave(here::here(paste0("figs/", YEAR, "/abund_est.png")), x,
           width = 6.5, height = 4, units = "in")
  } else {
    ggsave(here::here(paste0("figs/", YEAR, "/biom_est.png")), x,
           width = 6.5, height = 4, units = "in")
  }

  x
}
