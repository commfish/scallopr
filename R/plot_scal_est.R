#' plot_scal_est
#'
#' @param scal_est
#' @param abundance
#' @param YEAR
#'
#' @return
#' @export plot_scal_est
#'
#' @examples
#' plot_scal_est(scal_est, abundance = TRUE, YEAR)
#' plot_scal_est(scal_est, abundance = FALSE, YEAR)
#' @description Plot scallop abundance (abundance = TRUE) or biomass (abundance = FALSE) estimates by bed.
#' Saves output to the appropriate figs folder.
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
