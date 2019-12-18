#' Plot Meat Weight ~ Round Weight
#'
#' Scatter plot of meat weight as a function of round weight.
#' @param scal_awl Output of `clean_awl()`. See function help file.
#' @param tows Output of `clean_tow()`. See function help file.
#' @param YEAR Survey year. Default is defined in global options.
#'
#' @return
#' @details Creates a sactter plot of meat weight as a function of round weight. Lines are drawn using `geom_smooth()` and respresent a genral additive model y ~ s(x) fit using cubic regression splines. Saves output to the appropriate figs folder.
#' @export plot_mw_rw
#'
#' @examples
#' plot_mw_rw(scal_awl, tows, YEAR)

plot_mw_rw <- function(scal_awl, tows, YEAR){

  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(bed_dist_area_names, by = c("Bed" = "Bed_Code")) %>%
    ggplot(aes(rwt_lb, mwt_lb, color = District)) +
    geom_point(alpha = 0.12) +
    geom_smooth(se = F, method = "gam", formula = y ~ s(x, bs = "cs")) +
    labs(y = "Meat weight (lb)", x = "Round weight (lb)") +
    scale_color_grey() +
    theme(legend.justification=c(1,0), legend.position=c(.2,.65)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/mwt_rwt.png")),
         plot = x, width = 6.5, height = 4, units = "in")

  x

}
