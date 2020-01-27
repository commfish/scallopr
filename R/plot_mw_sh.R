#' Plot Meat Weight ~ Shell height
#'
#' Scatter plot of meat weight as a function of shell height.
#' @param scal_awl Output of `clean_awl()`. See function help file.
#' @param tows Output of `clean_tow()`. See function help file.
#' @param YEAR Survey year. Default is defined in global options.
#'
#' @return
#' @details Creates a sactter plot of meat weight as a function of shell height. Lines are drawn using `geom_smooth()` and respresent a genral additive model y ~ s(x) fit using cubic regression splines. Saves output to the appropriate figs folder.
#' @export plot_mw_sh
#'
#' @examples
#' plot_mw_sh(scal_awl, tows, YEAR)

plot_mw_sh <- function(scal_awl, tows, YEAR){
  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(bed_dist_area_names, by = c("Bed" = "Bed_Code")) %>%
    ggplot(aes(sh, mwt_lb,  color = District)) +
    geom_point(alpha = 0.1) +
    geom_smooth(se = F, method = "gam", formula = y ~ s(x, bs = "cs")) +
    labs(y = "Meat weight (lb)", x = "Shell height (mm)") +
    scale_color_grey() +
    theme(legend.justification=c(1,0), legend.position=c(.2,.65)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/mwt_sh.png")),
         plot = x, width = 6.5, height = 4, units = "in")

  x

}
