#' plot_mw_sh
#'
#' @param scal_awl
#' @param tows
#' @param Districts
#' @param YEAR
#'
#' @return
#' @export plot_mw_sh
#'
#' @examples
#' plot_mw_sh(scal_awl, tows, Districts, YEAR)
#' @description Plot scallop meat weight/shell height relationships, by bed.
#' Saves output to the appropriate figs folder.
plot_mw_sh <- function(scal_awl, tows, Districts, YEAR){
  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(Districts, by = "Bed") %>%
    ggplot(aes(sh, mwt_lb,  color = District)) +
    geom_point(alpha = 0.1) +
    geom_smooth(se = F) +
    labs(y = "Meat weight (lb)", x = "Shell height (mm)") +
    scale_color_grey() +
    theme(legend.justification=c(1,0), legend.position=c(.2,.65)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/mwt_sh.png")),
         plot = x, width = 6.5, height = 4, units = "in")

  x

}
