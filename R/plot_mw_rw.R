#' plot_mw_rw
#'
#' @param scal_awl
#' @param tows
#' @param Districts
#' @param YEAR
#'
#' @return
#' @export plot_mw_rw
#'
#' @examples
#' plot_mw_rw(scal_awl, tows, Districts, YEAR)
#' @description Plot scallop meat weight/round weight relationships, by bed.
#' Saves output to the appropriate figs folder.
plot_mw_rw <- function(scal_awl, tows, Districts, YEAR){

  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(Districts, by = "Bed") %>%
    ggplot(aes(rwt_lb, mwt_lb, color = District)) +
    geom_point(alpha = 0.12) +
    geom_smooth(se = F) +
    labs(y = "Meat weight (lb)", x = "Round weight (lb)") +
    scale_color_grey() +
    theme(legend.justification=c(1,0), legend.position=c(.2,.65)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/mwt_rwt.png")),
         plot = x, width = 6.5, height = 4, units = "in")

  x

}
