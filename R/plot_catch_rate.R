#' plot_catch_rate
#'
#' @param scal_catch
#' @param tows
#' @param YEAR
#' @param Bed_levels
#'
#' @return
#' @export plot_catch_rate
#'
#' @examples
#' plot_catch_rate(scal_catch, tows, YEAR, Bed_levels)
#' @description Plot survey catch rates for large and small scallops by bed.
#' Saves output to the appropriate figs folder.
plot_catch_rate <- function(scal_catch, tows, YEAR, Bed_levels){

  output_dir <- file.path("figs", YEAR)

  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }

  scal_catch %>%
    filter(Size == "small" | Size == "large") %>%
    select(tow_id, Size, N = count) %>%
    left_join(tows, by = "tow_id") %>%
    mutate(Bed=factor(Bed, levels = Bed_levels)) %>%
    ggplot(aes(Bed, N, fill = Size)) +
    geom_boxplot() +
    ylab("Scallops / Tow") + xlab("Bed") +
    stat_summary(fun.data = function(x) c(y = -25, label = length(x)),
                 geom ="text") +
    scale_fill_manual(values = c("white", 'lightgray'), name = "Size class") +
    theme(legend.justification=c(1,0), legend.position=c(.95,.7)) +
    scale_y_continuous(label=scales::comma) +
    coord_cartesian(ylim = c(-25,600)) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/catch_rates.png")), plot = x,
         width = 6.5, height = 4)

  x
}
