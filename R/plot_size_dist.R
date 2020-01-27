#' Plot scallop size distribution
#'
#' Shell height composition, by bed.
#' @param scal_awl Output of `clean_awl()`. See function help file.
#' @param scal_catch Output of `clean_catch()`. See function help file.
#' @param tows Output of `clean_tow()`. See function help file.
#' @param YEAR Survey year. Default is defined in global options.
#' @param binwidth Histogram size bin width. Default = 2 mm.
#'
#' @return
#' @details Plot scallop size distributions weighted by sample sizes, by bed. Saves output to the appropriate 'figs' folder.
#' @export plot_size_dist
#'
#' @examples
#' plot_size_dist(scal_awl, scal_catch, tows, YEAR)

plot_size_dist <- function(scal_awl, scal_catch, tows, YEAR, binwidth = 2){

  scal_catch %>%
    ungroup %>%
    filter(Size != "clapper") %>%
    select(tow_id, N = count, size = Size) -> tow_count

  scal_awl %>%
    dplyr::select(tow_id, size, sh) %>%
    filter(!is.na(sh), sh > 0, !is.na(size)) %>%
    add_count(tow_id, size) %>% # add subsample sizes by size class within each tow
    left_join(tow_count, by = c("tow_id", "size")) %>%
    left_join(tows, by = "tow_id") %>%
    filter(n <= N, N * n != 0) %>%
    mutate(wt = (N / n) / area_swept,
           Bed = factor(Bed, levels = Bed_levels)) %>%
    add_count(Bed) %>%
    mutate(label = paste0(Bed, ": N = ", n)) %>%
    ggplot(aes(sh, weight=wt)) +
    geom_histogram(color = "black", fill = "darkgray", binwidth = binwidth) +
    facet_wrap(~Bed, ncol=2, dir = 'v', scales = 'free_y') +
    xlab("\nShell height (mm)") +
    scale_y_continuous("Weighted shell height counts\n", label = scales::comma) +
    theme(strip.background = element_blank()) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/size_dist.png")), plot = x, width = 6.5, height = 4)

  x
}
