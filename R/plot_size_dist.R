#' plot_size_dist
#'
#' @param scal_awl
#' @param scal_catch
#' @param tows
#' @param YEAR
#'
#' @return
#' @export plot_size_dist
#'
#' @examples
#' plot_size_dist(scal_awl, scal_catch, tows, YEAR)
#' @description Plot scallop size distributions weighted by sample sizes, by bed.
#' Saves output to the appropriate figs folder.
#'
plot_size_dist <- function(scal_awl, scal_catch, tows, YEAR){

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
    geom_histogram(color = "black", fill = "darkgray", bins = 75) +
    facet_wrap(~Bed, ncol=2, dir = 'v', scales = 'free_y') +
    xlab("\nShell height (mm)") +
    scale_y_continuous("Weighted shell height counts\n", label = scales::comma) +
    theme(strip.background = element_blank()) -> x

  ggsave(here::here(paste0("figs/", YEAR, "/size_dist.png")), plot = x, width = 6.5, height = 8)

  x
}
