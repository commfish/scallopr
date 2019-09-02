#' clean_bed
#'
#' @param tows
#'
#' @return a .csv table of number of survey areas and number of tows by bed
#' @export clean_bed
#'
#' @examples
clean_bed <- function(tows){

  area %>%
    full_join(count(tows, Bed)) %>%
    drop_na %>%
    rename(tows = n) %>%
    mutate(Bed = factor(Bed)) -> x

  write_csv(x, here::here(paste0("output/",YEAR,"/beds.csv")))
  x

}
