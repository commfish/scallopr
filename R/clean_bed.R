#' Clean bed data
#'
#' Summarize surveyed bed data
#' @param tows Data frame or tibble containing the fields "tow_id", "Bed", and "area_swept" (case sensitive), pertaining to tows made during the scallop survey year in question. See output of clean_tow().
#'
#' @details Joins bed area data (see ?area for details) with survey tow data (see output of clean_tow()), and summarizes result.
#'
#' @return Object of class data.frame containing abbreviate bed name, bed area (sq nm), number of station grids, and number of tows within each bed during the survey year. Result is also saved as a .csv file named 'beds.csv' in sub-directory named 'output'.
#'  Ignore warning messages regarding to class of joining variables.
#'
#' @export clean_bed
#'
#' @examples
#' clean_bed(tows)

clean_bed <- function(tows){
    area %>%
    full_join(count(tows, Bed)) %>%
    drop_na %>%
    rename(tows = n) %>%
    mutate(Bed = factor(Bed)) -> x

  write_csv(x, here::here(paste0("output/",YEAR,"/beds.csv")))
  x

}
