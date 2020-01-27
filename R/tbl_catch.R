#' Catch Summary Table
#'
#' Catch summary by bed
#' @param scal_catch Output of `clean_catch()`. See function help file.
#'
#' @param beds Output of `clean_bed()`. See function help file.
#'
#' @details Number of stations and tows for surveyed beds in the 2017 statewide scallop dredge survey with total scallop catches, average scallop densities and corresponding CVs by scallop size class.
#' @return Tibble and .csv file
#' @export
#' @examples
#' tbl_catch <- tbl_catch(scal_catch, beds)
tbl_catch <- function(scal_catch, beds){

  scal_catch %>%
    filter(Size != "clapper") %>%
    group_by(Bed, Size) %>%
    summarize(tows = n(),
              catch = sum(count),
              est = mean(count / area_swept), # Q = 1
              CV  = 100 *
                round(sqrt(var(count / area_swept) / (tows-1)) / est, 2)) %>%
    left_join(beds) %>%
    dplyr::select(Bed,
                  Area = area_nm2,
                  Tows = tows,
                  Size,
                  Catch = catch,
                  CatchRate = est,
                  CV) -> x

  write_csv(x, paste0("./output/", YEAR, "/bed_catch_results_tbl.csv"))
  x

}
