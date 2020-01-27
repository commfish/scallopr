#' Gonad Status Summary Table
#'
#' Observed gonad status by bed.
#' @param scal_awl Output of `clean_awl()`. See function help file.
#'
#' @param tows Data frame or tibble containing the fields "tow_id", "Bed", and "area_swept" (case sensitive), pertaining to tows made during the scallop survey year in question. See output of clean_tow().
#'
#' @details Observed gonad status by bed. Values are percent of sampled scallops ≥100 mm. N denotes the sample size.
#' @return Tibble and .csv file
#' @export
#' @examples
#' tbl_gonad(scal_awl, tows)
tbl_gonad <- function(scal_awl, tows){

  tbl_names <- c("Bed","N", "Immature", "Empty", "Init. Recovery", "Filling", "Full", "Unknown")

  scal_awl %>%
    left_join(tows) %>%
    filter(!is.na(gonad)) %>%
    count(Bed, gonad) %>%
    group_by(Bed) %>%
    mutate(prop = round(n / sum(n) * 100, 1)) %>%
    spread(gonad, prop) %>%
    replace(is.na(.), 0) %>%
    summarise(N = sum(n),
              zero = round(max(`0`),1),
              one = round(max(`1`),1),
              two = round(max(`2`),1),
              three = round(max(`3`),1),
              four = round(max(`4`),1),
              five = round(max(`5`),1)) %>%
    rename_at(names(.), function(x) tbl_names) -> x

  write_csv(x, paste0("output/", YEAR, "/gonad.csv"))
  x
}
