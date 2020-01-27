#' Mud Blister Summary Table
#'
#' Create a table on prevalence of mud blisters
#' @param scal_awl Output of `clean_awl()`. See function help file.
#'
#' @param tows Data frame or tibble containing the fields "tow_id", "Bed", and "area_swept" (case sensitive), pertaining to tows made during the scallop survey year in question. See output of clean_tow().
#'
#' @details Writes a summary table on the proportion of sampled shells (N) with 0%, 1-24%, 25-49%, 50-74%, and 75-100% mud blister coverage.
#' @return Tibble and .csv file
#' @export
#' @examples
#' tbl_blister(scal_awl, tows)

tbl_blister <- function(scal_awl, tows){

  tbl_names <- c("Bed", "N", "0%", "1-24%", "25-49%", "50-74%", "75-100%")

  scal_awl %>%
    left_join(tows) %>%
    filter(!is.na(mud)) %>%
    count(Bed, mud) %>%
    group_by(Bed) %>%
    mutate(prop = round(n / sum(n) * 100, 1)) %>%
    spread(mud, prop) %>%
    replace(is.na(.), 0) %>%
    mutate(`4` = 0) %>%
    summarise(N = sum(n),
              zero = round(max(`0`),1),
              one = round(max(`1`),1),
              two = round(max(`2`),1),
              three = round(max(`3`),1),
              four = round(max(`4`),1)) %>%
    rename_at(names(.), function(x) tbl_names) -> x

  write_csv(x, here::here(paste0("output/", YEAR, "/mud.csv")))
  x
}
