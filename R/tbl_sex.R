#' Sex Summary Table
#'
#' Proportion of large scallops by sex.
#' @param scal_awl Output of `clean_awl()`. See function help file.
#'
#' @param tows Data frame or tibble containing the fields "tow_id", "Bed", and "area_swept" (case sensitive), pertaining to tows made during the scallop survey year in question. See output of clean_tow().
#'
#' @details Observed sex ratios (percent of scallops ≥100 mm). N denotes the sample size.
#' @return Tibble and .csv file
#' @export
#' @examples
#' tbl_sex(scal_awl, tows)
tbl_sex <- function(scal_awl, tows){

  scal_awl %>%
    left_join(tows, by = "tow_id") %>%
    filter(!is.na(sex), size == "large") %>%
    group_by(Bed, sex) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n) * 100, digits = 1)) %>%
    group_by(Bed) %>%
    mutate(N = sum(n)) %>%
    dplyr::select(-N, everything(), -n) %>%
    spread(., sex, freq) %>%
    replace(is.na(.), 0) %>%
    rename(unkown = '0', male = '1', female = '2') %T>%
    write_csv(here::here(paste0("output/", YEAR, "/sex_tbl.csv")))

}
