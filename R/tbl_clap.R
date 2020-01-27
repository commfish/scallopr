#' Clapper Summary Table
#'
#' Create a table on prevalence of mud blisters
#' @param scal_catch Output of `clean_catch()`. See function help file.
#'
#' @param scal_awl Output of `clean_awl()`. See function help file.
#'
#' @details Bed percentages of clappers and weak meats from survey data. Meat condition was assessed only for subsampled large scallops. N denotes the sample size.
#' @return Tibble and .csv file
#' @export
#' @examples
#' tbl_clap(scal_catch, scal_awl)

tbl_clap <- function(scal_catch, scal_awl){

  scal_awl %>%
    left_join(tows) %>%
    filter(size!="all", size!='small') %>%
    group_by(Bed) %>%
    summarise(N_weak = n(),
              weak = round(sum(weak, na.rm = T) / n() * 100, digits = 1)) -> x


  scal_catch %>%
    group_by(Bed) %>%
    filter(Size!='all') %>%
    summarise(clap = sum(count[Size =="clapper"]),
              N = sum(count),
              prop = round(clap / N * 100, digits = 1)) %>%
    left_join(x) -> x

  names(x) <- c("Bed", "Clappers", "N", "% clappers", "N (large)", "Weak meats")
  write_csv(paste0("./output/", YEAR, "/clapper_weak_tbl.csv"))
  x
}
