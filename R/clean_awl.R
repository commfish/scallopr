#' Clean survey biological data
#'
#' Filter and trim scallop biological data collected from each tow
#' @param awl_data Data frame of biological data collected at each tow, downloaded from the survey database
#'
#' @param tows Data frame or tibble containing the fields "tow_id", "Bed", and "area_swept" (case sensitive), pertaining to tows made during the scallop survey year in question. See output of clean_tow().
#'
#' @details  Filters biological data (i.e. awl data) for succesful tows, converts weight units to pounds, and trims data to include: tow, shell height,
#' size class, round weight, meat weight, sex, gonad condition, and presence of weak meats, worms, or mud blisters.
#'
#' @return Object of class data.frame containing the above mentioned variables. Result is also saved as a .csv file named 'awl_tbl.csv' in sub-directory named 'output'.
#' @export clean_awl
#'
#' @examples
#'clean_awl(awl_data, tows)

clean_awl <- function(awl_data, tows){

  if(YEAR < 2019) {
    awl_data %>%
      filter(RACE_CODE == 74120,
             EVENT_ID %in% tows$tow_id,
             SCAL_SIZE_CLASS == 1 | SCAL_SIZE_CLASS == 2) %>% # no clappers
      transmute(tow_id = EVENT_ID,
                size = recode(SCAL_SIZE_CLASS, "2" = "small", "1" = "large",
                              .missing = "clapper"),
                sh = SHELL_HEIGHT_MM,
                rwt_lb = WHOLE_WT_GRAMS * 0.00220462,
                mwt_lb = MEAT_WEIGHT_GRAMS * 0.00220462,
                sex = SEX_SW,
                weak = MEAT_CONDITION_SW,
                gonad = SCAL_GONAD_COND,
                worm = SHELL_WORM_SW,
                mud = MUD_BLISTER_SW)  -> x

  } else {
    awl_data %>%
      filter(rcode == 74120,
             tow %in% tows$tow_id,
             samp_grp %in% c(1, 2)) %>%
      transmute(tow_id = tow,
                sh = size,
                size = recode(samp_grp, "2" = "small", "1" = "large"),
                rwt_lb = whole_wt * 0.00220462,
                mwt_lb = meat_weight * 0.00220462,
                sex = sex,
                weak = meat_condition,
                gonad = gonad,
                worm = shell_worm,
                mud = mud_blister)  -> x
  }
  write_csv(x, here::here(paste0("output/",YEAR,"/awl_tbl.csv")))
  x
}
