#' Clean catch data
#'
#' Filter and summarize catch data specific to scallops
#' @param catch_data Survey catch data, downloaded from survey database
#'
#' @param tows Data frame or tibble containing the fields "tow_id", "Bed", and "area_swept" (case sensitive), pertaining to tows made during the scallop survey year in question. See output of clean_tow().
#'
#' @param year Four digit year. Default is object YEAR, specified in analysis script.
#'
#' @details Filters and summarizes survey catch data to include the number and weight (lbs) of scallops in both size classes (i.e., large and small) for each tow.
#'
#' @return Tibble including tow ID, count of scallops caught, weight in pounds, size class, abbreviated bed name, and area swept (sq nm) during tow. Ignore warning messages regarding to class of joining variables, and mutation of grouping variables.
#' @export clean_catch
#'
#' @examples
#' scal_catch <- clean_catch(catch_data, tows, YEAR)

clean_catch <- function(catch_data, tows, year=YEAR){

  if(!exists("year")){stop("Must specify year")}

  if(YEAR < 2019){
    catch_data %>%
      dplyr::filter(YEAR == YEAR,
                    RACE_CODE == 74120,
                    CONDITION_CODE_RII %in% c("01", "1", "52"),
                    EVENT_ID %in% tows$tow_id) %>%  # filter out uneccessary data
      dplyr::transmute(tow_id = EVENT_ID,
                       size = dplyr::case_when(SCAL_SIZE_CLASS==2 ~ "small",
                                               SCAL_SIZE_CLASS==1 ~ "large",
                                               SCAL_SIZE_CLASS==3 ~ "clapper"),
                       count = COUNT,
                       wt_lb = SAMPLE_WT_KG * 2.20462) %>%
      dplyr::group_by(tow_id, size) %>%
      dplyr::summarize(count = sum(count), wt_lb = sum(wt_lb)) %>%
      dplyr::right_join(expand.grid(size = c("clapper", "small", "large", "all"),
                                    tow_id = tows$tow_id)) %>%
      dplyr::mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      within(count[size == "all"] <- count[size == "small"] +
               count[size == "large"]) %>% # add "all" to counts
      within(wt_lb[size == "all"] <- wt_lb[size == "small"] +
               wt_lb[size == "large"]) %>% # add "all" to wts
      dplyr::mutate(Size = factor(size, levels = Size_levels)) %>%
      dplyr::select(-size) %>%
      dplyr::left_join(tows, by = "tow_id") %>%
      dplyr::ungroup() -> x

  } else {
    catch_data %>%
      dplyr::filter(rcode == 74120,
                    samp_grp %in% c(1, 2),
                    tow %in% tows$tow_id) %>%  # filter out uneccessary data
      dplyr::transmute(tow_id = tow,
                       size = dplyr::case_when(samp_grp == 1 ~ "large",
                                               samp_grp == 2 ~ "small"),
                       count = samp_cnt,
                       wt_lb = samp_wt * 2.20462) %>%
      dplyr::group_by(tow_id, size) %>%
      dplyr::summarize(count = sum(count), wt_lb = sum(wt_lb)) %>%
      dplyr::right_join(expand.grid(size = c("small", "large"),
                                    tow_id = tows$tow_id)) %>% # fill in data for tows that may not have caught a size catagory
      dplyr::mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # within(count[size == "all"] <- count[size == "small"] + count[size == "large"]) %>% # add "all" to counts
      # within(wt_lb[size == "all"] <- wt_lb[size == "small"] + wt_lb[size == "large"]) %>% # add "all" to wts
      dplyr::mutate(Size = factor(size, levels = Size_levels)) %>%
      dplyr::select(-size) %>%
      dplyr::left_join(tows, by = "tow_id") %>%
      dplyr::ungroup() -> x

  }

  write_csv(x, here::here(paste0("output/",YEAR,"/scal_catch.csv")))
  x

}
