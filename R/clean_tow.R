#' Clean tow data
#'
#' Filter and extract information from haul data
#' @param events_data  A data.frame of haul events from the weathervane scallop survey. Data format differs by survey year.
#'
#' @param year Four digit year. Default is object YEAR, specified in analysis script.
#'
#' @details Filters tow (i.e., haul) data to only include tows with gear performance 1 (satisfactory) and haul type 10 (15 min, 1 nm), extracts tow ID, bed name, and area swept (distance converted to nm), and
#' checks for duplicate tows per station.
#'
#' @return Object of class data.frame containing tow ID, abbreviate bed name, and area swept (sq nm) during each tow. Result is also saved as a .csv file named 'beds.csv' in sub-directory named 'output'.
#' @export
#' @examples
#' clean_tow(events_data)
#' clean_tow(events_data, 2019)

clean_tow <- function(events_data, year=YEAR){

  if(!exists("year")){stop("Must specify year")}

  output_dir <- file.path("output", YEAR)

  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Good to go!")
  }

  if(year < 2019){
    events_data %>%
      dplyr::filter(YEAR==YEAR,
        GEAR_PERFORMANCE_CODE_SW == 1,
        STATION_TYPE %in%
          c("Standard", "Repeat", "Standard Non-Selected")) %>%
      # multiple station types
      dplyr::transmute(tow_id = EVENT_ID,
                       Bed = factor(BED_SW),
                       area_swept = TOW_LENGTH_DESIGNATED * 0.00131663,
                       # in nautical miles
                       #area_swept = TOW_LENGTH_FIELD * 0.00131663,
                       # in nautical miles - switched for 2018 to make run before getting complete dataset from Mumm
                       station = STATION_ID) %>%
      dplyr::mutate(bedsta = paste0(tow_id, station)) -> x
  } else {
    events_data %>%
      dplyr::filter(gear_perf == 1,
                    haul_type == 10) %>%
      dplyr::transmute(tow_id = tow,
                       Bed = factor(bed_code),
                       area_swept = distance * 0.00131663, # in nautical miles
                       station = station) %>%
      dplyr::mutate(bedsta = paste0(tow_id, station)) -> x
  }

  # make sure that there are no duplicate data

  if(dim(x)[1] != length(unique(x$bedsta))) {
    stop("Repeated station tows")
  } else {
    select(x, -station, - bedsta) -> x
  }

  # save the output
  write_csv(x, paste0("./output/", YEAR, "/tows.csv"))
  x
}
