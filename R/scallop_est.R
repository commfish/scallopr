#' scalllop_est
#'
#' @param scal_catch
#' @param beds
#' @param Q
#' @param abundance
#' @param boot
#'
#' @description An estimate of scallop abundance (numbers or weight) with bootstrap or lognormal CIs can be estimated.
#' @export scallop_est
#'
#' @examples
#' abund_est <- scallop_est(scal_catch, beds, 0.83, abundance = TRUE, boot = TRUE)
#' biom_est <- scallop_est(scal_catch, beds, 0.83, abundance = False, boot = TRUE)
scallop_est <- function(scal_catch, beds, Q, abundance = TRUE, boot = TRUE){

  if(abundance){
    scal_catch %>%
      filter(Size != "clapper") %>%
      mutate(value = count) %>%
      left_join(beds) %>%
      group_by(Bed) %>%
      mutate(tows = n()) %>%
      ungroup() -> data

    filename = here::here(paste0("output/", YEAR, "/scal_abund_estimates.csv"))

  } else {

    scal_catch %>%
      filter(Size != "clapper") %>%
      mutate(value = wt_lb) %>%
      left_join(beds) %>%
      group_by(Bed) %>%
      mutate(tows = n()) %>%
      ungroup() -> data

    filename = here::here(paste0("output/", YEAR, "/scal_biom_estimates.csv"))
  }

  # calculate base estimate with lognormal CIs

  data %>%
    group_by(Bed, Size) %>%
    mutate(est = mean(value / (Q * area_swept)),
           cv = sqrt(var(value / (Q * area_swept)) / (tows - 1)) / est) %>%
    mutate(est = est * area_nm2) %>%
    summarise(est = mean(est), cv = mean(cv)) %>%
    mutate(l95 = est * exp(-1.96 * sqrt(log(1 + cv^2))),
           u95 = est * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
    dplyr::select(-cv) %>%
    mutate_if(is.numeric, round, 0) -> est

  # bootstrap ci function

  boot_ci <- function(split){

    rsample::analysis(split) %>%
      mutate(est = mean(value / (Q * area_swept)) * area_nm2) %>%
      summarise(est = mean(est))
  }


  if(boot){

    data %>%
      nest(-Bed, -Size) %>%
      mutate(est = map(data, ~rsample::bootstraps(., 1000))) %>%
      unnest(est) %>%
      mutate(models = map(splits, ~boot_ci(.x))) %>%
      unnest(models) %>%
      group_by(Bed, Size) %>%
      summarise(l95 = quantile(est, 0.25),
                u95 = quantile(est, 0.975)) -> ci

    est %>%
      dplyr::select(-l95, -u95) %>%
      left_join(ci) %>%
      mutate_if(is.numeric, round, 0) -> est

  }

  write_csv(est, filename)
  est
}
