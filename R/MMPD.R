# computing MMPD for one harmony pair

rank_MMPD <- function(.data,
                      harmony_tbl = NULL,
                      response = NULL,
                      hierarchy_tbl = NULL,
                      create_gran_data = TRUE,...){

MMPD_data <- MMPD(.data,
           harmony_tbl,
           response,
           hierarchy_tbl,
           create_gran_data,...)

MMPD_data_unlist <- unlist(MMPD_data)

harmony_tbl %>%
  mutate(MMPD = MMPD_data_unlist) %>%
  dplyr::mutate(MMPD = round(MMPD, 2)) %>%
  dplyr::arrange(dplyr::desc(MMPD)) %>%
  dplyr::filter(!is.na(MMPD))
}


MMPD <- function(.data,
                 harmony_tbl = NULL,
                 response = NULL,
                 hierarchy_tbl = NULL,
                 create_gran_data = TRUE,...){

harmony_data <- create_harmony_data(.data, harmony_tbl, response, hierarchy_tbl, create_gran_data, ...)

  (1:length(harmony_data)) %>%
  purrr::map(function(rowi) {
    harmony_datai <- harmony_data %>% magrittr::extract2(rowi)
    namesi <- names(harmony_datai)
    harmony_datai_wide <- harmony_datai %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        response = harmony_datai[[response]]
      ) %>%
      dplyr::select(-!!response) %>%
      tidyr::pivot_wider(
        names_from = namesi[2],
        values_from = response,
        values_fn = list(response = list)
      )
    MMPD1(harmony_datai_wide)
})
}

MMPD1 <- function(harmony_datai){
  harmony_datai %>%
    normalise_max_JSdist() %>%
    median_by_log()
}

