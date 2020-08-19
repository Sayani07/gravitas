# compute MMPD for one harmony pair
#' Normalise max pairwise distance for one variable
#' @param .data a tsibble, if cyclic granularity needs to be constructed or a list consisting of tibbles for each pair of cyclic granularity in the harmony table
#' @param harmony_tbl A tibble of harmonies and their levels obtained from the function().
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#' @param create_gran_data if data corresponding to a pair of cyclic granularity needs to be created
#' @param response response variable.
#' @param quantile_prob numeric vector of probabilities with values in [0,1].
#' @param dist_distribution Underlying distribution of distances.
#' @param dist_ordered if levels of the time granularity is ordered.
#' @param ... Other arguments passed on to individual methods.
#' @return  MMPD for one harmony pair
#' @export rank_MMPD

rank_MMPD <- function(.data,
                      harmony_tbl = NULL,
                      response = NULL,
                      hierarchy_tbl = NULL,
                      create_gran_data = TRUE,
                      quantile_prob = seq(0.01, 0.99, by = 0.01),
                      dist_distribution = 'normal',
                      dist_ordered = TRUE,...){

  MMPD_data <- MMPD(.data,
                    harmony_tbl,
                    response,
                    hierarchy_tbl,
                    create_gran_data,...)

  MMPD_data_unlist <- unlist(MMPD_data)

  harmony_tbl %>%
    dplyr::mutate(MMPD = MMPD_data_unlist) %>%
    dplyr::mutate(MMPD = round(MMPD, 2)) %>%
    dplyr::arrange(dplyr::desc(MMPD)) %>%
    dplyr::filter(!is.na(MMPD))
}


MMPD <- function(.data,
                 harmony_tbl = NULL,
                 response = NULL,
                 hierarchy_tbl = NULL,
                 create_gran_data = NULL,...){

  harmony_data <- create_harmony_data(.data,
                                      harmony_tbl,
                                      response,
                                      hierarchy_tbl,
                                      create_gran_data = create_gran_data, ...)

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

