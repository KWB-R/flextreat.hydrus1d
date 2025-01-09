#' Helper function: combine dataframes
#'
#' @param df_list df_list
#'
#' @return df
#' @export
#'
combine_dataframes <- function(df_list) {
  # Ueberprüfen, ob die Liste nicht leer ist
  if (length(df_list) == 0) {
    stop("Die Liste der DataFrames ist leer")
  }

  # Ueberprüfen, ob alle Elemente in der Liste DataFrames sind
  if (!all(sapply(df_list, is.data.frame))) {
    stop("Alle Elemente in der Liste muessen DataFrames sein")
  }

  # Kombiniere die DataFrames mit left_join
  combined_df <- Reduce(function(x, y) dplyr::left_join(x,
                                                        y,
                                                        by = intersect(names(x),
                                                                       names(y))),
                        df_list)

  return(combined_df)
}


#' Recalculate c_top with virtual storage
#'
#' @param atm atmosphere time series data
#' @param tlevel tlevel of model output (use \code{kwb.hydrus1d::read_tlevel})
#' @param crit_v_top critical v_top rate (default: - 0.05)
#'
#' @return tibble with modified c_top values in order to respect that evaporation
#' is often larger compared to precipitation
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr across arrange first if_else lag group_by mutate summarize
#' rename_with left_join cur_column sym
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_replace
recalculate_ctop_with_virtualstorage <- function(atm,
                                                 tlevel,
                                                 crit_v_top = - 0.05) {

  tlevel_aggr_date <- aggregate_tlevel(tlevel)

  c_tops <- names(atm)[stringr::str_detect(names(atm), "cTop")]

vs_list <- lapply(seq_len(length(c_tops)), function(i) {

  # Datum in Date-Format umwandeln
  data <- atm[,c("tAtm", "Prec", c_tops[i])] %>%
    dplyr::bind_cols(tibble::tibble(
      evapo_r_modelled = tlevel_aggr_date$evap[seq_len(nrow(atm))],
      v_top = tlevel_aggr_date$v_top[seq_len(nrow(atm))])) %>%
    dplyr::mutate(
      store = dplyr::if_else(#Prec - evapo_r_modelled < 0,
        v_top >= crit_v_top,
        1,
        0)
      ) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("cT"), ~ . * Prec, .names = "Prec_{.col}"))


  # Funktion zum Identifizieren zusammenhaengender Gruppen
  data_grouped <- data %>%
    dplyr::arrange(tAtm) %>%
    dplyr::mutate(group = cumsum(store != dplyr::lag(store, default = dplyr::first(store))))

  # Summiere "load_in" und bestimme Anfangs- und Enddatum
  result <- data_grouped %>%
    #dplyr::filter(store == 1) %>%
    dplyr::group_by(group, store) %>%
    dplyr::summarize(
      tAtm_start = min(tAtm),
      tAtm_end = max(tAtm),
      tAtm_diff = as.integer(diff.Date(c(tAtm_start, tAtm_end))),
      Prec_first = dplyr::first(Prec),
      Prec_sum = sum(Prec),
      # Summe der Werte in allen Spalten, die mit "Prec_c" beginnen
      dplyr::across(
        starts_with("Prec_c"),
        .names = "{.col}_{.fn}",
        .fns = list(first = ~first(.), sum = ~sum(.))
      ),
      .groups = "drop"
    )

  result2 <- result %>%
    dplyr::left_join(result %>%
                       dplyr::filter(store == 1) %>%
                       dplyr::mutate(group = group + 1) %>%
                       dplyr::select(group, tidyselect::matches("Prec_cTop.*_sum")) %>%
                       dplyr::rename_with(~ sub("^Prec_c", "store_Prec_c", .),
                                          tidyselect::starts_with("Prec_c")),
                     by = "group"
    ) %>%
    mutate(across(matches("^Prec_cTop.*_first"),
                  ~ (. + get(paste0("store_",
                                    stringr::str_replace(pattern = "_first",
                                                         replacement = "_sum",
                                                         dplyr::cur_column())))) / Prec_first,
                  .names = "cor_{col}"),
           across(matches("^Prec_cTop.*_sum"),
                  ~ (. + get(paste0("store_", dplyr::cur_column()))) / Prec_sum,
                  .names = "cor_{col}")
    )

  data[data$store == 1, c_tops[i]] <- 0

  cor_Prec_sel <- sprintf("cor_Prec_%s_first", c_tops[i])

  result3 <- data %>%
    dplyr::left_join(result2 %>%
                       dplyr::select(tidyselect::all_of(c("group",
                                                        "tAtm_start",
                                                        cor_Prec_sel))),
                     by = c("tAtm" = "tAtm_start")) %>%
    dplyr::mutate(!!dplyr::sym(c_tops[i]) := dplyr::if_else(store == 0 & !is.na(.data[[cor_Prec_sel]]),
                                               .data[[cor_Prec_sel]],
                                               .data[[c_tops[i]]]))


  result3
  })


vs_list_df <- lapply(seq_len(length(c_tops)), function(i) {

vs_list[[i]][,c("tAtm", c_tops[i])]

})


vs_df <- combine_dataframes(df_list = vs_list_df)


list(list = vs_list,
     df = vs_df)

}

# result4 <- dplyr::left_join(atm[,"tAtm"],
#                             result2 %>%
#                               dplyr::mutate(cor_Prec_cTop_perStep = cor_Prec_cTop_sum/(tAtm_diff+1)) %>%
#                               dplyr::select("tAtm_start", "store", "cor_Prec_cTop_perStep"),
#                             by = c("tAtm" = "tAtm_start")
# ) %>%
#   tidyr::fill(cor_Prec_cTop_perStep, .direction = "down") %>%
#   tidyr::fill(group, .direction = "down") %>%
#   tidyr::fill(store, .direction = "down")
#
#
#
# result_store <- result %>%
#   dplyr::filter(store == 1) %>%
#   dplyr::select(group, Prec_sum) %>%
#   dplyr::mutate(group = group + 1) %>%
#   dplyr::rename(Prec_sum_stored = Prec_sum)
#
# result_inf <- result %>%
#   dplyr::filter(store == 0) %>%
#   dplyr::select(group, Prec_sum) %>%
#   dplyr::rename(Prec_sum_inf = Prec_sum)
#
#
# result_merged <- result_inf %>%
#   dplyr::left_join(result_store, by = "group") %>%
#   dplyr::mutate(Prec_sum_stored = dplyr::if_else(is.na(Prec_sum_stored),
#                                                  0,
#                                                  Prec_sum_stored),
#                 Prec_cor = dplyr::if_else(Prec_sum_inf > 0,
#                                           (Prec_sum_inf  + Prec_sum_stored)/Prec_sum_inf,
#                                           1))
#
# View(result_merged)
#
#
# result %>%
#   dplyr::mutate(tAtm = purrr::map2(tAtm_start, tAtm_end, seq)) %>%
#   tidyr::unnest(tAtm) %>%
#   dplyr::select(group, store, tAtm, Prec_cor)


