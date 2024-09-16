file_irrig <- "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Bewässerungsmenge_Braunschweig_2020_2023.xlsx"

irrig <- kwb.db::hsGetTable(file_irrig, tbl = "my_data") %>%
  dplyr::select(! tidyselect::contains("mm"))

names(irrig) <- stringr::str_remove(names(irrig), pattern = "\\s.*")


irrig_update <- irrig %>%
  dplyr::rename("Monat_num" = "Monat") %>%
  dplyr::mutate("Monat" = withr::with_locale(new = c("LC_TIME" = "de_DE"), {
    format(as.Date(sprintf("2000-%02d-01", Monat_num)), "%b")
  })) %>%
  dplyr::relocate(Monat) %>%
  dplyr::relocate(Monat_num, .after = Monat) %>%
  tidyr::gather(key = "Typ", value = "Menge_m3", - Monat, - Monat_num, - Jahr) %>%
  dplyr::mutate(Typ = stringr::str_remove(Typ, "menge")) %>%
  dplyr::arrange(Typ, Jahr, Monat_num)

irrig_old <- readr::read_csv2("inst/extdata/input-data/Beregnungsmengen_AVB.csv")



label_to_remove <- sprintf("%d-%02d_%s", irrig_old$Jahr, irrig_old$Monat_num, irrig_old$Typ) %>%
  unique()

irrig_update_tmp <- irrig_update %>%
  dplyr::mutate(label = sprintf("%d-%02d_%s",Jahr,  Monat_num, Typ)) %>%
  dplyr::filter(!label %in% label_to_remove) %>%
  dplyr::select(-label)


irrig_new <- dplyr::bind_rows(irrig_old, irrig_update_tmp) %>%
  dplyr::arrange(Typ, Jahr, Monat_num)

readr::write_csv2(irrig_new, "inst/extdata/input-data/Beregnungsmengen_AVB.csv")

#'## 2700ha (https://www.abwasserverband-bs.de/de/was-wir-machen/verregnung/)
irrigation_area_sqm <- 27000000

irrigation <- irrig_old %>%
  dplyr::rename(irrigation_m3 = .data$Menge_m3,
                source = .data$Typ,
                month = .data$Monat_num,
                year = .data$Jahr) %>%
  dplyr::mutate(date_start = as.Date(sprintf("%d-%02d-01",
                                             .data$year,
                                             .data$month)),
                days_in_month = as.numeric(lubridate::days_in_month(.data$date_start)),
                date_end =  as.Date(sprintf("%d-%02d-%02d",
                                            .data$year,
                                            .data$month,
                                            .data$days_in_month)),
                source = kwb.utils::multiSubstitute(.data$source,
                                                    replacements = list("Grundwasser" = "groundwater.mmPerDay",
                                                                        "Klarwasser" = "clearwater.mmPerDay")),
                irrigation_cbmPerDay = .data$irrigation_m3/.data$days_in_month,
                irrigation_area_sqm = irrigation_area_sqm,
                irrigation_mmPerDay = 1000*irrigation_cbmPerDay/irrigation_area_sqm) %>%
  dplyr::select(.data$year,
                .data$month,
                .data$days_in_month,
                .data$date_start,
                .data$date_end,
                .data$source,
                .data$irrigation_mmPerDay,
                .data$irrigation_area_sqm) %>%
  tidyr::pivot_wider(names_from = .data$source,
                     values_from = .data$irrigation_mmPerDay)

usethis::use_data(irrigation, overwrite = TRUE)

