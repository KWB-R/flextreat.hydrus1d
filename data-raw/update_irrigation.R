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
