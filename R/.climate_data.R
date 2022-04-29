rdwd::updateRdwd()

rdwd::findID("Braunschweig")
rdwd::selectDWD(name = "Braunschweig", res = "daily")

url_bs_rain <- rdwd::selectDWD(name = "Braunschweig",
                               res = "hourly",
                               var = "precipitation",
                               per = "historical" )


bs_rain <- dplyr::rename(rain[,c("MESS_DATUM","R1")],
                     "datetime" = "MESS_DATUM",
                     "precipitation_mm" = "R1") %>%
       dplyr::mutate("date" = as.Date(datetime))

precipitation_daily <- bs_rain %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(rain_mm = sum(precipitation_mm))



head(precipitation_daily)

