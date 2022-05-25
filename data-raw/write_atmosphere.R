library(flextreat.hydrus1d)

atm <- prepare_atmosphere_data()
atm_selected <- select_hydrologic_years(atm)
atm_prep <- prepare_atmosphere(atm_selected)


atmos <- kwb.hydrus1d::write_atmosphere(atm = atm_prep,
                                        MaxAL = nrow(atm_prep)
                                        )


writeLines(text = atmos,
           "inst/extdata/model/test/ATMOSPH.IN")



writeLines(text = atmos,
           system.file("extdata/model/test/ATMOSPH.IN", package = "flextreat.hydrus1d"))

