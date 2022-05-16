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



exe_dir <- "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"
exe_name <- "H1D_CALC.EXE"
exe_path <- file.path(exe_dir, exe_name)

target_dir <- "inst/extdata/model"
target_path <- file.path(target_dir, exe_name)

fs::file_copy(path = exe_path,
              new_path = fs::path_abs(target_path)
              )

model_dir <- fs::path_abs(sprintf("%s/%s\n",
                                  target_dir,
                                  "test"))

writeLines(model_dir,
           fs::path_abs(sprintf("%s/LEVEL_01.DIR",
                   target_dir)))
shell(cmd = sprintf("cd %s && %s",
                    fs::path_abs(target_dir),
                    exe_name),
      intern = FALSE)

a_level <- kwb.hydrus1d::read_alevel(file.path(target_dir, "test/A_LEVEL.out"))
