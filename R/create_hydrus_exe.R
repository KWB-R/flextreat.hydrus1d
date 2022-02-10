#' Create Hydrus1D executable (only on Windows!)
#'
#' @return path to Hydrus1D v4.0.8 executable
#' @export
#' @source \url{https://github.com/phydrus/source_code/tree/main}
#' @importFrom archive archive_extract
#' @importFrom fs file_copy
#' @importFrom kwb.utils windowsPath
#' @importFrom stringr str_replace
#' @importFrom pkgbuild rtools_path
create_hydrus_exe <- function() {

  is_windows = Sys.info()[1] == "Windows"

  if(is_windows) {
    ### Compile Hydrus1D v4.08 executable
    ### see: https://github.com/phydrus/source_code/issues/1#issuecomment-1034675697

    tdir <- file.path(getwd(), "hydrus1d")

    #Download source code for latest open-source Hydrus1D release
    archive::archive_extract(archive = "https://github.com/phydrus/source_code/archive/refs/heads/main.zip",
                             dir = tdir,
                             strip_components = 1
    )

    fs::file_copy(path = file.path(tdir, "make.bat"),
                  new_path = file.path(tdir, "source/make.bat"),
                  overwrite = TRUE
    )

    binpath <- kwb.utils::windowsPath(stringr::str_replace(pkgbuild::rtools_path(),
                                                           "usr",
                                                           "mingw64"))

    if(dir.exists(binpath)) {
      cmd <- paste0("set PATH=%PATH%;", binpath, "\n",
                    "cd ", kwb.utils::windowsPath(file.path(tdir, "source\n")),
                    "make.bat")
      batch_name <- "compile_hydrus1d.bat"
      writeLines(cmd, batch_name)
      shell(cmd = batch_name)
      return(file.path(tdir, "source/hydrus.exe"))
    } else {
      stop("Rtools not installed")
    }
  } else {
    stop("Not implemented yet for Linux/MacOS")
  }

}
