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
create_hydrus_exe <- function()
{
  is_windows <- Sys.info()[1] == "Windows"

  if (! is_windows) {

    system2(paste0("cd ", file.path(tdir, "source\n"), "make -f makefile"))

    return(file.path(tdir, "source/hydrus"))
  }

  ### Compile Hydrus1D v4.08 executable
  ### see: https://github.com/phydrus/source_code/issues/1#issuecomment-1034675697

  tdir <- file.path(getwd(), "hydrus1d")

  #Download source code for latest open-source Hydrus1D release
  url <- "https://github.com/phydrus/source_code/archive/refs/heads/main.zip"

  archive::archive_extract(archive = url, dir = tdir, strip_components = 1L)

  copy_or_overwrite <- function(from, to) fs::file_copy(
    path = file.path(tdir, from),
    new_path = file.path(tdir, to),
    overwrite = TRUE
  )

  copy_or_overwrite("make.bat", "source/make.bat")
  copy_or_overwrite("Makefile", "source/Makefile")

  binpath <- stringr::str_replace(pkgbuild::rtools_path(), "usr", "mingw64")

  if (! dir.exists(binpath)) {
    stop("Rtools not installed")
  }

  cmd <- c(
    paste0("set PATH=%PATH%;", kwb.utils::windowsPath(binpath)),
    paste("cd", kwb.utils::windowsPath(file.path(tdir, "source"))),
    "make.bat"
  )

  batch_name <- "compile_hydrus1d.bat"

  writeLines(cmd, batch_name)

  shell(batch_name)

  file.path(tdir, "source/hydrus.exe")
}
