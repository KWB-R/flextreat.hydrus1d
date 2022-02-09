### How to build an R package from scratch
# Enable universe(s) by kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('kwb.pkgbuild')


usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Michael Rustler",
               orcid = "0000-0003-0647-7726",
               url = "https://mrustl.de")


pkg <- list(name = "flextreat.hydrus1d",
            title = "R Package for Soil Water Balance and Solute Transport Modelling Scenarios for Project Flextreat",
            desc  = "R Package for Soil Water Balance and Solute Transport Modelling Scenarios for Project Flextreat.")

kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("modelling")

### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c("kwb.python")

  sapply(pkg_dependencies, function(x) usethis::use_package(x, type = "Suggests"))

  desc::desc_add_remotes("kwb-r/kwb.python",normalize = TRUE)

  desc::desc_normalize()

}

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages(pkg$name)
