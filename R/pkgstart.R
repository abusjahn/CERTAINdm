.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "Package CERTAINdm is still experimental,",
    "please report problems and suggestions to the maintainer"
  ))
}

#' @importFrom lubridate dmy
NULL

#' @importFrom purrr modify pluck
NULL

#' @importFrom readr parse_number
NULL

#' @import dplyr
NULL

# #' @import tidyr
# NULL

#' @import knitr
NULL

#' @importFrom rlist list.append
NULL

#' @importFrom readxl excel_sheets read_excel
NULL

#' @importFrom janitor remove_empty
NULL

#' @importFrom tidyselect any_of all_of
NULL

#' @importFrom wrappedtools FindVars cat_desc_table cn 
NULL

#' @importFrom stringr str_replace