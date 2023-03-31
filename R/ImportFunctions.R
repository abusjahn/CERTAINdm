pacman::p_load(tidyverse, wrappedtools,
               readxl, rlist, Hmisc, dataxray, janitor)
# conflict_scout()
# conflicts_prefer(dplyr::filter)

# datadir <- 'Data/'

# detect type
detect_date <- function(column) {
  column_as_date <- dmy(column)
  if(length(na.omit(column_as_date)) == length(na.omit(column))){
    column <- column_as_date
  }
  return(column)
}
# detect_date(certain_datalist$Patient$TxDate2)

change2date <- function(data){
  data <- modify(data,detect_date)
  return(data)
}

# modify(certain_datalist, change2date)


detect_numeric <- function(column){
  if(is.character(column)){
    column_as_number <- parse_number(column) |> as.numeric()
    if(length(na.omit(as.numeric(column)))==length(na.omit(column_as_number)) &
       length(na.omit(column_as_number))>0) {
      column <- column_as_number
    }
  }
  return(column)
}

change2numeric <- function(data){
  data <- modify(data,detect_numeric)
  return(data)
}

# modify(certain_datalist, change2numeric)

detect_yn <- function(column){
  if(is.character(column)){

    if(all(na.omit(column) %in% c('Yes','No'))) {
      column <- factor(column, levels=c('Yes','No'))
    }
  }
  return(column)
}

change2yn <- function(data){
  data <- modify(data,detect_yn)
  return(data)
}

# change2yn(certain_datalist$Patient)
# show all Excelfiles

show_excelfiles <- function(folder='Data',pattern=''){
  excelfiles <- dir(path = folder,
                    pattern = paste0(pattern,'.*\\.xl.+'),
                    full.names = TRUE)
  return(excelfiles)
}
# show_excelfiles()
# certain_files <- show_excelfiles(pattern = 'Ori')
# certain_files

# show sheets in selected Excelfile

show_excelsheets <- function(file) {
  excelsheets <- readxl::excel_sheets(file)
  return(excelsheets)
}
# certain_sheets <- show_excelsheets(certain_files[1])
# certain_sheets

# import (selected?) sheets from Excelfile

import_excelsheets <- function(file, sheets=NULL, skip_empty_cols=FALSE){
  if(is.null(sheets)) {
    sheets <- show_excelsheets(file)
  }
  if(is.numeric(sheets)) {
    sheets <- show_excelsheets(file)[sheets]
  }

  exceldata_list <- list()
  for (sheet_i in sheets){
    imported <- readxl::read_excel(path = file, sheet = sheet_i) |>
      change2numeric() |>
      change2date() |>
      change2yn()

    if(skip_empty_cols) {
      imported <- remove_empty(imported,which = 'cols')
    }
    exceldata_list <- rlist::list.append(exceldata_list,imported)
    names(exceldata_list)[length(exceldata_list)] <- sheet_i
  }
  return(exceldata_list)
}
# certain_datalist <- import_excelsheets(file = certain_files[1],
#                                        sheets = c(1,2,5))

# create summary for all sheet
sheet_summary <- function(data=certain_datalist){
  description_list <- list()
  for(sheet_i in names(data)){
    sheet_data <- data |> pluck(sheet_i)
    desc_categories <-
      dplyr::summarize(sheet_data,
                       across(where(function(x) {is.character(x) | is.factor(x)}),
                              list(
                                `n valid`=~length(na.omit(.x)) |> as.character(),
                                `n missing`=~sum(is.na(.x)) |> as.character(),
                                Level=~cat_desc_stats(.x, singleline = T) |>
                                  pluck('level'),
                                Frequency=~cat_desc_stats(.x, singleline = T) |>
                                  pluck('freq') |> as.character()),
                              .names = "{.col}::{.fn}")) |>
      pivot_longer(everything(),
                   names_to = c('Variable','Info'),
                   names_sep = '::') |>
      pivot_wider(names_from = Info, values_from = value)

    desc_numbers <-
      dplyr::summarize(sheet_data,
                       across(where(is.numeric),
                              list(
                                `n valid`=~length(na.omit(.x)) |> as.character(),
                                `n missing`=~sum(is.na(.x)) |> as.character(),
                                Median_Quart_Min_Max=~median_quart(.x, range = T,roundDig = 3)),
                              .names = "{.col}::{.fn}")) |>
      pivot_longer(everything(),
                   names_to = c('Variable','Info'),
                   names_sep = '::') |>
      pivot_wider(names_from = Info, values_from = value)

    desc_dates <-
      dplyr::summarize(sheet_data,
                       across(where(is.Date),
                              list(
                                `n valid`=~length(na.omit(.x)) |> as.character(),
                                `n missing`=~sum(is.na(.x)) |> as.character(),
                                Median_Quart_Min_Max=~median_quart(.x, range = T,roundDig = 3)),
                              .names = "{.col}::{.fn}")) |>
      pivot_longer(everything(),
                   names_to = c('Variable','Info'),
                   names_sep = '::') |>
      pivot_wider(names_from = Info, values_from = value)

    description <- list(categories=desc_categories,
                        Numbers=desc_numbers,
                        Dates=desc_dates)
    # description <- Hmisc::describe(sheet_data,
    #                                exclude.missing = FALSE,
    #                                n.unique=10)
    description_list <- rlist::list.append(description_list, description)
    names(description_list)[length(description_list)] <- sheet_i
  }
  return(description_list)
}

# certain_summaries <- sheet_summary()
# certain_summaries$Patient$TxDate1

# clean (selected?) columns

# filter cases / visits by ID

# reshape adam-style very long columns into measures

# reshape long2wide with cols selection

# merge selected sheets
