
# detect type
#' Detection and casting for columns containing DD.MM.YYYY
#' 
#' \code{detect_date} is an internal function, taking a vector as input
#' and returning either original or casted vector
#' 
#' @param column A vector (typically a column from an imported sheet)
#' 
#' @return A vector of type date if suitable, original structure otherwise 
#' 
#' @export
detect_date <- function(column) {
  column_as_date <- suppressWarnings(dmy(column))
  if((length(na.omit(suppressWarnings(column_as_date))) == 
      length(na.omit(column)) &
      length(na.omit(column))>0)){
    column <- column_as_date
  }
  return(column)
}
# detect_date(certain_datalist$Patient$TxDate2)

#' Detection and casting for date columns
#' 
#' \code{change2date} internal function 
#' 
#' @param data The tibble to convert
#' 
#' @return  A mutated tibble
#' 
#' @export
change2date <- function(data){
  data <- modify(data,detect_date)
  return(data)
}

# modify(certain_datalist, change2date)

#' Detection of numeric variables
#' Tests if all entries can be casted to numeric
#' 
#' \code{detect_numeric} internal function
#' 
#' @param column A vector (typically a column from an imported sheet)
#' 
#' @return  A vector of type numeric if suitable, original structure otherwise
#' 
#' @export

detect_numeric <- function(column){
  if(is.character(column)){
    column_as_number <- suppressWarnings(
      column |> 
        str_replace(',','.') |> parse_number() |> as.numeric())
    if(suppressWarnings(column |> 
       str_replace(',','.') |> as.numeric() |> 
       na.omit() |> length())==length(na.omit(column_as_number)) &
       length(na.omit(column_as_number))>0) {
      column <- column_as_number
    }
  }
  return(column)
}

#' Detection and casting for numeric columns
#' 
#' \code{change2numeric} internal function 
#' 
#' @param data The tibble to convert
#' 
#' @return  A mutated tibble
#' 
#' @export

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

#' Detection and casting for yes/no columns
#' 
#' \code{change2yn} internal function 
#' 
#' @param data The tibble to convert
#' 
#' @return  A mutated tibble
#' 
#' @export

change2yn <- function(data){
  data <- modify(data,detect_yn)
  return(data)
}

# change2yn(certain_datalist$Patient)
# show all Excelfiles

# empty cols result in type logical, change to char

empty2char <- function(data) {
  data <- mutate(data,
                 across(where(is.logical),
                        as.character))
}


detect_code <- function(data) {
  label_code_cols <- NULL
  codecols <- FindVars('Code$',colnames(data))
  if(codecols$count>0){
    labelcols <- FindVars(str_replace(codecols$names,'(.+)Code','^\\1$'),
                          colnames(data))
    iscode <- (codecols$index %in% (labelcols$index+1))
    label_code_cols <- tibble(
      Label=labelcols$names,
      Code=codecols$names[iscode])
  }
  return(label_code_cols)
}

#' Show Excelfiles in selected folder
#' 
#' \code{show_excelfiles} internal function 
#' 
#' @param folder The folder to check
#' 
#' @param pattern optional search pattern to narrow down search
#' 
#' @return  A vector with full paths
#' 
#' @export
#' 
show_excelfiles <- function(folder='Data',pattern=''){
  excelfiles <- dir(path = folder,
                    pattern = paste0(pattern,'.*\\.xl.+'),
                    full.names = TRUE)
  return(excelfiles)
}
# show_excelfiles()
# certain_files <- show_excelfiles(pattern = 'Ori')
# certain_files

## Show sheets in selected Excelfile
#' 
#' \code{show_excelsheets} lists all sheets found in the selected Excelfile 
#' 
#' @param file Path to Excelfile
#' 
#' @return  A vector with sheetnames
#' 
#' @export
show_excelsheets <- function(file) {
  excelsheets <- excel_sheets(file)
  return(excelsheets)
}
# certain_sheets <- show_excelsheets(certain_files[1])
# certain_sheets

#' Import (selected?) sheets from Excelfile
#' 
#' \code{import_exelsheets} imports all or selected sheets 
#' 
#' @param file The path to the Excelfile
#' 
#' @param sheets A vector with sheet numbers to include, 
#' ignored if NULL (the default)
#' 
#' @param skip_empty_cols Logical, if FALSE (default), empty columns are retained
#' 
#' @param skip_codecols Logical, if TRUE (default), columns with numeric level for categories are removed
#' 
#' @return  A named list with tibbles for imported sheets
#' 
#' @export
import_excelsheets <- function(file, 
                               sheets=NULL, 
                               skip_empty_cols=FALSE,
                               skip_codecols=TRUE){
  if(is.null(sheets)) {
    sheets <- show_excelsheets(file)
  }
  if(is.numeric(sheets)) {
    sheets <- show_excelsheets(file)[sheets]
  }
  
  exceldata_list <- list()
  for (sheet_i in sheets){
    imported <- readxl::read_excel(path = file, sheet = sheet_i,
                                   guess_max = 10^5) |>
      empty2char() |> 
      change2numeric() |>
      change2date() |>
      change2yn()
    if(skip_empty_cols) {
      imported <- janitor::remove_empty(imported,which = 'cols')
    }
    label_code <- detect_code(imported)
    if(skip_codecols & !is.null(label_code)){
      imported <- select(imported,-all_of(label_code$Code))
    }
    exceldata_list <- list.append(exceldata_list,imported)
    names(exceldata_list)[length(exceldata_list)] <- sheet_i
  }
  return(exceldata_list)
}
# certain_datalist <- import_excelsheets(file = certain_files[1],
#                                        sheets = c(1,2,5))

#' Create summary for all imported sheets
#' 
#' \code{sheet_summary} creates suitable summaries for all columns  
#' 
#' @param data List of tibbles
#' 
#' @param exclude Vector of colum names not to be summarized, 
#' by default Nr, PatientStudyID, MonthAndYearOfBirth
#' 
#' @return List with entries per sheet  
#' 
#' @export
sheet_summary <- function(data,
                          exclude=c('Nr',
                                    'PatientStudyID',
                                    # 'Code',
                                    'MonthAndYearOfBirth')){
  description_list <- list()
  for(sheet_i in names(data)){
    sheet_data <- data |> pluck(sheet_i)
    exc_vars <- FindVars(exclude, 
                                       allnames = colnames(sheet_data))
    if(exc_vars$count>0)  {
      sheet_data <- select(sheet_data,-all_of(exc_vars$names))
    }
    if(ncol(select(sheet_data, where(function(x) {
      is.character(x) | is.factor(x)})))>0){
      desc_categories <-
      cat_desc_table(sheet_data,
                     sheet_data |> dplyr::select(where(function(x) {
                       is.character(x) | is.factor(x)})) |> cn(),
                     indentor='  - ')
    
    desc_nas <- 
      dplyr::summarize(sheet_data,
                       across(where(function(x) {is.character(x) | is.factor(x)}),
                              list(
                                `n valid`=~length(na.omit(.x)) |> as.character(),
                                `n missing`=~sum(is.na(.x)) |> as.character()),
                              # Level=~cat_desc_stats(.x,
                              #                       singleline = TRUE,
                              #                       separator = '\n') |>
                              #   pluck('level'),
                              # Frequency=~cat_desc_stats(.x,
                              #                           singleline = TRUE,
                              #                           separator = '\n') |>
                              #   pluck('freq') |> as.character()),
                              .names = "{.col}::{.fn}")) |>
      pivot_longer(everything(),
                   names_to = c('Variable','Info'),
                   names_sep = '::') |>
      pivot_wider(names_from = Info, values_from = value)
    
    desc_categories <- left_join(desc_categories,desc_nas)    
    } else {
      desc_categories <- tibble(Variable='no text variables')
    }
    if(ncol(select(sheet_data, where(is.numeric)))>0){
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
    } else {
      desc_numbers <- tibble(Variable='no numeric variables')
    }    
    if(ncol(select(sheet_data, where(is.numeric)))>0){
      desc_dates <-
        dplyr::summarize(sheet_data,
                         across(where(is.Date),
                                list(
                                  `n valid`=~length(na.omit(.x)) |> as.character(),
                                  `n missing`=~sum(is.na(.x)) |> as.character(),
                                  Min_Max=~paste(
                                    suppressWarnings(min(.x,na.rm = TRUE)),
                                    suppressWarnings(max(.x,na.rm = TRUE)),
                                    sep = ' -> ')),
                                .names = "{.col}::{.fn}")) |>
        pivot_longer(everything(),
                     names_to = c('Variable','Info'),
                     names_sep = '::') |>
        pivot_wider(names_from = Info, values_from = value)
    }  else {
      desc_dates <- tibble(Variable='no date variables')
    }
    description <- list(Categories=desc_categories,
                        Numbers=desc_numbers,
                        Dates=desc_dates)
    # description <- Hmisc::describe(sheet_data,
    #                                exclude.missing = FALSE,
    #                                n.unique=10)
    description_list <- list.append(description_list, description)
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
