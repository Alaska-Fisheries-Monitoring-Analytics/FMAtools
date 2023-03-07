#' @title Create a Complete, Ordered Set of P. Halibut Report Tables in MSExcel
#'
#' @description \code{tables_2xl()} creates an MSExcel workbook with all the P. halibut report tables in the same order presented in the report, with appropriate sheetnames.
#'
#' @param .dirpth1 A path to a folder where files will be read from. Default \code{path_data}.
#' @param .dirpth2 An optional second path, different from \code{.dirpth1}, to a folder where files will be read from. Default \code{path_tables/supplemental/}. Set to \code{NULL} if not needed.
#'
#' @details
#' \code{tables_2xl()} can pull and read files from two different directory paths (\code{.dirpth1, .dirpth2}).
#' In the base case use, there are nine (9) static tables that are fixed from year-to-year are housed in \code{path_data} and read using \code{list.files(path = .dirpth1)}.
#' Then tables that are being updated each year are read from \code{paste0(path_tables, "supplemental/")} and read using \code{list.files(path = .dirpth2)}.
#' The two sets of files are put together in the proper order based on the order in \code{table_file_names_list} and \code{table_file_names_df}, which are created in \code{/scripts/Table_File_Name_List.R}.
#'
#' The nine static files, located in \code{path_data} are:
#' \enumerate{
#'   \item \code{BottomTrawl_Mortality_Rates.csv}
#'   \item \code{Pot_Mortality_Rates.csv}
#'   \item \code{HookandLine_Injury_Rates.csv}
#'   \item \code{CS_Data_Collection_Table.csv"}
#'   \item \code{NCS_Expansion_Factor_Descriptions.csv}
#'   \item \code{IPHC_Length_Weight_Conversion_Table.csv}
#'   \item \code{CS_Federal_Managed_Fisheries.csv}
#'   \item \code{NCS_Federal_Managed_Fisheries.csv}
#'   \item \code{State_Managed_Fisheries.csv}
#' }
#'
#' @note **IMPORTANT** The order of the table file names are critical for getting properly ordered output in the final MSExcel workbook.
#' Names and order are determined by \code{table_file_names_list} and \code{table_file_names_df}, which are created in \code{/scripts/Table_File_Name_List.R}.
#'
#' @return Returns nothing. Saves an MSExcel workbook of tables to \code{path_tables/supplemental/}
#'
#' @seealso \code{/scripts/Table_File_Name_List.R}, \code{()}
#'
#' @examples
#' \dontrun{
#'    tables_2xl(.dirpth1 = path_data, .dirpth2 = paste0(path_tables, "supplemental/"))
#' }
#'
#' @export


tables_2xl <- function(.dirpth1 = path_data,
                       .dirpth2 = paste0(path_tables, "supplemental/")){

#----------------------------
# Handle Fixed Tables Found
#      in .dirpth1
#----------------------------
# get the names of all the .csv files from .dirpth1
filenms_dir1 <- list.files(.dirpth1, pattern = ".*?\\.csv")

#Get the .dirpth1 files with their path
dir1filnms <- filenms_dir1[filenms_dir1 %in% table_file_name_list]

# compare filenms_dir1 to table_file_name_list and grab the file names from
# that list that are in .dirpth1

if(length(dir1filnms) != 9){
  stop("There is a problem. The number of files in the .dirpth1\n   that match the files in the table_file_name_list\n     is not equal to nine (9).\n   Verify that all the files you need are in both\n     the list and also in .dirpth1.")
}else{

  #Get the full path and file names for .dirpth1
  full_filenms_dir1 <- file.path(.dirpth1, dir1filnms, fsep = "")

  #Read in the files from .dirpth1
  dir1_files <- lapply(full_filenms_dir1,
                       FUN = function(x){
                         out <- qdr(pth = x)
                         return(out)})

  # Match the file names with their numbered file name from
  # table_file_name_df$xl_sheet_name

  matched_num_file_name <- data.frame(table_file_names = dir1filnms,
                                      stringsAsFactors = FALSE)%>%
                           dplyr::left_join(., table_file_name_df%>%
                                                dplyr::select(table_file_names,
                                                              xl_sheet_name))%>%
                           qdf()

  names(dir1_files) <- matched_num_file_name$xl_sheet_name

}

if(!is.null(.dirpth2)){

#----------------------------
# Handle Dynamic Tables Found
#      in .dirpth2
#----------------------------
  # get the names of all the .csv files from .dirpth2
  filenms_dir2 <- list.files(.dirpth2, pattern = ".*?\\.csv")

  #Get the .dirpth2 files with their path
  dir2filnms <- filenms_dir2[filenms_dir2 %in% table_file_name_list]
  full_filenms_dir2 <- file.path(.dirpth2, dir2filnms, fsep = "")

  # check
  chk = length(full_filenms_dir2) - (length(table_file_name_list)-
                                       length(full_filenms_dir1))
  if(chk > 0){
    stop("There are TOO MANY files being read from .dirpth2")
  }
  if(chk < 0){
    stop("There are TOO FEW files being read in;\n   it's not clear which directory path is creating the problem.")
  }

  #Read in the files from .dirpth2
  dir2_files <- lapply(full_filenms_dir2,
                       FUN = function(x){
                         out <- qdr(pth = x)
                         return(out)})

  # Match the file names with their numbered file name from
  # table_file_name_df$xl_sheet_name

  matched_num_file_name2 <- data.frame(table_file_names = dir2filnms,
                                      stringsAsFactors = FALSE)%>%
    dplyr::left_join(., table_file_name_df%>%
                       dplyr::select(table_file_names,
                                     xl_sheet_name))%>%
    qdf()

  # check
  if(! nrow(matched_num_file_name2) == length(dir2_files)){
    stop("The names and files do not match for .dirpth2")
  }

  names(dir2_files) <- matched_num_file_name2$xl_sheet_name


}

#--------------------------------------------------
# Put all files together & sort by numeric order
#--------------------------------------------------
all_out_files <- c(dir1_files, dir2_files)
all_out_files <- all_out_files[stringr::str_sort(names(all_out_files), numeric = TRUE)]

return(all_out_files)

}
