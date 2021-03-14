#' read_zipped_folder
#'
#' \code{read_zipped_folder}
#' @param export_path_path Path to the Castor export_path zip.
#' @examples
#' \dontrun{
#' data_list <- read_zipped_folder(export_path_path)
#' }
#' @import magrittr
#' @import dplyr
#' @import stringr
#' @export
read_zipped_folder <- function(export_path){
  if (is.null(export_path)) stop("Please select a Castor csv export path.")
  if (!grepl(export_path, pattern = ".*\\.zip")) stop("Please select a valid zip folder.")

  tryCatch(
    expr = {files_in_zip <- unzip(export_path, list = TRUE)$Name},
    error = function(e) stop("Folder could not be unzipped.")
  )

  data_list <- list()
  for (name in files_in_zip) {
    if (nchar(name) > 100) {
      warning(paste("File", name, "has more then 100 characters and will be skipped."))
      next
    } else {
      name_data <- stringr::str_replace(name, pattern = "\\.csv", replacement = "")
      data_list[[name_data]] <- read.csv2(unz(export_path, files_in_zip[grepl(files_in_zip,
                                                                         pattern = name,
                                                                         ignore.case = TRUE)]),
                                     header = TRUE,
                                     stringsAsFactors = FALSE,
                                     sep = ";",
                                     fileEncoding = "UTF-8-BOM",
                                     encoding = "UTF-8")
      names(data_list[[name_data]]) <-
        tolower(names(data_list[[name_data]])) %>%
        stringr::str_replace_all(pattern = "\\.ja$",
                                 replacement = "")
      if (name_data %in% c("report_variablelist", "study_variablelist")) {
        names(data_list[[name_data]])[1] <- "phase.name"
      } else if (name_data == "field_options") {
        names(data_list[[name_data]])[1] <- "option.group.name"
      }
    }
  }
  return(data_list)
}
