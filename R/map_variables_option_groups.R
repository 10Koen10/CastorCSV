#' map_variables_option_groups
#'
#' \code{map_variables_option_groups}
#' @param data The datasets, in the data_list from the read_zipped_folder output,
#' which you want to map the values and option groups.
#' @param variable_list The variable list. Depending on the data param, choose
#' report_variablelist for report data, study_variablelist for study data or
#' survey_variablelist for survey data.
#' data.
#' @param field_options The field options data in the data_list output from
#' read_zipped_folder.
#' @param checkbox_options Choose the language options for the checkboxes,
#' for example: c("No", "Yes") or c("Nee", "Ja"). In Castoredc the negative
#' option is 0 and the positive 1. Please mention the negative option first
#' and then the positive.
#' @examples
#' \dontrun{
#' lapply(data_list[c("study_data")],
#'        map_variables_option_groups,
#'        data_list$study_variablelist,
#'        data_list$field_options)
#' }
#' @import magrittr
#' @export
map_variables_option_groups <- function(data,
                                        variable_list,
                                        field_options,
                                        checkbox_options = c("No", "Yes")){
  variable_list$variable.name <- tolower(variable_list$variable.name)
  column_names <-
    names(data)[names(data) %in% variable_list$variable.name]

  column_names_option_group <-
    names(data)[!names(data) %in% variable_list$variable.name &
                  vapply(strsplit(names(data),"\\."),
                         `[`, 1,
                         FUN.VALUE = character(1)) %in% variable_list$variable.name]

  for (name in column_names_option_group) {
    index_function <-
      which(variable_list$variable.name == vapply(strsplit(name,"\\."),
                                                  `[`, 1,
                                                  FUN.VALUE = character(1)))
    if (variable_list$field.type[index_function] == "checkbox") {
      data[[name]] <- factor(x = data[[name]],
                             levels = c(0, 1),
                             labels = checkbox_options)
    }
  }

  for (name in column_names) {
    index_function <- which(variable_list$variable.name == name)

    if (variable_list$field.type[index_function] == "checkbox") {
      data[[name]] <- factor(x = data[[name]],
                             levels = c(0, 1),
                             labels = checkbox_options)
    } else if (variable_list$field.type[index_function] == "date") {
      data[[name]] <-
        as.Date(data[[name]], format = "%d-%m-%Y")
    } else if (variable_list$field.type[index_function] == "datetime") {
      data[[name]] <-
        as.POSIXct(data[[name]], format = "%d-%m-%Y %H:%M")
    } else if (variable_list$field.type[index_function] %in% c("radio", "dropdown")) {
      get_option_group <-
        dplyr::filter(field_options,
                      field_options$option.group.name ==
                        variable_list$optiongroup.name[index_function])
      data[[name]] <-
        factor(x = data[[name]],
               levels = get_option_group[["option.value"]],
               labels = get_option_group[["option.name"]])
    } else if (variable_list$field.type[index_function] == "string" |
               variable_list$field.type[index_function] == "textarea") {
      data[[name]] <- as.character(data[[name]])
    }
  }
  return(data)
}
