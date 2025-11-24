
#' Publish data package on EDI
#'
#' This is a wrapper function and utilizes the package EMLaide to send an API
#' request to EDI.
#'
#' This function assumes that `make_eml_edi` was used to create an EML metadata
#' document and that the file structure follows the documentation. INSERT link.
#'
#' This function also assumes that the user has an EDI account and credentials
#' are stored in their .Renviron as EDI_USER_ID and EDI_PASSWORD

#' @param publish_type Indicate whether you are publishing an update to an existing
#' package or publishing a new data package. Option are: "new", "update"
#'
#' @param edi_number EDI number of the data package being published. This should
#' match the EDI number in the EML document. If you are updating an existing package
#' the EDI number will be one version greater than the existing package.
#'
#' @param publish_environment Environment where data are being published. Recommended to
#' publish on the staging environment first to review the data package and then
#' publish to production. Option are: "staging", "production"
#'
#' @return A message describing if the upload was successful or not.
#'
#' @export
publish_data_edi <- function(path_eml_file, publish_type, edi_number, publish_environment) {

  path_eml_file <- here("data-raw", "eml", paste0(edi_number, ".xml")) # Created using make-metadata-eml.R

  if (publish_type == "update") {
    parts <- str_split(edi_number, "\\.")[[1]]
    parts[length(parts)] <- as.character(as.numeric(parts[length(parts)]) - 1)
    existing_edi_number <- paste(parts, collapse = ".")

    update_edi_package(
      user_id = Sys.getenv("EDI_USER_ID"),
      password = Sys.getenv("EDI_PASSWORD"),
      existing_package_identifier = existing_edi_number,
      eml_file_path = path_eml_file,
      environment = publish_environment
    )
  } else if (publish_type == "new") {
    upload_edi_package(
      user_id = Sys.getenv("EDI_USER_ID"),
      password = Sys.getenv("EDI_PASSWORD"),
      eml_file_path = path_eml_file,
      environment = publish_environment
    )
  }
}
