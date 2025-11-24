
#' Make metadata XML required for EDI data publication
#'
#' This is a wrapper function and utilizes the package EMLassemblyline to
#' facilitate the creation of an EML document
#' @import here
#' @import readr
#' @import purrr
#' @import dplyr
#' @param data_file_names A list of file names for data tables to be included in
#' the publication. For instance, "catch.csv" or c("catch.csv", "trap.csv")
#'
#' @param attributes_file_names A list of file names for the attributes metadata
#' csv templates. The naming convention for the files should be "attributes_NAME_OF_DATA_TABLE.csv"
#'
#' @param title Title of the data publication. The title should be clear, concise,
#' and descriptive.
#'
#' @param geography Description of the geography. For instance, "Feather River"
#'
#' @param coordinates These are the bounding coordinates entered as: North bounding,
#' East bounding, South bounding, West bounding.
#'
#' @param maintenance This describes how often the data publication will be updated.
#' The options are: daily, weekly, monthly, annually, complete.
#'
#' @param edi_number The EDI number to be used for the data publication. If the EML
#' is to update an existing package, the number for the next version should be input.
#' For instance, if the existing EDI package is `edi.1234.1` then the you should
#' input `edi.1234.2` because you are creating the EML for the next version.
#' If this is for a new package, please reserve a new EDI number. INSERT SEE DOCUMENTATION.
#'
#' @return An EML file within `data-raw/eml`. EMLassemblyline will include messages
#' to ensure the EML file validates and will also flag any issues.
#'
#' @details
#'
#' This function assumes that all appropriate metadata templates have been completed
#' Check that the following files in metadata_templates have been manually filled
#' in and are correct and generate any additional files as needed:
#' abstract.txt
#' attributes_csv_templates/attributes_csv_template.csv (one per data table)
#' custom_units.txt
#' intellectual_rights.txt (DO NOT CHANGE unless using another license)
#' keywords.txt
#' methods.docx
#' personnel.txt
#'
#' This function also assumes that the user has an EDI account and credentials
#' are stored in their .Renviron as EDI_USER_ID
#'
#'
#' @examples
#'
#' make_eml_edi(data_file_names = c("microhabitat_observations.csv","survey_locations.csv"),
#'              attributes_file_names = c("microhabitat_attributes.csv","survey_locations_attributes.csv"),
#'              title = "Distribution and habitat use of juvenile steelhead and other fishes of the lower Feather River",
#'              geography = "Feather River",
#'              coordinates = c("39.4621", "-121.604633", "39.21215", "-121.632636"),
#'              maintenance = "complete",
#'              edi_number = "edi.test")
#'
#' @export
make_eml_edi <- function(data_file_names,
                              attributes_file_names,
                              title,
                              geography,
                              coordinates,
                              maintenance,
                              edi_number) {
  if (missing(data_file_names)) {
    stop("The 'data_file_names' argument is required.", call. = FALSE)
  }
  if (missing(attributes_file_names)) {
    stop("The 'attributes_file_names' argument is required.", call. = FALSE)
  }
  if (missing(title)) {
    stop("The 'title' argument is required.", call. = FALSE)
  }
  if (missing(geography)) {
    stop("The 'geography' argument is required.", call. = FALSE)
  }
  if (missing(coordinates)) {
    stop("The 'coordinates' argument is required.", call. = FALSE)
  }
  if (missing(maintenance)) {
    stop("The 'maintenance' argument is required.", call. = FALSE)
  }
  if (missing(edi_number)) {
    stop("The 'edi_number' argument is required.", call. = FALSE)
  }

  # Define paths for your metadata templates, data, and EML

  path_templates <- here("data-raw", "metadata_templates")
  path_data <- here("data-raw", "data_objects")
  path_eml <- here("data-raw", "eml")

  # Read in your data file to find the start and end dates for temporal coverage

  date_ranges <- map_dfr(data_file_names, function(file_name) {
    df <- read_csv(paste0(path_data, "/", file_name))
    tibble(
      min_date = min(df$date, na.rm = TRUE),
      max_date = max(df$date, na.rm = TRUE)
    )
  })

  # Get overall min and max for the start and end date
  start_date <- min(date_ranges$min_date)
  end_date <- max(date_ranges$max_date)

  # Attributes metadata - csv to txt

  # It is easier to fill in the data dictionaries as csv and then translate to .txt file

  walk(attributes_file_names, function(file_name) {
    metadata_csv <- read_csv(here(
      path_templates,
      "attributes_csv_template",
      paste0(file_name)
    ))

    names_without_ext <- sub("\\.[^.]*$", "", file_name)

    write.table(
      metadata_csv,
      file = here(
        path_templates,
        paste0(names_without_ext, ".txt")
      ),
      sep = "\t",
      row.names = FALSE,
      quote = FALSE
    )
  })

  # Personnel metadata - csv to txt

  personnel_csv <- read_csv(here(
    path_templates,
    "personnel_csv_template",
    "personnel.csv"
  ))
  write.table(
    personnel_csv,
    file = here(path_templates, "personnel.txt"),
    sep = "\t",
    row.names = F,
    quote = F
  )

  # Make EML from metadata templates

  make_eml(
    path = path_templates,
    data.path = path_data,
    eml.path = path_eml,
    dataset.title = title,
    temporal.coverage = c(start_date, end_date),
    geographic.description = geography,
    geographic.coordinates = coordinates,
    maintenance.description = maintenance,
    data.table = data_file_names,
    user.id = Sys.getenv("EDI_USER_ID"),
    user.domain = "EDI",
    package.id = edi_number
  )
}
