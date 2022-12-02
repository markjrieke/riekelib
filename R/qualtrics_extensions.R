#' Fetch a list of Qualtrics surveys by name via the Qualtrics API
#'
#' @return
#' For a single value supplied to `survey_names`: a tibble of the survey
#' responses from `qualtRics::fetch_survey()`. For multiple `survey_names`, a
#' tibble with colums `survey_name`, `survey_id`, and `responses`. The `responses`
#' contains the responses for each survey as returned by
#' `qualtRics::fetch_survey()`.
#'
#' @param survey_names A vector of survey names as they appear in the Qualtrics UI.
#' @param ... Additional arguments to be passed to `qualtRics::fetch_survey()`.
#'
#' @export
#'
#' @importFrom dplyr enquos
#' @importFrom qualtRics all_surveys
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom qualtRics fetch_survey
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' # fetch a single survey by name
#' my_survey <- fetch_surveys("my cool survey")
#'
#' # fetch multiple surveys by name
#' my_surveys <- fetch_surveys(c("my cool survey", "my lame survey"))
#' }
fetch_surveys <- function(survey_names, ..., time_zone = "America/Chicago") {

  # define survey_names internally
  ui_names <- survey_names

  # get args passed on to `fetch_survey()`
  dots <- dplyr::enquos(...)

  # get a list of survey ids
  surveys <- qualtRics::all_surveys()
  surveys <- dplyr::filter(surveys, name %in% ui_names)

  # get list of survey_names that don't appear in ui
  not_fetched <- survey_names[which(!survey_names %in% surveys$name)]

  # stop if all surveys are missing or spelled incorrectly
  if (nrow(surveys) == 0) cli::cli_abort("No surveys match values supplied to `survey_names`")

  # warn if some are missing or spelled incorrectly
  if (length(not_fetched) > 0) {

    cli::cli_warn(
      c("The following `survey_names` were not found in the Qualtrics UI and were skipped:",
        setNames(not_fetched, rep("x", length(not_fetched))))
    )

  }

  # if only one survey name provided, just return the responses, else return a nested tibble
  if (nrow(surveys) == 1) {

    id <- dplyr::pull(surveys, id)
    surveys <- qualtRics::fetch_survey(id, ..., time_zone = time_zone)

  } else {

    surveys <- dplyr::select(surveys, survey_name = name, survey_id = id)
    surveys <- dplyr::mutate(surveys, responses = purrr::map(survey_id, ~qualtRics::fetch_survey(.x, !!!dots, time_zone = time_zone)))

  }

  return(surveys)

}
