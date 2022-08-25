#' Fetch a Qualtrics survey by name via the Qualtrics API
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This has been replaced in favor of the more flexible [fetch_surveys()].
#'
#' @param survey_name Survey name as it appears in the Qualtrics UI
#' @param ... Additional arguments to be passed to `qualtRics::fetch_survey()`
#'
#' @export
#'
#' @importFrom qualtRics all_surveys
#' @importFrom qualtRics fetch_id
#' @importFrom qualtRics fetch_survey
#'
#' @examples
#' \dontrun{
#' # fetch all survey responses
#' mysurvey <- fetch_survey_named("my survey name")
#'
#' # fetch survey based on return date
#' mysurvey <- fetch_survey_named("my survey name", start_date = "2021-07-01", end_date = "2021-07-30")
#' }
#'
fetch_survey_named <- function(survey_name, ...) {

  lifecycle::deprecate_warn("8/25/22", "fetch_survey_named()", "fetch_surveys()")

  survey <- qualtRics::all_surveys()
  survey <- qualtRics::fetch_id(survey, survey_name)
  survey <- qualtRics::fetch_survey(survey, ...)

  return(survey)

}

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
fetch_surveys <- function(survey_names, ...) {

  # get args passed on to `fetch_survey()`
  dots <- dplyr::enquos(...)

  # get a list of survey ids
  surveys <- qualtRics::all_surveys()
  surveys <- dplyr::filter(surveys, name %in% survey_names)

  # if only one survey name provided, just return the responses, else return a nested tibble
  if (length(survey_names) == 1) {

    id <- dplyr::pull(surveys, id)
    surveys <- qualtRics::fetch_survey(id, ...)

  } else {

    surveys <- dplyr::select(surveys, survey_name = name, survey_id = id)
    surveys <- dplyr::mutate(surveys, responses = purrr::map(survey_id, ~qualtRics::fetch_survey(.x, !!!dots)))

  }

  return(surveys)
}
