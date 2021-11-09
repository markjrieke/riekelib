#' Fetch a Qualtrics survey by name via the Qualtrics API
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

  survey <- qualtRics::all_surveys()
  survey <- qualtRics::fetch_id(survey, survey_name)
  survey <- qualtRics::fetch_survey(survey, ...)

  return(survey)

}
