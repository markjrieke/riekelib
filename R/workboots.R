# -------------------------------externals--------------------------------------

#' Fit and predict from a workflow using many bootstrap resamples.
#'
#' @param workflow An un-fitted workflow object.
#' @param training_data A tibble or dataframe of data to be resampled and used for training.
#' @param new_data A tibble or dataframe used to make predictions.
#' @param n_models An integer for the number of bootstrap resampled models that will be created.
#' @param seed A single seed value to be passed to `set.seed()`.
#' @param ... Additional params passed to `rsample::bootstraps()`.
#'
#' @export
#'
#' @importFrom rsample bootstraps
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest
#' @importFrom purrr map_dfc
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' library(tidymodels)
#'
#' # setup a workflow without fitting
#' wf <-
#'   workflow() %>%
#'   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
#'   add_model(linear_reg())
#'
#' # fit and predict 125 bootstrap resampled models to mtcars
#' wf %>%
#'   predict_boots(training_data = mtcars, new_data = mtcars, n_models = 125)
#' }
predict_boots <- function(workflow,
                          training_data,
                          new_data,
                          n_models = 100,
                          seed = 123,
                          ...) {

  # create resamples from training set
  training_boots <-
    rsample::bootstraps(
      training_data,
      times = n_models,
      ...
    )

  # map sequence of indices to `predict_single_boot()`
  # returns a column of predictions for each model
  preds <-
    purrr::map_dfc(
      seq(1, n_models),
      ~predict_single_boot(
        workflow = workflow,
        boot_splits = training_boots,
        new_data = new_data,
        index = .x,
        seed = seed
      )
    )

  # nest & return predictions in long format
  preds <- tibble::rowid_to_column(preds)

  preds <-
    tidyr::pivot_longer(
      preds,
      dplyr::starts_with(".pred_"),
      names_to = "model",
      values_to = ".pred"
    )

  preds <-
    tidyr::nest(
      preds,
      .preds = c(model, .pred)
    )

  return(preds)

}

#' Fit and estimate variable importance from a workflow using many bootstrap resamples.
#'
#' @param workflow An un-fitted workflow object.
#' @param training_data A tibble or dataframe of data to be resampled and used for training.
#' @param n_models An integer for the number of bootstrap resampled models that will be created.
#' @param seed A single seed value to be passed to `set.seed()`.
#' @param ... Additional params passed to `rsample::bootstraps()`.
#'
#' @export
#'
#' @importFrom rsample bootstraps
#' @importFrom purrr map_dfr
#' @importFrom dplyr rename_with
#' @importFrom tidyr nest
#'
#' @examples
#' \dontrun{
#' library(tidymodels)
#'
#' # setup a workflow without fitting
#' wf <-
#'   workflow() %>%
#'   add_recipe(recipe(qsec ~ wt, data = mtcars)) %>%
#'   add_model(linear_reg())
#'
#' # fit and estimate variable importance from 125 bootstrap resampled models
#' wf %>%
#'   vi_boots(training_data = mtcars, n_models = 125)
#' }
vi_boots <- function(workflow,
                     training_data,
                     n_models = 100,
                     seed = 123,
                     ...) {

  # create resamples from training set
  training_boots <-
    rsample::bootstraps(
      training_data,
      times = n_models,
      ...
    )

  # map sequence of indices to `vi_single_boot()`
  # returns a variable + importance for each model (number of cols may vary by model type)
  bootstrap_vi <-
    purrr::map_dfr(
      seq(1, n_models),
      ~vi_single_boot(
        workflow = workflow,
        boot_splits = training_boots,
        index = .x,
        seed = seed
      )
    )

  # rename cols
  bootstrap_vi <- dplyr::rename_with(bootstrap_vi, tolower)

  # return a nested tibble
  bootstrap_vi <- tidyr::nest(bootstrap_vi, importance = -variable)

  return(bootstrap_vi)

}
# -------------------------------internals--------------------------------------

#' Fit a model and predict based on a single bootstrap resample
#'
#' @param workflow An un-fitted workflow object.
#' @param boot_splits A bootstrap split object created by `rsample::bootstraps()`.
#' @param new_data New data to make predictions
#' @param index Index of `boot_splits` to use for training
#' @param seed A single seed value to be passed to `set.seed()`.
#'
#' @importFrom rsample training
#' @importFrom generics fit
#' @importFrom stats predict
#' @importFrom dplyr rename
#' @importFrom rlang sym
#'
predict_single_boot <- function(workflow,
                                boot_splits,
                                new_data,
                                index,
                                seed = 123) {

  # get training data from bootstrap resample split
  boot_train <-
    rsample::training(
      boot_splits$splits[[index]]
    )

  # set seed
  set.seed(seed + index)

  # fit workflow to training data
  model <- generics::fit(workflow, boot_train)

  # predict given model and new data
  preds <- stats::predict(model, new_data)

  # rename .pred col based on index number
  preds <- dplyr::rename(preds, !!rlang::sym(paste0(".pred_", index)) := .pred)

  return(preds)

}

#' Fit a model and get the variable importance based on a single bootstrap resample
#'
#' @param workflow An un-fitted workflow object.
#' @param boot_splits A bootstrap split object created by `rsample::bootstraps()`.
#' @param index Index of `boot_splits` to use for training
#' @param seed A single seed value to be passed to `set.seed()`.
#'
#' @importFrom rsample training
#' @importFrom generics fit
#' @importFrom vip vi
#' @importFrom workflows pull_workflow_fit
#'
vi_single_boot <- function(workflow,
                           boot_splits,
                           index,
                           seed = 123) {

  # get training data from bootstrap resample split
  boot_train <-
    rsample::training(
      boot_splits$splits[[index]]
    )

  # set seed
  set.seed(seed + index)

  # fit workflow to to the training data
  model <- generics::fit(workflow, boot_train)

  # get the variable importance from the model
  vi_boot <- vip::vi(workflows::pull_workflow_fit(model))

  return(vi_boot)

}
