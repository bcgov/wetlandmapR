#' Run model and diagnostics for wetland prediction
#'
#' @param qdatafn training data filename.
#' @param model.type model type to use (string). One of "RF", "QRF", or "CF".
#' @param model.folder folder where the output folder will be created.
#' @param unique.rowname unique identifier column name in the training data.
#' @param predList character vector of predictor names that match the column
#'   names in the training data.
#' @param predFactor character vector of predictor names that are factors (categorical), or
#'   FALSE if there are no categorical predictors.
#' @param response.name response variable (string); column name in the training data.
#' @param response.type response type (string). One of "binary", "categorical", or "continuous".
#' @param seed number (integer) used to initialize randomization.
#' @param MODELfn optional filename to use for saved model output. If \code{MODELfn = NULL} (default)
#'   a unique name is created automatically using the date and time the model is run.
#' @param na.action optional string to specify action to take if there are NA values
#'   in the predictor data. Defaults to \code{na.roughfix}.
#' @param ... any other parameters to pass to ModelMap::model.build.
#' @return list containing model object from ModelMap::model.build and output model folder string.
wetland_model <- function(qdatafn,
                          model.type,
                          model.folder,
                          unique.rowname,
                          predList,
                          predFactor,
                          response.name,
                          response.type,
                          seed,
                          MODELfn = NULL,
                          na.action = "na.roughfix",
                          ...
                          ) {

  # Generate unique model run name if MODELfn param is NULL
  if (is.null(MODELfn)) {
    MODELfn <- format(Sys.time(), "%Y%m%d-%H%M%S")
  }

  # Create new folder for model output
  dir.create(model.folder.out <- file.path(model.folder, MODELfn), showWarnings = TRUE)
  model.folder.out.base <- file.path(model.folder.out, MODELfn)


  # Run the model
  model.obj <- ModelMap::model.build(model.type = model.type,
                                     qdata.trainfn = qdatafn,
                                     folder = model.folder.out,
                                     unique.rowname = unique.rowname,
                                     MODELfn = MODELfn,
                                     predList = predList,
                                     predFactor = predFactor,
                                     response.name = response.name,
                                     response.type = response.type,
                                     na.action = na.action,
                                     seed = seed,
                                     ...)

  # TO DO:
  # Copy model params to log - read {MODELfn}_model_building_arguments.txt and
  # write param values to log.csv, so they can be re-used as input to subsequent
  # model runs.
  # ...


  # Model Diagnostics
  model.pred <- ModelMap::model.diagnostics(model.obj = model.obj,
                                            qdata.trainfn = qdatafn,
                                            folder = model.folder.out,
                                            MODELfn = MODELfn,
                                            unique.rowname = unique.rowname,
                                            # Model Validation Arguments
                                            prediction.type = "OOB",
                                            device.type = "pdf",
                                            cex = 1.2)

  return(list(model.obj, model.folder.out))
}

#' Wetland map production
#'
#' @param model.obj model object returned from \code{wetland_model}.
#' @param folder folder name used for output
#' @param MODELfn filename to use for saved model output.
#' @param rastLUTfn filename of a .csv for a rastLUT.
#' @param na.action optional string to specify action to take if there are NA values
#'   in the prediction data. Defaults to \code{na.omit}.
#' @param ... any other parameters to pass to ModelMap::model.mapmake.
wetland_map <- function (model.obj,
                         folder,
                         MODELfn,
                         rastLUTfn,
                         na.action = "na.omit",
                         ...) {

  # model.mapmake() creates an ascii text file and an imagine image file of predictions for each map pixel.
  model.mapmake(model.obj = model.obj,
                folder = folder,
                MODELfn = MODELfn,
                rastLUTfn = rastLUTfn,
                na.action = na.action)
}
