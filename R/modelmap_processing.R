#' Run model for wetland prediction
#'
#' @param qdatafn training data filename.
#' @param model.type model type to use (string). One of "RF", "QRF", or "CF".
#' @param model.folder folder where the output folder will be created.
#' @param unique.rowname unique identifier column name in the training data.
#' @param predList character vector of predictor names that match the column
#'   names in the training data.
#' @param predFactor character vector of predictor names that are factors
#'   (categorical), or FALSE if there are no categorical predictors.
#' @param response.name response variable (string); column name in the training
#'   data.
#' @param response.type response type (string). One of "binary", "categorical",
#'   or "continuous".
#' @param seed number (integer) used to initialize randomization.
#' @param aoi.col optional string to specify the column name in the training
#'   data which identifies seperate AOIs. Setting this parameter means a
#'   seperate model object will be output each using a sub-set of training
#'   points defined by the values in this column. All sub-sets will be run
#'   unless the \code{aoi.target} parameter is specified. To run the model using
#'   all training points, use \code{aoi.col = NULL} (default).
#' @param aoi.target optional character vector to identify which AOI to run
#'   the model on. If \code{aoi.target = NULL} (default), and \code{aoi.col}
#'   has been specified, the model will be run on each AOI in turn. \code{aoi.
#'   target} will be appended on to the output folder name created for each AOI.
#' @param MODELfn optional filename to use for saved model output. If
#'   \code{MODELfn = NULL} (default), a unique name is created automatically
#'   using the date and time the model is run.
#' @param na.action optional string to specify action to take if there are NA
#'   values in the predictor data. Defaults to \code{na.roughfix}.
#' @param ... any other parameters to pass to ModelMap::model.build.
#' @return list containing model object from ModelMap::model.build for each AOI;
#'   list elements are named by the output model folder string (including AOI
#'   target name if aoi.target is provided).
wetland_model <- function(qdatafn,
                          model.type,
                          model.folder,
                          unique.rowname,
                          predList,
                          predFactor,
                          response.name,
                          response.type,
                          seed,
                          aoi.col = NULL,
                          aoi.target = NULL,
                          MODELfn = NULL,
                          na.action = "na.roughfix",
                          ...
                          ) {

  # TO DO:
  # Add response.target param (character vector) used to subset qdata to include
  # only those rows that have a value in response.name column that matches those
  # in response.target.
  # ...

  qdata <- read.csv(qdatafn, stringsAsFactors = FALSE)

  # Generate unique model run name if MODELfn param is NULL
  if (is.null(MODELfn)) {
    MODELfn <- format(Sys.time(), "%Y%m%d-%H%M%S")
  }

  # Don't use aoi.target if aoi.col is not specified
  if (is.null(aoi.col)) {
    aoi.target <- NULL
  } else {
    # If aoi.col is specified but aoi.target is NULL, find all unique values in
    # aoi.col
    if (is.null(aoi.target)) {
      aoi.target <- unique(qdata[[aoi.col]])
    }
  }

  # Prepend aoi.target to model run name
  MODELfn <- paste(MODELfn, aoi.target, sep = "-")

  # Create return list based on number of AOIs
  out.list <- vector(mode = "list", length = length(MODELfn))
  names(out.list) <- MODELfn

  # For each AOI...
  for (i in 1:length(MODELfn)) {

    # Create new folder for model output
    dir.create(model.folder.out <- file.path(model.folder, MODELfn[i]),
               showWarnings = TRUE)

    # Subset training data based on AOI column, or use the full dataset
    if (!is.null(aoi.col)) {
      qdata.aoi <- qdata[qdata[[aoi.col]] == aoi.target[i],]
    } else {
      qdata.aoi <- qdata
    }

    # Run the model
    #---------------
    model.obj <- ModelMap::model.build(model.type = model.type,
                                       qdata.trainfn = qdata.aoi,
                                       folder = model.folder.out,
                                       unique.rowname = unique.rowname,
                                       MODELfn = MODELfn[i],
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
    # model runs. Optional - append to existing log file to keep a running record
    # of model runs.
    # ...


    # Model Diagnostics
    #-------------------

    # TO DO:
    # Set par for better pdf layout; set back to original values after.
    # ...
    model.pred <- ModelMap::model.diagnostics(model.obj = model.obj,
                                              qdata.trainfn = qdata.aoi,
                                              folder = model.folder.out,
                                              MODELfn = MODELfn[i],
                                              unique.rowname = unique.rowname,
                                              # Model Validation Arguments
                                              prediction.type = "OOB",
                                              device.type = "pdf",
                                              cex = 1.2)

    # TO DO:
    # Save diagnostic values (section commented out below) to an output csv?
    # Which values to save?
    # Write diagnostic values to same log.csv as model params. Append to existing
    # file so that multiple model runs can be recorded and compared.
    # ...

    # # Look at confusion matrix text output, read into R
    # # For categorical models, this file contains the observed category for each
    # # location, category predicted by majoirty vote, as well as one column for each
    # # category observed in the data, giving the proportion of trees that voted for
    # # that category.
    # CMX.CSV <- read.table(file.path(model.folder.out, paste0(MODELfn, "_pred_cmx.csv")),
    #                       header = FALSE,
    #                       sep = ",",
    #                       stringsAsFactors = FALSE)
    #
    # PRED <- read.table(file.path(model.folder.out, paste0(MODELfn, "_pred.csv")),
    #                    header = TRUE,
    #                    sep = ",",
    #                    stringsAsFactors = TRUE)
    #
    # # To calculate conf. matrix, we use observed and predicted columns. Read.table()
    # # function converts columns containing strings to factors.
    # # Because there are many categories present in the observed data, to get a
    # # symetric confusion matrix it is important to make sure all levels are presents
    # # in both factors. These lines are needed for numeric categories, redundant for
    # # character categories.
    # PRED$pred <- as.factor(PRED$pred)
    # PRED$obs <- as.factor(PRED$obs)
    #
    # # Adjust levels so all values are included in both observed and predicted.
    # LEVELS <- unique(c(levels(PRED$pred), levels(PRED$obs)))
    # PRED$pred <- factor(PRED$pred, levels = LEVELS)
    # PRED$obs <- factor(PRED$obs, levels = LEVELS)
    #
    # # Calculate confusion matrix
    # # Calculate the errors of Omission and Comission
    # CMX <- table(predicted = PRED$pred, observed = PRED$obs)
    # CMX.diag <- diag(CMX)
    # CMX.OMISSION <- 1 - (CMX.diag / apply(CMX, 2, sum))
    # CMX.COMISSION <- 1 - (CMX.diag / apply(CMX, 1, sum))
    #
    # # Calculate PCC (Percent correctly classified)
    # CMX.PCC <- sum(CMX.diag) / sum(CMX)
    #
    # # Calculate Kappa
    # CMX.KAPPA <- PresenceAbsence::Kappa(CMX)
    #
    # # Calculate MAUC. Calculated from the category specific predicitons (% of trees
    # # that voted for each category)
    # VOTE <- HandTill2001::multcap(response = PRED$obs,
    #                               predicted = as.matrix(PRED[, -c(1, 2, 3)]))
    # MAUC <- HandTill2001::auc(VOTE)

    out.list[[MODELfn[i]]] <- model.obj
  }

  return(out.list)
}


#' Wetland map production
#'
#' @param model.obj model object returned from \code{wetland_model}.
#' @param folder folder name used for output
#' @param MODELfn filename to use for saved model output.
#' @param rastLUTfn filename of a .csv for a rastLUT.
#' @param na.action optional string to specify action to take if there are NA
#'   values in the prediction data. Defaults to \code{na.omit}.
#' @param ... any other parameters to pass to ModelMap::model.mapmake.
wetland_map <- function (model.obj,
                         folder,
                         MODELfn,
                         rastLUTfn,
                         na.action = "na.omit",
                         ...) {

  # model.mapmake() creates an ascii text file and an imagine .img file of
  # predictions for each map pixel.
  model.mapmake(model.obj = model.obj,
                folder = folder,
                MODELfn = MODELfn,
                rastLUTfn = rastLUTfn,
                na.action = na.action,
                ...)
}
