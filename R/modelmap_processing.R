#' Wetland prediction model
#'
#' This function runs \code{\link[ModelMap]{model.build}} to build a wetland
#' model using training data attributed with predictor values, such as from
#' \code{\link{grid_values_at_sp}}.
#'
#' If an AOI has been used to prepare the training data, the model can be run
#' for each seperate AOI by using the \code{aoi.col} parameter, or specific AOIs
#' by also using the \code{aoi.target} parameter.
#'
#' Response targets can also be provided, so the input training data is
#' restricted to only those target response points.
#'
#' As well as building the model, this function produces diagnostics by running
#' \code{\link[ModelMap]{model.diagnostics}}.
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
#' @param response.target optional character vector used to subset training
#'   data to include only those rows that have a value in \code{response.name}
#'   column that matches those in \code{response.target}.
#' @param aoi.col optional string to specify the column name in the training
#'   data which identifies seperate AOIs. The values in this column must not
#'   contain the character "-". Setting this parameter means a seperate model
#'   object will be output each using a sub-set of training points defined by
#'   the values in this column. All sub-sets will be run unless the
#'   \code{aoi.target} parameter is specified. To run the model using all
#'   training points, use \code{aoi.col = NULL} (default).
#' @param aoi.target optional character vector to identify which AOI to run
#'   the model on. If \code{aoi.target = NULL} (default), and \code{aoi.col}
#'   has been specified, the model will be run on each AOI in turn. \code{aoi.
#'   target} will be appended on to the output folder name created for each AOI.
#' @param MODELfn optional filename to use for saved model output. If
#'   \code{MODELfn = NULL} (default), a unique name is created automatically
#'   using the date and time the model is run. If \code{aoi.col} is specified,
#'   each unique value in this field is appended to \code{MODELfn} for each
#'   model run.
#' @param na.action optional string to specify action to take if there are NA
#'   values in the predictor data. Defaults to \code{na.roughfix}.
#' @param ... any other parameters to pass to \code{\link[ModelMap]{model.build}}.
#'
#' @return list containing model object from \code{\link[ModelMap]{model.build}}
#'   for each AOI; list elements are named by the output model folder string
#'   (including AOI target name if aoi.target is provided).
#'
#' @examples
#' \dontrun{
#' # RastLUT:
#' rastLUT <- read.csv("data/rastLUT.csv",
#'                     header = FALSE,
#'                     stringsAsFactors = FALSE)
#'
#' # Character vector of the predictor names, used as input to the model:
#' predList <- rastLUT[, 2]
#'
#' # Training data, that has been intersected with an AOI layer:
#' qdatafn <- "data/training_points_attd.csv"
#'
#' model.out <- wetland_model(qdatafn = qdatafn,
#'                            model.type = "RF",
#'                            model.folder = "./output",
#'                            unique.rowname = "OBJECTID",
#'                            predList = predList,
#'                            predFactor = FALSE,
#'                            response.name = "T_W_Class",
#'                            response.type = "categorical",
#'                            seed = 44,
#'                            response.target = c("Wb", "Wf", "Ws"),
#'                            aoi.col = "BASIN")
#' }
#' @export
wetland_model <- function(qdatafn,
                          model.type,
                          model.folder,
                          unique.rowname,
                          predList,
                          predFactor,
                          response.name,
                          response.type,
                          seed,
                          response.target = NULL,
                          aoi.col = NULL,
                          aoi.target = NULL,
                          MODELfn = NULL,
                          na.action = "na.roughfix",
                          ...
                          ) {

  qdata <- read.csv(qdatafn, stringsAsFactors = FALSE)

  # Subset training data based on response.target, or use the full dataset
  if (!is.null(response.target)) {
    qdata <- qdata[qdata[[response.name]] %in% response.target,]
  }

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
  if (!is.null(aoi.target)) {
    MODELfn <- paste(MODELfn, aoi.target, sep = "-")
  }

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
#' This function runs \code{\link[ModelMap]{model.mapmake}} to generate raster
#' prediction surfaces using model output from \code{\link{wetland_model}}.
#'
#' The name of the model object, output from \code{\link{wetland_model}} is used
#' to determine the output folder where the ERDAS Imagine .img output raster is
#' saved.
#'
#' If an AOI was used to generate the model object(s), the same AOI
#' SpatialPolygon object can be provided as input to this function so that
#' the output raster is produced for the corresponding AOI. If a single model
#' object has been generated for a specific AOI, that model can be applied to a
#' larger extent by not providing an AOI input to this function. If an AOI is
#' provided, the rasters listed in \code{rastLUT} are clipped to the AOI
#' polygon used to generate the model.
#'
#' Temporary rasters and temporary rastLUTs specific to each AOI are saved in
#' the output folder; these temporary files are deleted once the prediction
#' surface has been created.
#'
#' @param model.out list object returned from \code{wetland_model}. List
#'   contains model object(s) from \code{\link[ModelMap]{model.build}} for each
#'   AOI; list elements are named by the output model folder string (including
#'   AOI target name if aoi.target is provided).
#' @param model.folder folder where the output from \code{wetland_model} was
#'   created. Same as \code{model.folder} input to \code{wetland_model}.
#' @param rastLUTfn filename of a .csv or dataframe of a rastLUT.
#' @param aoi optional SpatialPolygon object that was used to intersect with
#'   input points. This parameter is required if \code{model.out} was generated
#'   using an AOI.
#' @param aoi.col optional string to specify the column name in the \code{aoi}
#'   data which identifies seperate AOIs. Required if \code{aoi} is provided.
#' @param na.action optional string to specify action to take if there are NA
#'   values in the prediction data. Defaults to \code{na.omit}.
#' @param ... any other parameters to pass to \code{ModelMap::model.mapmake}.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # RastLUT:
#' rastLUT <- read.csv("data/rastLUT.csv",
#'                     header = FALSE,
#'                     stringsAsFactors = FALSE)
#'
#' # Character vector of the predictor names, used as input to the model:
#' predList <- rastLUT[, 2]
#'
#' # Training data, that has been intersected with an AOI layer:
#' qdatafn <- "data/training_points_attd.csv"
#'
#' # Run the model
#' model.out <- wetland_model(qdatafn = qdatafn,
#'                            model.type = "RF",
#'                            model.folder = "./output",
#'                            unique.rowname = "OBJECTID",
#'                            predList = predList,
#'                            predFactor = FALSE,
#'                            response.name = "T_W_Class",
#'                            response.type = "categorical",
#'                            seed = 44,
#'                            response.target = c("Wb", "Wf", "Ws"),
#'                            aoi.col = "BASIN")
#'
#' # AOI layer:
#' aoi_polys <- raster::shapefile("data/aoi.shp")
#'
#' wetland_map(model.out = model.out,
#'             model.folder = "./output",
#'             rastLUTfn = rastLUT,
#'             aoi = aoi_polys,
#'             aoi.col = "BASIN")
#' }
#' @export
wetland_map <- function (model.out,
                         model.folder,
                         rastLUTfn,
                         aoi = NULL,
                         aoi.col = NULL,
                         na.action = "na.omit",
                         ...) {

  # Read rastLUTfn if not a dataframe
  if (!is.data.frame(rastLUTfn)) {
    rastLUT <- read.csv(rastLUTfn,
                        header = FALSE,
                        stringsAsFactors = FALSE)
  } else {
    rastLUT <- rastLUTfn
  }

  # Read predList from rastLUT
  predList <- rastLUT[, 2]

  # Create a RasterStack from rastLUT rasters if AOI is used. This will be used
  # to clip rasters to AOIs
  if (!is.null(aoi)) {
    rs <- stack_rasters(unique(rastLUT[ ,1]), aligned = TRUE)

    # Remove any layers/bands not in the rastLUT
    rs <- raster::subset(rs, rastLUT[ ,2])
  }

  # Loop through all model objects in model.out
  for (i in 1:length(model.out)) {

    # Model was run with an AOI, but no AOI provided
    if (is.null(aoi) & length(model.out) > 1) {

      stop("Your model output contains more than 1 model object, but no AOI
           object has been provided. Please set the aoi parameter.")

    # AOI provided, but no AOI column specified
    } else if (!is.null(aoi) & is.null(aoi.col)) {

      stop("The aoi.col parameter is required if aoi parameter is not NULL.")

    # AOI has been provided...so clip the rasters
    } else if (!is.null(aoi)) {

      # Extract the AOI target name from the model name
      aoi.target <- tail(strsplit(names(model.out)[i], "-")[[1]], n = 1)

      # Check that the aoi.target exists in aoi object's aoi.col field
      if (!aoi.target %in% as.character(as.data.frame(aoi)[,aoi.col])) {

        stop(paste0("The AOI used for this model run (", aoi.target,
                    ") does not exist in the AOI object provided."))

      } else {
        # Clip rasters (from rastLUT) to AOI and save in temp folder

        # Extract AOI target poly
        aoi.target.shp <- aoi[aoi[[aoi.col]] == aoi.target,]

        # Clip rasters to poly
        rs.crop <- raster::crop(rs, extent(aoi.target.shp))
        rs.mask <- raster::mask(rs.crop, aoi.target.shp)

        # Write clipped rasters to temp files
        model.folder.tmp <- file.path(model.folder, paste0("tmp_", aoi.target))
        dir.create(model.folder.tmp, showWarnings = FALSE)
        rs.files <- paste0(file.path(model.folder.tmp, names(rs.mask)), ".img")
        raster::writeRaster(rs.mask,
                            filename = rs.files,
                            bylayer = TRUE,
                            overwrite = TRUE,
                            format = "HFA")

        # Generate temp rastLUT
        # All temp rasters are single band
        rastLUT <- data.frame(rs.files, predList, 1, stringsAsFactors = FALSE)
      }
    }

    MODELfn <- names(model.out)[i]
    model.folder.out <- file.path(model.folder, MODELfn)

    # Make map raster
    #-------------------

    # model.mapmake() creates an ascii text file and an imagine .img file of
    # predictions for each map pixel.
    ModelMap::model.mapmake(model.obj = model.out[[i]],
                            folder = model.folder.out,
                            MODELfn = MODELfn,
                            rastLUTfn = rastLUT,
                            na.action = na.action,
                            ...)

    # Clean up any temp folders
    unlink(file.path(model.folder, "tmp_*"), recursive = TRUE)

  }

  # TO DO:
  # Setup mapcodes
  # Produce map
  # ...
}
