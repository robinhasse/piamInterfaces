#' generateIASASubmission
#'
#' Generates an IIASA submission from REMIND or MAgPIE runs by applying a project-specific mapping.
#' The script starts from 'mifs' which can be a directory with mif files, a vector of files or a
#' quitte object. In outputDirectory/outputFilename, you will get the data in a joint xlsx or mif file.
#'
#' To provide the mapping, two options exist:
#' - If you want to generate the mapping from one or more mappings from the inst/mappings folder,
#'   set mapping = c("AR6", "AR6_NGFS") or so.
#' - Alternatively, you can provide a path or a vector of paths to mapping files. If you provide your own mapping
#'   files, make sure they follow the standard format (see `getTemplate` for more information)
#' - It is also possible, to mix both options, e.g. c("AR6", "/path/to/mapping_file.csv")
#'
#' In any case, multiple mapping files will be concatenated.
#'
#' iiasatemplate is a xlsx or yaml file provided by IIASA with the variable + unit definitions that are
#' accepted in the database. The function 'priceIndicesIIASA' will be called to calculate price indices
#' that are missing or with the wrong base year. 'checkIIASASubmission' will be called to remove all variables
#' that are not accepted in the database.
#'
#' For all elements of the parameter mapping that contain a summation file in inst/summations,
#' the function 'checkSummations' is called to verify variable summation checks.
#'
#' To alter the data, you can use those parameters: model, addToScen, removeFromScen and timesteps.
#'
#' For a broader overview of the submission process, consult
#' https://github.com/remindmodel/remind/blob/develop/tutorials/13_Submit_to_IIASA_database.md
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param mifs path to mif files or directories with mif files of a REMIND run,
#'             or quitte object
#' @param model name of model as registered with IIASA
#' @param mapping mapping names such as c("AR6", "AR6_NGFS") or a vector of mapping file names.
#'        If NULL, the user is asked. Multiple mappings are concatenated.
#' @param removeFromScen regular expression to be removed from scenario name (optional). Example: '_d50|d95'
#' @param addToScen string to be added as prefix to scenario name (optional)
#' @param outputDirectory path to directory for the generated submission (default: output).
#'        If NULL, no files are written and `logFile` and `outputFilename` have no effect.
#' @param logFile path to the logfile with warnings as passed to generateMappingfile, checkIIASASubmission
#'        (default: outputDirectory/submission_log.txt). Set to FALSE for none.
#'        If `outputDirectory` is set to NULL, this parameter has no effect.
#' @param outputFilename filename of the generated submission. Must be mif or xlsx file.
#'        If NULL, submission data is returned.
#'        If `outputDirectory` is set to NULL, this parameter has no effect.
#' @param iiasatemplate optional filename of xlsx or yaml file provided by IIASA
#'        used to delete superfluous variables and adapt units
#' @param generatePlots boolean, whether to generate plots of failing summation checks
#' @param timesteps timesteps that are accepted in final submission
#' @param checkSummation either TRUE to identify summation files from mapping, or filename, or FALSE
#' @param mappingFile has no effect and is only kept for backwards-compatibility
#' @param naAction a function which indicates what should happen when the data contain NA values.
#' @importFrom quitte as.quitte write.IAMCxlsx write.mif
#' @importFrom dplyr filter mutate distinct inner_join bind_rows tibble
#' @importFrom stringr str_trim
#' @examples
#' \dontrun{
#' # Simple use. Generates submission file in output folder:
#' generateIIASASubmission(
#'   mifs = "/path/to/REMIMD/mifs",
#'   model = "REMIND-MAgPIE 2.1-4.2",
#'   mapping = "NAVIGATE"
#' )
#' }
#' @export
generateIIASASubmission <- function(mifs = ".", # nolint cyclocomp_linter
                                    mapping = NULL,
                                    model = NULL,
                                    removeFromScen = NULL,
                                    addToScen = NULL,
                                    outputDirectory = "output",
                                    outputFilename = "submission.xlsx",
                                    logFile = if (is.null(outputFilename)) NULL else
                                      paste0(gsub("\\.[a-zA-Z]+$", "_log.txt", outputFilename)),
                                    iiasatemplate = NULL,
                                    generatePlots = FALSE,
                                    timesteps = c(seq(2005, 2060, 5), seq(2070, 2100, 10)),
                                    checkSummation = TRUE,
                                    mappingFile = NULL,
                                    naAction = "na.omit") {

  # process input parameters ----

  if (! is.null(mappingFile)) {
    warning("mappingFile is deprecated and ignored. If you got here via output.R -> export -> xlsx_IIASA,
            please pick a newer xlsx_IIASA.R file from remindmodel/develop")
  }

  if (isTRUE(timesteps == "all")) timesteps <- seq(1, 3000)

  if (is.null(outputDirectory)) {
    outputFilename <- NULL
    logFile <- NULL
  }

  logFile <- setLogFile(outputDirectory, logFile)

  # renaming to a more accurate name while maintaining backwards-compatibility
  message("# Generate mapping from ", paste(mapping, collapse = ", "))

  mapData <- NULL

  for (i in seq_along(mapping)) {
    t <- getMapping(mapping[i]) %>%
      filter(.data$piam_variable != "", !is.na(.data$piam_variable), .data$piam_variable != "TODO") %>%
      mutate(
        "piam_variable" = removePlus(.data$piam_variable),
        "piam_factor" = ifelse(is.na(.data$piam_factor), 1, as.numeric(.data$piam_factor))
      ) %>%
      dplyr::bind_rows(tibble("weight" = "NULL")) %>% # add the optional weight column if not present
      mutate(
        "piam_weight" = .data$weight
      ) %>%
      select("variable", "unit", "piam_variable", "piam_unit", "piam_factor", "piam_weight")
    checkUnitFactor(t, logFile = logFile, failOnUnitMismatch = FALSE)
    mapData <- rbind(mapData, t)
  }

  # read in data from mifs ----

  # for each directory, include all mif files
  mifdata <- readMifs(mifs)

  dupl <- mifdata %>% select(-"value") %>% filter(duplicated(mifdata)) %>% droplevels()
  if (nrow(dupl) > 0) {
    stop("Duplicated data found: ",
         "\n  - Models: ", paste(levels(dupl$model), collapse = ", "),
         "\n  - Scenarios: ", paste(levels(dupl$scenario), collapse = ", ")
        )
  }

  if (any(grepl("^Price\\|.*\\|Moving Avg$", levels(mifdata$variable))) &&
      ! any(grepl("^Price\\|.*\\|Rawdata$", levels(mifdata$variable)))) {
   warning("Your data contains no Price|*|Rawdata variables. If it is based on a remind2 version",
           " before 1.111.0 on 2023-05-26, please use piamInterfaces version 0.9.0 or earlier, see PR #128.")
  }

  mifdata <- renameOldVariables(mifdata, mapData$piam_variable, logFile = logFile)
  mifdata <- checkFixUnits(mifdata, mapData, logFile = logFile, failOnUnitMismatch = FALSE)
  mifdata <- .setModelAndScenario(mifdata, model, removeFromScen, addToScen)

  # apply mapping to data ----

  message("# Apply generated mapping to data")

  mifdata <- mifdata %>%
    filter(.data$period %in% timesteps) %>%
    mutate(
      "piam_variable" = removePlus(str_trim(.data$variable)),
      "piam_unit" = str_trim(.data$unit)
      ) %>%
    select(-c("variable", "unit")) %>%
    distinct()

  submission <- mifdata %>%
    inner_join(mapData, by = "piam_variable", relationship = "many-to-many")

  # check for unit mismatches in data and mapping
  unitMismatches <- submission %>%
    select("variable" = "piam_variable", "mifs" = "piam_unit.x", "mappings" = "piam_unit.y") %>%
    filter(.data$mifs != .data$mappings) %>%
    distinct()

  if (nrow(unitMismatches) > 0) {
    warning("Unit mismatches between data and mapping found for some variables: \n",
            paste0(utils::capture.output(unitMismatches), collapse = "\n"))
  }
  submission <- submission %>%
    .resolveWeights(weightSource = mifdata) %>%
    mutate("value" = .data$piam_factor * .data$value) %>%
    select("model", "scenario", "region", "period", "variable", "unit", "value") %>%
    quitteSort()

  submission <- aggregate(
    value ~ model + region + scenario + period + variable + unit,
    data = submission,
    FUN = "sum",
    na.action = naAction
  )

  # apply corrections using IIASA template ----

  if (!is.null(iiasatemplate) && file.exists(iiasatemplate)) {
    submission <- priceIndicesIIASA(submission, iiasatemplate, scenBase = NULL)
    submission <- checkIIASASubmission(submission, iiasatemplate, logFile, failOnUnitMismatch = FALSE)
  } else if (! is.null(iiasatemplate)) {
    message("# iiasatemplate ", iiasatemplate, " not found, returning full list of variables.")
  }

  # perform summation checks ----
  prefix <- gsub("\\.[A-Za-z]+$", "", if (is.null(outputFilename)) "output" else basename(outputFilename))

  if (isTRUE(checkSummation)) checkSummation <- intersect(mapping, names(summationsNames()))
  sumFiles <- setdiff(checkSummation, FALSE)
  if (length(sumFiles) > 0) {
    message("# Apply summation checks")
    for (sumFile in setdiff(sumFiles, FALSE)) {
      invisible(checkSummations(submission, template = mapData,
                              summationsFile = sumFile, logFile = logFile, logAppend = TRUE,
                              outputDirectory = outputDirectory, generatePlots = generatePlots,
                              dataDumpFile = paste0(prefix, "_checkSummations.csv"),
                              plotprefix = paste0(prefix, "_")))
    }
  }

  # write or return data ----

  if (is.null(outputFilename)) {
    return(submission)
  } else {
    if (grepl("\\.xlsx?$", outputFilename)) {
      quitte::write.IAMCxlsx(submission, file.path(outputDirectory, outputFilename))
    } else {
      submission <- submission %>% mutate(value = ifelse(is.na(.data$value), "", .data$value))
      quitte::write.mif(submission, file.path(outputDirectory, outputFilename))
    }
    message("\n\n# Output file written: ", file.path(outputDirectory, outputFilename))
  }
}

.setModelAndScenario <- function(dt, modelname, scenRemove = NULL, scenAdd = NULL) {
    scenarioNames <- unique(dt$scenario)
    if (! is.null(modelname)) {
      dt$model <- modelname
      message("# Correct model name to '", modelname, "'.")
    }
    if (! is.null(scenRemove) && ! scenRemove %in% "") {
      dt$scenario <- gsub(scenRemove, "", dt$scenario)
      message("# Adapt scenario names: '", scenRemove, "' will be removed.")
    }
    if (! is.null(scenAdd)) {
      if (all(grepl(scenAdd, unique(dt$scenario), fixed = TRUE))) {
        message("Prefix ", scenAdd, " already found in all scenario names. Skipping.")
      } else {
        dt$scenario <- paste0(scenAdd, dt$scenario)
        message("# Adapt scenario names: '", scenAdd, "' will be prepended.")
      }
    }
    if (length(unique(dt$scenario)) < length(scenarioNames)) {
      message(length(scenarioNames), " scenario names before changes: ", paste(scenarioNames, collapse = ", "))
      message(length(unique(dt$scenario)), " scenario names after changes:  ",
              paste(unique(dt$scenario), collapse = ", "))
      stop("Changes to scenario names lead to duplicates. Adapt scenRemove='",
           scenRemove, "' and scenAdd='", scenAdd, "'!")
    }

    dt$scenario <- as.factor(dt$scenario)
    return(dt)
}

# resolve the weight column if present else return
.resolveWeights <- function(dataframe, weightSource) {
  if (all(is.null(dataframe$piam_weight)) || all(is.na(dataframe$piam_weight))) {
    message("No weights to resolve. Skipping.")
    return(dataframe)
  } else {
    message("Resolving weights for weighted average variables.")
  }
  normalizedData <- dataframe %>%
    mutate(
      "piam_weight" = removePlus(.data$piam_weight)
    )
  normalizedWeights <- weightSource %>%
    mutate(
      "piam_variable" = removePlus(.data$piam_variable),
    )
  # ensure the source data variables are normalised in the same way
  normalizedData %>%
  # split on the rows whose weight column needs resolving from a point variable
  filter(.data$piam_weight %in% unique(normalizedWeights$piam_variable)) %>%
  # join on the weight column and replace pointer with value
  left_join(
    normalizedWeights,
    by = c("model", "scenario", "region", "period", "piam_weight" = "piam_variable"),
    relationship = "many-to-one") %>%
  select(
    "model",
    "scenario",
    "region",
    "variable",
    "unit",
    "period",
    "value" = "value.x",
    "piam_factor",
    "piam_weight" = "value.y") %>%
    group_by(.data$model, .data$scenario, .data$region, .data$period, .data$variable, .data$unit) %>%
    filter(! is.na(.data$value)) %>%
    filter(! is.na(.data$piam_weight)) %>%
    mutate(
      "piam_weight" = .data$piam_weight / sum(.data$piam_weight)
    ) %>%
    ungroup() %>%
    # apply the weights
    mutate(
      "value" = .data$value * .data$piam_weight) %>%
    select(-c("piam_weight")) %>%
  # recombine with the non-weighted columns
  rbind(
    normalizedData %>%
      filter(! (.data$piam_weight %in%  unique(normalizedWeights$piam_variable))) %>%
      select(
        "model",
        "scenario",
        "region",
        "variable",
        "unit",
        "period",
        "value",
        "piam_factor"
      )
  )
}
