#' Checks for a run if the variables sum up as expected and logs spotted gaps
#'
#' @md
#' @author Falk Benke, Oliver Richters
#' @param mifFile path to the mif file to apply summation checks to, or quitte object
#' @param dataDumpFile file where data.frame with the data analysis is saved. If NULL, result is returned
#' @param outputDirectory path to directory to place logFile and dataDumpFile
#' @param logFile file where human-readable summary is saved. If NULL, write to stdout
#' @param logAppend boolean whether to append or overwrite logFile
#' @param summationsFile in inst/summations folder that describes the required summation groups
#' @param template mapping template to be loaded
#' @param remindVar REMIND/MAgPIE variable column name in template
#' @importFrom dplyr group_by summarise ungroup left_join mutate arrange %>% filter select desc
#' @importFrom magclass unitsplit
#' @importFrom rlang sym syms
#' @importFrom utils write.table
#' @importFrom stringr str_pad
#' @importFrom quitte as.quitte
#'
#' @export
checkSummations <- function(mifFile, outputDirectory = ".", template = "AR6", summationsFile = "AR6",
                            logFile = NULL, logAppend = FALSE, dataDumpFile = "checkSummations.csv",
                            remindVar = "piam_variable") {
  if (!is.null(outputDirectory) && !dir.exists(outputDirectory) && ! is.null(c(logFile, dataDumpFile))) {
    dir.create(outputDirectory, recursive = TRUE)
  }

  .calculateCheckSum <- function(name, x) {
    tmp <- x %>%
      group_by(!!!syms(c("model", "scenario", "region", "period"))) %>%
      summarise(checkSum = sum(!!sym("value") * !!sym("factor")), .groups = "drop") %>%
      ungroup()
    tmp$variable <- name
    return(tmp)
  }

  summationGroups <- getSummations(summationsFile)
  if (summationsFile %in% names(summationsNames())) {
    summationsFile <- gsub(".*piamInterfaces", "piamInterfaces", summationsNames(summationsFile))
  }

  data <- quitte::as.quitte(mifFile) %>%
    filter(!!sym("variable") %in% unique(c(summationGroups$child, summationGroups$parent))) %>%
    left_join(summationGroups, by = c("variable" = "child"))

  checkVariables <- list()

  for (i in unique(summationGroups$parent)) {
    checkVariables[[i]] <- summationGroups[which(summationGroups[, "parent"] == i), "child"]
  }

  names(checkVariables) <- gsub(" [1-9]$", "", names(checkVariables))

  tmp <- NULL
  for (i in names(checkVariables)) {
    tmp <- rbind(tmp, .calculateCheckSum(
      i,
      filter(data, !!sym("parent") == i, !!sym("variable") %in% checkVariables[[i]])
    ))
  }

  tmp <- left_join(tmp, data, c("scenario", "region", "variable", "period", "model")) %>%
    mutate(
      diff = !!sym("checkSum") - !!sym("value"),
      reldiff = 100 * (!!sym("checkSum") - !!sym("value")) / !!sym("value")
    ) %>%
    select(-c("factor", "parent"))

  text <- paste0("\n### Analyzing ", if (is.null(ncol(mifFile))) mifFile else "provided data",
                 ".\n# Use ", summationsFile, " to check if summation groups add up.")

  # write data to dataDumpFile
  if (length(dataDumpFile) > 0) {
    dataDumpFile <- file.path(outputDirectory, dataDumpFile)
    write.table(
      arrange(tmp, desc(abs(!!sym("reldiff")))), sep = ";",
      file = dataDumpFile, quote = FALSE, row.names = FALSE)
  }
  # generate human-readable summary of larger differences
  fileLarge <- filter(tmp, abs(!!sym("reldiff")) >= 1, abs(!!sym("diff")) >= 0.001)
  problematic <- unique(c(fileLarge$variable))
  if (length(problematic) > 0) {
    templateData <- getTemplate(template)
    if (template %in% names(templateNames())) {
       template <- gsub(".*piamInterfaces", "piamInterfaces", templateNames(template))
    }
    text <- c(text, paste0("# Derive mapping from ", template))
    width <- 70
    text <- c(text, paste0("\n", str_pad(paste0("variable groups found in ",
                           basename(summationsFile)), width + 8, "right"),
            "corresponding REMIND/MAgPIE variables extracted from ", basename(template)))
    for (p in problematic) {
      signofdiff <- paste0(if (max(fileLarge$diff[fileLarge$variable == p]) > 0) "<",
                           if (min(fileLarge$diff[fileLarge$variable == p]) < 0) ">")
      childs <- summationGroups$child[summationGroups$parent == p]
      text <- c(text, paste0("\n", str_pad(paste(p, signofdiff), width + 5, "right"), "   ",
              paste0(unitsplit(templateData[, remindVar][unitsplit(templateData$Variable)$variable == p])$variable,
                     collapse = " + "), " ", signofdiff))
      for (ch in childs) {
        text <- c(text, paste0("   + ", str_pad(ch, width, "right"), "      + ",
                paste0(unitsplit(templateData[, remindVar][unitsplit(templateData$Variable)$variable == ch])$variable,
                       collapse = " + ")))
      }
      text <- c(text, paste0("Relative difference between ",
                        round(min(-fileLarge$reldiff[fileLarge$variable == p]), digits = 1), "% and ",
                        round(max(-fileLarge$reldiff[fileLarge$variable == p]), digits = 1), "%, ",
                        "absolute difference up to ",
                        round(max(abs(fileLarge$diff[fileLarge$variable == p])), digits = 2), " ",
                        paste0(unique(fileLarge$unit[fileLarge$variable == p]), collapse = ", "), ".")
      )
    }
    # print to log or stdout
    summarytext <- c("\n# Summary of summation group checks:",
      paste0("# ", length(problematic), " equations are not satisfied but should according to ",
            basename(summationsFile), "."),
      paste0("# All deviations can be found in the returned object",
             if (! is.null(dataDumpFile)) paste0(" and in ", dataDumpFile), ".")
    )
  } else {
    summarytext <- "\n# All summation checks were fine."
  }
  if (is.null(logFile)) {
    message(paste(c(text, summarytext, ""), collapse = "\n"))
  } else {
    if (! is.null(outputDirectory)) logFile <- file.path(outputDirectory, logFile)
    message(paste(c(text[1], summarytext,
            paste0("# Find log with human-readable information appended to ", logFile)), "", collapse = "\n"))
    write(c(text, summarytext, ""), file = logFile, append = logAppend)
  }
  return(invisible(tmp))
}
