#' createSankeyDiagram
#'
#' Writes the Sankey diagram to a HTML-file, to a specified file path.
#'
#' @template param_treatmentPathways
#' @template param_outputFile
#' @template param_returnHTML
#' @template param_groupCombinations
#' @template param_minCellCount
#'
#' @export
#'
#' @returns invisible(NULL)
#'
#' @examples
#' # treatmentPathways <- read.csv(treatmentPathways.csv)
#' 
#' # Dummy data, typically read from treatmentPathways.csv
#' treatmentPathways <- data.frame(
#'   path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
#'            "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
#'   freq = c(206, 6, 14, 48, 221),
#'   sex = rep("all", 5),
#'   age = rep("all", 5),
#'   index_year = rep("all", 5)
#' )
#' 
#' outputFile <- tempfile(pattern = "mySankeyDiagram", fileext = "html")
#'
#' createSankeyDiagram(
#'   treatmentPathways,
#'   outputFile,
#'   groupCombinations = FALSE,
#'   minCellCount = 5
#' )
createSankeyDiagram <- function(
    treatmentPathways,
    outputFile,
    returnHTML = FALSE,
    groupCombinations = FALSE,
    minCellCount = 5) {
  
  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )
  
  data <- treatmentPathways %>%
    dplyr::rowwise() %>%
    dplyr::mutate(path = stringr::str_split(.data$path, pattern = "-")) %>%
    dplyr::mutate(freq = as.integer(.data$freq))

  data <- data %>%
    tidyr::unnest_wider(path, names_sep = "")

  data <- data %>%
    dplyr::group_by_at(grep("path", names(data))) %>%
    dplyr::summarise(freq = sum(.data$freq), .groups = "drop")

  data[is.na(data)] <- "Stopped"

  result1 <- data %>%
    mutate(
      source = paste0("1.", .data$path1),
      target = paste0("2.", .data$path2)
    ) %>%
    select("source", "target", "freq")

  if (suppressWarnings(!is.null(data$path3))) {
    result2 <- data %>%
      mutate(
        source = paste0("2.", .data$path2),
        target = paste0("3.", .data$path3)
      ) %>%
      select("source", "target", "freq")

    links <- dplyr::bind_rows(
      result1, result2
    )
  } else {
    links <- result1
  }

  links <- links %>%
    dplyr::filter(.data$freq >= minCellCount) %>%
    dplyr::mutate(`%` = round(freq / sum(freq) * 100, 2)) %>%
    dplyr::select(-"freq")
  
  # Draw sankey network
  plot <- googleVis::gvisSankey(
    links,
    from = "source",
    to = "target",
    weight = "%",
    chartid = 1,
    options = list(sankey = "{node: { colors: ['#B5482A'], width: 5}}")
  )

  if (returnHTML) {
    return(plot)
  } else {
    message(sprintf("Writing Sankey diagram to %s", file.path(outputFile)))
    
    writeLines(
      text = plot$html$chart,
      con = file.path(outputFile)
    )
    
    return(invisible(NULL))
  }
}
utils::globalVariables(c(".", "freq", "combination"))


splitPathItems <- function(treatmentPathways) {
  data <- treatmentPathways %>%
    rowwise() %>%
    dplyr::mutate(path = stringr::str_split(.data$path, pattern = "-")) %>%
    dplyr::mutate(freq = as.integer(.data$freq))
  
  data <- data %>%
    tidyr::unnest_wider(path, names_sep = "")
  
  data <- data %>%
    dplyr::group_by_at(grep("path", names(data))) %>%
    dplyr::summarise(freq = sum(.data$freq), .groups = "drop")
  
  data[is.na(data)] <- "Stopped"
  return(data)
}

createLinks <- function(data) {
  result1 <- data %>%
    mutate(
      source = paste0("1.", .data$path1),
      target = paste0("2.", .data$path2)
    ) %>%
    select("source", "target", "freq")
  
  
  if (suppressWarnings(!is.null(data$path3))) {
    result2 <- data %>%
      mutate(
        source = paste0("2.", .data$path2),
        target = paste0("3.", .data$path3)
      ) %>%
      select("source", "target", "freq")
    
    links <- dplyr::bind_rows(
      result1, result2
    )
  } else {
    links <- result1
  }
  
  links <- links %>%
    dplyr::mutate(value = round(freq / sum(freq) * 100, 2)) %>%
    dplyr::select(-"freq")
}

doGroupCombinations <- function(treatmentPathways, groupCombinations) {
  if (groupCombinations) {
    treatmentPathways$path <- treatmentPathways$path %>%
      stringr::str_replace_all(
        pattern = "\\w+\\+\\w+",
        replacement = "Combination"
      )
  }
  return(treatmentPathways)
}

createLinkedData <- function(data) {
  links <- createLinks(data)

  nodes <- data.frame(
    names = c(links$source, links$target) %>% unique()
  )

  links$source <- lapply(links$source, nameToId, names = nodes$names) %>%
    unlist()

  links$target <- lapply(links$target, nameToId, names = nodes$names) %>%
    unlist()

  links <- links %>%
    dplyr::group_by(.data$source, .data$target) %>%
    dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
    as.data.frame()

  return(list(links = links, nodes = nodes))
}

nameToId <- function(item, names) {
  item <- item %>%
    stringr::str_replace(pattern = "\\(", replacement = "\\\\(") %>%
    stringr::str_replace(pattern = "\\)", replacement = "\\\\)") %>%
    stringr::str_replace(pattern = "\\+", replacement = "\\\\+") %>%
    stringr::str_replace(pattern = "\\&", replacement = "\\\\&") %>%
    stringr::str_replace(pattern = "\\.", replacement = "\\\\.")
  return(grep(sprintf("^%s$",item), names) - 1)
}

getColorPalette <- function(treatmentPathways) {
  palette <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  )
  
  n <- treatmentPathways$path |> 
    stringr::str_split(pattern = "-") |>
    unlist() |>
    unique() |>
    length()
  
  return(palette[0:n])
}

#' createSankeyDiagram2
#' 
#' Create sankey diagram, will replace `createSankeyDiagram`.
#'
#' @template param_treatmentPathways
#' @template param_groupCombinations
#' @param colors (`character(n)`) Vector of hex color codes.
#' @param ... Paramaters for \link[networkD3]{sankeyNetwork}.
#'
#' @return (`htmlwidget`)
#' @export
#'
#' @examples
#' # Dummy data, typically read from treatmentPathways.csv
#' treatmentPathways <- data.frame(
#'   path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
#'            "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
#'   freq = c(206, 6, 14, 48, 221),
#'   sex = rep("all", 5),
#'   age = rep("all", 5),
#'   index_year = rep("all", 5)
#' )
#' 
#' createSankeyDiagram2(treatmentPathways)
createSankeyDiagram2 <- function(treatmentPathways, groupCombinations = FALSE, colors = NULL, ...) {
  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )
  
  if (is.null(colors)) {
    colors <- sprintf(
      'd3.scaleOrdinal([%s])',
      paste0('"', getColorPalette(treatmentPathways),'"', collapse = ", ")
    )
  }
  
  data <- splitPathItems(treatmentPathways)
  
  linkedData <- createLinkedData(data)
  
  networkD3::sankeyNetwork(
    Links = linkedData$links,
    Nodes = linkedData$nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "names",
    units = "%",
    colourScale = colors,
    ...
  )
}
