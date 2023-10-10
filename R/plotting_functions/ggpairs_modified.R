ggpairs_modified <- function (data, mapping = NULL, columns = 1:ncol(data), title = NULL, 
          upper = list(continuous = "cor", combo = "box_no_facet", 
                       discrete = "count", na = "na"), lower = list(continuous = "points", 
                                                                    combo = "facethist", discrete = "facetbar", na = "na"), 
          diag = list(continuous = "densityDiag", discrete = "barDiag", 
                      na = "naDiag"), params = NULL, ..., xlab = NULL, ylab = NULL, 
          axisLabels = c("show", "internal", "none"), columnLabels = colnames(data[columns]), 
          labeller = "label_value", switch = NULL, showStrips = NULL, 
          legend = NULL, cardinality_threshold = 15, progress = NULL, 
          proportions = NULL, legends = stop("deprecated")) 
{
  GGally:::warn_deprecated(!missing(legends), "legends")
  GGally:::warn_if_args_exist(list(...))
  GGally:::stop_if_params_exist(params)
  isSharedData <- inherits(data, "SharedData")
  data_ <- GGally:::fix_data(data)
  data <- GGally:::fix_data_slim(data_, isSharedData)
  if (!missing(mapping) & !is.list(mapping) & missing(columns)) {
    columns <- mapping
    mapping <- NULL
  }
  GGally:::stop_if_bad_mapping(mapping)
  columns <- GGally:::fix_column_values(data, columns, columnLabels, 
                               "columns", "columnLabels")
  GGally:::stop_if_high_cardinality(data, columns, cardinality_threshold)
  upper <- GGally:::check_and_set_ggpairs_defaults("upper", upper, continuous = "cor", 
                                          combo = "box_no_facet", discrete = "count", na = "na")
  lower <- GGally:::check_and_set_ggpairs_defaults("lower", lower, continuous = "points", 
                                          combo = "facethist", discrete = "facetbar", na = "na")
  diag <- GGally:::check_and_set_ggpairs_defaults("diag", diag, continuous = "densityDiag", 
                                         discrete = "barDiag", na = "naDiag", isDiag = TRUE)
  axisLabels <- GGally:::fix_axis_label_choice(axisLabels, c("show", 
                                                    "internal", "none"))
  proportions <- GGally:::ggmatrix_proportions(proportions, data, columns)
  dataTypes <- GGally:::plot_types(data, columns, columns, allowDiag = TRUE)
  if (identical(axisLabels, "internal")) {
    dataTypes$plotType[dataTypes$posX == dataTypes$posY] <- "label"
  }
  ggpairsPlots <- lapply(seq_len(nrow(dataTypes)), function(i) {
    plotType <- dataTypes[i, "plotType"]
    posX <- dataTypes[i, "posX"]
    posY <- dataTypes[i, "posY"]
    xColName <- dataTypes[i, "xVar"]
    yColName <- dataTypes[i, "yVar"]
    if (posX > posY) {
      types <- upper
    }
    else if (posX < posY) {
      types <- lower
    }
    else {
      types <- diag
    }

    sectionAes <- GGally:::add_and_overwrite_aes(GGally:::add_and_overwrite_aes(aes_(x = as.name(xColName), 
                                                                   y = as.name(yColName)), mapping), types$mapping)
    args <- list(types = types, sectionAes = sectionAes)
    if (plotType == "label") {
      args$label <- columnLabels[posX]
    }
    plot_fn <- GGally:::ggmatrix_plot_list(plotType)
    p <- do.call(plot_fn, args)
    return(p)
  })

  ggpairsPlots[[c(12)]]$fn <- function (data = NULL, mapping = NULL, size = 10, color = "grey20",...)
  {
    a <- data.frame(x = 1, y = 1, label = "NA")
    p <- ggplot(data = a, aes_string(x = "x", y = "y", label = "label")) +
      geom_text(color = color, size = size, ...) + theme(axis.line = element_blank(),
                                                         axis.text.x = element_blank(), axis.text.y = element_blank(),
                                                         axis.ticks = element_blank(), axis.title.x = element_blank(),
                                                         axis.title.y = element_blank(), legend.background = element_blank(),
                                                         legend.key = element_blank(), legend.text = element_blank(),
                                                         legend.title = element_blank(), panel.background = element_blank(),
                                                         panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), plot.background = element_blank(),
                                                         plot.title = element_blank(), strip.background = element_blank(),
                                                         strip.text.x = element_blank(), strip.text.y = element_blank())
    p
  }

  ggpairsPlots[[c(12)]]$dataPos <- 1

  ggpairsPlots[[c(12)]]$gg <- NULL



  ggpairsPlots[[c(30)]]$fn <- function (data = NULL, mapping = NULL, size = 10, color = "grey20",...)
  {
    a <- data.frame(x = 1, y = 1, label = "NA")
    p <- ggplot(data = a, aes_string(x = "x", y = "y", label = "label")) +
      geom_text(color = color, size = size, ...) + theme(axis.line = element_blank(),
                                                         axis.text.x = element_blank(), axis.text.y = element_blank(),
                                                         axis.ticks = element_blank(), axis.title.x = element_blank(),
                                                         axis.title.y = element_blank(), legend.background = element_blank(),
                                                         legend.key = element_blank(), legend.text = element_blank(),
                                                         legend.title = element_blank(), panel.background = element_blank(),
                                                         panel.border = element_blank(), panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(), plot.background = element_blank(),
                                                         plot.title = element_blank(), strip.background = element_blank(),
                                                         strip.text.x = element_blank(), strip.text.y = element_blank())
    p
  }

ggpairsPlots[[30]]$dataPos <- 1

ggpairsPlots[[30]]$gg <- NULL

  plotMatrix <- ggmatrix(plots = ggpairsPlots, byrow = TRUE, 
                         nrow = length(columns), ncol = length(columns), xAxisLabels = (if (axisLabels == 
                                                                                              "internal") 
                           NULL
                           else columnLabels), yAxisLabels = (if (axisLabels == 
                                                                  "internal") 
                             NULL
                             else columnLabels), labeller = labeller, switch = switch, 
                         showStrips = showStrips, showXAxisPlotLabels = identical(axisLabels, 
                                                                                  "show"), showYAxisPlotLabels = identical(axisLabels, 
                                                                                                                           "show"), title = title, xlab = xlab, ylab = ylab, 
                         data = data_, gg = NULL, progress = progress, legend = legend, 
                         xProportions = proportions, yProportions = proportions)
  plotMatrix
}
