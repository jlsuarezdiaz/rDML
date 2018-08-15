# PLOT FUNCTIONS WRAPPER

source("R/filter.R")
source("R/utils.R")


# classifier_plot <- function(X, y, clf, attrs = NULL, sections = "mean", fitted = FALSE, title = NULL, subtitle = NULL, xrange = NULL, yrange = NULL, xlabel = NULL,
#                            ylabel = NULL, grid_split = c(400,400), grid_step = c(0.1,0.1), label_legend = TRUE, legend_loc = "lower right", cmap = NULL,
#                            label_colors = NULL, plot_points = TRUE, plot_regions = TRUE, region_intensity = 0.4, legend_plot_points = TRUE, legend_plot_regions = TRUE,
#                            legend_on_axis = TRUE, ...){
#  return(pre_plotters_$classifier_plot(X = X, y = y, attrs = numeric.as.integer(attrs), sections = sections, fitted = fitted, title = title, subtitle = subtitle,
#                                       xrange = as.numeric(xrange), yrange = as.numeric(yrange), xlabel = xlabel, ylabel = ylabel, grid_split = as.integer(grid_split),
#                                       grid_step = as.numeric(grid_step), label_legend = label_legend, legend_loc = legend_loc, cmap = cmap,
#                                       label_colors = label_colors, plot_points = plot_points, plot_regions = plot_regions, region_intensity = region_intensity,
#                                       legend_plot_points = legend_plot_points, legend_plot_regions = legend_plot_regions, legend_on_axis = legend_on_axis,
#                                       ... = ...))
#}

#' DML plots with k-NN.
#'
#' A 2D-scatter plot for a labeled dataset to plot regions defined by a k-NN classifier and a distance. The distance can be provided by a metric PSD matrix, a matrix of a linear transformation, or by a distance metric
#' learning algorithm, that can learn the distance during the plotting, or it can be fitted previously.
#'
#' @param X array-like of size (N x d), where N is the number of samples, and d is the number of features.
#' @param y array-like of size N, where N is the number of samples.
#' @param k  The number of neighbors for the k-NN classifier. Integer.
#' @param attrs A list of two items specifying the dataset attributes to show in the scatter plot. The items can be the keys, if X is a data.frame,
#'        or integer indexes with the attribute position. If None, the two first attributes will be taken.
#' @param sections It specifies how to take sections in the features space, if there are more than two features in the dataset. It is used to plot the classifier
#'       fixing the non-ploting attributes in this space section. Allowed values are:
#'       - 'mean' : takes the mean of the remaining attributes to plot the classifier region.
#'       - 'zeros' : takes the remaining attributes as zeros to plot the classifier region.
#' @param metric A positive semidefinite matrix of size (d x d), where d is the number of features. Ignored if dml or transformer is specified.
#' @param transformer A matrix of size (d' x d), where d is the number of features and d' is the desired dimension. Ignored if dml is specified.
#' @param dml A distance metric learning algorithm constructed from a function in `dml`. If metric, transformer and dml are None, no distances are used in the plot.
#' @param dml_fitted  Specifies if the DML algorithm is already fitted. If True, the algorithm's fit method will not be called. Boolean.
#' @param transform If True, projects the data by the learned transformer and plots the transform data. Else, the classifier region will
#'                  be ploted with the original data, but the regions will change according to the learned distance.
#' @param title An optional title for the plot.
#' @param subtitle An optional subtitle for the plot.
#' @param xrange A list with two items, specifying the minimum and maximum range to plot in the X axis.
#'        If None, it will be calculated according to the maximum and minimum of the X feature.
#' @param yrange A list with two items, specifying the minimum and maximum range to plot in the Y axis.
#'        If None, it will be calculated according to the maximum and minimum of the Y feature.
#' @param xlabel An optional title for the X axis.
#' @param ylabel An optional title for the Y axis.
#' @param grid_split A list with two items, specifying the number of partitions, in the X and Y axis, to make in the plot to paint
#'        the classifier region. Each split will define a point where the predict method of the classifier is evaluated.
#'        It can be None. In this case, the `grid_step` parameter will be considered.
#' @param grid_step A list with two items, specifying the distance between the points in the grid that defines the classifier plot.
#'        Each created point in this way will define a point where the predict method of the classifier is evaluated.
#'        It is ignored if the parameter `grid_split` is not NULL.
#' @param label_legend If True, a legend with the labels and its colors will be ploted.
#' @param legend_loc Specifies the legend position. Ignored if legend is not plotted. Allowed values are:
#'        'best' (0), 'upper right' (1), 'upper left' (2), 'lower left' (3), 'lower right' (4),
#'        'right' (5), 'center left' (6), 'center right' (7), 'lower center' (8),
#'        'upper center' (9), 'center' (10).
#'        Alternatively can be a 2-tuple giving x, y of the lower-left corner of the legend in axes coordinates.
#' @param cmap A string defining a python's matplotlib colormap.
#' @param label_colors A list of size C with matplotlib colors, or strings specitying a color, where C is the number of classes in y. Each class will
#'        be plotted with the corresponding color. If cmap is NULL and label_colors is NULL, a default Colormap is used.
#' @param plot_points If True, points will be plotted.
#' @param plot_regions If True, the classifier regions will be plotted.
#' @param region_intensity A float between 0 and 1, indicating the transparency of the colors in the classifier regions respect the point colors.
#' @param legend_plot_points If True, points are plotted in the legend.
#' @param legend_plot_regions If True, classifier regions are plotted in the legend.
#' @param legend_on_axis If True, the legend is plotted inside the scatter plot. Else, it is plotted out of the scatter plot.
#' @param ... Additional arguments for `Matplotlib.suplots` python's method.
#' @return A Python's `matplotlib.figure.Figure` object with the plot.
#' @export
knn_plot <- function(X, y, k=1, attrs=NULL, sections = "mean", metric = NULL, transformer = NULL, dml = NULL, dml_fitted = FALSE, transform = TRUE,
                     title = NULL, subtitle = NULL, xrange = NULL, yrange = NULL, xlabel = NULL, ylabel = NULL, grid_split = c(400,400), grid_step = c(0.1,0.1),
                     label_legend = TRUE, legend_loc = "lower right", cmap = NULL, label_colors = NULL, plot_points = TRUE, plot_regions = TRUE,
                     region_intensity = 0.4, legend_plot_points = TRUE, legend_plot_regions = TRUE, legend_on_axis = TRUE, ...){
  if(!is.null(dml)){
    dml = dml$py
  }

  return(pre_plotters_$knn_plot(X = X, y = y, k = as.integer(k), attrs = numeric.as.integer(attrs), sections = sections, metric = metric,
                                transformer = transformer, dml = dml, dml_fitted = dml_fitted, transform = transform, title = title, subtitle = subtitle,
                                xrange = xrange, yrange = yrange, xlabel = xlabel, ylabel = ylabel, grid_split = as.integer(grid_split),
                                grid_step = as.numeric(grid_step), label_legend = label_legend, legend_loc = legend_loc, cmap = cmap, label_colors = label_colors,
                                plot_points = plot_points, plot_regions = plot_regions, region_intensity = as.numeric(region_intensity),
                                legend_plot_points = legend_plot_points, legend_plot_regions = legend_plot_regions, legend_on_axis = legend_on_axis, ... = ...))
}

#' Multiple DML plotting.
#'
#' This functions allows multiple 2D-scatter plots for a labeled dataset, to plot regions defined by different classifiers and distances.
#' The distances can be provided by a metric PSD matrix, a matrix of a linear transformation, or by a distance metric
#' learning algorithm, that can learn the distance during the plotting, or it can be fitted previously.
#'
#' @param X array-like of size (N x d), where N is the number of samples, and d is the number of features.
#' @param y array-like of size N, where N is the number of samples.
#' @param nrow Number of rows of the figure. If any of nrow or ncol is None, it will be generated automatically. Integer.
#' @param ncol Number of columns of the figure. If any of nrow or ncol is None, it will be generated automatically. Integer.
#' @param ks  The number of neighbors for the k-NN classifier in each plot. List size must be equal to the number of plots.
#' @param attrs A list of two items specifying the dataset attributes to show in the scatter plot. The items can be the keys, if X is a data.frame,
#'        or integer indexes with the attribute position. If None, the two first attributes will be taken.
#' @param sections It specifies how to take sections in the features space, if there are more than two features in the dataset. It is used to plot the classifier
#'       fixing the non-ploting attributes in this space section. Allowed values are:
#'       - 'mean' : takes the mean of the remaining attributes to plot the classifier region.
#'       - 'zeros' : takes the remaining attributes as zeros to plot the classifier region.
#' @param metrics The metric PSD matrix to use in each plot. List size must be equal to the number of plots.
#'                metrics[i] is ignored if transformers[i] or dmls[i] are provided.
#' @param transformers A linear transformation to use in each plot. List size must be equal to the number of plots.
#'        transformers[i] will be ignored if dmls[i] is provided.
#' @param dmls A distance metric learning algorithm for each plot. List size must be equal to the number of plots.
#' @param dml_fitted  Specifies if the DML algorithm is already fitted. If True, the algorithm's fit method will not be called. Boolean.
#' @param transforms For each plot where the list item is True, it projects the data by the learned transformer and plots the transform data.
#'        Else, the classifier region will be ploted with the original data, but the regions will change according to the learned distance.
#'        List size must be equal to the number of plots.
#' @param title An optional title for the plot.
#' @param subtitles Optional titles for each subplot. List size must be equal to the number of plots.
#' @param xlabels Optional titles for the X axis. List size must be equal to the number of plots.
#' @param ylabels Optional titles for the Y axis. List size must be equal to the number of plots.
#' @param grid_split A list with two items, specifying the number of partitions, in the X and Y axis, to make in the plot to paint
#'        the classifier region. Each split will define a point where the predict method of the classifier is evaluated.
#'        It can be None. In this case, the `grid_step` parameter will be considered.
#' @param grid_step A list with two items, specifying the distance between the points in the grid that defines the classifier plot.
#'        Each created point in this way will define a point where the predict method of the classifier is evaluated.
#'        It is ignored if the parameter `grid_split` is not NULL.
#' @param label_legend If True, a legend with the labels and its colors will be ploted.
#' @param legend_loc Specifies the legend position. Ignored if legend is not plotted. Allowed values are:
#'        'best' (0), 'upper right' (1), 'upper left' (2), 'lower left' (3), 'lower right' (4),
#'        'right' (5), 'center left' (6), 'center right' (7), 'lower center' (8),
#'        'upper center' (9), 'center' (10).
#'        Alternatively can be a 2-tuple giving x, y of the lower-left corner of the legend in axes coordinates.
#' @param cmap A string defining a python's matplotlib colormap.
#' @param label_colors A list of size C with matplotlib colors, or strings specitying a color, where C is the number of classes in y. Each class will
#'        be plotted with the corresponding color. If cmap is NULL and label_colors is NULL, a default Colormap is used.
#' @param plot_points If True, points will be plotted.
#' @param plot_regions If True, the classifier regions will be plotted.
#' @param region_intensity A float between 0 and 1, indicating the transparency of the colors in the classifier regions respect the point colors.
#' @param legend_plot_points If True, points are plotted in the legend.
#' @param legend_plot_regions If True, classifier regions are plotted in the legend.
#' @param legend_on_axis If True, the legend is plotted inside the scatter plot. Else, it is plotted out of the scatter plot.
#' @param ... Additional arguments for `Matplotlib.suplots` python's method.
#' @return A Python's `matplotlib.figure.Figure` object with the plot.
#' @export
dml_multiplot <- function(X, y, nrow = NULL, ncol = NULL, ks = NULL, attrs = NULL, sections = "mean", metrics = NULL, transformers = NULL,
                          dmls = NULL, dml_fitted = FALSE, transforms = NULL, title = NULL, subtitles = NULL, xlabels = NULL, ylabels = NULL,
                          grid_split = c(400,400), grid_step = c(0.1,0.1), label_legend = TRUE, legend_loc = "center right", cmap = NULL, label_colors = NULL,
                          plot_points = TRUE, plot_regions = TRUE, region_intensity = 0.4, legend_plot_points = TRUE, legend_plot_regions = TRUE,
                          legend_on_axis = FALSE, ...){
  dmls = sapply(dmls, function(x){ if(!is.null(dml)){return(x$py)} else{ return(NULL)}})
  return(pre_plotters_$dml_multiplot(X = X, y = y, nrow = as.integer_or_null(nrow), ncol = as.integer_or_null(ncol), ks = as.integer_or_null(ks), attrs = attrs,
                                     sections = sections, metrics = metrics, transformers = transformers, dmls = dmls, dml_fitted = dml_fitted,
                                     transforms = transforms, title = title, subtitles = subtitles, xlabels = xlabels, ylabels = ylabels,
                                     grid_split = as.integer(grid_split), grid_step = as.numeric(grid_step), label_legend = label_legend, legend_loc = "center right",
                                     cmap = cmap, label_colors = label_colors, plot_points = plot_points, plot_regions = plot_regions,
                                     region_intensity = as.numeric(region_intensity), legend_plot_points = legend_plot_points, legend_plot_regions = legend_plot_regions,
                                     legend_on_axis = legend_on_axis, ... = ...))
}

#' DML pairplots with k-NN.
#'
#' This function allows multiple 2D-scatter plots for different pairs of attributes of the same dataset, and
#' to plot regions defined by a k-NN classifier and a distance.
#' The distance can be provided by a metric PSD matrix, a matrix of a linear transformation, or by a distance metric
#' learning algorithm, that can learn the distance during the plotting, or it can be fitted previously.
#'
#' @param X array-like of size (N x d), where N is the number of samples, and d is the number of features.
#' @param y array-like of size N, where N is the number of samples.
#' @param k  The number of neighbors for the k-NN classifier. Integer.
#' @param attrs A list specifying the dataset attributes to show in the scatter plot. The items can be the keys, if X is a pandas dataset,
#'        or integer indexes with the attribute position. If None, and xattrs and yattrs are NULL, all the attributes will be taken.
#' @param xattrs A list specifying the dataset attributes to show in X axis. The items can be the keys, if X is a pandas dataset,
#'        or integer indexes with the attribute position. Ignored if attrs is specified.
#' @param yattrs A list specifying the dataset attributes to show in Y axis. The items can be the keys, if X is a pandas dataset,
#'        or integer indexes with the attribute position. Ignored if attrs is specified.
#' @param diag What to plot on the diagonal subplots. Allowed options are:
#'        - "hist" : An histogram of the data will be plot for the attribute.
#' @param sections It specifies how to take sections in the features space, if there are more than two features in the dataset. It is used to plot the classifier
#'       fixing the non-ploting attributes in this space section. Allowed values are:
#'       - 'mean' : takes the mean of the remaining attributes to plot the classifier region.
#'       - 'zeros' : takes the remaining attributes as zeros to plot the classifier region.
#' @param metric A positive semidefinite matrix of size (d x d), where d is the number of features. Ignored if dml or transformer is specified.
#' @param transformer A matrix of size (d' x d), where d is the number of features and d' is the desired dimension. Ignored if dml is specified.
#' @param dml A distance metric learning algorithm constructed from a function in `dml`. If metric, transformer and dml are None, no distances are used in the plot.
#' @param dml_fitted  Specifies if the DML algorithm is already fitted. If True, the algorithm's fit method will not be called. Boolean.
#' @param transform If True, projects the data by the learned transformer and plots the transform data. Else, the classifier region will
#'                  be ploted with the original data, but the regions will change according to the learned distance.
#' @param title An optional title for the plot.
#' @param xrange A list with two items, specifying the minimum and maximum range to plot in the X axis.
#'        If None, it will be calculated according to the maximum and minimum of the X feature.
#' @param yrange A list with two items, specifying the minimum and maximum range to plot in the Y axis.
#'        If None, it will be calculated according to the maximum and minimum of the Y feature.
#' @param xlabel An optional title for the X axis.
#' @param ylabel An optional title for the Y axis.
#' @param grid_split A list with two items, specifying the number of partitions, in the X and Y axis, to make in the plot to paint
#'        the classifier region. Each split will define a point where the predict method of the classifier is evaluated.
#'        It can be None. In this case, the `grid_step` parameter will be considered.
#' @param grid_step A list with two items, specifying the distance between the points in the grid that defines the classifier plot.
#'        Each created point in this way will define a point where the predict method of the classifier is evaluated.
#'        It is ignored if the parameter `grid_split` is not NULL.
#' @param label_legend If True, a legend with the labels and its colors will be ploted.
#' @param legend_loc Specifies the legend position. Ignored if legend is not plotted. Allowed values are:
#'        'best' (0), 'upper right' (1), 'upper left' (2), 'lower left' (3), 'lower right' (4),
#'        'right' (5), 'center left' (6), 'center right' (7), 'lower center' (8),
#'        'upper center' (9), 'center' (10).
#'        Alternatively can be a 2-tuple giving x, y of the lower-left corner of the legend in axes coordinates.
#' @param cmap A string defining a python's matplotlib colormap.
#' @param label_colors A list of size C with matplotlib colors, or strings specitying a color, where C is the number of classes in y. Each class will
#'        be plotted with the corresponding color. If cmap is NULL and label_colors is NULL, a default Colormap is used.
#' @param plot_points If True, points will be plotted.
#' @param plot_regions If True, the classifier regions will be plotted.
#' @param region_intensity A float between 0 and 1, indicating the transparency of the colors in the classifier regions respect the point colors.
#' @param legend_plot_points If True, points are plotted in the legend.
#' @param legend_plot_regions If True, classifier regions are plotted in the legend.
#' @param legend_on_axis If True, the legend is plotted inside the scatter plot. Else, it is plotted out of the scatter plot.
#' @param ... Additional arguments for `Matplotlib.suplots` python's method.
#' @return A Python's `matplotlib.figure.Figure` object with the plot.
#' @export
knn_pairplots <- function(X, y, k=1, attrs=NULL, xattrs=NULL, yattrs=NULL, diag="hist", sections="mean", metric=NULL, transformer=NULL, dml=NULL,
                          dml_fitted=FALSE, title=NULL, grid_split=c(400,400), grid_step=c(0.1,0.1), label_legend=TRUE, legend_loc="center right",
                          cmap=NULL, label_colors=NULL, plot_points=TRUE, plot_regions=TRUE, region_intensity=0.4, legend_plot_points=TRUE,
                          legend_plot_regions=TRUE, legend_on_axis=FALSE, ...){
  if(!is.null(dml)){
    dml=dml$py
  }
  return(pre_plotters_$knn_pairplots(X = X, y = y, k = as.integer(k), attrs = attrs, xattrs = xattrs, yattrs = yattrs, diag = diag, sections = sections,
                                     metric = metric, transformer = transformer, dml = dml, dml_fitted = dml_fitted, title = title,
                                     grid_split = as.integer(grid_split), grid_step = as.numeric(grid_step), label_legend = label_legend,
                                     legend_loc = legend_loc, cmap = cmap, label_colors = label_colors, plot_points = plot_points,
                                     plot_regions = plot_regions, region_intensity = as.numeric(region_intensity), legend_plot_points = legend_plot_points,
                                     legend_plot_regions = legend_plot_regions, legend_on_axis = legend_on_axis,...=...))
}

#' The tune algorithms global list.
#'
#' This list contains all the tune functions.
dml_plotter = list()
for(key in names(pre_plotters_)){
  console.log(paste("Adding R-Plotter ",toString(key),"..."))
  dml_plotter[[key]] = get(key)
}
