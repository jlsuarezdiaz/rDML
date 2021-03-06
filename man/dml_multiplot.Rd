% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dml_plot.R
\name{dml_multiplot}
\alias{dml_multiplot}
\title{Multiple DML plotting.}
\usage{
dml_multiplot(X, y, nrow = NULL, ncol = NULL, ks = NULL,
  attrs = NULL, sections = "mean", metrics = NULL,
  transformers = NULL, dmls = NULL, dml_fitted = FALSE,
  transforms = NULL, title = NULL, subtitles = NULL,
  xlabels = NULL, ylabels = NULL, grid_split = c(400, 400),
  grid_step = c(0.1, 0.1), label_legend = TRUE,
  legend_loc = "center right", cmap = NULL, label_colors = NULL,
  plot_points = TRUE, plot_regions = TRUE, region_intensity = 0.4,
  legend_plot_points = TRUE, legend_plot_regions = TRUE,
  legend_on_axis = FALSE, ...)
}
\arguments{
\item{X}{array-like of size (N x d), where N is the number of samples, and d is the number of features.}

\item{y}{array-like of size N, where N is the number of samples.}

\item{nrow}{Number of rows of the figure. If any of nrow or ncol is None, it will be generated automatically. Integer.}

\item{ncol}{Number of columns of the figure. If any of nrow or ncol is None, it will be generated automatically. Integer.}

\item{ks}{The number of neighbors for the k-NN classifier in each plot. List size must be equal to the number of plots.}

\item{attrs}{A list of two items specifying the dataset attributes to show in the scatter plot. The items can be the keys, if X is a data.frame,
or integer indexes with the attribute position. If None, the two first attributes will be taken.}

\item{sections}{It specifies how to take sections in the features space, if there are more than two features in the dataset. It is used to plot the classifier
fixing the non-ploting attributes in this space section. Allowed values are:
- 'mean' : takes the mean of the remaining attributes to plot the classifier region.
- 'zeros' : takes the remaining attributes as zeros to plot the classifier region.}

\item{metrics}{The metric PSD matrix to use in each plot. List size must be equal to the number of plots.
metrics[i] is ignored if transformers[i] or dmls[i] are provided.}

\item{transformers}{A linear transformation to use in each plot. List size must be equal to the number of plots.
transformers[i] will be ignored if dmls[i] is provided.}

\item{dmls}{A distance metric learning algorithm for each plot. List size must be equal to the number of plots.}

\item{dml_fitted}{Specifies if the DML algorithm is already fitted. If True, the algorithm's fit method will not be called. Boolean.}

\item{transforms}{For each plot where the list item is True, it projects the data by the learned transformer and plots the transform data.
Else, the classifier region will be ploted with the original data, but the regions will change according to the learned distance.
List size must be equal to the number of plots.}

\item{title}{An optional title for the plot.}

\item{subtitles}{Optional titles for each subplot. List size must be equal to the number of plots.}

\item{xlabels}{Optional titles for the X axis. List size must be equal to the number of plots.}

\item{ylabels}{Optional titles for the Y axis. List size must be equal to the number of plots.}

\item{grid_split}{A list with two items, specifying the number of partitions, in the X and Y axis, to make in the plot to paint
the classifier region. Each split will define a point where the predict method of the classifier is evaluated.
It can be None. In this case, the `grid_step` parameter will be considered.}

\item{grid_step}{A list with two items, specifying the distance between the points in the grid that defines the classifier plot.
Each created point in this way will define a point where the predict method of the classifier is evaluated.
It is ignored if the parameter `grid_split` is not NULL.}

\item{label_legend}{If True, a legend with the labels and its colors will be ploted.}

\item{legend_loc}{Specifies the legend position. Ignored if legend is not plotted. Allowed values are:
'best' (0), 'upper right' (1), 'upper left' (2), 'lower left' (3), 'lower right' (4),
'right' (5), 'center left' (6), 'center right' (7), 'lower center' (8),
'upper center' (9), 'center' (10).
Alternatively can be a 2-tuple giving x, y of the lower-left corner of the legend in axes coordinates.}

\item{cmap}{A string defining a python's matplotlib colormap.}

\item{label_colors}{A list of size C with matplotlib colors, or strings specitying a color, where C is the number of classes in y. Each class will
be plotted with the corresponding color. If cmap is NULL and label_colors is NULL, a default Colormap is used.}

\item{plot_points}{If True, points will be plotted.}

\item{plot_regions}{If True, the classifier regions will be plotted.}

\item{region_intensity}{A float between 0 and 1, indicating the transparency of the colors in the classifier regions respect the point colors.}

\item{legend_plot_points}{If True, points are plotted in the legend.}

\item{legend_plot_regions}{If True, classifier regions are plotted in the legend.}

\item{legend_on_axis}{If True, the legend is plotted inside the scatter plot. Else, it is plotted out of the scatter plot.}

\item{...}{Additional arguments for `Matplotlib.suplots` python's method.}
}
\value{
A Python's `matplotlib.figure.Figure` object with the plot.
}
\description{
This functions allows multiple 2D-scatter plots for a labeled dataset, to plot regions defined by different classifiers and distances.
The distances can be provided by a metric PSD matrix, a matrix of a linear transformation, or by a distance metric
learning algorithm, that can learn the distance during the plotting, or it can be fitted previously.
}
