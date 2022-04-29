#' Returns side-by-side boxplots for the two array inputs
#'
#' Takes in one object, the same one returned from the constructor function
#' Creates a new dataframe of the arrays in the object
#' Graphs side-by-side boxplots for both of the arrays
#'
#' @param obj object containing the arrays which will be plotted
#'
#' @return
#' @export
#'
#' @examples
#' #plot_Rttest <- function(obj = object)
plot_Rttest <- function(obj) {

  # initialize two arrays of the input data
  x <- obj$`Dataframe X and Y`[1]
  y <- obj$`Dataframe X and Y`[2]

  # name the arrays which will be plotted
  Arrays <- list("X", "Y")

  # create a dataframe and melt the values into categorical separations
  max_length <- max(c(length(x), length(y)))
  df <- data.frame(X = c(x, rep(NA, max_length - length(x))),
                   Y = c(y, rep(NA, max_length - length(y))))
  plot_data <- reshape2::melt(df, 0)

  # creates side-by-side boxplots colored by category, adds labels and title
  boxplot1 <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = factor(plot_data[,'variable']), y = plot_data[,'value'], fill = factor(plot_data[,'variable']))) +
    ggplot2::geom_boxplot() + ggplot2::labs(title = "Array Boxplot Comparison", x = "Array", y = "Values")
  boxplot1
}
