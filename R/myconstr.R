#' Returns the results from a t-test
#'
#' Takes in three parameters, two arrays of numbers, and the confidence interval
#' Uses both arrays to perform a t-test, saving the dataframe from the two arrays, level of confidence, confidence interval, and p-value
#'
#' @param x first inserted array
#' @param y second inserted array
#' @param alpha confidence level
#' @param paired boolean to determine if t-test is paired
#'
#' @return
#' @export
#'
#' @examples
#' # myconstr(x = array1, y = array2, alpha = 0.05, paired = FALSE)
myconstr = function(x, y, alpha = 0.05, paired = paired) {
  # create a df to hold x and y when different lengths
  max_length <- max(c(length(x), length(y)))
  df.data <- data.frame(X = c(x, rep(NA, max_length - length(x))),
                        Y = c(y, rep(NA, max_length - length(y))))

  # determine if the t.test has equal variances
  v <- var.test(x,y)
  v2 <- v$p.value
  # use t.test w var.equal set to true when p-val < alpha
  if (v2 < alpha) {
    t <- t.test(x, y, var.equal = TRUE, paired = paired)
  }
  # use t.test w var.equal set to false when p-val > alpha
  else {
    t <- t.test(x, y, var.equal = FALSE, paired = paired)
  }

  # Store df, alpha, confidence interval and the p value in list obj
  obj = list(df.data, alpha = alpha, t$conf.int[1], t$conf.int[2], t$p.value, paired)
  names(obj) <- c("Dataframe X and Y", "Alpha Value", "CI Lower", "CI Upper",
                  "P Value", "Paired")

  # Change object type to Rttest
  class(obj) <- "Rttest"

  # Return the object from the function environment
  obj

  # Call the print_Rttest function
  Rttest.jtr::print_Rttest(obj)
  # Call the plot_Rttest function
  Rttest.jtr::plot_Rttest(obj)

}
