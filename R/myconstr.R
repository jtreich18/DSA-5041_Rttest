#' Returns the results from a t-test
#'
#' Takes in three parameters, two arrays of numbers, and the confidence interval
#' Uses both arrays to perform a t-test, saving the dataframe from the two arrays, level of confidence, confidence interval, and p-value
#'
#' @param x first inserted array
#' @param y second inserted array
#' @param alpha confidence level
#'
#' @return
#' @export
#'
#' @examples
#' # myconstr(x = array1, y = array2, alpha = 0.05)
myconstr = function(x, y, alpha = 0.05) {

  # create a df to hold x and y when different lengths
  max_length <- max(c(length(x), length(y)))
  df.data <- data.frame(X = c(x, rep(NA, max_length - length(x))),
                        Y = c(y, rep(NA, max_length - length(y))))

  # perform t.test for equal variances
  t <- stats::t.test(x, y, mu = 0, var.equal = TRUE)

  # Store df, alpha, confidence interval and the p value in list obj
  obj = list(df.data, alpha = alpha, t$conf.int[1], t$conf.int[2], t$p.value)
  names(obj) <- c("Dataframe X and Y", "Alpha Value", "CI Lower", "CI Upper",
                  "P Value")

  # Change object type to Rttest
  class(obj) <- "Rttest"

  # Return the object from the function environment
  obj

}
