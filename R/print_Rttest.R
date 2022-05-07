#' Prints results from previous t-test
#'
#' Takes in an object containing a dataframe with two arrays, the confidence level, confidence intervals, and p value
#'
#' @param obj object containing results from previous t-test
#'
#' @return
#' @export
#'
#' @examples
#' # print_Rttest(obj)
print_Rttest <- function(obj) {

  names(obj) <- c("Dataframe X and Y", "Alpha Value", "CI Lower", "CI Upper",
                  "P Value", "Paired")

  sprintf("A 95% confidence interval for %f - %f", obj$`CI Lower`, obj$`CI Upper`)
  print(list( obj$`Alpha Value`, obj$`CI Lower`, obj$`CI Upper`, obj$`P Value`))
  obj1 <- obj$`Dataframe X and Y`

  print(knitr::kable(obj1, full_width = F, format = "markdown", row.names = TRUE))

}
