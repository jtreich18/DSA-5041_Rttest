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

  message("A 95% confidence interval for ", obj$`CI Lower`, " - ", obj$`CI Upper`)
  if (obj$`P Value` < obj$`Alpha Value`) {
    print("We will reject the null hypothesis and conclude these population means are different with 95% confidence")
  }
  else{
    print("We will fail to reject the null hypothesis and conclude these population means are similar with 95% confidence")
  }
  print(list( obj$`Alpha Value`, obj$`CI Lower`, obj$`CI Upper`, obj$`P Value`))
  obj1 <- obj$`Dataframe X and Y`

  print(knitr::kable(obj1, full_width = F, format = "markdown", row.names = TRUE))

}
