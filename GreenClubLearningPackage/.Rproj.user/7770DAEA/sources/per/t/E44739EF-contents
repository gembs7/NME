#' Chi Square Test of Independence
#'
#' Given an observed rectangular matrix, retreive the test statistic
#' and p-value for the chi-squared test of independence
#'
#' @param observed_matrix A rectangular matrix of observed values
#' @return a list containing 3 values
#' \itemize{
#'     \item \code{expected_matrix} A matrix with equivalent dimensions
#'     of the observed containing the expected counts
#'     \item \code{ts} The chi square test statistic
#'     \item \code{p} The chi square test p-value
#' }
#' @examples
#' # testing 2X2 matrix
#' matrix1 <- matrix(c(10, 20, 15, 25), nrow = 2, byrow = TRUE)
#' result <- ChisqInd(matrix1)
#' print( result$expected_matrix )
#' print( result$ts )
#' print( result$p )
#' @export
ChisqInd <- function(observed_matrix) {
  # expected frequencies
  row_totals <- rowSums(observed_matrix)
  col_totals <- colSums(observed_matrix)
  total_observed <- sum(observed_matrix)

  expected_matrix <- outer(row_totals, col_totals) / total_observed

  # test statistic
  chi_square_statistic <- sum((observed_matrix - expected_matrix)^2 / expected_matrix)

  # df
  df <- (nrow(observed_matrix) - 1) * (ncol(observed_matrix) - 1)

  # p-value
  p_value <- 1 - stats::pchisq(chi_square_statistic, df)

  return (list("expected_matrix" = expected_matrix, "ts" = chi_square_statistic, "p" = p_value))

}
