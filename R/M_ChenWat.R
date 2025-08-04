#' Calculate and Plot Chen & Watanabe 1989 Model Mortality Estimates
#'
#' This function implements the Chen & Watanabe 1989 method for estimating natural mortality.
#' It automatically generates an age sequence based on the Von Bertalanffy parameters provided,
#' and can optionally return a plot of the mortality estimates.
#'
#' @param VB_params A list containing Von Bertalanffy parameters:
#'        - Linf: Asymptotic length.
#'        - K: Growth coefficient.
#'        - t0: Theoretical age at length zero.
#'        - tmax: Maximum age.
#' @param age_seq Optional; a numeric vector of ages at which to estimate mortality.
#'        Defaults to a sequence from 0 to tmax.
#' @param plot Logical; indicates whether to return a ggplot object of the mortality estimates.
#'        Defaults to TRUE.
#' @return Depending on the plot parameter, either a data frame or a ggplot object.
#' @importFrom ggplot2 ggplot geom_line geom_point labs theme_classic
#' @examples
#' library(ggplot2)
#'
#' # Define parameters using VB_params
#' VB_params <- VB_params(45, 0.2, -0.5, 7, 0.005, 2.98, 13, 4)
#'
#' # Estimate Natural mortality using Chen & Watanabe 1989 Model and plot the results
#' results_ChenWat <- ChenWat(VB_params, seq(0, VB_params$tmax, by = 1), TRUE)
#' results_ChenWat
#'
#' # To get the data frame without plotting for further analysis:
#' table_ChenWat <- ChenWat(VB_params, seq(0, VB_params$tmax, by = 1), FALSE)
#' # Display the results as a data frame
#' print(table_ChenWat)
#' # Plot the results manually
#' ggplot(table_ChenWat, aes(x = age, y = M_age)) +
#'   geom_line() + geom_point() +
#'   labs(title = "Chen & Watanabe 1989 Natural Mortality Estimates",
#'        x = "Age", y = bquote("Natural Mortality Rate"~(year^-1))) +
#'   theme_classic()
#'
#' @export
ChenWat <- function(VB_params, age_seq = NULL, plot = TRUE) {
  if (!all(c("K", "t0", "tmax") %in% names(VB_params))) {
    stop("The VB_params must contain K, t0, and tmax.")
  }
  K <- VB_params$K
  t0 <- VB_params$t0
  tmax <- VB_params$tmax

  # Automatically generate age_seq if not provided
  if (is.null(age_seq)) {
    age_seq <- seq(0, tmax, by = 1)
  }

  TM <- (-1 / K) * log(1 - exp(K * t0)) + t0
  a0 <- 1 - exp(-K * (TM - t0))
  a1 <- K * exp(-K * (TM - t0))
  a2 <- -(0.5) * K^2 * exp(-K * (TM - t0))
  M_age <- ifelse(age_seq <= TM,
                  K / (1 - exp(-K * (age_seq - t0))),
                  ifelse(age_seq >= TM,
                         K / (a0 + a1 * (age_seq - TM) + a2 * (age_seq - TM)^2),
                         0))
  M_ChenWat <- data.frame(age = age_seq, M_age = M_age)

  if (plot) {
    return(ggplot(M_ChenWat, aes(x = age, y = M_age)) +
             geom_line() + geom_point() +
             labs(title = "Chen & Watanabe 1989 Mortality Estimates",
                  x = "Age", y = bquote("Natural Mortality Rate"~(year^-1))) +
             theme_classic())
  } else {
    return(M_ChenWat)
  }
}
