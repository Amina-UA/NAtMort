#' Estimate Natural Mortality Using Lorenzen (1996) Method
#'
#' Calculates natural mortality (M) using Lorenzen (1996) method.
#' The mortality is estimated from individual weight (W) using the empirical equation:
#'
#' \deqn{M = 3.0 \left( \frac{W}{W_\infty} \right)^{-0.288}}
#'
#' where \eqn{W} is the body weight and \eqn{W_\infty} is the asymptotic weight.
#'
#' Reference:
#' Lorenzen, K. (1996). The relationship between body weight and natural mortality in juvenile and adult fish:
#' a comparison of natural ecosystems and aquaculture. *Journal of Fish Biology*, 49(4), 627–647.
#'
#' @param LA Numeric vector. Observed or predicted lengths at age.
#' @param Linf Asymptotic length.
#' @param a Weight–length coefficient.
#' @param b Weight–length exponent.
#'
#' @return A data frame with columns: `Age`, `Length`, `Weight`, and `M_lorenzen`.
#' @examples
#' vb <- VB_params(Linf = 80, K = 0.25, t0 = -0.5, tmax = 10, a = 0.01, b = 3, Temp = 14, Lm = 35)
#' mort_lor <- Lorenzen(LA = vb$LA, Linf = vb$Linf, a = vb$a, b = vb$b)
#' print(mort_lor)
#'
#' # Plot
#' library(ggplot2)
#' ggplot(mort_lor, aes(x = Age, y = M_lorenzen)) +
#'   geom_line(color = "darkgreen") +
#'   geom_point(color = "darkgreen") +
#'   labs(title = "Natural Mortality (Lorenzen, 1996)",
#'        x = "Age", y = "Mortality rate (year⁻¹)") +
#'   theme_minimal()
#' @export
Lorenzen <- function(LA, Linf, a, b) {
  # Age vector based on length-at-age vector
  ages <- seq_along(LA) - 1

  # Convert lengths to weights using W = a * L^b
  W <- a * LA^b
  Winf <- a * Linf^b

  # Apply Lorenzen (1996) equation
  M_lorenzen <- 3.0 * (W / Winf)^(-0.288)

  return(data.frame(
    Age = ages,
    Length = LA,
    Weight = W,
    M_lorenzen = M_lorenzen
  ))
}
