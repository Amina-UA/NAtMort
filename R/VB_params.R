#' Create a List of Von Bertalanffy Growth Parameters
#'
#' This function creates a named list of biological and growth parameters used for stock assessment,
#' including von Bertalanffy growth model parameters, weight–length relationship, ambient temperature,
#' and life-history-based maturity information.
#'
#' @param Linf Asymptotic length (in cm).
#' @param K Growth coefficient (per year).
#' @param t0 Theoretical age at length zero (in years).
#' @param a Coefficient 'a' of the length–weight relationship.
#' @param b Exponent 'b' of the length–weight relationship.
#' @param Temp Ambient water temperature (°C).
#' @param Lm Length at 50\% maturity (in cm).
#' @param t50 Optional. Age at 50\% maturity (in years). If not provided, it is estimated internally using the inverse von Bertalanffy growth function.
#' @param tmax Optional. Maximum age (in years). If not provided, it is estimated from Linf using the equation from Pauly (1980).
#'
#' @details
#' If \code{tmax} is not provided, it is estimated using the equation by Pauly (1980):
#' \deqn{t_{max} = t_0 + \frac{3}{K}}
#'
#' If \code{t50} is not provided, it is estimated using the inverse of the von Bertalanffy equation as proposed by Leal et al. (2011):
#' \deqn{t_{50} = t_0 - \frac{1}{K} \ln(1 - \frac{L_m}{L_{\infty}})}
#'
#' The function also returns a vector of predicted lengths-at-age (from 0 to tmax).
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{Linf}, \code{K}, \code{t0}, \code{a}, \code{b}, \code{Temp}, \code{Lm}, \code{t50}, \code{tmax}
#'   \item \code{LA}: A vector of predicted lengths-at-age from 0 to tmax
#' }
#'
#' @references
#' Pauly, D. (1980). On the interrelationships between natural mortality, growth parameters, and mean environmental temperature in 175 fish stocks. ICES J. Mar. Sci., 39(2): 175–192.
#'
#' Leal, M. C., et al. (2011). Fish reproductive biology: implications for assessment and management. *Journal of Applied Ichthyology*, 27(2), 245–254.
#'
#' García-Rodríguez, E., Vivas, M., Esteban, A., Pérez-Gil, J. L., & Ruíz García, C. (2021).
#' Scientific Advisory Committee on Fisheries (SAC), Working Group on Stock Assessment of Demersal Species (WGSAD). *General Fisheries Commission for the Mediterranean (GFCM)*.
#'
#' @examples
#' # Example using Mullus barbatus (GSA 06) from García-Rodríguez et al. (2021)
#' vb <- VB_params(
#' Linf = 34.5,
#' K = 0.34,
#' t0 = -0.143,
#' a = 0.0056,
#' b = 3.2488,
#' Temp = 13,
#' Lm = 13.7
#' )
#'
#' # Print estimated t50 and tmax
#' vb$t50
#' vb$tmax
#'
#' # Plot length-at-age
#' plot(0:vb$tmax, vb$LA, type = "o", col = "blue",
#'      xlab = "Age (years)", ylab = "Length (cm)",
#'      main = "Length-at-Age (Mullus barbatus)")
#'
#' @export
VB_params <- function(Linf, K, t0, a, b, Temp, Lm, t50 = NULL, tmax = NULL) {
  # Estimate t50 from Lm if not provided (Leal et al. 2011)
  if (is.null(t50)) {
    t50 <- t0 - (1 / K) * log(1 - Lm / Linf)
  }

  # Estimate tmax from Linf if not provided (Pauly 1980)
  if (is.null(tmax)) {
    tmax <- t0 + 3 / K
  }

  # Generate predicted lengths-at-age (LA)
  ages <- 0:tmax
  LA <- Linf * (1 - exp(-K * (ages - t0)))

  return(list(
    Linf = Linf,
    K = K,
    t0 = t0,
    a = a,
    b = b,
    Temp = Temp,
    Lm = Lm,
    t50 = t50,
    tmax = tmax,
    LA = LA
  ))
}
