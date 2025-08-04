#' Calculate Natural Mortality Using Gislason Method (2008)
#'
#' Estimates natural mortality (M) using the Gislason et al. (2008) method, based on both observed and von Bertalanffy predicted lengths.
#'
#' \strong{Equation:}
#' \deqn{M = c \cdot L^{-d} \cdot k}
#' where \eqn{L} is length-at-age, \eqn{k} is the growth coefficient,
#' and \eqn{c, d} are empirical constants used internally by \code{FSA::metaM()}.
#'
#' @param VB_params A list from the \code{VB_params()} function, which must include \code{LA}, \code{Linf}, \code{K}, and \code{tmax}.
#' @param LA Optional. A numeric vector of observed lengths-at-age. If \code{NULL}, only VB-predicted lengths from \code{VB_params$LA} are used.
#'
#' @return A data frame with columns: \code{ages}, \code{m.gis.bio} (if \code{LA} is provided), and \code{m.gis.vb} (from VB prediction).
#'
#' @examples
#' # Generate VB parameters
#' params <- VB_params(45, 0.2, -0.5, 10, 0.005, 3, 15, Lm = 20)
#'
#' # Gislason with only VB-predicted lengths
#' G1 <- Gislason(params)
#' print(G1)
#'
#' # Gislason with observed length-at-age (e.g., field data)
#' obs_LA <- c(5, 12, 20, 26, 30, 35, 38, 41, 43, 44, 44.5)
#' G2 <- Gislason(params, LA = obs_LA)
#' print(G2)
#'
#' # Plot result
#' library(ggplot2)
#' ggplot(G1, aes(x = ages, y = m.gis.vb)) +
#'   geom_line(color = "steelblue") +
#'   geom_point(color = "steelblue") +
#'   labs(title = "Gislason Natural Mortality Estimate (VB-based)",
#'        x = "Age", y = "Mortality Rate (year^-1)") +
#'   theme_minimal()
#'
#' @importFrom FSA metaM
#' @export
Gislason <- function(VB_params, LA = NULL) {
  ages <- 0:VB_params$tmax
  Linf <- VB_params$Linf
  k <- VB_params$K
  t0 <- VB_params$t0

  if (!requireNamespace("FSA", quietly = TRUE)) {
    stop("Package 'FSA' is required but not installed. Please install it with install.packages('FSA')")
  }

  lvec.vb <- VB_params$LA

  m.gis.vb <- sapply(lvec.vb, function(x) {
    FSA::metaM(method = 'Gislason', Linf = Linf, k = k, L = x)$M
  })

  result <- data.frame(ages = ages, m.gis.vb = m.gis.vb)

  if (!is.null(LA)) {
    if (length(LA) != length(ages)) {
      stop("Length of LA must match number of age classes (0 to tmax).")
    }

    m.gis.bio <- sapply(LA, function(x) {
      FSA::metaM(method = 'Gislason', Linf = Linf, k = k, L = x)$M
    })

    result$m.gis.bio <- m.gis.bio
  }

  return(result)
}
