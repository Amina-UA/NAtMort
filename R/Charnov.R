#' Estimate Natural Mortality Using Charnov et al. (2012) via FSA
#'
#' Estimates age-specific natural mortality (M) using the Charnov et al. (2012) method
#' implemented in \code{\link[FSA]{metaM}}.
#'
#' \strong{Equation:}
#' \deqn{M = K \cdot \left(\frac{L_\infty}{L}\right)^{1.5}}
#'
#' where:
#' \itemize{
#'   \item \eqn{M} is the natural mortality rate
#'   \item \eqn{L_\infty} is the asymptotic length
#'   \item \eqn{L} is the length-at-age (either observed or predicted)
#'   \item \eqn{K} is the von Bertalanffy growth coefficient
#' }
#'
#' @param VB_params A list from the \code{VB_params()} function, which must include \code{Linf}, \code{K}, \code{t0}, and \code{tmax}.
#' @param LA Optional. A numeric vector of observed lengths-at-age. If provided, Charnov M estimates will be calculated from these.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \code{m.cha.bio}: Charnov M from observed lengths (if provided)
#'   \item \code{m.cha.vb}: Charnov M from von Bertalanffy predicted lengths
#' }
#'
#' @examples
#' vb <- VB_params(Linf = 45, K = 0.2, t0 = -0.5, a = 0.005, b = 3,
#'                 Temp = 13, Lm = 20)
#'
#' # Estimate from VB lengths
#' Charnov(vb)
#'
#' # Estimate with observed lengths
#' Lstk <- c(10, 15, 25, 30, 35)
#' Charnov(vb, LA = Lstk)
#'
#' @references
#' Charnov, E.L., Gislason, H., & Pope, J.G. (2012). Evolutionary assembly rules for fish life histories.
#' \emph{Fish and Fisheries}, 14(2), 213–224.
#'
#' Source code reference: \url{https://github.com/fishR-Core-Team/FSA/blob/main/R/metaM.R}
#'
#' @importFrom FSA metaM
#' @export
Charnov <- function(VB_params, LA = NULL) {
  if (!requireNamespace("FSA", quietly = TRUE)) {
    stop("The 'FSA' package is required. Install it with: install.packages('FSA')")
  }

  required_fields <- c("Linf", "K", "t0", "tmax")
  missing <- setdiff(required_fields, names(VB_params))
  if (length(missing) > 0) {
    stop("Missing required VB_params fields: ", paste(missing, collapse = ", "))
  }

  Linf <- VB_params$Linf
  K <- VB_params$K
  t0 <- VB_params$t0
  tmax <- VB_params$tmax

  # Age vector
  ages <- 0:tmax
  lvec.vb <- Linf * (1 - exp(-K * (ages - t0)))

  # Charnov M from VB-predicted lengths
  m.cha.vb <- sapply(lvec.vb, function(x) {
    FSA::metaM(method = "Charnov", Linf = Linf, K = K, L = x)$M
  })

  result <- data.frame(m.cha.vb = m.cha.vb)

  # If observed lengths provided
  if (!is.null(LA)) {
    m.cha.bio <- sapply(LA, function(x) {
      FSA::metaM(method = "Charnov", Linf = Linf, K = K, L = x)$M
    })
    result <- cbind(m.cha.bio = m.cha.bio, result)
  }

  return(result)
}
