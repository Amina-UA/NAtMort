#' Estimate Scalar Natural Mortality (M) Using Empirical Methods
#'
#' Uses the \code{metaM()} function from the \pkg{FSA} package to estimate scalar natural mortality (M)
#' based on life-history parameters. Multiple empirical methods are included based on literature.
#'
#' @param VB_params A named list of von Bertalanffy and life-history parameters from \code{\link{VB_params}}.
#' @param method Character string. One of \code{"all"}, \code{"tmax"}, \code{"K"}, \code{"Hoenig"}, \code{"Pauly"}, \code{"FAMS"}.
#'
#' @return A data frame with methods and corresponding M values.
#'
#' @details
#' The following categories of empirical methods are supported:
#' \itemize{
#'   \item \strong{"all"} — All available methods in FSA.
#'   \item \strong{"tmax"} — Based on maximum age (e.g., Then et al. 2015).
#'   \item \strong{"K"} — Based on von Bertalanffy growth coefficient (e.g., Jensen 1996).
#'   \item \strong{"Hoenig"} — Methods from Hoenig (1983), Hewitt & Hoenig (2005).
#'   \item \strong{"Pauly"} — Pauly (1980) temperature-based methods.
#'   \item \strong{"FAMS"} — Family of additional methods including Gislason, Lorenzen, Charnov, etc.
#' }
#'
#' For method descriptions and equations, see the FSA GitHub: \url{https://github.com/fishR-Core-Team/FSA/blob/main/R/metaM.R}
#'
#' @references
#' Hoenig, J.M. (1983). Empirical use of longevity data to estimate mortality rates. Fishery Bulletin.
#' Pauly, D. (1980). On the interrelationships between natural mortality, growth parameters, and temperature in 175 fish stocks.
#' Jensen, A.L. (1996). Beverton and Holt life history invariants result from optimal trade-off of reproduction and survival.
#' Then, A.Y., Hoenig, J.M., Hall, N.G., Hewitt, D.A. (2015). Evaluating natural mortality estimators.
#' García-Rodríguez et al. (2021). Scientific Advisory Committee on Fisheries (SAC), WGSAD. GFCM.
#'
#' @importFrom FSA metaM
#' @export
#'
#' @examples
#' # Example with Mullus barbatus (GSA 06) from García-Rodríguez et al. (2021)
#' params <- VB_params(Linf = 34.5, K = 0.34, t0 = -0.143, tmax = 5,
#'                     a = 0.0056, b = 3.2488, Temp = 13, Lm = 13.7)
#' scalar_M(params, method = "Pauly")

scalar_M <- function(VB_params, method = c("all", "tmax", "K", "Hoenig", "Pauly", "FAMS")) {
  method <- match.arg(method)

  if (!requireNamespace("FSA", quietly = TRUE)) {
    stop("Package 'FSA' is required. Please install it with install.packages('FSA')")
  }

  # Required inputs
  required <- c("Linf", "K", "t0", "tmax", "Temp", "t50")
  if (!all(required %in% names(VB_params))) {
    stop("VB_params must include: Linf, K, t0, tmax, Temp, t50")
  }

  # Method categories
  method_sets <- list(
    all = c("PaulyLNoT", "HewittHoenig", "JensenK1", "PaulyL", "HoenigO", "HoenigOF", "tmax", "JensenK2"),
    tmax = c("tmax"),
    K = c("JensenK1", "JensenK2"),
    Hoenig = c("HewittHoenig", "HoenigO", "HoenigOF"),
    Pauly = c("PaulyLNoT", "PaulyL"),
    FAMS = c("Gislason", "Charnov", "ZhangMegreyD", "ZhangMegreyP", "ChenWatanabe", "PetersonWroblewski")
  )

  selected_methods <- method_sets[[method]]

  # Call metaM
  M_estimates <- data.frame(FSA::metaM(
    method = selected_methods,
    Linf = VB_params$Linf,
    K = VB_params$K,
    t0 = VB_params$t0,
    tmax = VB_params$tmax,
    Temp = VB_params$Temp,
    t50 = VB_params$t50,
    Winf = VB_params$a * VB_params$Linf^VB_params$b,
    L = VB_params$LA
  ))

  M_estimates$Method <- rownames(M_estimates)
  rownames(M_estimates) <- NULL

  return(M_estimates)
}
