#' Plot Von Bertalanffy Growth Curve
#'
#' This function plots the Von Bertalanffy growth curve using estimated parameters and includes optional annotations
#' for \code{Linf}, \code{t50} (age at 50% maturity), and \code{Lm} (length at 50% maturity).
#'
#' @param VB_params A list of parameters from \code{\link{VB_params}()}, including growth, maturity, and predicted lengths.
#' @param Linf_line Optional. A numeric value to add a horizontal line at \code{Linf} (dashed line).
#'
#' @return A \code{ggplot} object representing the Von Bertalanffy growth curve with optional annotations.
#'
#' @importFrom ggplot2 ggplot geom_line geom_hline geom_vline annotate labs theme_classic aes xlim
#' @export
#'
#' @examples
#' # Example using Mullus barbatus parameters (GSA 06)
#' # Reference: García-Rodríguez et al. (2021), SAC, WGSAD
#' vb <- VB_params(
#'   Linf = 34.5,    # cm
#'   K = 0.34,       # 1/year
#'   t0 = -0.143,    # year
#'   a = 0.0056,     # weight-length coefficient
#'   b = 3.2488,     # weight-length exponent
#'   Temp = 13,      # °C
#'   Lm = 13.7       # cm
#' )
#'
#' plotVB(vb, Linf_line = vb$Linf)
plotVB <- function(VB_params, Linf_line = NULL) {
  ages <- seq(0, VB_params$tmax + 15, by = 0.1)
  lengths <- VB_params$Linf * (1 - exp(-VB_params$K * (ages - VB_params$t0)))
  growth_data <- data.frame(Age = ages, Length = lengths)

  eq_text <- bquote(L(t) == .(VB_params$Linf) * (1 - e^{- .(VB_params$K) * (t - .(VB_params$t0))}))

  p <- ggplot(growth_data, aes(x = Age, y = Length)) +
    geom_line() +
    labs(title = "Von Bertalanffy Growth Model",
         x = "Age (years)", y = "Length (cm)") +
    theme_classic() +
    xlim(0, VB_params$tmax + 10) +
    annotate("text", x = VB_params$tmax * 0.95, y = VB_params$Linf * 1.05,
             label = as.expression(eq_text), parse = TRUE)

  if (!is.null(Linf_line)) {
    p <- p + geom_hline(yintercept = Linf_line, linetype = "dashed", color = "black")
  }

  if (!is.null(VB_params$t50)) {
    p <- p + geom_vline(xintercept = VB_params$t50, linetype = "dotted", color = "red") +
      annotate("text", x = VB_params$t50, y = 0, label = "t50", vjust = -1, color = "red")
  }

  if (!is.null(VB_params$Lm)) {
    p <- p + geom_hline(yintercept = VB_params$Lm, linetype = "dotted", color = "purple") +
      annotate("text", x = 0, y = VB_params$Lm, label = "Lm", hjust = -0.1, color = "purple")
  }

  return(p)
}

#' Plot Von Bertalanffy Growth Curve with Noise
#'
#' Adds simulated noise to the Von Bertalanffy growth curve to reflect natural variability.
#' A smoothed curve with a confidence interval is also displayed.
#'
#' @param VB_params A list of parameters from \code{\link{VB_params}()}, including growth, maturity, and predicted lengths.
#' @param Linf_line Optional numeric value to add a horizontal line at \code{Linf} (dashed line).
#' @param noise_sd Standard deviation of noise to add to length values. Default is 0.5.
#'
#' @return A \code{ggplot} object representing the noisy Von Bertalanffy growth curve with optional annotations.
#'
#' @importFrom ggplot2 ggplot geom_line geom_point geom_smooth geom_hline geom_vline annotate labs theme_classic aes xlim
#' @importFrom stats rnorm
#' @export
#'
#' @examples
#' # Example using Mullus barbatus parameters (GSA 06)
#' # Reference: García-Rodríguez et al. (2021), SAC, WGSAD
#' vb <- VB_params(
#'   Linf = 34.5,    # cm
#'   K = 0.34,       # 1/year
#'   t0 = -0.143,    # year
#'   a = 0.0056,     # weight-length coefficient
#'   b = 3.2488,     # weight-length exponent
#'   Temp = 13,      # °C
#'   Lm = 13.7       # cm
#' )
#'
#' plotVBnoise(vb, Linf_line = vb$Linf, noise_sd = 0.5)
plotVBnoise <- function(VB_params, Linf_line = NULL, noise_sd = 0.5) {
  ages <- seq(0, VB_params$tmax + 15, by = 0.1)
  lengths <- VB_params$Linf * (1 - exp(-VB_params$K * (ages - VB_params$t0)))
  noisy_lengths <- lengths + rnorm(length(lengths), mean = 0, sd = noise_sd)
  growth_data <- data.frame(Age = ages, Length = noisy_lengths)

  eq_text <- bquote(L(t) == .(VB_params$Linf) * (1 - e^{- .(VB_params$K) * (t - .(VB_params$t0))}))

  p <- ggplot(growth_data, aes(x = Age, y = Length)) +
    geom_line() +
    geom_point(color = "blue", size = 1, alpha = 0.5) +
    geom_smooth(method = "loess", se = TRUE) +
    labs(title = "Von Bertalanffy Growth Model with Noise",
         x = "Age (years)", y = "Length (cm)") +
    theme_classic() +
    xlim(0, VB_params$tmax + 10) +
    annotate("text", x = VB_params$tmax * 0.95, y = VB_params$Linf * 1.05,
             label = as.expression(eq_text), parse = TRUE)

  if (!is.null(Linf_line)) {
    p <- p + geom_hline(yintercept = Linf_line, linetype = "dashed", color = "black")
  }

  if (!is.null(VB_params$t50)) {
    p <- p + geom_vline(xintercept = VB_params$t50, linetype = "dotted", color = "red") +
      annotate("text", x = VB_params$t50, y = 0, label = "t50", vjust = -1, color = "red")
  }

  if (!is.null(VB_params$Lm)) {
    p <- p + geom_hline(yintercept = VB_params$Lm, linetype = "dotted", color = "purple") +
      annotate("text", x = 0, y = VB_params$Lm, label = "Lm", hjust = -0.1, color = "purple")
  }

  return(p)
}
