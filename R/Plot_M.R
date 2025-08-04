#' Plot Natural Mortality Estimates (M) from Scalar and Empirical Methods
#'
#' This function takes von Bertalanffy growth parameters and an age vector, computes
#' natural mortality (M) using scalar and empirical methods (Charnov, Lorenzen, Chen-Watanabe,
#' Gislason, etc.), and produces a unified plot. Scalar M methods are repeated across ages for comparison.
#'
#' @param VB_params A list from \code{VB_params()}.
#' @param age_vector A numeric vector of ages.
#' @param L A numeric vector of observed lengths-at-age, same length as \code{age_vector}.
#' @param save_path Optional file path to save the plot (e.g., "M_plot.png").
#'
#' @return A list with:
#' \describe{
#'   \item{data}{A data frame of all M estimates by method and age.}
#'   \item{scalar_table}{A data frame of scalar M values.}
#'   \item{plot}{A ggplot2 object.}
#' }
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
Plot_M <- function(VB_params,
                   age_vector,
                   L,
                   save_path = NULL) {
  if (!requireNamespace("FSA", quietly = TRUE)) stop("FSA package is required.")

  # Validate input
  required <- c("Linf", "K", "t0", "tmax", "Temp", "t50")
  if (!all(required %in% names(VB_params))) {
    stop("VB_params must include: Linf, K, t0, tmax, Temp, t50")
  }

  # Get scalar M values
  scalarM_output <- scalar_M(VB_params)

  # Repeat scalar M values across all ages
  get_M_vector <- function(method_name) {
    val <- scalarM_output$M[scalarM_output$Method == method_name]
    if (length(val) == 0) rep(NA, length(age_vector)) else rep(val, length(age_vector))
  }

  # Age-varying methods
  M_Charnov   <- Charnov(VB_params, LA = L)$m.cha.bio
  M_Gislason  <- Gislason(VB_params, LA = L)$m.gis.bio
  M_Lorenzen  <- Lorenzen(VB_params, age_vector)
  M_ChenWat   <- ChenWat(VB_params, age_vector)

  # Scalar M values (repeat across ages)
  scalar_cols <- lapply(scalarM_output$Method, get_M_vector)
  names(scalar_cols) <- scalarM_output$Method

  # Combine into data frame
  M_df <- data.frame(
    Age = age_vector,
    Charnov = M_Charnov,
    Gislason = M_Gislason,
    Lorenzen = M_Lorenzen,
    ChenWatanabe = M_ChenWat,
    scalar_cols
  )

  # Melt for ggplot
  M_long <- reshape2::melt(M_df, id.vars = "Age", variable.name = "Method", value.name = "M")

  # Plot all methods uniformly
  p <- ggplot(M_long, aes(x = Age, y = M, color = Method)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = "Natural Mortality Estimates by Method",
      x = "Age (years)",
      y = expression(M~(year^{-1})),
      color = "Method"
    ) +
    theme_classic()

  # Optional save
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 8, height = 5, dpi = 300)
  }

  return(list(
    data = M_df,
    scalar_table = scalarM_output,
    plot = p
  ))
}
