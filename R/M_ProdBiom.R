#' Calculate and Plot Abella et al. (1998) Model Mortality Estimates
#'
#' Estimates age-specific natural mortality using the Abella et al. (1998) method.
#'
#' @param VB_params A list from `VB_params()` containing growth and biological parameters.
#' @param N0 Initial population size (default = 100000).
#' @param initial_Ma Initial guess for parameter Ma.
#' @param initial_B Initial guess for parameter B.
#' @param max_iterations Maximum optimization iterations.
#' @param tolerance Optimization convergence threshold.
#' @param plot Logical; if TRUE returns a ggplot object; else a data.frame.
#'
#' @return Either a `data.frame` or a `ggplot` object.
#' @export
ProdBiom <- function(VB_params, N0 = 100000,
                     initial_Ma = 0.2, initial_B = 0.4,
                     max_iterations = 50, tolerance = 0.01, plot = TRUE) {
  specific_ages <- c(2.4, 2.56, 2.72, 2.88, 3.04, 3.20, 8)
  guessed_Mrate <- 0.3

  age_seq <- seq(0, VB_params$tmax + 5, by = 0.5)
  age_seq[1] <- 0.000001
  mean_ages <- (age_seq[-1] + age_seq[-length(age_seq)]) / 2
  size <- VB_params$Linf * (1 - exp(-VB_params$K * (age_seq - VB_params$t0)))

  Wstart <- VB_params$a * size^VB_params$b
  Wmid <- VB_params$a * (VB_params$Linf * (1 - exp(-VB_params$K * mean_ages)))^VB_params$b

  objective_function <- function(params) {
    Ma <- params[1]
    B <- params[2]
    N <- M <- Blost <- Production <- numeric(length(mean_ages))
    N[1] <- N0

    for (i in 2:length(mean_ages)) {
      M[i] <- Ma + B / mean_ages[i]
      N[i] <- N[i - 1] * exp(-M[i])
    }

    for (i in 2:length(mean_ages)) {
      idx <- which.min(abs(age_seq - mean_ages[i]))
      Blost[i] <- (N[i - 1] - N[i]) * Wstart[idx]
      Production[i] <- log(Wstart[idx] / Wstart[which.min(abs(age_seq - mean_ages[i - 1]))]) *
        (N[i - 1] * exp(-M[i] / 2)) * Wmid[i - 1]
    }

    ratio <- sum(Blost, na.rm = TRUE) / sum(Production, na.rm = TRUE)
    penalty <- sum(sapply(specific_ages, function(a) {
      age_index <- which.min(abs(age_seq - a))
      estM <- Ma + B / age_seq[age_index]
      (estM - guessed_Mrate)^2
    }))

    (ratio - 1)^2 + penalty
  }

  Ma <- initial_Ma
  B <- initial_B
  ratio_diff <- Inf
  iteration <- 0

  while (iteration < max_iterations && ratio_diff > tolerance) {
    iteration <- iteration + 1
    opt_results <- optim(c(Ma, B), objective_function, method = "L-BFGS-B", lower = c(0, 0))
    Ma <- opt_results$par[1]
    B <- opt_results$par[2]

    N <- M <- Blost <- Production <- numeric(length(mean_ages))
    N[1] <- N0
    for (i in 2:length(mean_ages)) {
      M[i] <- Ma + B / mean_ages[i]
      N[i] <- N[i - 1] * exp(-M[i])
    }
    for (i in 2:length(mean_ages)) {
      idx <- which.min(abs(age_seq - mean_ages[i]))
      Blost[i] <- (N[i - 1] - N[i]) * Wstart[idx]
      Production[i] <- log(Wstart[idx] / Wstart[which.min(abs(age_seq - mean_ages[i - 1]))]) *
        (N[i - 1] * exp(-M[i] / 2)) * Wmid[i - 1]
    }

    ratio_diff <- abs(sum(Blost, na.rm = TRUE) / sum(Production, na.rm = TRUE) - 1)
  }

  M_rate <- Ma + B / age_seq
  Bstart <- N * Wstart[1:length(N)]
  mean_M <- c(Ma, (B / diff(mean_ages)) * log(head(mean_ages, -1) / tail(mean_ages, -1)) + Ma)

  results <- data.frame(
    Age = age_seq,
    Size = size,
    Wstart = Wstart,
    N = c(N, rep(NA, length(age_seq) - length(N))),
    Bstart = c(Bstart, rep(NA, length(age_seq) - length(Bstart))),
    Blost = c(Blost, rep(NA, length(age_seq) - length(Blost))),
    MeanM = c(mean_M, rep(NA, length(age_seq) - length(mean_M))),
    Bmid = c(Wmid, rep(NA, length(age_seq) - length(Wmid))),
    Production = c(Production, rep(NA, length(age_seq) - length(Production))),
    Mrate = M_rate
  )

  filtered <- results[results$Age >= 1 & results$Age <= VB_params$tmax, ]

  if (plot) {
    return(ggplot(filtered, aes(x = Age, y = Mrate)) +
             geom_line() + geom_point() +
             labs(title = "Abella et al. (1998) Mortality Rate",
                  x = "Age", y = expression("Mortality Rate (year"^-1*")")) +
             theme_minimal())
  } else {
    return(filtered)
  }
}
