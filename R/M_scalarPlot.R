#' Plot Scalar M Estimates and Save Results (Line Plot Version)
#'
#' Plots scalar M estimates using lines, adds mean and reference lines, labels methods,
#' and saves the results as an RData file.
#'
#' @param VB_params A list from \code{VB_params()} containing growth and biological parameters.
#' @param method Character. One of "all", "tmax", "K", "Hoenig", "Pauly", "FAMS". Default = "all".
#' @param save_data Logical. Save output as "M_cons.RData". Default = TRUE.
#'
#' @return A ggplot object showing scalar M estimates by method.
#' @export
M_scalarPlot <- function(VB_params, method = "all", save_data = TRUE) {
  if (!requireNamespace("FSA", quietly = TRUE)) stop("FSA package is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required.")

  method <- match.arg(method, c("all", "tmax", "K", "Hoenig", "Pauly", "FAMS"))

  method_sets <- list(
    tmax = c("tmax", "tmax1"),
    K = c("K1", "K2", "JensenK1", "JensenK2"),
    Hoenig = c("HoenigNLS", "HoenigO", "HoenigOF", "HoenigOM", "HoenigOC",
               "HoenigO2", "HoenigO2F", "HoenigO2M", "HoenigO2C", "HoenigLM", "HewittHoenig"),
    Pauly = c("PaulyLNoT", "PaulyL", "PaulyW"),
    FAMS = c("Gislason", "Charnov", "ZhangMegreyD", "ZhangMegreyP", "RikhterEfanov1", "RikhterEfanov2",
             "QuinnDeriso", "ChenWatanabe", "PetersonWroblewski", "AlversonCarney"),
    all = c("HoenigNLS","HoenigO","HoenigOF","HoenigOM","HoenigOC",
            "HoenigO2","HoenigO2F","HoenigO2M","HoenigO2C",
            "HoenigLM","HewittHoenig","tmax","tmax1",
            "PaulyLNoT","PaulyL","PaulyW",
            "K1","K2","JensenK1","JensenK2","Gislason","Charnov",
            "ZhangMegreyD","ZhangMegreyP","RikhterEfanov1","RikhterEfanov2",
            "QuinnDeriso","ChenWatanabe","PetersonWroblewski","AlversonCarney")
  )
  methods <- method_sets[[method]]

  M_vals <- data.frame(Method = character(), M = numeric())
  for (m in methods) {
    tryCatch({
      val <- FSA::metaM(
        method = m,
        Linf = VB_params$Linf,
        K = VB_params$K,
        t0 = VB_params$t0,
        tmax = VB_params$tmax,
        Temp = VB_params$Temp,
        t50 = VB_params$t50,
        L = VB_params$Lm,
        Winf = VB_params$a * VB_params$Linf^VB_params$b
      )$M
      M_vals <- rbind(M_vals, data.frame(Method = m, M = val))
    }, error = function(e) {
      message(sprintf("Skipping %s: %s", m, e$message))
    })
  }

  if (nrow(M_vals) == 0) stop("No M values were estimated.")

  M_vals$Index <- seq_len(nrow(M_vals))
  mean_M <- mean(M_vals$M, na.rm = TRUE)

  if (save_data) {
    save(M_vals, file = "M_cons.RData")
    message("M estimates saved to M_cons.RData")
  }

  library(ggplot2)
  p <- ggplot(M_vals, aes(x = Index, y = M)) +
    geom_point(group = 1, color = "steelblue", linewidth = 1.2) +
    geom_point(color = "steelblue", size = 2) +
    geom_text(aes(label = Method), vjust = -1.2, size = 2.5, angle = 0) +
    geom_hline(yintercept = mean_M, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0.2, linetype = "dotted", color = "blue") +
    annotate("text", x = max(M_vals$Index) + 0.5, y = mean_M,
             label = paste0("Mean M = ", round(mean_M, 3)),
             color = "red", hjust = 0, size = 3) +
    annotate("text", x = max(M_vals$Index) + 0.5, y = 0.2,
             label = "M = 0.2", color = "blue", hjust = 0, size = 3) +
    scale_y_continuous(breaks = seq(0, max(M_vals$M, mean_M, 0.2) + 0.2, by = 0.2)) +
    scale_x_continuous(breaks = seq(min(M_vals$Index), max(M_vals$Index), by = 2)) +
    labs(title = "Natural Mortality Estimates using FSA",
         x = "Method", y = expression(M~(year^{-1}))) +
    theme_classic()

  return(p)
}
