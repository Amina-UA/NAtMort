# NAtMort package
Natural mortality (M) is a critical concept in understanding the dynamics of biological populations. It represents the rate at which individuals in a population die due to natural causes, excluding human-related activities such as fishing or harvesting. This parameter is essential in fisheries science and ecological research, as it helps scientists model population trends, assess ecosystem health, and make informed management decisions. By providing tools and models to estimate this parameter, researchers can gain valuable insights into species' life histories and survival rates, ultimately supporting conservation and resource management efforts.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Examples](#examples)
- [Functions](#functions)
- [Contributing](#contributing)
- [License](#license)


Features

Multiple M estimators in one place (harmonized I/O): Charnov, Gislason, Lorenzen, Chen–Watanabe.

Unified plotting via plot_M() to compare methods with CIs and sensitivity bands.

Life-history helpers: VB_params() supports Lm (length at 50% maturity) and auto-derives t50 when absent.

Batch tools for multiple stocks/species and tidy exports.

Reproducibility first: seeds, session info, and consistent rounding options.

## Installation

Instructions for installing the package:

```r
# Install from GitHub
install.packages("devtools")
devtools::install_github("Amina-UA/NatMort")
library(NAtMort)

# Von Bertalanffy params (example)
vb <- VB_params(Linf = 27.5, k = 0.45, t0 = -0.1, Lm = 15.8)  # t50 inferred from Lm

# Run a panel of estimators
m_out <- estimate_M_all(
  Linf = vb$Linf, k = vb$k, t0 = vb$t0,
  Lm = vb$Lm, t50 = vb$t50,
  age = 2:6,                       # optional: age vector
  L   = c(12, 14, 16, 18, 20)      # optional: length vector
)

# Compare methods
plot_M(m_out, by = "age", show_ci = TRUE, title = "Natural Mortality by Age")
```
