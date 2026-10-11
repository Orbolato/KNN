#!/usr/bin/env Rscript

# Lightweight regression tests for the maintained KNN/DWNN implementation.
# These checks focus on experiment structure and deterministic helper behavior;
# the optional smoke test exercises a complete 10-fold run when kknn is present.

source(file.path("R", "soil_classification.R"))

assert_true <- function(x, msg) {
  if (!isTRUE(x)) stop(msg, call. = FALSE)
}

assert_equal <- function(a, b, tolerance = 1e-10) {
  if (!isTRUE(all.equal(a, b, tolerance = tolerance, check.attributes = FALSE))) {
    stop("Assertion failed", call. = FALSE)
  }
}

# All maintained public entry points should remain syntactically valid.
for (file in c(
    file.path("R", "soil_classification.R"),
    file.path("R", "cli.R"),
    "basic_results.R",
    "results_cg.R",
    "results_rf.R")) {
  parse(file = file)
}

# Paper structure and preprocessing regression checks.
assert_equal(length(main_feature_combinations()), 33L)
train <- data.frame(x = c(0, 5, 10), y = c(10, 20, 30))
validation <- data.frame(x = 2.5, y = 15)
test <- data.frame(x = 7.5, y = 25)
scaled <- scale_split(train, validation, test, c("x", "y"))
assert_equal(scaled$train$x, c(0, .5, 1))
assert_equal(scaled$validation$x, .25)
assert_equal(scaled$test$y, .75)
assert_equal(
  macro_recall(c("A", "B", "B", "B"), c("A", "A", "B", "B")),
  .75
)

iqr_data <- data.frame(
  Depth_m = 1:10,
  x = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 100),
  class = c(rep(1, 5), rep(2, 5))
)
assert_true(
  10L %in% find_iqr_candidates(iqr_data, "class", c("Depth_m", "x")),
  "IQR screening failed"
)

# The historical balancing rule should equalize class counts at its target size.
balance_data <- data.frame(
  x = c(1:5, 10:13),
  y = c(2:6, 20:23),
  class = c(rep(1, 5), rep(2, 4))
)
balanced <- balance_training_data(
  balance_data, "class", c("x", "y"), minimum_per_class = 4L
)
assert_equal(as.integer(table(balanced$class)), c(8L, 8L))

# When the archived workspace is available, verify its expected fold contract.
if (file.exists("data.RData")) {
  archived <- load_archived_workspace()
  assert_equal(length(archived$folds_complete), 10L)
  assert_equal(length(archived$folds_geology), 10L)
  assert_equal(length(archived$validation_folds), 10L)
}

# Optional end-to-end smoke test. CI can still validate static/core behavior if
# kknn is unavailable, while normal repository CI installs it for this check.
if (requireNamespace("kknn", quietly = TRUE)) {
  set.seed(123)
  n <- 100L
  data <- data.frame(
    x = c(rnorm(50, -2, .25), rnorm(50, 2, .25)),
    y = c(rnorm(50, -2, .25), rnorm(50, 2, .25)),
    class = c(rep(1, 50), rep(2, 50))
  )
  folds <- split(seq_len(n), rep(1:10, each = 10))
  validation_folds <- c(2:10, 1)
  result <- run_configuration(
    data, folds, validation_folds, "class", c("x", "y"), "rectangular",
    minimum_per_class = 8L, max_k = 5L, verbose = FALSE
  )
  assert_equal(nrow(result$folds), 10L)
  assert_true(
    all(is.finite(result$folds$accuracy)),
    "Smoke test returned non-finite accuracy"
  )
}

cat("All KNN/DWNN core tests passed.\n")
