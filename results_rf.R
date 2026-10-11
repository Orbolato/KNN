#!/usr/bin/env Rscript

# Complementary raw-friction-ratio analysis for Carvalho & Ribeiro (2019).
# It reuses the common paper-oriented pipeline while replacing the normalized
# friction-ratio input with the archived raw Rf_p feature set.

source(file.path("R", "soil_classification.R"))
source(file.path("R", "cli.R"))

parsed <- parse_cli()
if (cli_flag(parsed, "help")) {
  cat(paste(
    "Raw friction-ratio complementary analysis for Carvalho & Ribeiro (2019).",
    "",
    "Usage: Rscript results_rf.R [options]",
    "Options are the same as basic_results.R.",
    sep = "\n"
  ))
  quit(status = 0L)
}

context <- resolve_context(parsed)
mode <- analysis_mode(parsed)
features <- raw_friction_ratio_feature_set()

# Evaluate the same two distance-based classifiers against both historical soil
# labels so this complementary run remains directly comparable to the main grid.
configurations <- expand.grid(
  kernel = c("rectangular", "gaussian"),
  output = c("SBTn", "MSBTn"),
  stringsAsFactors = FALSE
)
max_configurations <- min(
  cli_integer(parsed, "max-configurations", nrow(configurations)),
  nrow(configurations)
)
if (max_configurations < 1L) stop("--max-configurations must be positive.", call. = FALSE)
configurations <- configurations[seq_len(max_configurations), , drop = FALSE]
max_k_value <- cli_value(parsed, "max-k", Inf)
max_k <- if (identical(max_k_value, Inf)) Inf else as.integer(max_k_value)
minimum_per_class <- cli_integer(parsed, "minimum-per-class", 1000L)
if (minimum_per_class < 1L) stop("--minimum-per-class must be positive.", call. = FALSE)
if (!is.infinite(max_k) && (is.na(max_k) || max_k < 1L)) {
  stop("--max-k must be a positive integer.", call. = FALSE)
}

results <- vector("list", nrow(configurations))
summary_rows <- vector("list", nrow(configurations))

for (index in seq_len(nrow(configurations))) {
  config <- configurations[index, ]
  result <- run_configuration(
    data = context$complete,
    folds = context$folds_complete,
    validation_folds = context$validation_folds,
    output = config$output,
    features = features,
    kernel = config$kernel,
    selection_strategy = mode$selection_strategy,
    legacy_rng_reset = mode$legacy_rng_reset,
    minimum_per_class = minimum_per_class,
    max_k = max_k,
    verbose = !cli_flag(parsed, "quiet")
  )

  key <- sprintf("%s__%s__%s", config$kernel, config$output, paste(features, collapse = "+"))
  results[[index]] <- result
  names(results)[[index]] <- key
  summary_rows[[index]] <- cbind(
    data.frame(
      kernel = config$kernel,
      output = config$output,
      features = paste(features, collapse = "+"),
      stringsAsFactors = FALSE
    ),
    result$summary
  )
}

# Save compact summaries plus metadata identifying the execution mode and class-
# balancing settings used for this rerun.
output_dir <- cli_value(parsed, "output-dir", "results")
metadata <- list(
  analysis = "raw_friction_ratio",
  mode = mode$label,
  selection_strategy = mode$selection_strategy,
  legacy_rng_reset = mode$legacy_rng_reset,
  minimum_per_class = minimum_per_class,
  configurations_run = nrow(configurations),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
write_grid_outputs(results, summary_rows, output_dir, "raw_friction_ratio_analysis", metadata)
message(sprintf("Finished %d raw-friction-ratio configuration(s).", nrow(configurations)))
