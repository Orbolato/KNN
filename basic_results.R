#!/usr/bin/env Rscript

source(file.path("R", "soil_classification.R"))
source(file.path("R", "cli.R"))

parsed <- parse_cli()
if (cli_flag(parsed, "help")) {
  cat(paste(
    "Main KNN/DWNN analysis for Carvalho & Ribeiro (2019).",
    "",
    "Usage:",
    "  Rscript basic_results.R [options]",
    "",
    "Options:",
    "  --workspace PATH            Archived workspace (default: data.RData)",
    "  --regenerate-folds           Regenerate folds from CSV files instead",
    "  --fold-seed N                Seed used only when regenerating folds (default: 2019)",
    "  --selection-strategy NAME    balanced (paper default), validation_best, or legacy_test_best",
    "  --legacy                     Reproduce key historical 2018 implementation choices",
    "  --minimum-per-class N        Balancing lower bound (paper default: 1000)",
    "  --max-configurations N       Run only the first N configurations (smoke testing)",
    "  --max-k N                    Optional upper bound for k search",
    "  --output-dir PATH            Output directory (default: results)",
    "  --quiet                      Suppress per-fold progress messages",
    sep = "\n"
  ))
  quit(status = 0L)
}

context <- resolve_context(parsed)
mode <- analysis_mode(parsed)
feature_combinations <- main_feature_combinations()
configurations <- expand.grid(
  kernel = c("rectangular", "gaussian"),
  output = c("SBTn", "MSBTn"),
  feature_index = seq_along(feature_combinations),
  stringsAsFactors = FALSE
)

max_configurations <- cli_integer(parsed, "max-configurations", nrow(configurations))
max_configurations <- min(max_configurations, nrow(configurations))
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
  features <- feature_combinations[[config$feature_index]]
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

  key <- sprintf(
    "%s__%s__%s",
    config$kernel,
    config$output,
    paste(features, collapse = "+")
  )
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

output_dir <- cli_value(parsed, "output-dir", "results")
metadata <- list(
  analysis = "main",
  mode = mode$label,
  selection_strategy = mode$selection_strategy,
  legacy_rng_reset = mode$legacy_rng_reset,
  minimum_per_class = minimum_per_class,
  configurations_run = nrow(configurations),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
write_grid_outputs(results, summary_rows, output_dir, "main_analysis", metadata)
message(sprintf("Finished %d configuration(s).", nrow(configurations)))
