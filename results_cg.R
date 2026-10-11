#!/usr/bin/env Rscript

source(file.path("R", "soil_classification.R"))
source(file.path("R", "cli.R"))

parsed <- parse_cli()
if (cli_flag(parsed, "help")) {
  cat(paste(
    "Geological-age (CG) complementary analysis for Carvalho & Ribeiro (2019).",
    "",
    "Usage: Rscript results_cg.R [options]",
    "Options are the same as basic_results.R; --max-configurations is supported.",
    sep = "\n"
  ))
  quit(status = 0L)
}

context <- resolve_context(parsed)
mode <- analysis_mode(parsed)
feature_sets <- geological_age_feature_sets()
configurations <- expand.grid(
  kernel = c("rectangular", "gaussian"),
  feature_index = seq_along(feature_sets),
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
  features <- feature_sets[[config$feature_index]]
  result <- run_configuration(
    data = context$geology,
    folds = context$folds_geology,
    validation_folds = context$validation_folds,
    output = "MSBTn",
    features = features,
    kernel = config$kernel,
    selection_strategy = mode$selection_strategy,
    legacy_rng_reset = mode$legacy_rng_reset,
    minimum_per_class = minimum_per_class,
    max_k = max_k,
    verbose = !cli_flag(parsed, "quiet")
  )

  key <- sprintf("%s__MSBTn__%s", config$kernel, paste(features, collapse = "+"))
  results[[index]] <- result
  names(results)[[index]] <- key
  summary_rows[[index]] <- cbind(
    data.frame(
      kernel = config$kernel,
      output = "MSBTn",
      features = paste(features, collapse = "+"),
      stringsAsFactors = FALSE
    ),
    result$summary
  )
}

output_dir <- cli_value(parsed, "output-dir", "results")
metadata <- list(
  analysis = "geological_age",
  mode = mode$label,
  selection_strategy = mode$selection_strategy,
  legacy_rng_reset = mode$legacy_rng_reset,
  minimum_per_class = minimum_per_class,
  configurations_run = nrow(configurations),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
write_grid_outputs(results, summary_rows, output_dir, "geological_age_analysis", metadata)
message(sprintf("Finished %d geological-age configuration(s).", nrow(configurations)))
