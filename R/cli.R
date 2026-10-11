# Lightweight command-line helpers used by the repository entry points.

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- list()
  positional <- character()
  index <- 1L

  while (index <= length(args)) {
    token <- args[[index]]
    if (!startsWith(token, "--")) {
      positional <- c(positional, token)
      index <- index + 1L
      next
    }

    token <- substring(token, 3L)
    if (grepl("=", token, fixed = TRUE)) {
      parts <- strsplit(token, "=", fixed = TRUE)[[1L]]
      options[[parts[[1L]]]] <- paste(parts[-1L], collapse = "=")
      index <- index + 1L
      next
    }

    if (index < length(args) && !startsWith(args[[index + 1L]], "--")) {
      options[[token]] <- args[[index + 1L]]
      index <- index + 2L
    } else {
      options[[token]] <- TRUE
      index <- index + 1L
    }
  }

  list(options = options, positional = positional)
}

cli_value <- function(parsed, name, default = NULL) {
  value <- parsed$options[[name]]
  if (is.null(value)) default else value
}

cli_flag <- function(parsed, name) {
  value <- parsed$options[[name]]
  if (is.null(value)) return(FALSE)
  if (is.logical(value)) return(isTRUE(value))
  tolower(as.character(value)) %in% c("1", "true", "yes", "y", "on")
}

cli_integer <- function(parsed, name, default) {
  value <- cli_value(parsed, name, default)
  converted <- suppressWarnings(as.integer(value))
  if (is.na(converted)) {
    stop(sprintf("--%s must be an integer.", name), call. = FALSE)
  }
  converted
}

resolve_context <- function(parsed) {
  workspace <- cli_value(parsed, "workspace", "data.RData")
  if (file.exists(workspace) && !cli_flag(parsed, "regenerate-folds")) {
    message(sprintf("Using archived datasets/folds from %s", workspace))
    return(load_archived_workspace(workspace))
  }

  complete_path <- cli_value(parsed, "complete", "complete.csv")
  geology_path <- cli_value(parsed, "geology", "geology.csv")
  seed <- cli_integer(parsed, "fold-seed", 2019L)
  datasets <- load_cpt_data(complete_path, geology_path)

  message(sprintf("Regenerating stratified folds with seed %d", seed))
  list(
    complete = datasets$complete,
    geology = datasets$geology,
    folds_complete = make_stratified_folds(datasets$complete$SBTn, seed = seed),
    folds_geology = make_stratified_folds(datasets$geology$MSBTn, seed = seed),
    validation_folds = make_validation_fold_map(10L)
  )
}

analysis_mode <- function(parsed) {
  if (cli_flag(parsed, "legacy")) {
    return(list(
      selection_strategy = "legacy_test_best",
      legacy_rng_reset = TRUE,
      label = "legacy-2018"
    ))
  }

  selection_strategy <- cli_value(parsed, "selection-strategy", "balanced")
  allowed <- c("balanced", "validation_best", "legacy_test_best")
  if (!selection_strategy %in% allowed) {
    stop(
      sprintf(
        "--selection-strategy must be one of: %s",
        paste(allowed, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    selection_strategy = selection_strategy,
    legacy_rng_reset = cli_flag(parsed, "legacy-rng-reset"),
    label = "paper-faithful"
  )
}

write_grid_outputs <- function(results, summary_rows, output_dir, stem, metadata) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(metadata = metadata, results = results),
    file.path(output_dir, paste0(stem, ".rds"))
  )
  utils::write.csv(
    do.call(rbind, summary_rows),
    file.path(output_dir, paste0(stem, "_summary.csv")),
    row.names = FALSE
  )
}
