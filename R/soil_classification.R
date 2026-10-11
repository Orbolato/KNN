# Core functions for the 2019 Soils and Rocks CPT classification study.
#
# The implementation follows the methodology reported in:
# Carvalho, L.O. & Ribeiro, D.B. (2019). Soil Classification System from
# Cone Penetration Test Data Applying Distance-Based Machine Learning Algorithms.
# Soils and Rocks, 42(2), 167-178. https://doi.org/10.28927/SR.422167

.require_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' is required for this operation.", package),
      call. = FALSE
    )
  }
}

validate_columns <- function(data, columns, data_name = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "%s is missing required column(s): %s",
        data_name,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_cpt_data <- function(
    complete_path = "complete.csv",
    geology_path = "geology.csv") {
  if (!file.exists(complete_path)) {
    stop(sprintf("Complete dataset not found: %s", complete_path), call. = FALSE)
  }
  if (!file.exists(geology_path)) {
    stop(sprintf("Geological dataset not found: %s", geology_path), call. = FALSE)
  }

  list(
    complete = utils::read.csv(complete_path, check.names = FALSE),
    geology = utils::read.csv(geology_path, check.names = FALSE)
  )
}

load_archived_workspace <- function(path = "data.RData") {
  if (!file.exists(path)) {
    stop(sprintf("Archived workspace not found: %s", path), call. = FALSE)
  }

  env <- new.env(parent = baseenv())
  loaded <- load(path, envir = env)
  required <- c("dataNet", "dataGeo", "foldsNet", "foldsGeo", "validFolds")
  missing <- setdiff(required, loaded)

  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Archived workspace is missing required object(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    complete = env$dataNet,
    geology = env$dataGeo,
    folds_complete = env$foldsNet,
    folds_geology = env$foldsGeo,
    validation_folds = as.integer(env$validFolds)
  )
}

make_stratified_folds <- function(labels, k = 10L, seed = 2019L) {
  .require_namespace("caret")
  if (length(labels) < k) {
    stop("The number of observations must be at least k.", call. = FALSE)
  }
  set.seed(seed)
  caret::createFolds(labels, k = k, list = TRUE, returnTrain = FALSE)
}

make_validation_fold_map <- function(k = 10L) {
  # Reproduces the historical validation-fold selection logic used by the
  # original scripts (seeds 37, 74, ..., 370), while avoiding mutable state
  # outside the function.
  choices <- seq_len(k)
  validation <- integer(k)

  for (test_fold in seq_len(k)) {
    set.seed(test_fold * 37L)
    if (test_fold < k) {
      candidates <- choices[choices != test_fold]
      selected <- sample(candidates, 1L)
    } else {
      selected <- choices[[1L]]
    }
    validation[[test_fold]] <- selected
    choices <- choices[choices != selected]
  }

  validation
}

paper_feature_sets <- function() {
  list(
    raw = c("Depth_m", "qt_MPa", "fs_kPa", "u_kPa"),
    ist_normalized = c("Depth_m", "Qt1", "Fr_p", "Bq"),
    fsb_normalized = c("Depth_m", "Qtn", "Fr_p", "U2")
  )
}

main_feature_combinations <- function() {
  unlist(
    lapply(paper_feature_sets(), function(features) {
      unlist(
        lapply(2:length(features), function(size) {
          combn(features, size, simplify = FALSE)
        }),
        recursive = FALSE
      )
    }),
    recursive = FALSE
  )
}

geological_age_feature_sets <- function() {
  list(
    c("Qtn", "Fr_p", "U2"),
    c("CG", "Qtn", "Fr_p", "U2"),
    c("Depth_m", "qt_MPa", "fs_kPa", "u_kPa"),
    c("CG", "Depth_m", "qt_MPa", "fs_kPa", "u_kPa")
  )
}

raw_friction_ratio_feature_set <- function() {
  c("Depth_m", "qt_MPa", "Rf_p", "u_kPa")
}

subset_model_data <- function(data, output, features) {
  validate_columns(data, c(features, output))
  data[, c(features, output), drop = FALSE]
}

remove_unclassified <- function(data, output) {
  # The historical scripts remove IST/SBTn class 0. FSB/MSBTn class 0 is
  # intentionally retained, consistent with the published preprocessing.
  if (identical(output, "SBTn")) {
    data <- data[data[[output]] != 0, , drop = FALSE]
  }
  data
}

fit_minmax <- function(train, features) {
  validate_columns(train, features, "training data")
  mins <- vapply(train[features], min, numeric(1), na.rm = TRUE)
  maxs <- vapply(train[features], max, numeric(1), na.rm = TRUE)
  spans <- maxs - mins

  if (any(!is.finite(mins)) || any(!is.finite(maxs))) {
    stop("Non-finite values were found while fitting min-max scaling.", call. = FALSE)
  }
  if (any(spans == 0)) {
    stop(
      sprintf(
        "Cannot min-max scale constant feature(s): %s",
        paste(names(spans)[spans == 0], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(min = mins, max = maxs)
}

apply_minmax <- function(data, scaler, features) {
  result <- data
  for (feature in features) {
    result[[feature]] <-
      (result[[feature]] - scaler$min[[feature]]) /
      (scaler$max[[feature]] - scaler$min[[feature]])
  }
  result
}

scale_split <- function(train, validation, test, features) {
  scaler <- fit_minmax(train, features)
  list(
    train = apply_minmax(train, scaler, features),
    validation = apply_minmax(validation, scaler, features),
    test = apply_minmax(test, scaler, features),
    scaler = scaler
  )
}

macro_recall <- function(predicted, observed) {
  predicted_chr <- as.character(predicted)
  observed_chr <- as.character(observed)
  observed_classes <- sort(unique(observed_chr[!is.na(observed_chr)]))
  all_classes <- sort(unique(c(predicted_chr, observed_chr)))
  all_classes <- all_classes[!is.na(all_classes)]

  predicted_factor <- factor(predicted_chr, levels = all_classes)
  observed_factor <- factor(observed_chr, levels = all_classes)
  confusion <- table(predicted_factor, observed_factor)

  recalls <- vapply(observed_classes, function(class_value) {
    denominator <- sum(confusion[, class_value])
    if (denominator == 0) return(NA_real_)
    confusion[class_value, class_value] / denominator
  }, numeric(1))
  mean(recalls, na.rm = TRUE)
}

find_iqr_candidates <- function(
    train,
    output,
    features,
    depth_feature = "Depth_m",
    multiplier = 1.5) {
  screening_features <- setdiff(features, depth_feature)
  if (length(screening_features) == 0L) {
    return(integer())
  }

  candidates <- logical(nrow(train))
  classes <- sort(unique(train[[output]]))

  for (class_value in classes) {
    class_rows <- which(train[[output]] == class_value)
    for (feature in screening_features) {
      values <- train[class_rows, feature]
      quartiles <- stats::quantile(
        values,
        probs = c(0.25, 0.75),
        na.rm = TRUE,
        names = FALSE,
        type = 7
      )
      iqr <- quartiles[[2L]] - quartiles[[1L]]
      lower <- quartiles[[1L]] - multiplier * iqr
      upper <- quartiles[[2L]] + multiplier * iqr
      candidates[class_rows] <- candidates[class_rows] |
        values < lower | values > upper
    }
  }

  which(candidates)
}

make_formula <- function(output, features) {
  stats::reformulate(features, response = output)
}

edit_nearest_neighbor <- function(
    train,
    output,
    features,
    kernel,
    candidate_rows = NULL,
    distance = 2) {
  .require_namespace("kknn")
  validate_columns(train, c(features, output), "training data")

  if (is.null(candidate_rows)) {
    candidate_rows <- find_iqr_candidates(train, output, features)
  }
  if (length(candidate_rows) == 0L) {
    return(train)
  }

  work <- train
  work$.paper_row_id <- seq_len(nrow(work))
  candidate_ids <- work$.paper_row_id[candidate_rows]
  formula <- make_formula(output, features)

  for (candidate_id in candidate_ids) {
    current_index <- which(work$.paper_row_id == candidate_id)
    if (length(current_index) != 1L || nrow(work) <= 1L) next

    candidate <- work[current_index, setdiff(names(work), ".paper_row_id"), drop = FALSE]
    reference <- work[-current_index, setdiff(names(work), ".paper_row_id"), drop = FALSE]
    reference[[output]] <- as.factor(reference[[output]])
    candidate[[output]] <- factor(candidate[[output]], levels = levels(reference[[output]]))

    prediction <- kknn::kknn(
      formula = formula,
      train = reference,
      test = candidate,
      k = 1,
      distance = distance,
      kernel = kernel,
      scale = FALSE
    )$fitted.values

    if (as.character(prediction[[1L]]) != as.character(candidate[[output]][[1L]])) {
      work <- work[-current_index, , drop = FALSE]
    }
  }

  work$.paper_row_id <- NULL
  work
}

balance_training_data <- function(
    train,
    output,
    features,
    seed = 1L,
    minimum_per_class = 1000L,
    legacy_rng_reset = FALSE) {
  validate_columns(train, c(features, output), "training data")
  classes <- sort(unique(train[[output]]))
  class_counts <- vapply(
    classes,
    function(class_value) sum(train[[output]] == class_value),
    integer(1)
  )
  target_count <- max(minimum_per_class, 2L * min(class_counts))

  if (!legacy_rng_reset) set.seed(seed)
  result <- train

  for (class_value in classes) {
    class_rows <- result[result[[output]] == class_value, , drop = FALSE]
    n_class <- nrow(class_rows)

    if (n_class > target_count) {
      if (legacy_rng_reset) set.seed(seed)
      remove_count <- n_class - target_count
      removed <- sample(seq_len(n_class), remove_count)
      keep_rows <- class_rows[-removed, , drop = FALSE]
      result <- rbind(
        result[result[[output]] != class_value, , drop = FALSE],
        keep_rows
      )
    } else if (n_class < target_count) {
      synthetic_count <- target_count - n_class
      d_plus_one <- length(features) + 1L
      if (n_class < d_plus_one) {
        stop(
          sprintf(
            "Class %s has only %d rows; at least %d are required for the paper's d+1 synthetic-sample rule.",
            class_value,
            n_class,
            d_plus_one
          ),
          call. = FALSE
        )
      }

      synthetic <- vector("list", synthetic_count)
      for (index in seq_len(synthetic_count)) {
        if (legacy_rng_reset) set.seed(seed)
        sampled <- sample(seq_len(n_class), d_plus_one)
        new_row <- class_rows[1L, , drop = FALSE]
        for (feature in features) {
          new_row[[feature]] <- mean(class_rows[sampled, feature])
        }
        new_row[[output]] <- class_value
        synthetic[[index]] <- new_row
      }
      result <- rbind(result, do.call(rbind, synthetic))
    }
  }

  rownames(result) <- NULL
  result
}

prepare_fold <- function(
    data,
    folds,
    validation_folds,
    fold_index,
    output,
    features,
    kernel,
    legacy_rng_reset = FALSE,
    minimum_per_class = 1000L) {
  test_rows <- folds[[fold_index]]
  validation_rows <- folds[[validation_folds[[fold_index]]]]
  training_rows <- setdiff(seq_len(nrow(data)), c(test_rows, validation_rows))

  train <- data[training_rows, , drop = FALSE]
  validation <- data[validation_rows, , drop = FALSE]
  test <- data[test_rows, , drop = FALSE]

  train <- remove_unclassified(train, output)
  validation <- remove_unclassified(validation, output)
  test <- remove_unclassified(test, output)

  initial_scaled <- scale_split(train, validation, test, features)
  train_clean <- edit_nearest_neighbor(
    initial_scaled$train,
    output,
    features,
    kernel
  )

  train_balanced <- balance_training_data(
    train_clean,
    output,
    features,
    seed = 1L,
    minimum_per_class = minimum_per_class,
    legacy_rng_reset = legacy_rng_reset
  )

  unbalanced <- scale_split(
    train_clean,
    initial_scaled$validation,
    initial_scaled$test,
    features
  )
  balanced <- scale_split(
    train_balanced,
    initial_scaled$validation,
    initial_scaled$test,
    features
  )

  list(unbalanced = unbalanced, balanced = balanced)
}

as_classification_frames <- function(split, output) {
  train <- split$train
  validation <- split$validation
  test <- split$test

  class_levels <- sort(unique(c(
    as.character(train[[output]]),
    as.character(validation[[output]]),
    as.character(test[[output]])
  )))
  class_levels <- class_levels[!is.na(class_levels)]
  train[[output]] <- factor(train[[output]], levels = class_levels)
  validation[[output]] <- factor(validation[[output]], levels = class_levels)
  test[[output]] <- factor(test[[output]], levels = class_levels)

  list(train = train, validation = validation, test = test)
}

predict_kknn <- function(train, new_data, output, features, k, kernel, distance = 2) {
  .require_namespace("kknn")
  fit <- kknn::kknn(
    formula = make_formula(output, features),
    train = train,
    test = new_data,
    k = k,
    distance = distance,
    kernel = kernel,
    scale = FALSE
  )
  fit$fitted.values
}

choose_training_variant <- function(
    prepared,
    output,
    features,
    kernel,
    strategy = c("balanced", "validation_best", "legacy_test_best")) {
  strategy <- match.arg(strategy)
  unbalanced <- as_classification_frames(prepared$unbalanced, output)
  balanced <- as_classification_frames(prepared$balanced, output)

  # Section 4.2 of the paper states that class imbalance is prevented by
  # balancing the training data. That is therefore the default behavior.
  if (strategy == "balanced") {
    return(list(data = balanced, variant = "balanced", score = NA_real_))
  }

  # The historical 2018 script selected between balanced and unbalanced
  # training data using k=3 on the test fold. That leaks hold-out information.
  # "validation_best" keeps the same optional selection idea but uses the
  # validation fold; "legacy_test_best" is retained only for auditing old runs.
  evaluation_name <- if (strategy == "validation_best") "validation" else "test"

  unbalanced_prediction <- predict_kknn(
    unbalanced$train,
    unbalanced[[evaluation_name]],
    output,
    features,
    k = 3L,
    kernel = kernel
  )
  balanced_prediction <- predict_kknn(
    balanced$train,
    balanced[[evaluation_name]],
    output,
    features,
    k = 3L,
    kernel = kernel
  )

  unbalanced_score <- macro_recall(
    unbalanced_prediction,
    unbalanced[[evaluation_name]][[output]]
  )
  balanced_score <- macro_recall(
    balanced_prediction,
    balanced[[evaluation_name]][[output]]
  )

  if (balanced_score > unbalanced_score) {
    list(data = balanced, variant = "balanced", score = balanced_score)
  } else {
    list(data = unbalanced, variant = "unbalanced", score = unbalanced_score)
  }
}

tune_odd_k <- function(
    train,
    validation,
    output,
    features,
    kernel,
    max_k = Inf) {
  largest_possible <- nrow(train)
  if (is.finite(max_k)) largest_possible <- min(largest_possible, as.integer(max_k))
  if (largest_possible %% 2L == 0L) largest_possible <- largest_possible - 1L
  if (largest_possible < 1L) stop("Training data are too small to tune k.", call. = FALSE)

  previous_score <- -Inf
  best_k <- 1L
  best_score <- -Inf

  for (k in seq.int(1L, largest_possible, by = 2L)) {
    prediction <- predict_kknn(train, validation, output, features, k, kernel)
    score <- macro_recall(prediction, validation[[output]])

    if (score < previous_score) break
    best_k <- k
    best_score <- score
    previous_score <- score
  }

  list(k = best_k, validation_score = best_score)
}

run_fold <- function(
    data,
    folds,
    validation_folds,
    fold_index,
    output,
    features,
    kernel,
    selection_strategy = c("balanced", "validation_best", "legacy_test_best"),
    legacy_rng_reset = FALSE,
    minimum_per_class = 1000L,
    max_k = Inf) {
  selection_strategy <- match.arg(selection_strategy)
  prepared <- prepare_fold(
    data = subset_model_data(data, output, features),
    folds = folds,
    validation_folds = validation_folds,
    fold_index = fold_index,
    output = output,
    features = features,
    kernel = kernel,
    legacy_rng_reset = legacy_rng_reset,
    minimum_per_class = minimum_per_class
  )

  chosen <- choose_training_variant(
    prepared,
    output,
    features,
    kernel,
    strategy = selection_strategy
  )

  tuned <- tune_odd_k(
    chosen$data$train,
    chosen$data$validation,
    output,
    features,
    kernel,
    max_k = max_k
  )

  test_prediction <- predict_kknn(
    chosen$data$train,
    chosen$data$test,
    output,
    features,
    tuned$k,
    kernel
  )
  test_score <- macro_recall(test_prediction, chosen$data$test[[output]])

  data.frame(
    fold = fold_index,
    k = tuned$k,
    accuracy = 100 * test_score,
    validation_accuracy = 100 * tuned$validation_score,
    training_variant = chosen$variant,
    stringsAsFactors = FALSE
  )
}

summarize_configuration <- function(fold_results) {
  k_table <- table(fold_results$k)
  modal_k <- as.integer(names(k_table)[which.max(k_table)])
  modal_rows <- fold_results$k == modal_k

  data.frame(
    elected_k = modal_k,
    mean_accuracy = mean(fold_results$accuracy),
    sd_accuracy = stats::sd(fold_results$accuracy),
    mean_accuracy_at_elected_k = mean(fold_results$accuracy[modal_rows]),
    sd_accuracy_at_elected_k = stats::sd(fold_results$accuracy[modal_rows]),
    stringsAsFactors = FALSE
  )
}

run_configuration <- function(
    data,
    folds,
    validation_folds,
    output,
    features,
    kernel,
    selection_strategy = c("balanced", "validation_best", "legacy_test_best"),
    legacy_rng_reset = FALSE,
    minimum_per_class = 1000L,
    max_k = Inf,
    verbose = TRUE) {
  selection_strategy <- match.arg(selection_strategy)
  fold_results <- do.call(
    rbind,
    lapply(seq_along(folds), function(fold_index) {
      if (verbose) {
        message(
          sprintf(
            "output=%s kernel=%s features=[%s] fold=%d/%d",
            output,
            kernel,
            paste(features, collapse = ","),
            fold_index,
            length(folds)
          )
        )
      }
      run_fold(
        data,
        folds,
        validation_folds,
        fold_index,
        output,
        features,
        kernel,
        selection_strategy = selection_strategy,
        legacy_rng_reset = legacy_rng_reset,
        minimum_per_class = minimum_per_class,
        max_k = max_k
      )
    })
  )

  list(
    folds = fold_results,
    summary = summarize_configuration(fold_results)
  )
}

write_analysis_results <- function(results, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(results, output_path)
  invisible(output_path)
}
