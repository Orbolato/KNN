# CPT Soil Classification with KNN and DWNN

Public R implementation associated with:

**Lucas Orbolato Carvalho and Dimas Betioli Ribeiro, “Soil Classification System from Cone Penetration Test Data Applying Distance-Based Machine Learning Algorithms,” *Soils and Rocks*, 42(2), 167–178, 2019.**

- DOI: https://doi.org/10.28927/SR.422167
- Paper website: https://soilsandrocks.com/search/sr-422167

## Scope

This repository contains the distance-based machine-learning experiments reported in the 2019 paper. It evaluates classical **k-nearest neighbors (KNN)** and a Gaussian **distance-weighted nearest-neighbor (DWNN)** variant for classifying soils from cone penetration test (CPT) data.

The cleaned implementation preserves the study design while making the code portable, modular, and auditable. The original datasets, archived R workspace, and saved result workspaces remain in the repository unchanged.

## Method implemented

The main experiment retains the paper’s core protocol:

- two soil-classification outputs represented in the archived data as `SBTn` and `MSBTn`;
- three four-variable CPT feature families:
  - raw measurements: `Depth_m`, `qt_MPa`, `fs_kPa`, `u_kPa`;
  - normalized set: `Depth_m`, `Qt1`, `Fr_p`, `Bq`;
  - normalized set: `Depth_m`, `Qtn`, `Fr_p`, `U2`;
- all 2-, 3-, and 4-variable combinations inside each family, giving **33 feature combinations**;
- rectangular-kernel KNN and Gaussian-kernel DWNN;
- Euclidean distance;
- 10-fold evaluation with separate test and validation folds;
- min–max normalization fitted on training data;
- class-wise IQR screening followed by an edited-nearest-neighbor check;
- class balancing by undersampling larger classes and generating synthetic minority examples from the mean of `d + 1` samples;
- odd `k` values selected on the validation fold;
- final performance summarized using mean class recall, matching the accuracy measure used by the original scripts.

The main grid therefore contains **132 configurations**: 2 classifiers × 2 outputs × 33 feature combinations.

Two complementary analyses from the original repository are also retained:

- `results_cg.R`: evaluates input sets with and without the geological-age variable `CG` on the geological subset;
- `results_rf.R`: evaluates the raw friction-ratio feature set (`Rf_p`).

## Important cleanup of the historical implementation

The 2018 public scripts predate the final 2019 paper and contain two implementation details that are not appropriate for a clean paper-oriented implementation:

1. they compare balanced and unbalanced training sets using performance on the **test fold** before final evaluation;
2. they call `set.seed(1)` inside every synthetic-sample iteration, which can repeatedly select the same source observations when generating minority-class examples.

The cleaned default follows the paper more closely:

- **balanced training is used by default**, consistent with the paper’s stated treatment of class imbalance;
- validation data, not test data, are used for model tuning;
- the balancing random-number generator is seeded once, so successive synthetic samples can differ.

For audit purposes, `--legacy` restores the two historical choices above. The old implementation also remains fully recoverable from Git history, and the archived `.RData` result files have not been modified.

Because the cleaned default removes test-set leakage and changes the synthetic-sampling sequence, newly generated numerical results should be treated as a **paper-faithful rerun**, not assumed to be byte-for-byte identical to the archived 2018 output workspaces.

## Repository structure

```text
.
├── R/
│   ├── soil_classification.R   # Shared preprocessing, balancing, KNN/DWNN, evaluation
│   └── cli.R                   # Lightweight command-line helpers
├── basic_results.R             # Main 132-configuration experiment
├── results_cg.R                # Geological-age complementary experiment
├── results_rf.R                # Raw friction-ratio complementary experiment
├── complete.csv                # Complete CPT dataset archived with the original repository
├── geology.csv                 # Geological subset archived with the original repository
├── data.RData                  # Historical data/fold workspace
├── *_outputs.RData             # Historical saved result workspaces
├── tests/
│   └── test_core.R             # Core-method and smoke tests
└── .github/workflows/tests.yml # Automated R tests
```

## Requirements

A current R installation and the `kknn` package are required for the analysis:

```r
install.packages("kknn")
```

The default commands use the archived `data.RData` object, including the historical folds. If you explicitly regenerate folds from the CSV files, the `caret` package is also required:

```r
install.packages("caret")
```

## Run the main analysis

From the repository root:

```bash
Rscript basic_results.R
```

The full run evaluates all 132 configurations and can be computationally intensive.

For a short smoke test:

```bash
Rscript basic_results.R --max-configurations 1 --max-k 5 --minimum-per-class 50 --quiet
```

Results are written to `results/` as an RDS file plus a compact CSV summary.

### Useful options

```text
--workspace PATH             archived workspace (default: data.RData)
--regenerate-folds           regenerate folds from complete.csv/geology.csv
--fold-seed N                seed used when regenerating folds (default: 2019)
--selection-strategy NAME    balanced, validation_best, or legacy_test_best
--legacy                     reproduce key historical 2018 implementation choices
--minimum-per-class N        lower bound used by the balancing rule (paper default: 1000)
--max-configurations N       restrict the number of configurations
--max-k N                    cap the odd-k search (useful for smoke tests)
--output-dir PATH            output directory (default: results)
--quiet                      suppress per-fold progress messages
```

The **paper-oriented default** is `--selection-strategy balanced` with `--minimum-per-class 1000`.

## Run the complementary analyses

Geological-age analysis:

```bash
Rscript results_cg.R
```

Raw friction-ratio analysis:

```bash
Rscript results_rf.R
```

The same smoke-test and legacy options are accepted.

## Historical mode

To audit the behavior of the original public scripts while using the cleaned code structure:

```bash
Rscript basic_results.R --legacy
```

This mode intentionally reproduces the historical test-fold choice between balanced/unbalanced training and the repeated RNG reset during synthetic-sample generation. It is provided for comparison, not recommended for new analyses.

## Tests

Run:

```bash
Rscript tests/test_core.R
```

The tests cover the 33-feature-combination design, min–max scaling, macro recall, IQR screening, the balancing rule, archived-workspace structure, parsing of all public R entry points, and a small end-to-end KNN/DWNN smoke test.

## Data and archived outputs

`complete.csv`, `geology.csv`, `data.RData`, `basic_outputs.RData`, `outputs_cg.RData`, and `outputs_rf.RData` are historical artifacts distributed with the original repository. The 2026 cleanup does not rewrite them.

The archived workspace is preferred by default because it preserves the historical folds used by the original public implementation. Regenerating folds creates a new, seeded partition and should be regarded as a new run.

## Citation

If this code supports your work, please cite the paper:

```text
Carvalho, L. O., & Ribeiro, D. B. (2019).
Soil Classification System from Cone Penetration Test Data Applying
Distance-Based Machine Learning Algorithms. Soils and Rocks, 42(2), 167–178.
https://doi.org/10.28927/SR.422167
```

## License

No separate software license has been assigned to this repository. The presence of source code on GitHub should not be interpreted as granting rights beyond those provided by applicable law or an explicit license.
