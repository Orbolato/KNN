# User manual for basic_results.R
# 1) Set up the database in the CSV format with the inputs and outputs arranged in columns with these head identifications: Depth_m for depth (m), qt_MPa for the total (corrected) raw cone resistance (MPa), fs_kPa for the raw lateral friction (kPa), Rf_p for the friction ratio (%), Qt1 for the Robertson's (1990) normalized cone resistance, Fr_p for the normalized lateral friction ratio (%), Bq for the Robertson's (1990) excess pore pressure normalization and U2 for the Robertson's (2016) excess pore pressure normalization
# 2) Update the databases directories in the first code section, wherein the CSV databases are read and kept in variables
# 3) Run the 10-fold creation section
# 4) Run the checking of classes proportions inside the folds
# 5) Run the settlement of validation folds section
# 6) Save the workspace image so that the folds are defined and fixed for all source codes
# 7) Run the application section
