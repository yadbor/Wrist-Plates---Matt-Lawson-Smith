# Ingest raw data files and clean up, dividing into cycles with `bluer`

library(dplyr) # for manipulating dataframes
library(ggplot2) # for plotting

# If you don't have bluer install it with 
# install.packages("devtools")
# devtools::install_github(repo = "yadbor/bluer")
library(bluer) # routines for analysing mechanical test data

# Define some helper functions to simplify the code
all_csv_files <- function(data_root){
  file_paths <- list.files(
    path = data_root,
    pattern = "csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  retrun (file_paths)
}
slurp_VIC_files <- function(file_list) {
  # Read the VIC-3D data files maned in `file_list` 
  # and return as a combined tibble, labelled witht he base file name.
  names(file_list) <- basename(file_list)
  results <- file_list |>
    purrr::map(\(f) readr::read_csv(file = f, show_col_types = FALSE)) |>
    # put the names from the raw_files list into a column called "filename"
    bind_rows(.id = "filename") |>
    # clean up and simplify some column names
    rename(frame = "File Number", time = "Time_1") |>
    rename_with(~ stringr::str_remove(.x, "\\s*\\[.+\\]") |> stringr::str_to_lower()) |>
    select(-"time_0") # Don't need two time columns
}
parse_metadata <- function(df, patterns, make_factors = TRUE) {
  # Split the filename column into test metadata, based on the supplied patterns
  pattern_names = names(patterns)
  pattern_names = pattern_names[nzchar(pattern_names)] # remove empty names
  df <- df |>
    tidyr::separate_wider_regex(cols = filename,
                                patterns = patterns,
                                cols_remove = FALSE,
                                #too_few = "debug"
    )
  if (make_factors) {
    df <- df |>
      mutate(across(all_of(pattern_names), factor))
  }
  return (df)
}
mark_cycles <- function(results) {
  cycles <- results |> 
    mutate(position = -1.0 * position, load = -1.0 * load) |> # Invert these axes
    group_by(filename) |> # group each unique test
    mutate(as_tibble(bluer::label_cycles(position))) |> # and label the cycles
    mutate(uid = paste0(filename, cycle)) # uniquely identify each cycle
  return (cycles) # Not needed as automatically returns result of last statement
}


### Cadaver data #####################################

data_root <- here::here("data-raw", "cadaver_csv_files")
# Find all potential data files
file_paths <- all_csv_files(data_root)

# Restrict the files to those in this study
file_paths <- file_paths |>
  stringr::str_subset(pattern = "(?i)Marker_Data") # Need case insensitive match

# Read all VIC-3D result files and combine them.
all_results <- slurp_VIC_files(file_paths)

# Clean up known errors and fix the spelling of parts of the filename.
# Fix a known typo. Need it here after the mis-named file has been read.
all_results <- all_results |>
  mutate(filename = stringr::str_replace(filename, "(?i)poximal", "proximal"))

# Define the parts of the filename to use for metadata with regexps.
# Note that the combined pattern must match the **whole** of the column, and 
# any un-named elements are discarded.
patterns = c(id = "^[^_]+", 
             "_", 
             arm = "[^_]+", 
             "_(?i)plate\\d?", 
             plate = "[L|S]", 
             "_",
             marker = "(?i)distal|proximal", ".*"
             )

# Parse filename column to extract metadata for each test.
all_results <- all_results |> parse_metadata(patterns)

# Fix known spelling errors in the markers
all_results <- all_results |>
  mutate(marker = forcats::fct_relabel(marker, stringr::str_to_lower))

# Label each cycle for each test
cycles <- mark_cycles(all_results)
readr::write_csv(cycles, file = here::here("data", "cadaver_cycles.csv"))

### Model data #####################################

data_root <- here::here("data-raw", "Experiment 2", "Results", "VIC-3D data")
# Find all potential data files
file_paths <- all_csv_files(data_root)
# Read all VIC-3D result files and combine them.
all_results <- slurp_VIC_files(file_paths)

# Define the parts of the filename to use for metadata with regexps.
# Note that the combined pattern must match the **whole** of the column, and 
# any un-named elements are discarded.
patterns = c(plate = "^\\w+", 
             "\\D*", 
             id = "\\d+", 
             "\\s*T", 
             rep = "[\\d\\.]+", 
             ".*"
             )

# Parse filenames to extract metadata for each test.
all_results <- all_results |> parse_metadata(patterns)

# Deal with special cases. 
# In this study, some tests were redone and called T1.2 and T2.2 
# To make the names uniform, delete everything after the "." making T1 & T2
all_results <- all_results |> 
  mutate(rep = forcats::fct_relabel(rep, \(x) stringr::str_remove(x, "\\..*")))
  
# Label each cycle for each test
cycles <- mark_cycles(all_results)
readr::write_csv(cycles, file = here::here("data", "experiment2_cycles.csv"))
