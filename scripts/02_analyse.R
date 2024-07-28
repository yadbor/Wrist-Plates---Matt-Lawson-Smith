library(bluer) # If missing use remotes::install_github(repo = "yadbor/bluer")
library(dplyr)
library(ggplot2)

# Load the previously extracted data
results <- readr::read_csv(here::here("data", "combined_results.csv"))

# Add a test_ID to uniquely identify each test easily
results <- results |>
  mutate(test_id = str_c(plate_type, plate_id, rep, sep = "."))

# For each test, use position to segment into cycles and load/unload segments
results <- results |> 
  group_by(test_id) |>                                       # Group by test
  mutate(seg = as_tibble(bluer::label_cycles(position, span = 3))) |>  # Find each cycle
  tidyr::unnest(seg)                                         # Cycle info as cols

# Note that the data collection timer sometimes has odd jumps at the start.
# This is not a problem if we use the second cycle of each test pair.

# Only want the load segment of each test
load_only <- results |> filter(seg == "load", cycle %% 2  == 0) 

# Check plot - all data
all_plot <- load_only |> 
  ggplot() + 
  aes(x = position, y = moment, colour = as.factor(cycle), group=test_id) + 
  geom_line() + 
  facet_grid(rows = vars(plate_type), cols = vars(plate_id))

repeat_plot <- load_only |> 
  filter(plate_id == 1) |> 
  ggplot() + 
  aes(x = angle, y = phi, colour = as.factor(cycle), group=test_id) + 
  geom_line() + 
  facet_grid(rows = vars(plate_type), cols = vars(rep))

bad_long  <- load_only |> 
  filter(plate_id == 1, rep %in% 3:4) |> 
  ggplot() + 
  aes(x = angle, y = phi, colour = as.factor(cycle), group=test_id) + 
  geom_line() + 
  facet_grid(rows = vars(rep), cols = vars(cycle))

# Normalise the data to all start at zero displacement