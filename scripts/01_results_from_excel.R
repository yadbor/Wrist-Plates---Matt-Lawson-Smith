library(readxl)
library(dplyr)
library(stringr)

data_folder <- here::here("data-raw", "Testing Results", "Data New Calcs 13.06.2024")

# To convert load to moment need to know the lever arm of each type of plate
lever_arm = list(short = 48, long = 70)

workbooks <- list.files(data_folder, pattern = ".*xls", full.names = TRUE) |> 
  str_subset("\\~\\$", negate = TRUE) # Remove any Excel lockfiles

# Get the list of workbooks into a tibble, then for each file 
# get the names of all of it's sheets that have "#" into a list column.
# These are all the results sheets. Unnest them into rows and read each sheet.
results <- tibble(path = workbooks) |>
  mutate(sheet = purrr::map(path, \(w) readxl::excel_sheets(w) |> str_subset("#"))) |> 
  tidyr::unnest(cols = c(sheet))

# Remove known issues
# "Short Plate #1 T3_Data2" replaces "Short Plate #1 T3"
# "Short Plate #3_data2" replaces "Short Plate #3_data"
results <- results |>
  filter(sheet != "Short Plate #1 T3" & 
         sheet != "Short Plate #3_data" 
         )

# Read each sheet in each file, using map2 to go across both lists in parallel
results <- results |>
  mutate(data = purrr::map2(path, sheet, ~ read_excel(.x, .y))) 

# Now unnest the data list column into the columns from each sheet, 
# then extract the useful ones and give them sensible names.
results <- results |> 
  tidyr::unnest(cols = c(data)) |>
  select(path, sheet, frame = "File Number", 
         U = "U [mm]", V = "V [mm]", W = "W [mm]", 
         phi = starts_with("phi"), time = "Time_1", 
         position = "position", load = "load")

# Get the test description from the sheet name. Repeated tests have rep as Tn,
# but if only tested once there is no number, so can't just use a regex. 
# Get the whole chunk with the regex, extract any Tn and fill missing NA with 1.
results <- results |> 
  tidyr::separate_wider_regex(sheet, 
                              patterns = c(plate_type = "^\\w+", ".*#", plate_id = "\\d+", rest = ".*"), 
                              cols_remove = FALSE) |> 
  mutate(rep = str_extract(rest, "(?<=T)\\d") |> as.integer()) |> 
  tidyr::replace_na(list(rep = 1)) |>
  select(-rest) # Drop the redundant column

# Reorder the columns to make it easier to read
results <- results |>
  relocate(path, sheet, plate_type, plate_id, rep) |>
  # Change the plate names to lower case for consistency & 'cause it looks better
  mutate(plate_type = str_to_lower(plate_type))

# Invert load, position and V direction to get compressive load and extension
results <- results |>
  mutate(load = -load, position = -position, V = -V)

# Add the torque and plate bend, calculated from the lever_arm
results <- results |>
  mutate(arm = as.numeric(lever_arm[plate_type]),  # in mm
         moment = load * arm / 1000,               # in N.mm
         angle = atan2(position, arm) * (180 / pi) # in degrees
         ) |>
  select(-arm) # don't need this any more

# Write the combined results dataframe for later analysis
readr::write_csv(results, file = here::here("data", "combined_results.csv"))
