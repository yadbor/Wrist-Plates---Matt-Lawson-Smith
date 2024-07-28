library(dplyr)
library(stringr)

dic_folder = here::here("data-raw", "Testing Results", "Data New Calcs 13.06.2024")


results <- dplyr::tibble(
  path = list.files(
    path = dic_folder,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  ),
  filename = basename(path), 
  foldername = basename(dirname(path)),
) 

# First find which ones are really data files that we want.
# All extracted files have "File Number" as the first characters of the header
# The ones we want also need to have "V", "phi" and "load" in the header row.
# Accept them in any order? Count the hits and make sure they're all there.
test_results <- function(filename) {
  l = readLines(filename, n = 1)
  if(length(l) > 0 ) {
    #return(stringr::str_detect(l, "File Number.*phi.*load"))
    hits = stringr::str_count(l, "File Number|V|phi|load")
    return(hits == 4) # found them all
  } else {
    return(FALSE)
  }
}

results <- results |> 
  filter(purrr::map_lgl(path, test_results))

# Now extract test info from the file & folder names
results <- results |>
  tidyr::separate_wider_regex(filename,
                              c(plate_type = "\\w+", ".*#", id = ".*(?=\\.csv)", ".*csv"),
                              cols_remove = FALSE) |>
  mutate(plate_num = str_extract(id, "^\\d")) |>
  tidyr::separate_wider_regex(foldername, 
                              c(".*?[\\_\\s]+(?=\\d)", date = ".*"), 
                              cols_remove = FALSE)

# Clean up the extracted information
results <- results |>
  mutate(date = str_replace_all(date, "\\.", "\\-")) |> # known typo in one name
  mutate(tested = lubridate::ymd_hm(date)) # Make into a real timestamp
  
# there are some repeated data files, looks like multiple extractions.
# Take the most recent for multiples in the same folder, for which we need to 
# get the file modified time
results <- results |>
  mutate(m_time = file.mtime(path)) |>      # File last changed      
  group_by(foldername) |>
  slice_max(m_time)                         # Keep the most recent in each folder

# Now add a repetition count for each plate, as some were tested multiple times.
results <- results |>
  group_by(plate_type, plate_num) |>        # Group the plates
  arrange(plate_type, plate_num, tested) |> # Get the tests in order
  mutate(rep = row_number())
  