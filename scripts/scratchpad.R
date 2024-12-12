

# list cols (as didn't include `frame`)
cycles |> select(u:w) |> tidyr::pivot_longer(cols = u:w, names_to = "axis") |> tidyr::pivot_wider(names_from = marker, values_fn = list) |> rename(abs_distal = distal) |> mutate(distal = purrr::map2(abs_distal, proximal, ~ .x - .y)) |> tidyr::pivot_longer(c(abs_distal, proximal, distal), names_to = "marker")

# numeric cols, one point per frame
# To subtract one marker from another, first get them into seperate columns,
# then do the subtraction, then put them back into one column x their components.
# 
# Take a dataframe with columns (marker, u, v, w)
# pivot to (marker, axis, value), then back to (axis, distal, proximal)
# Subtract proximal from distal, then pivot to (marker, axis, value) 
# and finally pivot back to (marker, u, v, w)
cycles |> 
  group_by(frame, .add = TRUE) |>
  select(frame, u:w) |> 
  tidyr::pivot_longer(cols = u:w, names_to = "axis") |> 
  tidyr::pivot_wider(names_from = marker) |> 
  mutate(abs_distal = distal, distal = abs_distal - proximal) |> 
  tidyr::pivot_longer(c(abs_distal, proximal, distal), names_to = "marker") |> 
  tidyr::pivot_wider(names_from = axis)


loading |> select(id, arm, plate, cycle, marker, seg, frame, time, load, position, u:w)


cycles |> 
  filter(seg == "load") |>
  #group_by(frame, .add = TRUE) |> 
  select(time, frame, u:w) |> 
  tidyr::pivot_longer(cols = u:w, names_to = "axis")  |> 
  tidyr::pivot_wider(names_from = marker)|> 
  mutate(abs_distal = distal, distal = abs_distal - proximal)  |> 
  tidyr::pivot_longer(c(abs_distal, proximal, distal), names_to = "marker") |> 
  tidyr::pivot_wider(names_from = axis) |> arrange(id, arm, plate, cycle, marker, frame) |>      tidyr::pivot_longer(cols = c(u, v, w)) |> 
  mutate(dt = time - first(time)) |> 
  mutate(uid = paste0(id, arm, plate, marker, cycle)) |> 
  ggplot() + 
  aes(x = dt, y = value, colour = marker,group = uid) + 
  geom_point(size = 0.5, alpha = 0.5) + geom_line() +
  facet_grid(name ~ id+plate)

# Change to motion
cycles |> 
  filter(seg == "load") |>
  #group_by(frame, .add = TRUE) |> 
  select(time, frame, u:w) |> 
  tidyr::pivot_longer(cols = u:w, names_to = "axis")  |> 
  tidyr::pivot_wider(names_from = marker)|> 
  mutate(abs_distal = distal, distal = abs_distal - proximal)  |> 
  tidyr::pivot_longer(c(abs_distal, proximal, distal), names_to = "marker") |> 
  tidyr::pivot_wider(names_from = axis) |> arrange(id, arm, plate, cycle, marker, frame) |>     mutate(dt = time - first(time), motion = sqrt(u^2 + v^2 + w^2)) |> 
  mutate(uid = paste0(id, arm, plate, marker, cycle)) |> 
  ggplot() + 
  aes(x = dt, y = motion, colour = marker,group = uid) + 
  geom_point(size = 0.5, alpha = 0.5) + geom_line() +
  facet_grid(plate~id)


loading_2 <- loading |> select(-filename) |>
select(id:frame, time, u:w) |>
tidyr::pivot_longer(cols = u:w, names_to = "axis") |>
tidyr::pivot_wider(names_from = marker) |>
mutate(abs_distal = distal, distal = abs_distal - proximal) |>
tidyr::pivot_longer(cols = c(abs_distal, proximal, distal), names_to = "marker") |>
tidyr::pivot_wider(names_from = axis) |> arrange(id, arm, plate, cycle, marker, frame)


loading_2 |> mutate(uid = paste0(id, arm,plate,cycle), dt = time - first(time)) |>
  tidyr::pivot_longer(cols = c(u, v, w)) |> 
  ggplot() + 
  aes(x = dt, y = value, colour = marker, group = uid) + 
  geom_point(size = 0.5, alpha = 0.25) + 
  facet_grid(name ~ id+plate)


results <- stiffness_all_cycles |>
rename_with( ~ stringr::str_remove(.x, "_estimate")) |>
ungroup() |>
tidyr::pivot_longer(cols = distal:position, names_to = "location") |>
mutate(location = forcats::fct(location, levels = c("proximal", "distal", "position"))) |>
mutate(plate = forcats::fct_recode(plate, long = "L", short = "S"))


results |> tidyr::pivot_wider(names_from = plate, values_from = value) |> group_nest(location) |> mutate( res = purrr::map(data, ~ t_TOST(.x$long, .x$short, paired = TRUE, eqb = 10) |> tidy_tost() )) |> tidyr::unnest(res) |> select(-data)

results |> group_by(location) |> group_map( ~ t_TOST(value ~plate, data = .x, hypothesis = "EQU", paired = TRUE, eqb = 10) |> _$TOST)


# Plot differences between stiffness for each test
# 
results |> 
  tidyr::pivot_wider(names_from = plate, values_from = value) |> 
  arrange(location, id) |> 
  ggplot() + 
  aes(x = id, ymax = long, ymin = short) + geom_linerange() + 
  geom_line(aes(y = long, group = location), colour = 3, linetype = "dashed") + 
  geom_point(aes(y = long), colour = 3) + 
  geom_line(aes(y = short, group = location), colour = 2, linetype = "dashed") + 
  geom_point(aes(y = short), colour = 2) + 
  facet_grid(location ~ ., scales = "free")


# Means etc
# Group means
results |> summarise(group_mean = mean(value), .by = location)
# plate means
results |> 
  summarise(mean = mean(value), .by = c(location, plate)) |> 
  tidyr::pivot_wider(values_from = mean, names_from = plate, names_prefix = "mean_")

# TOST_t can also be converted to a h_test list, so can we can use broom::tidy()
# |> as_htest() |> broom::tidy()
results |>
  group_by(location) |>
  group_modify(
    ~ t_TOST(value ~plate, data = .x, hypothesis = "EQU", paired = TRUE, eqb = eqb_list[.y$location]) |>
      as_htest() |> broom::tidy()
  ) |> bind_cols(eqb = eqb_list)

