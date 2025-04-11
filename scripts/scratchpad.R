

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

# Compare results table to estimate +/- eqb
results |> 
group_by(location) |> 
  group_modify(
    ~ TOSTER::t_TOST(value ~ plate, data = .x, hypothesis = "EQU", paired = TRUE, eqb = eqb_list[.y$location]) |> 
      TOSTER::as_htest() |> broom::tidy()
  )
# # A tibble: 3 × 9
# # Groups:   location [3]
# location estimate statistic p.value parameter conf.low conf.high method        alternative
# <fct>       <dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl> <chr>         <chr>      
# 1 proximal   -35.2     -1.30    0.884         8   -82.1      11.7  Paired t-test equivalence
# 2 distal     -10.7     -0.740   0.760         8   -28.2       6.74 Paired t-test equivalence
# 3 position    -1.10    -0.302   0.615         8    -4.52      2.33 Paired t-test equivalence

estimates <- results |> 
  group_by(location) |> 
  group_modify(
    ~ TOSTER::t_TOST(value ~ plate, data = .x, hypothesis = "EQU", paired = TRUE, eqb = eqb_list[.y$location]) |> 
      TOSTER::as_htest() |> broom::tidy()
  ) |> select(estimate)

left_join(estimates, eqb_list |> tibble::enframe(name = "location")) |> 
  mutate(lo = estimate - value, hi = estimate + value)

# Doesn't; give th econfidence intervals
# # A tibble: 3 × 5
# # Groups:   location [3]
# location estimate value     lo      hi
# <chr>       <dbl> <dbl>  <dbl>   <dbl>
# 1 proximal   -35.2  3.77  -39.0  -31.4  
# 2 distal     -10.7  2.52  -13.2   -8.20 
# 3 position    -1.10 0.541  -1.64  -0.557
# 
# 


# Paired t-test
results |> 
  nest_by(location) |>
  mutate(model = list(t.test(formula = Pair(value, plate) ~ 1, data = data)), 
        summary = list(broom::tidy(model))
        ) |> 
  tidyr::unnest(summary) |>
  select(-data, -model)


# But this gives different results???
results |> 
  mutate(location = ordered(location, levels = c("proximal", "distal", "position"))) |>
  pivot_wider(names_from = plate, values_from = value) |> 
  nest(.by = location) |> 
  mutate(
    t_test = map(data, ~ t.test(.x$long, .x$short, paired = TRUE) |> broom::tidy())
    ) |> 
  unnest(t_test)


# Pairwise using t.test
results |> 
  mutate(location = ordered(location, levels = c("proximal", "distal", "position"))) |>
  pivot_wider(names_from = plate, values_from = value) |>
  nest(.by = location) |> 
  mutate(
    t_test = map(data, ~ t.test(.x$long, .x$short, paired = TRUE) |> broom::tidy())
    ) |> 
  unnest(t_test) |>
  arrange(location)

# Pairwise using pairwise.t.test
results %>%
  mutate(location = ordered(location, levels = c("proximal", "distal", "position"))) |>
  nest_by(location) %>%
  mutate(
    t_test = list(pairwise.t.test(x = data$value, 
                                  g = data$plate, 
                                  p.adjust = "bonferroni") |> broom::tidy()
                  )
    ) |> 
  unnest(t_test) |>
  arrange(location)

# Power os a paired t.test
results |>  
  mutate(location = ordered(location, levels = c("proximal", "distal", "position"))) |>
  pivot_wider(names_from = plate, values_from = value) |> 
  nest(.by = location) |> 
  mutate(t_test = map(data, 
                      ~ {delta = .x$short -.x$long; 
                         power.t.test(delta = mean(delta), 
                                      sd = sd(delta), 
                                      sig.level = 0.05, 
                                      power = 0.8, 
                                      type = "paired", 
                                      alternative = "two.sided")
                        } |> broom::tidy())
         ) |> 
  unnest(t_test) |>
  arrange(location)



