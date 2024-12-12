loading |>
  group_by(id, arm, plate, marker, cycle) |>
  tidyr::nest(data = all_of(load, position)) %>%
  mutate(map_lm = purrr::map(data, ~lm(load ~ position, data = .)),
         map_seg = purrr::map2(data, map_lm, ~segmented(.y, seg.Z = ~ position)))