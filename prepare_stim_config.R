library(tidyverse)

# this function is used to determine the real set size from the ratio input
prepare_dot_count <- function(small_set, big_set, range_dot_count = c(4, 15)) {
  repeat {
    small_set_count <- round(runif(1, range_dot_count[1], range_dot_count[2]))
    big_set_count <- small_set_count * (big_set / small_set)
    if (big_set_count %% 1 == 0 && (big_set_count >= range_dot_count[1] && big_set_count <= range_dot_count[2]))
      break
  }
  list(small_set_count = small_set_count, big_set_count = big_set_count)
}

# this function is used to generate the final stimuli config for each
prepare_stim_base <- function(small_set_count, big_set_count,
                              range_dot_size = c(10, 25),
                              range_position = c(1920, 1080),
                              margin = 100) {
  n_dots <- small_set_count + big_set_count
  repeat {
    small_set_size <- round(runif(small_set_count, range_dot_size[1], range_dot_size[2]))
    big_set_size <- round(runif(big_set_count, range_dot_size[1], range_dot_size[2]))
    # make sure the surface area are equal
    if (sum(small_set_size ^ 2) == sum(big_set_size ^ 2))
      break
  }
  repeat {
    stims <- tibble(
      id = 1:n_dots,
      size = c(small_set_size, big_set_size),
      set = c(rep("small", small_set_count), rep("big", big_set_count))
    ) %>%
      mutate(
        position_x = round(runif(n_dots, margin, range_position[1] - margin)),
        position_y = round(runif(n_dots, margin, range_position[2] - margin))
      )
    pairs <- as_tibble(t(combn(stims$id, 2)), .name_repair = ~c("from", "to")) %>%
      mutate(
        dist_min = map2_dbl(
          from, to,
          ~ sum(with(stims, size[id %in% c(.x, .y)]))
        ),
        dist_real = map2_dbl(
          from, to,
          ~ stims %>%
            slice(c(.x, .y)) %>%
            select(starts_with("position")) %>%
            dist()
        )
      )
    # make sure the distance between each pair of dot is large enough
    if (all(pairs$dist_real > pairs$dist_min + 5))
      break
  }
  stims
}

configs <- jsonlite::fromJSON("config.json", simplifyVector = TRUE)
set.seed(20190721)
set_stim <- configs$base_stim %>%
  bind_rows(.id = "phase") %>%
  mutate(
    set_count = pmap(
      .,
      function(small_set, big_set, count, ...) {
        replicate(
          count, prepare_dot_count(small_set, big_set, configs$range_dot_count),
          simplify = FALSE
        ) %>%
          bind_rows()
      }
    )
  ) %>%
  unnest(set_count) %>%
  select(-count) %>%
  add_column(id = 1:nrow(.), .before = 1L) %>%
  group_by(phase) %>%
  mutate(
    file_pre = if_else(phase == "practice", "p", ""),
    file_suf = row_number(phase)
  ) %>%
  ungroup() %>%
  mutate(
    pic_file = sprintf("%s%02d.%s", file_pre, file_suf, configs$format),
    type = if_else(id %% 2 == 1, "rb", "br")
  ) %>%
  select(-starts_with("file")) %>%
  mutate(
    stim = pmap_chr(
      .,
      function(small_set_count, big_set_count, type, ...) {
        prepare_stim_base(
          small_set_count, big_set_count,
          configs$range_dot_size, configs$range_position
        ) %>%
          mutate(
            color = case_when(
              type == "rb" ~ if_else(set == "small", "r", "b"),
              type == "br" ~ if_else(set == "small", "b", "r")
            )
          ) %>%
          select(-set) %>%
          jsonlite::toJSON()
      }
    )
  )
write_tsv(set_stim, "config_stim.txt")
