library(tidyverse)

# this function is used to determine the real set size from the ratio input
prepare_set_size <- function(small_set, big_set, min_size = 4, max_size = 15) {
  repeat {
    small_set_size <- round(runif(1, min_size, max_size))
    big_set_size <- small_set_size * (big_set / small_set)
    if (big_set_size %% 1 == 0 && (big_set_size >= min_size && big_set_size <= max_size))
      break
  }
  list(small_set_size = small_set_size, big_set_size = big_set_size)
}

# this function is used to generate the final stimuli config for each
prepare_stim_base <- function(small_set_size, big_set_size, min_pixel = 10, max_pixel = 25) {
  n_stims <- small_set_size + big_set_size
  repeat {
    small_set_pixel <- round(runif(small_set_size, min_pixel, max_pixel))
    big_set_pixel <- round(runif(big_set_size, min_pixel, max_pixel))
    # make sure the surface area are equal
    if (sum(small_set_pixel ^ 2) == sum(big_set_pixel ^ 2))
      break
  }
  repeat {
    stims <- tibble(
      id = 1:n_stims,
      size = c(small_set_pixel, big_set_pixel),
      set = c(rep("small", small_set_size), rep("big", big_set_size))
    ) %>%
      mutate(
        position_x = round(runif(n_stims, 100, 1820)),
        position_y = round(runif(n_stims, 100, 980))
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
    if (all(pairs$dist_real > pairs$dist_min + 10))
      break
  }
  stims
}

config_stim <- readxl::read_excel("config_stim.xlsx")
set.seed(20190721)
set_stim <- config_stim %>%
  mutate(
    set_size = pmap(
      .,
      function(small_set, big_set, count) {
        replicate(
          count, prepare_set_size(small_set, big_set),
          simplify = FALSE
        ) %>%
          bind_rows()
      }
    )
  ) %>%
  unnest(set_size) %>%
  select(-count) %>%
  mutate(type = if_else(row_number(small_set) %% 2 == 1, "rb", "br")) %>%
  mutate(
    stim = pmap_chr(
      .,
      function(small_set_size, big_set_size, type, ...) {
        prepare_stim_base(small_set_size, big_set_size) %>%
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
