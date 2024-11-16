library(tidyverse)
library(GGally)
library(scattermore)

# CORE FUNCTIONS

# Creates a 2-column df from the supplied `data` df according to the supplied mapping
select_xy <- function(data, mapping) {
  mapping_x <- mapping['x'][['x']]
  mapping_y <- mapping['y'][['y']]


  data_selected <- data |>
    select(!! mapping_x, !! mapping_y) |>
    rename(x = !! mapping_x, y = !! mapping_y)

  return(data_selected)
}

# Creates a 6-column tibble, each constisting of `num_points` points distributed along an SND
generate_data <- function(num_points) {
  # return(
  #   tibble(
  #     a1 = rnorm(num_points),
  #     a2 = rnorm(num_points),
  #     a3 = rnorm(num_points),
  #     a4 = rnorm(num_points),
  #     a5 = rnorm(num_points),
  #     a6 = rnorm(num_points),
  #     b1 = rnorm(num_points),
  #     b2 = rnorm(num_points),
  #     b3 = rnorm(num_points),
  #     b4 = rnorm(num_points),
  #     b5 = rnorm(num_points),
  #     b6 = rnorm(num_points),
  #     c1 = rnorm(num_points),
  #     c2 = rnorm(num_points),
  #     c3 = rnorm(num_points),
  #     c4 = rnorm(num_points),
  #     c5 = rnorm(num_points),
  #     c6 = rnorm(num_points)
  #   )
  # )

  return(
    tibble(
      a1 = rnorm(num_points),
      a2 = rnorm(num_points),
      a3 = rnorm(num_points),
      a4 = rnorm(num_points),
      a5 = rnorm(num_points),
      a6 = rnorm(num_points),
    )
  )
}

# Creates a subplot and returns the time it took to complete this operation
create_subplot_benchmark <- function(data, mapping, use_scattermost = TRUE) {
  print(paste(
    "RUNNING BENCHMARK ON SUBPLOT",
    use_scattermost
  ))


  start_time <- Sys.time()

  selected_data <- select_xy(data, mapping)

  if (use_scattermost) {
    generated_plot <- ggplot() + geom_scattermost(selected_data)
  } else {
    generated_plot <- ggplot() + geom_point(data, mapping = mapping)
  }

  benchmark_time <- difftime(Sys.time(), start_time, units = "secs")
  return(as.numeric(benchmark_time))
}

# Creates a subplot and returns the subplot's ggplot object.
create_subplot <- function(data, mapping, use_scattermost = TRUE) {
  selected_data <- select_xy(data, mapping)

  if (use_scattermost) {
    return( ggplot() + geom_scattermost(selected_data) )
  } else {
    return( ggplot() + geom_point(data, mapping) )
  }
}

# Returns the run time for a single ggpairs call
benchmark_ggpairs <- function(data, use_scattermost = TRUE) {
  num_data = data |> summarize(n()) |> pull()
  print(paste(
    "RUNNING BENCHMARK ON GGPAIRS",
    use_scattermost,
    num_data,
    sep = "-"
  ))

  start_time <- Sys.time()

  if (use_scattermost) {
    ggsave(
      paste("figures/data", num_data, use_scattermost, ".png", sep = "_"),
      ggpairs(
        data,
        upper = list(continuous = function(data, mapping) { create_subplot(data, mapping, use_scattermost) }, combo = "box_no_facet", discrete = "count", na = "na"),
        lower = list(continuous = function(data, mapping) { create_subplot(data, mapping, use_scattermost) }, combo = "box_no_facet", discrete = "count", na = "na")
      ),
      width = 20, height = 20, units = "cm"
    )
  } else {
    ggsave(
      paste("figures/data", num_data, use_scattermost, ".png", sep = "_"),
      ggpairs(
        data,
        upper = list(continuous = "points", combo = "box_no_facet", discrete = "count", na = "na"),
        lower = list(continuous = "points", combo = "box_no_facet", discrete = "count", na = "na")
      ),
      width = 20, height = 20, units = "cm"
    )
  }

  diff <- difftime(Sys.time(), start_time, units = "secs")
  return(as.numeric(diff))
}

# BENCHMARK

data_sizes <- c(1e3)
data_points <- lapply(data_sizes, generate_data)

# First, we benchmark the generation of individual subplots (no GGally call)
single_subplot_scattermore_times <- lapply(data_points, function (data) {
  return( create_subplot_benchmark(data, aes(x = a1, y = a2), TRUE) )
})

single_subplot_default_times <- lapply(data_points, function (data) {
  return( create_subplot_benchmark(data, aes(x = a1, y = a2), FALSE) )
})

# Then, we benchmark the generation of plot matrices using ggpairs
ggpairs_scattermore_times <- lapply(data_points, function (data) {
  benchmark_ggpairs(data, TRUE)
})

ggpairs_default_times <- lapply(data_points, function (data) {
  benchmark_ggpairs(data, FALSE)
})

# Convert our time data into a clear table
presentation_data <- tibble(
  num_points = data_sizes,
  subplot_scattermore = single_subplot_scattermore_times,
  subplot_default = single_subplot_default_times,
  pairs_scattermore = ggpairs_scattermore_times,
  pairs_default = ggpairs_default_times
)

presentation_data <- presentation_data |> pivot_longer(
  cols = c("subplot_scattermore", "subplot_default", "pairs_scattermore", "pairs_default"),
  names_to = "measurement",
  values_to = "value"
) |> mutate(num_value = as.numeric(value))

View(presentation_data)

write_csv(presentation_data, 'data/perf-data.csv')
