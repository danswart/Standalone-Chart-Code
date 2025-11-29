# Load libraries
# library(tidyverse)

# Force dplyr's select to take precedence
select <- dplyr::select
filter <- dplyr::filter

# Options
options(scipen = 999)
options(qic.clshade = T) # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.linecol = 'black') # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.signalcol = "firebrick") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.targetcol = "purple") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(DT.options = list(dom = 'pBlfrti')) # Add buttons, filtering, and top (t) pagination controls
options(shiny.maxRequestSize = 50 * 1024^2) # Set upload maximum to 50 MB
options(tigris_use_cache = TRUE)


flextable::set_flextable_defaults(
  font.size = 14,
  font.family = "Cabin",
  font.color = "black",
  table.layout = "fixed",
  border.color = "darkgray",
  padding.top = 3,
  padding.bottom = 3,
  padding.left = 4,
  padding.right = 4,
  line_spacing = 1.3,
  digits = 2,
  decimal.mark = ",",
  big.mark = " ",
  na_str = "<na>"
  # flextable::theme_alafoli()	|>  # BLAH
  # flextable::theme_apa()  # THIS IS NICE
  # flextable::theme_booktabs() |>  # NICE, MORE COMPACT
  # flextable::theme_box() |>   # OK, INCLUDES CELL BORDERS
  # flextable::theme_tron() |>  # 'DARK MODE' BLUE TEXT
  # flextable::theme_tron_legacy() |>   # 'DARK MODE' YELLOW TEXT
  # flextable::theme_vader() |>    # 'DARK MODE' WHITE TEXT
  # flextable::theme_vanilla() |>   # NOT SPECIAL
  # flextable::theme_zebra()	|>
)


# Set global theme for consistent plots
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(
        family = "Cabin",
        face = "bold",
        color = "darkgreen",
        size = 22,
        fill = "yellow",
        lineheight = 1.2,
        padding = ggplot2::margin(5.5, 5.5, 0.0, 5.5),
        margin = ggplot2::margin(0, 0, 5.5, 0)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        family = "Cabin",
        color = "darkgreen",
        face = "bold",
        size = 20,
        fill = "yellow",
        lineheight = 1.2,
        padding = ggplot2::margin(5.5, 5.5, 5.5, 5.5),
        margin = ggplot2::margin(10.5, 0, 5.5, 0)
      ),
      plot.caption = ggtext::element_markdown(
        family = "Cabin",
        size = 16,
        hjust = 1,
        color = "darkblue",
        face = "italic",
        fill = "yellow",
        lineheight = 1.0
      ),
      axis.text.x = ggtext::element_markdown(
        family = "Cabin",
        face = "bold",
        color = "blue",
        size = 16,
        angle = 45,
        hjust = 1
      ),
      # ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(
        family = "Cabin",
        face = "bold",
        color = "blue",
        size = 16
      ),
      # ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(
        family = "Cabin",
        face = "bold",
        color = "blue",
        size = 16,
        angle = 45,
        hjust = 1
      ),
      # ggplot2::element_blank(),
      axis.title.y = ggtext::element_markdown(
        family = "Cabin",
        face = "bold",
        color = "blue",
        size = 16
      ),
      # ggplot2::element_blank(),
      strip.text = ggtext::element_markdown(
        family = "Cabin",
        color = "black",
        size = ggplot2::rel(1.1),
        face = "italic",
        margin = ggplot2::margin(2, 0, 0.5, 0, "lines")
      ),
      axis.text = ggtext::element_markdown(
        family = "Cabin",
        color = "black"
      ),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "none",
      panel.spacing.x = grid::unit(1.5, "cm"),
      panel.spacing.y = grid::unit(1.5, "cm"),
      plot.margin = ggplot2::margin(20, 20, 20, 20, "pt")
    )
)


# Set seed for reproducibility
base::set.seed(123)


# Create a toy dataset
set.seed(123)

n <- 30
x <- 1:n
y <- 10 + 0.5 * x + stats::rnorm(n, mean = 0, sd = 1)
data <- data.frame(x = x, y = y)

# Introduce a perturbation to shift the mean higher after point 15
data$y[data$x > 15] <- data$y[data$x > 15] + 5 # Add 15 to the y values after point 15

# Split the data into two segments
data_first_15 <- data[data$x <= 15, ]
data_remaining <- data[data$x > 15, ]

# Fit linear models for each segment
model_first_15 <- stats::lm(y ~ x, data = data_first_15)
model_remaining <- stats::lm(y ~ x, data = data_remaining)

# Calculate standard deviations for each segment
residuals_sd_first_15 <- stats::sd(stats::residuals(model_first_15))
residuals_sd_remaining <- stats::sd(stats::residuals(model_remaining))

# Calculate control limits for each segment
data$centerline_first_15 <- stats::predict(model_first_15, newdata = data)
data$ucl_first_15 <- data$centerline_first_15 +
  3 * (residuals_sd_first_15 / 1.128)
data$lcl_first_15 <- data$centerline_first_15 -
  3 * (residuals_sd_first_15 / 1.128)

data$centerline_remaining <- stats::predict(model_remaining, newdata = data)
data$ucl_remaining <- data$centerline_remaining +
  3 * (residuals_sd_remaining / 1.128)
data$lcl_remaining <- data$centerline_remaining -
  3 * (residuals_sd_remaining / 1.128)

# Set Shewhart Rule #1 boundaries
sigma.signals <- y < data$lcl_first_15 |
  y > data$ucl_first_15 |
  y < data$lcl_remaining |
  y > data$ucl_remaining

# Add check for unusually long runs or unusually few crossings.

# runs analysis
runs <- sign(
  y - ifelse(x <= 15, data$centerline_first_15, data$centerline_remaining)
)
runs <- runs[runs != 0]
runs <- rle(runs)$lengths
n.obs <- sum(runs)
longest.run <- max(runs, na.rm = TRUE)
n.runs <- length(runs)
n.crossings <- n.runs - 1

longest.run.max <- round(log2(n.obs) + 3)
n.crossings.min <- stats::qbinom(.05, n.obs - 1, 0.5)

runs.signal <- longest.run > longest.run.max | n.crossings < n.crossings.min

# Plot a trended control chart
ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point(
    ggplot2::aes(x = x, y = y),
    shape = 19,
    size = 3,
    col = sigma.signals + 1
  ) +
  ggplot2::geom_line() +

  # Draw CL, UCL & LCL for the first 15 points
  ggplot2::geom_line(
    ggplot2::aes(y = ifelse(x <= 15, centerline_first_15, NA)),
    color = "blue",
    linetype = "solid",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ifelse(x <= 15, ucl_first_15, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ifelse(x <= 15, lcl_first_15, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +

  # Draw CL, UCL & LCL for the remaining points
  ggplot2::geom_line(
    ggplot2::aes(y = ifelse(x > 15, centerline_remaining, NA)),
    color = "green",
    linetype = "solid",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ifelse(x > 15, ucl_remaining, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ifelse(x > 15, lcl_remaining, NA)),
    color = "red",
    linetype = "dotted",
    linewidth = 1
  ) +

  # Add labels
  ggplot2::labs(
    title = "Trended Expectation Chart with Recalced Limits",
    subtitle = "Using Shewhart Rule #1 and Anhoej Run Rules",
    caption = "Perturbation of 3 at Point 16",
    x = "Observation",
    y = "Value"
  )
