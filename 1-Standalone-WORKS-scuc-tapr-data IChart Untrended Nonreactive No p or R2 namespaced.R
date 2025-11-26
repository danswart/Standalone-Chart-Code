# I-CHART UNTRENDED NON-REACTIVE NO P AND NO R^2 VALUES

# # Load Libraries - NAMESPACED - NOT NEEDED

# Options
options(scipen = 999)
options(qic.clshade = T) # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.linecol = 'black') # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.signalcol = "red") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.targetcol = "purple") # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(DT.options = list(dom = 'pBlfrti')) # Add buttons, filtering, and top (t) pagination controls
options(shiny.maxRequestSize = 50 * 1024^2) # Set upload maximum to 50 MB


# Set global theme for consistent plots
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 26),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 24),
      axis.title.x = ggplot2::element_text(face = "bold", size = 22),
      axis.title.y = ggplot2::element_text(face = "bold", size = 22),
      axis.text.x = ggplot2::element_text(
        face = "bold",
        size = 22,
        angle = 45,
        hjust = 1
      ),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      panel.spacing.x = grid::unit(1.5, "cm"),
      panel.spacing.y = grid::unit(1.5, "cm"),
      plot.margin = ggplot2::margin(20, 20, 20, 20, "pt")
    )
)

########  CONVERTING AN UPDATED VERSION OF THE CSV FILE IN WIDE FORMAT TO LONG FORMAT ##########

# # Load required packages
# library(tidyr)
# library(dplyr)   # for data manipulation, if needed
# library(readr)   # for reading and writing CSV files
#
# # Read the wide-format CSV file
# data_wide <- read_csv("data/SCUC Snapshots 1995 to 2023-WIDE.csv")
#
# # Convert from wide to long format.
# # In this example, we assume that the first column (e.g., an identifier or date)
# # should remain as-is, and all other columns will be gathered.
# # Convert from wide to long format
# data_long <- data_wide |>
#   pivot_longer(
#     cols = -(1:6),
#     names_to = "year",
#     values_to = "value"
#   )
#
# # Write the long-format data to a new CSV file - RENAME TO PREVIOUS FILENAME AFTER TESTING
# write_csv(data_long, "SCUC Snapshots 1995 to 2023-LONG-NEW.csv")

# Load the data by path
df1 <- vroom::vroom(
  file = here::here(
    "data",
    "20250318 staar_performance_district_level_grades_3_to_8_2018_2024_merged_cleaned_long.csv"
  )
)


# Load data locally, if needed
# df1 <- vroom::vroom(file = here::here("SCUC Snapshots 1995 to 2023-LONG.csv"))

##### Control chart calculations

# # Filter the data frame for control limits calc
# df1_control_limits <- df1 |>
#   dplyr::filter(grouping == "Asian",
#          section == "Standardized_Scores",
#          level_achieved == "Approaches_or_Above",
#          dplyr::between(year, 2010, 2016)
#          )

# Filter the data frame based on the conditions
df1_control_limits <- df1 |>
  dplyr::filter(
    grade_level_name == "Grade 3",
    group == "district",
    staar_grouping == "Approaches or Above",
    dplyr::between(year, 2018, 2024)
  )

#####  CALCULATE UNTRENDED CL, SD AND CONTROL LIMITS BASED ON CONTROL LIMITS DATA FRAME #####

emp_cl <<- mean(df1_control_limits$value, na.rm = TRUE) # Calculate empirical centerline
emp_sd <<- mean(abs(diff(df1_control_limits$value)), na.rm = TRUE) / 1.128 # Calculate empirical standard deviation
emp_lcl <<- max(emp_cl - (3 * emp_sd), 0) # If LCL is negative, set it to zero
emp_ucl <<- emp_cl + (3 * emp_sd) # Calculate empirical upper control limit


#####  END OF CALCULATING CL, LCL AND UEL BASED ON CONTROL LIMITS DATA FRAME df1_control_limits #####

#####  FILTER TO INCLUDED ONLY DISIRED ROWS TO INCLUDE IN UNTRENDED I-CHART  ##########

# Filter the data frame based on the desired control chart rows
# df1_modified <- df1 |>
#   dplyr::filter(grouping == "Asian",
#          section == "Standardized_Scores",
#          level_achieved == "Approaches_or_Above",
#          dplyr::between(year, 2012, 2023)
#          )  # Efficient range filtering)

# Filter the data frame based on the conditions
df1_modified <- df1 |>
  dplyr::filter(
    grade_level_name == "Grade 3",
    group == "district",
    subject == "Mathematics",
    staar_grouping == "Approaches or Above",
    dplyr::between(year, 2018, 2024)
  )


#####  Add logical column to 'df1_modified' based on Shewhart Rule #1 ONLY.

# Shewhart Rule #1: violations if mr value is outside UEL or LCL
sigma_signals <- df1_modified$value < emp_lcl | df1_modified$value > emp_ucl

# Rename the rule #1 violations and run rule violations
emp_pts_out_of_control <- sigma_signals

# Add the 'out_of_control' column to the dataframe
df1_modified$emp_pts_out_of_control <- emp_pts_out_of_control


# Runs analysis, for changing CL color and linetype
runs <- sign(df1_modified$value - emp_cl)
runs <- runs[runs != 0]
runs_lengths <- rle(runs)$lengths
n_obs <- sum(runs_lengths)
longest_run <- max(runs_lengths)
n_runs <- length(runs_lengths)
n_crossings <- n_runs - 1
longest_run_max <- round(log2(n_obs) + 3)
n_crossings_min <- stats::qbinom(.05, n_obs - 1, 0.5)

runs_signal <- longest_run > longest_run_max | n_crossings < n_crossings_min


#####  ADD EMPERICAL CL, SD, UEL, LCL AND RUNS_SIGNAL TO df1_modified DATA FRAME  #####

df1_modified <- df1_modified |>
  dplyr::mutate(
    emp_cl = emp_cl, # add empirical centerline
    emp_sd = emp_sd, # add empirical standard deviation
    emp_lcl = emp_lcl, # add empirical lcl
    emp_ucl = emp_ucl, # add empirical upper control limit
    runs_signal = as.logical(runs_signal) # Ensure it's a column, not a constant # add runs_signal
  )

#####  END OF FILTERING DATA FRAME  ##########

# HARD CODE CHAR VECTORS OF UNIQUE VALUES IN VARIOUS ROWS IN DATA FRAME

# Create char vector of unique entries in 'year' column
desired_years = unique(df1_modified$year)

# Create char vector of unique entries in 'group' column
desired_groups = unique(df1_modified$group)

# Create char vector of unique entries in 'grade_level' column
desired_grade_levels <- unique(df1_modified$grade_level_name)

# Create char vector of unique entries in 'subject' column
desired_subjects <- unique(df1_modified$subject)

# Create char vector of unique entries in 'achieve_level' column
desired_achieve_levels <- unique(df1_modified$staar_grouping)

# Create num vector of unique entries in 'value' column
desired_values <- unique(df1_modified$value)

# Specify chart type as a char vector for creating filenames
chart_type = "Untrended I-Chart"

# Specify the caption for the plots
caption <- glue::glue("Source: tea.texas.gov/reports-and-data")

# Replace zeros in the value column with NA
df1_modified$value <- ifelse(df1_modified$value == 0, NA, df1_modified$value)


#####  'df1_modified' data frame now has 7 columns  #####

# Compute x and y positions for geom_richtext before the ggplot call
x_pos <- min(df1_modified$year, na.rm = TRUE) # Ensure NA handling
y_pos <- max(df1_modified$value, na.rm = TRUE) # Ensure NA handling


################    Function to generate    #####################
################    AN UNTRENDED I-Chart    #####################

# PLOT OBJECT 'combined_plots' CONTAINING ALL CHARTS IN ONE MAP FUNCTION -------------------------------------

#####  CREATE A TRENDED I-CHART  IN GGPLOT2  #####

# Create dynamic title and subtitle
i_title <- glue::glue(
  "Expectation Chart:  % of Students in {desired_subjects}<br>Who achieved: {desired_achieve_levels}"
)

i_subtitle <- glue::glue(
  "In:   {desired_grade_levels}<br>For the Years Ended:   {min(desired_years)} - {max(desired_years)}"
)


# Create plot object I-chart USING GGPLOT2 ONLY
i_chart <- ggplot2::ggplot(
  df1_modified,
  ggplot2::aes(x = df1_modified$year, y = df1_modified$value)
) +

  ## Add lines connecting the points
  ggplot2::geom_line(color = "darkgray", linewidth = 1.2) +

  # Use 'color' to map logical TRUE/FALSE to red/black points
  ggplot2::geom_point(
    ggplot2::aes(
      x = year,
      y = value,
      color = factor(emp_pts_out_of_control) # Map to emp_pts_out_of_control to color the points
    ),
    size = 2.5
  ) +

  ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) + # Define colors for dots based on value of sigma_signals

  # CL line type depends on value of runs_signals
  ggplot2::geom_line(ggplot2::aes(y = emp_cl, linetype = factor(runs_signal))) +

  ggplot2::scale_linetype_manual(
    values = c("FALSE" = "solid", "TRUE" = "dashed")
  ) +

  ggplot2::geom_line(
    ggplot2::aes(y = df1_modified$emp_ucl),
    color = "red",
    linetype = "solid",
    linewidth = 1
  ) +

  ggplot2::geom_line(
    ggplot2::aes(y = df1_modified$emp_lcl),
    color = "red",
    linetype = "solid",
    linewidth = 1
  ) +

  # Add labels
  ggplot2::labs(
    title = i_title,
    subtitle = i_subtitle,
    caption = caption,
    x = "Academic Year",
    y = "Value"
  ) +

  # Disclose CL calculation
  ggtext::geom_richtext(
    ggplot2::aes(
      label = paste0(
        "<b><i>UEL, CL, LEL are based on the years:  ",
        min(df1_control_limits$year),
        " - ",
        max(df1_control_limits$year),
        "</i></b>"
      ),
      x = x_pos,
      y = y_pos
    ),
    size = 6, # Font size (adjusted to a reasonable level)
    color = "black",
    hjust = -.40, # Align near center
    vjust = -5, # Align above the plot
    fill = "lightblue",
    label.color = "black"
  ) +

  # Disclose CL linetype
  ggtext::geom_richtext(
    ggplot2::aes(
      label = paste0(
        "<b><i>If run rules violated, centerline will be dashed line",
        "</i></b>"
      ),
      x = max(df1_modified$year, na.rm = TRUE), # Ensure NA handling
      y = min(df1_modified$value, na.rm = TRUE) # Ensure NA handling
    ),
    size = 6, # Font size (adjusted to a reasonable level)
    color = "black",
    hjust = 1, # Align near center
    vjust = 3.0, # Align above the plot
    fill = "pink",
    label.color = "black"
  ) +

  # Add padding to both x and y axes in I-chart
  ggplot2::scale_x_continuous(
    breaks = unique(df1_modified$year),
    expand = c(0.15, 0.00),
  ) +

  ggplot2::scale_y_continuous(
    labels = function(y) {
      format(y, scientific = FALSE, big.mark = ",")
    },
    expand = c(0.25, 0.25) # Add padding on both sides of y-axis on i_chart
  )

# Theme and title the plot object i_chart

i_chart <- i_chart +
  ggplot2::theme(
    axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      color = "darkgreen",
      face = "bold",
      fill = "yellow",
      lineheight = 1.0,
      padding = ggplot2::margin(5.5, 5.5, 0.0, 5.5),
      margin = ggplot2::margin(0, 0, 5.5, 0)
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      color = "darkgreen",
      face = "bold",
      fill = "yellow",
      lineheight = 1.0,
      padding = ggplot2::margin(0.0, 5.5, 5.5, 5.5),
      margin = ggplot2::margin(0, 0, 5.5, 0)
    ),
    plot.caption = ggtext::element_markdown(
      family = "Cabin",
      color = "darkblue",
      face = "italic",
      fill = "yellow",
      lineheight = 1.0
    ),
    strip.text = ggtext::element_markdown(
      color = "orange",
      size = ggplot2::rel(1.1),
      face = "italic",
      margin = ggplot2::margin(2, 0, 0.5, 0, "lines")
    ),
    axis.text = ggtext::element_markdown(color = "black"),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.background = ggplot2::element_rect(fill = "white", color = NA)
  )


# Add labels for the first, middle, and last values of UEL, LCL, and centerline

i_chart <- i_chart +

  ggplot2::geom_text(
    ggplot2::aes(
      x = dplyr::first(year), # dplyr function to set the x position of text
      y = dplyr::first(emp_cl), # dplyr function to set the y position of the text
      label = "CenterLine", # format(round(dplyr::first(emp_cl), 1   # dplyr function to round first value in 'centerline' column
    ),
    color = "blue",
    vjust = -1,
    hjust = 1.1,
    size = 5
  ) +

  ##### To add a measurement label at the mid-point of the center line

  # ggplot2::geom_text(aes(x = stats::median(year),                           # stats function to set the x position of text
  #               y = stats::median(emp_cl),                     # stats function to set the y position of text
  #               label = format(round(stats::median(emp_cl), 1),   # stats function to round median value in 'centerline' column
  #                              big.mark = ","
  #                              )
  #               ),
  #           color = "blue",
  #           vjust = 1,
  #           size = 5
  #           ) +

  ggplot2::geom_text(
    ggplot2::aes(
      x = dplyr::last(year), # dplyr function to set the x position of text
      y = dplyr::last(emp_cl), # dplyr function to set the y position of the text
      label = format(
        round(
          dplyr::last(emp_cl),
          1 # dplyr function to round last value in 'centerline' column
        ),
      )
    ),
    color = "blue",
    vjust = 0,
    hjust = -0.1,
    size = 5
  ) +

  ggplot2::geom_text(
    ggplot2::aes(
      x = dplyr::first(year) + 0.5, # dplyr function to set the x position of text
      y = dplyr::first(emp_ucl), # dplyr function to set the y position of the text
      label = "Upper Expectation Limit", #format(round(dplyr::first(emp_ucl), 1   # dplyr function to round first value in 'ucl' column
    ),
    color = "red",
    vjust = -1,
    hjust = 1.1,
    size = 5
  ) +

  ggplot2::geom_text(
    ggplot2::aes(
      x = dplyr::last(year), # dplyr function to set the x position of text
      y = dplyr::last(emp_ucl), # dplyr function to set the y position of text
      label = format(
        round(
          dplyr::last(emp_ucl),
          1 # dplyr function to round last value in 'ucl' column
        ),
      )
    ),
    color = "red",
    vjust = -1,
    hjust = -0.1,
    size = 5
  ) +

  ggplot2::geom_text(
    ggplot2::aes(
      x = dplyr::first(year) + .5, # dplyr function to set the x position of text
      y = dplyr::first(emp_lcl), # dplyr function to set the y position of text
      label = "Lower Expectation Limit" # format(round(dplyr::first(emp_lcl), 1     # dplyr function to round first value in 'ucl' column
    ),
    color = "red",
    vjust = 1.75,
    hjust = 1.1,
    size = 5
  ) +

  ggplot2::geom_text(
    ggplot2::aes(
      x = dplyr::last(year), # dplyr function to set the x position of text
      y = dplyr::last(emp_lcl), # dplyr function to set the y position of text
      label = format(round(dplyr::last(emp_lcl), 1)),
    ),
    color = "red",
    vjust = 1.5,
    hjust = -0.1,
    size = 5
  )


# Display the chart
print(i_chart)

#####################

# print(section_unique)
# [1] "District_Perform"    "Expenses"            "Fund_Balances"       "SAT_ACT"
# [5] "Staff"               "Standardized_Scores" "Students"            "Taxes_and_Revenues"
# [9] "Teachers"

#####################

#  print(grouping_unique)
#  [1] "Total Students in District"
#  [2] "Total Number of Schools"
#  [3] "Attendance Rate"
#  [4] "Completion Rate"
#  [5] "Annual Graduate Count"
#  [6] "Annual RHSP-DAP-FHSP-E-FHSP-DLA Graduate Count"
#  [7] "4-Year Longitudinal Graduation Rate"
#  [8] "5-Year Longitudinal Graduation Rate"
#  [9] "6-Year Longitudinal Graduation Rate"
# [10] "Annual Dropout Rate"
# [11] "Annual Dropout Rate Gr 9-12"
# [12] "Four-year Dropout Rate"
# [13] "Exit-Level Cumulative Pass Rate"
# [14] "Instructional"
# [15] "Central Administrative"
# [16] "Campus Administrative-School Leadership"
# [17] "Plant Services"
# [18] "Other Operating"
# [19] "Non-Operating"
# [20] "Total Expenditures"
# [21] "Total Instructional Expenditures"
# [22] "Total Instructional ExpendPer Pupil"
# [23] "Total Operating Expenditures"
# [24] "Total Operating Expend Per Pupil"
# [25] "Regular Education"
# [26] "Un-Allocated Expenditures"
# [27] "Special Education"
# [28] "Compensatory-Accelerated Education"
# [29] "Bilingual-ESL Education"
# [30] "Career and Technology Education"
# [31] "Gifted and Talented Education"
# [32] "Athletics-Related Activities"
# [33] "High School Allotment"
# [34] "Prekindergarten"
# [35] "Fund Balance-EOY"
# [36] "Fund Balance of Budget"
# [37] "Percent Tested"
# [38] "Percent At Or Above Criterion"
# [39] "SAT-Mean Total Score"
# [40] "ACT-Mean Composite Score"
# [41] "Number of Students Per Total Staff"
# [42] "Number of Students Per Teacher"
# [43] "Total Staff FTE"
# [44] "Total Teacher FTE"
# [45] "Average Central Administr Salary"
# [46] "Average School Administr Salary"
# [47] "Average Profess Support Staff Salary"
# [48] "Average Teacher Salary"
# [49] "Minority"
# [50] "School Administrative"
# [51] "Professional Support Staff"
# [52] "Teachers"
# [53] "Educational Aides"
# [54] "Auxiliary Staff"
# [55] "White"
# [56] "Hispanic"
# [57] "African American"
# [58] "American Indian"
# [59] "Asian"
# [60] "Pacific Islander"
# [61] "2 or More Races"
# [62] "Economically Disadvantaged"
# [63] "SDAA Met ARD"
# [64] "All Subjects"
# [65] "Reading-ELA"
# [66] "Writing"
# [67] "Mathematics"
# [68] "Science"
# [69] "Social Studies"
# [70] "English Language Learners (ELL)"
# [71] "Career and Technology Ed"
# [72] "Gifted and Talented Ed"
# [73] "State"
# [74] "Local and Other"
# [75] "Federal"
# [76] "Taxable Value Per Pupil"
# [77] "Locally Adopted Tax Rate"
# [78] "Total Revenue"
# [79] "Total Revenue Per Pupil"
# [80] "Total Operating Revenue"
# [81] "Total Other Revenue"
# [82] "Two or More Races"
# [83] "With 5 or Fewer Years Experience"
# [84] "Average Years of Experience"
# [85] "With Advanced Degrees"
# [86] "Teacher Turnover Rate"
# [87] "Compensatory Education"
