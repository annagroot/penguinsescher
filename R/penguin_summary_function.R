#' Summarize penguin data
#'
#' Returns a data frame with summary statistics. Useful for quickly summarising penguins by attributes like flipper length or body mass.
#' @param data Loads data set of penguin statistics
#' @param variable Select which variable you want summarized
#' @return Returns a dataframe of the different aspects of selected variables
#' @examples
#' summarize_species(data = palmerpenguins::penguins,
#'                  variable = flipper_length_mm)
#' @export



# Function to calculate summary statistics for a specified numeric variable by species
summarize_species <- function(data, variable) {

  require(palmerpenguins)
  require(dplyr)

  data %>%
    group_by(species) %>%
    summarise(
      Count = n(),
      Mean = mean({{variable}}, na.rm = TRUE),
      SD = sd({{variable}}, na.rm = TRUE),
      Min = min({{variable}}, na.rm = TRUE),
      Max = max({{variable}}, na.rm = TRUE)
    ) %>%
    ungroup()
}

