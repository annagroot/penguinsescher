#' Plot penguin variable
#'
#' Plots a histogram of the selected attribute for each of the palmerpenguins species.
#' @param data Loads data set of penguin statistics
#' @param variable_name Select which variable you want summarized
#' @return Returns a histogram (ggplot) of each species based on a specified numeric variable
#' @examples
#' Plot the distribution of 'body_mass_g' for each penguin species
#' plot_species_distribution(palmerpenguins::penguins, "body_mass_g")
#' @export

# Function to create a histogram for each species based on a specified numeric variable
plot_species_distribution <- function(data, variable_name) {

  require(ggplot2)

  ggplot(data, aes_string(x = variable_name, fill = "species")) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~species, scales = "free_y") +
    labs(title = paste("Distribution of", variable_name, "by Species"),
         x = variable_name,
         y = "Frequency") +
    theme_minimal()
}
