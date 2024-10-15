#'@title Summary Statistics
#'@name stats
#'@description Calculate simple statistics by group level
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param ... one or more categorical variables
#'@returns a tibble with n, mean, and sd
#'@import dplyr
#'@examples
#'stats(mtcars, mpg, am)
#'stats(mtcars, mpg, am,vs)



data(mtcars)

stats <- function(data,x,...){
  data %>%
    group_by(...) %>%
    summarize(
      n=n(),
      mean=mean({{x}}),
      sd=sd({{x}})
    )
}
