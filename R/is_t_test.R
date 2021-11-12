#' Independent Samples t test function
#'
#' Runs an independent samples t-test using any data you want and produces a plot and some results in text.
#'
#' Takes the variables and returns some plots and results
#'
#' @param x Variable x. For our purposes, you will enter netball_skills here.
#' @param y Variable y. For our purposes, you will enter department here.
#'
#' @export

is_t_test <- function(data, x, y, x_name = "", y_name = "", plot_title = "") {

#Insert something here to check the data

x_mean <- mean(x)
y_mean <- mean(y)

t_test_results <- t.test(x,y)

#something that calculates the percentage chance that x drawn at random ...

#something that takes the results from above and makes them into text for the plot

plot_caption <- paste(x_name, "mean = ", x_mean, ",", y_name, "mean = ", y_mean)



bar_graph <- ggplot(data, aes(x = x, fill = y)) +
  geom_bar(stat = "count", position = "dodge", color = "black")+
  labs(
    x = "TBA",
    y = "Count",
    title = plot_title,
    caption = "TBA") +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0.5)) +
  scale_fill_discrete(
    name = "TBA",
    labels = c("TBA","TBA")
  )

}
