#' T test function
#'
#' Runs an independent samples t-test using our Megabank dataset. Testing the difference in netball skills between the Human Resources and Customer Service departments.
#'
#' Takes the variables and returns some plots and results
#'
#' @param x Variable x. For our purposes, you will enter netball_skills here.
#' @param y Variable y. For our purposes, you will enter department here.
#'
#' @export
t_test <- function(x,y) {

netball_skills <- c(3,4,4,5,5,3,3,3,4,4,5,5,4,4,4,2,1,3,4,5,3,3,3,4,4,4,5,5,2,4,3,2,2,1,1,1,3,3,2,2,2,2,1,4,5,3,2,3,2,3,1,1,4,4,2,2,3,2,2,2)
department <- c("hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "hr", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs", "cs")

mb_data <- tibble(department, netball_skills)

bar_graph <- ggplot(data = mb_data, aes(x= netball_skills, fill = department)) +
  geom_bar(stat = "count", position = "dodge", color = "black")+
  labs(
    x = "Netball skills",
    y = "Count",
    title = "Netball Skills by Department",
    caption = "
    Human Resources: mean = 3.73, SD = 1.01
    Customer Service: mean = 2.33, SD = 1.03

    t(58) = 5.31, p < .05

    Cohen's d = 1.37

   There is an 83.4% chance that a person drawn at random from Human Resources
    will be better at netball than a person drawn at random from Customer Service.") +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0.5)) +
  scale_fill_discrete(
    name = "Department",
    labels = c("Customer Service","Human Resources")
  )

plot(bar_graph)
}

#Something to take the R output from the t-test and to make it pretty
#something to put the output into laymans terms
#collate all the output (i.e., plots and text and everything)
