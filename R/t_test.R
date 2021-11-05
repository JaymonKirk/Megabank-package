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

hr_mean <- mean(megabank_data$netball_skills[megabank_data$department == "hr"])
cs_mean <- mean(megabank_data$netball_skills[megabank_data$department == "cs"])

t_test_output <- t.test(megabank_data$netball_skills[megabank_data$department == "hr"],megabank_data$netball_skills[megabank_data$department == "cs"])

bar_graph <- ggplot(data = megabank_data, aes(x= netball_skills, fill = department)) +
  geom_bar(stat = "count", position = "dodge", color = "black")+
  labs(
    x = "Netball skills",
    y = "Count",
    title = "Netball Skills by Department",
    caption = "
    Human Resources mean = 3.73, Customer Service mean = 2.33

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

effect_size <- cohen.d(megabank_data$netball_skills[megabank_data$department == "hr"],megabank_data$netball_skills[megabank_data$department == "cs"])

plot(bar_graph)
}

#Something to take the R output from the t-test and to make it pretty
#something to put the output into laymans terms
#collate all the output (i.e., plots and text and everything)
