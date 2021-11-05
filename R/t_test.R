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
       geom_bar(stat = "count", position = "dodge", color = "black")

effect_size <- cohen.d(megabank_data$netball_skills[megabank_data$department == "hr"],megabank_data$netball_skills[megabank_data$department == "cs"])

print(hr_mean)
print(cs_mean)
print(t_test_output)
plot(bar_graph)
print(effect_size)
}

#Something to take the R output from the t-test and to make it pretty
#something to put the output into laymans terms
#collate all the output (i.e., plots and text and everything)
