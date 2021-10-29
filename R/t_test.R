#' T test function
#'
#' Runs an independent samples t-test using our Megabank dataset.
#'
#' Takes the variables and returns some plots and results
#'
#' @param x Variable x
#' @param y Variable y
#'
#' @export
t_test <- function(x,y) { #x is netball skills and y is department

mean(x[y == "hr"])
mean(x[y == "cs"])

t.test(x[y == "hr"],x[y == "cs"])

ggplot(data = megabank_data, aes(x= x, fill = y)) +
       geom_bar(stat = "count", position = "dodge", color = "black")

ggplot(data = megabank_data, aes(x= y, y = x))+
  geom_boxplot(col = "black")+
  ggtitle("Netball ability by department")
}

#Something to take the R output from the t-test and to make it pretty
#something to put the output into laymans terms
#collate all the output (i.e., plots and text and everything)

#Function seems to only output the last thing that it sees. So when t.test is last it outputs that and when boxplot is last it outputs that
