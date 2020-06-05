library(rvest)

webpage <- read_html("https://bigwest.org/standings.aspx?standings=16")
table <- html_nodes(webpage, ".sidearm-standings-table .hide-on-medium-down")

standings <- html_text(table) %>% matrix(byrow = T, nrow = 10) %>% as.data.frame()
View(standings)
colnames(standings) <- c("School", "Conf", "CPct", "Conf.Home", "Conf.Away", "Overall", "Pct", "Home", "Away", "Neutral", "Streak")

standings <- standings[-1,]

standings <- html_table(webpage)[[1]][,-c(2,4,5,7)]

