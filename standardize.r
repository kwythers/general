options(digits=2)

student <- c("John Smith", "Angela Jones", "Bullwinkle Moose", "Rocky Racoon",
              "Janice Smith", "Micky Mouse", "Road Runner", "Wile Coyote", 
              "Bugs Bunny", "Elmer Fud")
math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
english <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 30)
roster <- data.frame(student, math, science, english, stringsAsFactors=FALSE)

z <- scale(roster[,2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)
y <- quantile(score, c(.8, .6, .4, .2))
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

names <- strsplit(roster$student, " ")
lastname <- sapply(names, "[", 2)
firstname <- sapply(names, "[", 1)
roster <- cbind(firstname, lastname, roster[, -1])
roster <- roster[order(lastname, firstname), ]
