
Y <- sample(LETTERS, 100, replace = TRUE)

plot(1:100, factor(Y), type = "l", yaxt = "n")

axis(2, at = 1:26, labels = LETTERS, cex.axis = 0.75)
