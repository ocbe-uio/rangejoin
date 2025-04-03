set.seed(1883312)
n <- 100L
example <- data.frame(
  case_control = sample(
    c("Case", "Control"), n,
    replace = TRUE, prob = c(1, 2)
  ),
  age = rlnorm(n, meanlog = 3.8, sdlog = 0.15),
  sex = sample(c("Male", "Female"), n, replace = TRUE)
)
save(example, file = "data/example.RData")
