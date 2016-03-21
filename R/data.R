#' Fake data before and after matching
"fake.df"
"fake.df.matched"

n <- 1000
set.seed(9782)

treatment <- function(V1, V2, pre.test) {
  t <-
    ifelse(
      test = (V1 == 1 &
                V2 == 1 & pre.test>30),
      yes = sample(c(0, 1), size = 1, prob = c(.7, .3)),
      no = sample(c(0, 1), size = 1, prob = c(.4, .6))
    )
  return(t)
}

pre <- function(V1, V2) {
  t <-
    ifelse(
      test = (V1 == 1 &
                V2 == 1),
      yes = rnorm(1, 50, 2),
      no = rnorm(1, 10, 3)
    )
  return(t)
}

post <- function(T, pre.test) {
  g <-
    ifelse(
      test = T == 1,
      yes = runif(1, 1.1, 1.2) * pre.test,
      no = runif(1, 0.9, 1.1) * pre.test
    )
}

fake.df <-
  data.frame(
    V1 = sample(
      c(0, 1),
      size = n,
      prob = c(.3, .7),
      replace = T
    ),
    V2 = sample(
      c(0, 1),
      size = n,
      prob = c(.6, .4),
      replace = T
    )
  ) %>% rowwise() %>%
  mutate(pre.test = pre(V1, V2)) %>%
  mutate(Treatment = treatment(V1, V2, pre.test)) %>%
  mutate(post.test = post(Treatment, pre.test))


#matching

m.out1 <- MatchIt::matchit(Treatment~V1+V2+pre.test,
                           method = "nearest", data = fake.df, replace = T)
fake.df.matched <- MatchIt::match.data(m.out1)

devtools::use_data(fake.df, fake.df.matched, overwrite = T)

# data("fake.df")
# data("fake.df.matched")
# X <- CheckBaseline(
#   raw.DF = fake.df,
#   matched.DF = fake.df.matched,
#   treatment = 'Treatment',
#   variables = c('V2', 'V1', 'pre.test')
# )
# X$propensity.plot
# X$baseline.plot
#
# lm1 <- lm(post.test ~ Treatment + V1 + V2, data = fake.df)
# lm2 <- lm(post.test ~ Treatment + V1 + V2, data = fake.df.matched)
# stargazer::stargazer(lm1, lm2, type='text')
