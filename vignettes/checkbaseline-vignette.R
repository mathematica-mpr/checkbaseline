## ----results='asis'------------------------------------------------------
data("fake.df")
lm1 <- lm(post.test ~ Treatment + V1 + V2, data = fake.df)
stargazer::stargazer(lm1, type='html', style = "aer")


## ----fig.height=4, fig.width=6-------------------------------------------
CheckBaseline(
  raw.DF = fake.df,
  treatment = 'Treatment',
  variables = c('V2', 'V1', 'pre.test')
)


## ---- fig.height=4, fig.width=6------------------------------------------
data("fake.df.matched")
X <- CheckBaseline(
  raw.DF = fake.df,
  matched.DF = fake.df.matched,
  treatment = 'Treatment',
  variables = c('V2', 'V1', 'pre.test')
)
X$propensity.plot
X$baseline.plot

## ----results='asis'------------------------------------------------------
lm2 <- lm(post.test ~ Treatment + V1 + V2, data = fake.df.matched, weights = weights)
stargazer::stargazer(lm1, lm2, type='html', style = "aer")

