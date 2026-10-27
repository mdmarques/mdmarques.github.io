#### nzavs-longitudinal-syntax-sleep-mental-health-ri-clpm 

### Marques, M.D., Wright, B.J., Lee, C.H.J., & Sibley, C.G. (2021).
### Increased sleep predicts annual decreases in psychological distress:
### Results from a six-year longitudinal panel sample. Sleep Health,
### 78(3), 368-374. https://doi.org/10.1016/j.sleh.2020.12.005 

### note x = sleep , y = k6distress

# Sleep (Hours sleep)
# During the past month, on average, how many hours of actual sleep did you get per night?

# Kessler-6 (0 = None of the time, 1 = A little of the time, 2 = Some of the time, 3 = Most of the time, 4 = All of the time)
# During the last 30 days, how often did.... you feel hopeless?
#  During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
#  During the last 30 days, how often did.... you feel restless or fidgety?
#  During the last 30 days, how often did.... you feel that everything was an effort?
#  During the last 30 days, how often did.... you feel worthless?
#  During the last 30 days, how often did.... you feel nervous?

S <- matrix(
  c(
    # x1    x2      x3      x4      x5      x6      y1      y2      y3      y4      y5      y6
    1.431,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,
    0.879,	1.495,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,
    0.798,	0.878,	1.382,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,
    0.772,	0.820,	0.874,	1.400,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,
    0.729,	0.790,	0.843,	0.874,	1.427,	NA,	NA,	NA,	NA,	NA,	NA,	NA,
    0.706,	0.741,	0.805,	0.822,	0.894,	1.301,	NA,	NA,	NA,	NA,	NA,	NA,
    -0.091,	-0.092,	-0.084,	-0.088,	-0.098,	-0.092,	0.458,	NA,	NA,	NA,	NA,	NA,
    -0.075,	-0.107,	-0.088,	-0.084,	-0.106,	-0.095,	0.324, 0.453,	NA,	NA,	NA,	NA,
    -0.079,	-0.105,	-0.116,	-0.096,	-0.112,	-0.098,	0.316,	0.335,	0.454,	NA,	NA,	NA,
    -0.066,	-0.094,	-0.096,	-0.115,	-0.112,	-0.100,	0.317,	0.332,	0.345,	0.484,	NA,	NA,
    -0.079,	-0.095,	-0.099,	-0.109,	-0.137,	-0.106,	0.307,	0.320,	0.325,	0.349,	0.462,	NA,
    -0.075,	-0.097,	-0.091,	-0.105,	-0.118,	-0.126,	0.308,	0.320,	0.329,	0.350,	0.359,	0.485
  ),
  nrow = 12,
  ncol = 12,
  byrow = TRUE
)

# Mirror the lower triangle into the upper triangle
S[upper.tri(S)] <- t(S)[upper.tri(S)]

colnames(S) <- rownames(S) <- c(
  "x1", "x2", "x3", "x4", "x5", "x6",
  "y1", "y2", "y3", "y4", "y5", "y6"
)

N <- 58347

means <- c(
  x1 = 7.005, x2 = 6.923, x3 = 6.902, x4 = 6.9, x5 = 6.898, x6 = 6.933,
  y1 = 0.891, y2 = 0.882, y3 = 0.888, y4 = 0.916, y5 = 0.898, y6 = 0.909
)

library(lavaan)

### Random Intercepts Cross Lagged Panel Model
riclpm6_model <- '

  # --------------------------------------------------
  # Create between components
  # Random intercepts
  # note x = sleep , y = k6distress
  # --------------------------------------------------

  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6


  # --------------------------------------------------
  # Create within-person centered variables
  # Within-person factors
  # --------------------------------------------------

  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wx6 =~ 1*x6

  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  wy6 =~ 1*y6


  # --------------------------------------------------
  # Measurement-error variances fixed to zero
  # --------------------------------------------------

  x1 ~~ 0*x1
  x2 ~~ 0*x2
  x3 ~~ 0*x3
  x4 ~~ 0*x4
  x5 ~~ 0*x5
  x6 ~~ 0*x6

  y1 ~~ 0*y1
  y2 ~~ 0*y2
  y3 ~~ 0*y3
  y4 ~~ 0*y4
  y5 ~~ 0*y5
  y6 ~~ 0*y6


  # --------------------------------------------------
  # Estimate lagged effects between within-person centered variables
  # Note these are constrained lagged effects
  # --------------------------------------------------

  wx2 ~ a*wx1 + b*wy1
  wx3 ~ a*wx2 + b*wy2
  wx4 ~ a*wx3 + b*wy3
  wx5 ~ a*wx4 + b*wy4
  wx6 ~ a*wx5 + b*wy5

  wy2 ~ c*wx1 + d*wy1
  wy3 ~ c*wx2 + d*wy2
  wy4 ~ c*wx3 + d*wy3
  wy5 ~ c*wx4 + d*wy4
  wy6 ~ c*wx5 + d*wy5


  # --------------------------------------------------
  # Estimate covariance between within-person centered variables at first wave
  # Covariance at Wave 1
  # --------------------------------------------------

  wx1 ~~ wy1


  # --------------------------------------------------
  # Estimate covariances between residuals of within-person centered variables 
  # Correlated innovations
  # --------------------------------------------------

  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  wx6 ~~ wy6


  # --------------------------------------------------
  # Random intercepts orthogonal to Wave-1
  # within-person factors
  # --------------------------------------------------

  RIx ~~ 0*wx1
  RIx ~~ 0*wy1
  RIy ~~ 0*wx1
  RIy ~~ 0*wy1


  # --------------------------------------------------
  # Random-intercept variances and covariance
  # --------------------------------------------------

  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy


  # --------------------------------------------------
  # Within-person variances; Residual variances 
  # --------------------------------------------------

  wx1 ~~ wx1 #Variances
  wy1 ~~ wy1

  wx2 ~~ wx2 #Residual variances
  wy2 ~~ wy2

  wx3 ~~ wx3
  wy3 ~~ wy3

  wx4 ~~ wx4
  wy4 ~~ wy4

  wx5 ~~ wx5
  wy5 ~~ wy5

  wx6 ~~ wx6
  wy6 ~~ wy6

  # --------------------------------------------------
  # Granger causality; Is b (x ~ y) different from c (y ~ x)
  # --------------------------------------------------

diff_bc := b - c
'

fitriclpm6 <- lavaan(
  model = riclpm6_model,
  sample.cov = S,
  sample.mean = means,
  sample.nobs = N,
  sample.cov.rescale = TRUE,
  meanstructure = TRUE,
  estimator = "ML",
  int.ov.free = TRUE,
  auto.var = FALSE,
  auto.cov.lv.x = FALSE
)

summary(
  fitriclpm6,
  fit.measures = TRUE,
  standardized = TRUE
)


### Cross Lagged Panel Model

clpm6_model <- '

  # --------------------------------------------------
  # Create between components
  # Random intercepts
  # note x = sleep , y = k6distress
  # --------------------------------------------------

  RIx =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6
  RIy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6


  # --------------------------------------------------
  # Create within-person centered variables
  # Within-person factors
  # --------------------------------------------------

  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3
  wx4 =~ 1*x4
  wx5 =~ 1*x5
  wx6 =~ 1*x6

  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  wy4 =~ 1*y4
  wy5 =~ 1*y5
  wy6 =~ 1*y6


  # --------------------------------------------------
  # Measurement-error variances fixed to zero
  # --------------------------------------------------

  x1 ~~ 0*x1
  x2 ~~ 0*x2
  x3 ~~ 0*x3
  x4 ~~ 0*x4
  x5 ~~ 0*x5
  x6 ~~ 0*x6

  y1 ~~ 0*y1
  y2 ~~ 0*y2
  y3 ~~ 0*y3
  y4 ~~ 0*y4
  y5 ~~ 0*y5
  y6 ~~ 0*y6


  # --------------------------------------------------
  # Estimate lagged effects between within-person centered variables
  # Note these are constrained lagged effects
  # --------------------------------------------------

  wx2 ~ a*wx1 + b*wy1
  wx3 ~ a*wx2 + b*wy2
  wx4 ~ a*wx3 + b*wy3
  wx5 ~ a*wx4 + b*wy4
  wx6 ~ a*wx5 + b*wy5

  wy2 ~ c*wx1 + d*wy1
  wy3 ~ c*wx2 + d*wy2
  wy4 ~ c*wx3 + d*wy3
  wy5 ~ c*wx4 + d*wy4
  wy6 ~ c*wx5 + d*wy5


  # --------------------------------------------------
  # Estimate covariance between within-person centered variables at first wave
  # Covariance at Wave 1
  # --------------------------------------------------

  wx1 ~~ wy1


  # --------------------------------------------------
  # Estimate covariances between residuals of within-person centered variables 
  # Correlated innovations
  # --------------------------------------------------

  wx2 ~~ wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  wx5 ~~ wy5
  wx6 ~~ wy6


  # --------------------------------------------------
  # Random intercepts orthogonal to Wave-1
  # within-person factors
  # --------------------------------------------------

  RIx ~~ 0*wx1
  RIx ~~ 0*wy1
  RIy ~~ 0*wx1
  RIy ~~ 0*wy1


  # --------------------------------------------------
  # Random-intercept variances and covariance
  # --------------------------------------------------

  RIx ~~ 0*RIx
  RIy ~~ 0*RIy
  RIx ~~ 0*RIy


  # --------------------------------------------------
  # Within-person variances; Residual variances 
  # --------------------------------------------------

  wx1 ~~ wx1 #Variances
  wy1 ~~ wy1

  wx2 ~~ wx2 #Residual variances
  wy2 ~~ wy2

  wx3 ~~ wx3
  wy3 ~~ wy3

  wx4 ~~ wx4
  wy4 ~~ wy4

  wx5 ~~ wx5
  wy5 ~~ wy5

  wx6 ~~ wx6
  wy6 ~~ wy6
  
  # --------------------------------------------------
  # Granger causality; Is b (x ~ y) different from c (y ~ x)
  # --------------------------------------------------

diff_bc := b - c
'

fitclpm6 <- lavaan(
  model = clpm6_model,
  sample.cov = S,
  sample.mean = means,
  sample.nobs = N,
  sample.cov.rescale = TRUE,
  meanstructure = TRUE,
  estimator = "ML",
  int.ov.free = TRUE,
  auto.var = FALSE,
  auto.cov.lv.x = FALSE
)

# assuming you have original data then you fit the model as you would any typical SEM
# e.g., fitclpm6 <- sem(clpm6_model, data = dat)

summary(
  fitclpm6,
  fit.measures = TRUE,
  standardized = TRUE
)