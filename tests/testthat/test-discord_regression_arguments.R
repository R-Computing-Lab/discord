# Load Utility Function

summarize_results <- function(.results) {
  results_df <- summary(.results)
  results_df <- as.data.frame(results_df$coefficients)
  return(results_df)
}

test_that("flu_2008 ~ edu_2008 + ses_2008 + race + sex", {
  control <- structure(list(Estimate = c(
    -0.336476511133115, 0.687580857991789,
    0.00644800750894167, 0.00373880024292657, -0.00949171887571476,
    0.0177884406432885, -0.0053543222270438, 0.825454572085471, 0.157388475818966
  ), `Std. Error` = c(
    0.499827483023709, 0.283512840454878, 0.0078369264223197,
    0.00743049708015387, 0.0130120268341526, 0.0174031075247567,
    0.188689149462283, 0.604770077676218, 0.187849641457435
  ), `t value` = c(
    -0.673185294049055,
    2.42521946056697, 0.822772495423414, 0.503169599906384, -0.729457370223209,
    1.02214162717685, -0.0283764182641253, 1.36490643726491, 0.837842833224831
  ), `Pr(>|t|)` = c(
    0.507537894178192, 0.0235567340166872, 0.419086477775916,
    0.619629802637589, 0.47308590807872, 0.317345341970847, 0.977606719699604,
    0.185486955990247, 0.410741487291563
  )), class = "data.frame", row.names = c(
    "(Intercept)",
    "flu_2008_mean", "edu_2008_diff", "ses_2008_diff", "edu_2008_mean",
    "ses_2008_mean", "sex_s1FEMALE", "race_s1BLACK", "sex_s2FEMALE"
  ))

  set.seed(18)
  new <- discord_regression(uniqueExtendedIDs,
    outcome = "flu_2008",
    predictors = c("edu_2008", "ses_2008")
  )
  new <- summarize_results(new)

  expect_equal(control, new, tolerance = 0.005)
})


test_that("flu_2008 ~ edu_2008 + ses_2008 + race", {
  control <- structure(list(Estimate = c(
    -0.235124192690362, 0.71099264669166,
    0.00344445018341046, 0.0047070074695217, -0.0082323480232854,
    0.0157167697110711, 0.77281723618361
  ), `Std. Error` = c(
    0.393506309164379,
    0.274208411099014, 0.00677869389732026, 0.00713836177829045,
    0.0125884096850946, 0.0165659209539934, 0.544088952034777
  ), `t value` = c(
    -0.597510604568588,
    2.59289145742115, 0.508128886712544, 0.659396037314457, -0.653962512280879,
    0.948741078429597, 1.42038766509307
  ), `Pr(>|t|)` = c(
    0.55554058136661,
    0.0156773127998133, 0.615817399026833, 0.515670211976789, 0.519106776754122,
    0.351834085782375, 0.167847101797896
  )), class = "data.frame", row.names = c(
    "(Intercept)",
    "flu_2008_mean", "edu_2008_diff", "ses_2008_diff", "edu_2008_mean",
    "ses_2008_mean", "race_s1BLACK"
  ))
  set.seed(18)
  new <- discord_regression(uniqueExtendedIDs,
    outcome = "flu_2008",
    id = "extended_id",
    predictors = c("edu_2008", "ses_2008"),
    sex = NULL
  )
  new <- summarize_results(new)

  expect_equal(control, new, tolerance = 0.005)
})

test_that("flu_2008 ~ edu_2008 + ses_2008", {
  control <- structure(list(Estimate = c(
    0.00380404936125335, 0.719661195795612,
    0.00513781733374805, 0.00110982627288863, -0.00993641796734952,
    0.0122253590396507
  ), `Std. Error` = c(
    0.362634823176579, 0.279453205417903,
    0.0068023543134284, 0.00680335438905165, 0.012773957559715, 0.0167000267364923
  ), `t value` = c(
    0.0104900277583133, 2.57524759724766, 0.755299870752921,
    0.163129275563628, -0.777865271659101, 0.732056255511033
  ), `Pr(>|t|)` = c(
    0.991710400535763,
    0.0160562638509747, 0.456857279310659, 0.871678000847593, 0.443669673352895,
    0.470682083269971
  )), class = "data.frame", row.names = c(
    "(Intercept)",
    "flu_2008_mean", "edu_2008_diff", "ses_2008_diff", "edu_2008_mean",
    "ses_2008_mean"
  ))

  set.seed(18)
  new <- discord_regression(uniqueExtendedIDs,
    outcome = "flu_2008",
    id = "extended_id",
    predictors = c("edu_2008", "ses_2008"),
    sex = NULL,
    race = NULL
  )
  new <- summarize_results(new)

  expect_equal(control, new, tolerance = 0.005)
})




test_that("flu_2008 ~ edu_2008 + ses_2008 + race + sex", {
  control <- structure(list(Estimate = c(
    0.10867561046245, 0.713512189509684,
    0.00646183601232571, 0.000536188139253307, -0.0500697449697817,
    0.510235172081887, 0.143280499220815
  ), `Std. Error` = c(
    0.336004980856874,
    0.244006124961659, 0.00609721346135854, 0.00624512361114144,
    0.184229385996122, 0.53822063216585, 0.183081432533809
  ), `t value` = c(
    0.323434522266031,
    2.92415688180696, 1.05980150658625, 0.0858570898895798, -0.27177936190286,
    0.94800373970922, 0.782605298843487
  ), `Pr(>|t|)` = c(
    0.749056221530932,
    0.00723955814652851, 0.299367283341155, 0.932263950512056, 0.78802351383196,
    0.352201974997601, 0.441209408596208
  )), class = "data.frame", row.names = c(
    "(Intercept)",
    "flu_2008_mean", "edu_2008_diff", "edu_2008_mean", "sex_s1FEMALE",
    "race_s1BLACK", "sex_s2FEMALE"
  ))
  set.seed(18)
  new <- discord_regression(uniqueExtendedIDs,
    outcome = "flu_2008",
    predictors = "edu_2008"
  )
  new <- summarize_results(new)

  expect_equal(control, new, tolerance = 0.005)
})
