# uniqueExtendedIDs <- sampleData %>%
#   dplyr::count(extended_id) %>%
#   dplyr::filter(n == 1) %>%
#   dplyr::select(-n) %>%
#   dplyr::left_join(sampleData)
#
# fitModel <- discordRegression(data = uniqueExtendedIDs, outcome = "flu_2008",
#                               predictors = c("edu_2008", "tnfi_2008"))
#
# bench32_twoPreds <- microbenchmark::microbenchmark(discordRegression(data = uniqueExtendedIDs, outcome = "flu_2008",
#                                                  predictors = c("edu_2008", "tnfi_2008")), times = 10)
#
# bench32_onePreds <-  microbenchmark::microbenchmark(discordRegression(data = uniqueExtendedIDs, outcome = "flu_2008",
#                                                                       predictors = c("tnfi_2008")), times = 10)
#
# profvis::profvis(discordRegression(data = uniqueExtendedIDs, outcome = "flu_2008",
#                   predictors = c("edu_2008", "tnfi_2008")))
#
#
# DT <- as.data.table(uniqueExtendedIDs)
#
# # Speed of slicing -- DT
# microbenchmark::microbenchmark(dplyr = DT %>% slice(1),
#                                DT = DT[1,],
#                                dplyrBase = uniqueExtendedIDs %>% slice(1),
#                                base = uniqueExtendedIDs[1,])
# # slicing is equal
# waldo::compare(DT %>% slice(1),
#                DT[1,])
#
# oneRowDT <- DT[1,]
# oneRow <- uniqueExtendedIDs[1,]
#
# # Speed of selecting USE BASE
# microbenchmark::microbenchmark(dplyr = oneRow %>% select(flu_2008_s1) > oneRow %>% select(flu_2008_s2),
#                                base = oneRow["flu_2008_s1"] > oneRow["flu_2008_s1"],
#                                DT = oneRowDT[,"flu_2008_s1"] > oneRowDT[,"flu_2008_s1"])
# # slicing is equal
# waldo::compare(oneRow %>% select(flu_2008_s1) > oneRow %>% select(flu_2008_s2),
#                oneRow["flu_2008_s1"] > oneRow["flu_2008_s1"])
#
# #compare mutate
#
# microbenchmark::microbenchmark(dplyr = oneRowDT %>% mutate(order = "S1"),
#                                DT = oneRowDT[, order := "S1"],
#                                dplyrBase = oneRow %>% mutate(order = "S1"),
#                                base = transform(oneRow, order = "S1"),
#                                baseDT = transform(oneRowDT, order = "S1"))
#
# waldo::compare(oneRowDT[, order := "S1"],
#                transform(oneRow, order = "S1"))
#
#
# microbenchmark::microbenchmark(original = map_df(1:nrow(uniqueExtendedIDs),
#                                                  ~discord::checkSiblingOrder(uniqueExtendedIDs, outcome = "flu_2008", row = .x)),
#                                updated = map_df(1:nrow(uniqueExtendedIDs),
#                                                 ~checkSiblingOrderUpdating(uniqueExtendedIDs, outcome = "flu_2008", row = .x)))
#
#
# oc <- "flu_2008"
# S1 <- paste0(oc, "_s1")
# oneRowDT[,..S1]
#
#
#
# bench32_twoPredUpdate <- microbenchmark::microbenchmark(regular = discordRegression(data = uniqueExtendedIDs, outcome = "flu_2008",
#                                                                                     predictors = c("edu_2008", "tnfi_2008")),
#                                                         update = discordRegressionUpdating(data = uniqueExtendedIDs, outcome = "flu_2008",
#                                                                           predictors = c("edu_2008", "tnfi_2008")),
#                                                         times = 10)
#
#
# a <- uniqueExtendedIDs %>% as.data.table()
#
# dt <- data.table(math_1 = c(3, 6, 9),
#                  math_2 = c(2, 4, 6),
#                  chem_1 = c(12, 16, 7),
#                  chem_2 = c(18, 13, 8))
# walk(1:nrow(dt), ~ set(dt, i = .x, j = "chem_diff", value = dt[.x, chem_1] - dt[.x, chem_2]))
# dt
#
# addDiff <- function(data, row, var) {
#
#   output <- set(x = data, i = row, j = paste0(var, "_diff"),
#                 value = data[row, paste0(var, "_2")] - data[row, paste0(var, "_1")])
#
#   return(output)
# }
#
#
# testtt <- function(data, row, var) {
#
#   output <- set(x = data, i = row, j = paste0(var, "_diff"),
#                 value = data[row, paste0(var, "_2")] - data[row, paste0(var, "_1")])
#   return(output)
# }
#
# b <- testtt(data = a, 1, var = "occ_2008")
#
#
#
# set(a, i = 1, j = "mutated", value = a[1, "edu_2002_s1"] - a[1, edu_2002_s2])
#
#
# oneLine <- function() {
#   b[1,order] == "S1"
# }
# twoLine <- function() {
#   data <- b[1,]
#   data[,order] == "S1"
# }
#
# microbenchmark::microbenchmark(oneLine(),
#                                twoLine())
#
#
#
