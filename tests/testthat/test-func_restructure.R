
# one categorical IV, one continuous IV
results11<-restructure(data=sample_data2,
                       pid='pairID',
                       num_con=1,
                       continuous_var1_1='height_1',
                       continuous_var1_2='height_2',
                       continuous_var2_1=NULL,
                       continuous_var2_2=NULL,
                       num_cat=1,
                       categorical_var1_1='marriage_1',
                       categorical_var1_2='marriage_2',
                       categorical_var2_1=NULL,
                       categorical_var2_2=NULL,
                       y_1='DV_1',
                       y_2='DV_2')


# two categorical IV, one continuous IV
results21<-restructure(data=sample_data2,
                       pid='pairID',
                       num_con=2,
                       continuous_var1_1='height_1',
                       continuous_var1_2='height_2',
                       continuous_var2_1='weight_1',
                       continuous_var2_2='weight_2',
                       num_cat=1,
                       categorical_var1_1='marriage_1',
                       categorical_var1_2='marriage_2',
                       categorical_var2_1=NULL,
                       categorical_var2_2=NULL,
                       y_1='DV_1',
                       y_2='DV_2')


# one categorical IV, two continuous IV
results12<-restructure(data=sample_data2,
                       pid='pairID',
                       num_con=1,
                       continuous_var1_1='height_1',
                       continuous_var1_2='height_2',
                       continuous_var2_1=NULL,
                       continuous_var2_2=NULL,
                       num_cat=2,
                       categorical_var1_1='marriage_1',
                       categorical_var1_2='marriage_2',
                       categorical_var2_1='test_1',
                       categorical_var2_2='test_2',
                       y_1='DV_1',
                       y_2='DV_2')


# two categorical IV, two continuous IV
results22<-restructure(data=sample_data2,
                       pid='pairID',
                       num_con=2,
                       continuous_var1_1='height_1',
                       continuous_var1_2='height_2',
                       continuous_var2_1='weight_1',
                       continuous_var2_2='weight_2',
                       num_cat=2,
                       categorical_var1_1='marriage_1',
                       categorical_var1_2='marriage_2',
                       categorical_var2_1='test_1',
                       categorical_var2_2='test_2',
                       y_1='DV_1',
                       y_2='DV_2')

# test how many columns are generated according to the function argument

test_that("restructure returns a data frame. the number of columns depends on the function argument. ", {
  expect_true(ncol(sample_data2)==11) # sample_data2 contains 11 columns.

  expect_true(ncol(results11)==11+2+2+2) # two for mean/difference score of continuous var1, two for two/three grouping variables for categorical var1, and two for mean/difference score of the DV
  expect_true(ncol(results21)==11+2+2+2+2)
  expect_true(ncol(results12)==11+2+2+2+2)
  expect_true(ncol(results22)==11+2+2+2+2+2)

})

# y_1 has to be always higher than y_2.

ex_sample2<-base::subset(sample_data2,DV_1 < DV_2)
ex11<-base::subset(results11,DV_1 < DV_2)
ex12<-base::subset(results12,DV_1 < DV_2)
ex21<-base::subset(results21,DV_1 < DV_2)
ex22<-base::subset(results22,DV_1 < DV_2)

test_that("y_1 should be always higher than y_2 ", {

  expect_false(nrow(ex_sample2)==0) #sample data is not always y_1>y_2
  expect_true(nrow(ex11)==0)
  expect_true(nrow(ex12)==0)
  expect_true(nrow(ex21)==0)
  expect_true(nrow(ex22)==0)
})

# roses are red, means are means


test_that("means are means", {

  #results11
  expect_identical(results11[,"DV_1DV_2_mean"], rowMeans(results11[,c('DV_1','DV_2')]))
  expect_identical(results11[,"height_1height_2_mean"], rowMeans(results11[,c('height_1','height_2')]))

  #results12

  expect_identical(results12[,"DV_1DV_2_mean"], rowMeans(results12[,c('DV_1','DV_2')]))
  expect_identical(results12[,"height_1height_2_mean"], rowMeans(results12[,c('height_1','height_2')]))


  #results21
  expect_identical(results21[,"DV_1DV_2_mean"], rowMeans(results21[,c('DV_1','DV_2')]))
  expect_identical(results21[,"height_1height_2_mean"], rowMeans(results21[,c('height_1','height_2')]))
  expect_identical(results21[,"weight_1weight_2_mean"], rowMeans(results21[,c('weight_1','weight_2')]))


  #results22
  expect_identical(results22[,"DV_1DV_2_mean"], rowMeans(results22[,c('DV_1','DV_2')]))
  expect_identical(results22[,"height_1height_2_mean"], rowMeans(results22[,c('height_1','height_2')]))
  expect_identical(results22[,"weight_1weight_2_mean"], rowMeans(results22[,c('weight_1','weight_2')]))

})


# roses are red, diff are diff


test_that("diff are diffs", {

  #results11
  expect_identical(results11[,"DV_1DV_2_diff"], results11[,'DV_1']- results11[,'DV_2'])
  expect_identical(results11[,"height_1height_2_diff"], results11[,'height_1']-results11[,'height_2'])
  #
  #results12
  expect_identical(results12[,"DV_1DV_2_diff"], results12[,'DV_1']- results12[,'DV_2'])
  expect_identical(results12[,"height_1height_2_diff"], results12[,'height_1']-results12[,'height_2'])
  #
  #
  # #results21
  expect_identical(results21[,"DV_1DV_2_diff"], results21[,'DV_1']- results21[,'DV_2'])
  expect_identical(results21[,"height_1height_2_diff"], results21[,'height_1']-results21[,'height_2'])
  expect_identical(results21[,"weight_1weight_2_diff"], results21[,'weight_1']-results21[,'weight_2'])
  #
  #
  # #results22
  expect_identical(results22[,"DV_1DV_2_diff"], results22[,'DV_1']- results22[,'DV_2'])
  expect_identical(results22[,"height_1height_2_diff"], results22[,'height_1']-results22[,'height_2'])
  expect_identical(results22[,"weight_1weight_2_diff"], results22[,'weight_1']-results22[,'weight_2'])

})


# moving on to lovely categorical variables!


test_that("how many unique values in here? look at the end of the variable name!", {

  #results11

  expect_true(length(unique(results11$marriage_1marriage_2_two))==2)
  expect_true(length(unique(results11$marriage_1marriage_2_three))==3)

  #results12


  expect_true(length(unique(results12$marriage_1marriage_2_two))==2)
  expect_true(length(unique(results11$marriage_1marriage_2_three))==3)

  expect_true(length(unique(results12$test_1test_2_two))==2)
  expect_true(length(unique(results12$test_1test_2_three))==3)
  # results 21

  expect_true(length(unique(results21$marriage_1marriage_2_two))==2)
  expect_true(length(unique(results21$marriage_1marriage_2_three))==3)


  #results22

  expect_true(length(unique(results22$marriage_1marriage_2_two))==2)
  expect_true(length(unique(results22$marriage_1marriage_2_three))==3)

  expect_true(length(unique(results22$test_1test_2_two))==2)
  expect_true(length(unique(results22$test_1test_2_three))==3)

})

# go deep

#results11
results11$marriage_two_test <- ifelse(results11$marriage_1==results11$marriage_2,"same","mixed")
results11$marriage_three_test <- ifelse(results11$marriage_1!=results11$marriage_2,"mixed",results11$marriage_1)

#results12
results12$marriage_two_test <- ifelse(results12$marriage_1==results12$marriage_2,"same","mixed")
results12$marriage_three_test <- ifelse(results12$marriage_1!=results12$marriage_2,"mixed",results12$marriage_1)

results12$test_two_test <- ifelse(results12$test_1==results12$test_2,"same","mixed")
results12$test_three_test <- ifelse(results12$test_1!=results12$test_2,"mixed",results12$test_1)

#results21
results21$marriage_two_test <- ifelse(results21$marriage_1==results21$marriage_2,"same","mixed")
results21$marriage_three_test <- ifelse(results21$marriage_1!=results21$marriage_2,"mixed",results21$marriage_1)

#results22

results22$marriage_two_test <- ifelse(results22$marriage_1==results22$marriage_2,"same","mixed")
results22$marriage_three_test <- ifelse(results22$marriage_1!=results22$marriage_2,"mixed",results22$marriage_1)

results22$test_two_test <- ifelse(results22$test_1==results22$test_2,"same","mixed")
results22$test_three_test <- ifelse(results22$test_1!=results22$test_2,"mixed",results22$test_1)

test_that("same, mixed, or? ", {

  #results11

  expect_identical(results11$marriage_1marriage_2_two,results11$marriage_two_test)
  expect_identical(results11$marriage_1marriage_2_three, results11$marriage_three_test)


  #results12

  expect_identical(results12$marriage_1marriage_2_two,results12$marriage_two_test)
  expect_identical(results12$marriage_1marriage_2_three, results12$marriage_three_test)
  expect_identical(results12$test_1test_2_two, results12$test_two_test)
  expect_identical(results12$test_1test_2_three, results12$test_three_test)

  # results 21
  expect_identical(results21$marriage_1marriage_2_two,results21$marriage_two_test)
  expect_identical(results21$marriage_1marriage_2_three, results21$marriage_three_test)



  #results22
  expect_identical(results22$marriage_1marriage_2_two,results22$marriage_two_test)
  expect_identical(results22$marriage_1marriage_2_three, results22$marriage_three_test)
  expect_identical(results22$test_1test_2_two, results22$test_two_test)
  expect_identical(results22$test_1test_2_three, results22$test_three_test)

})
