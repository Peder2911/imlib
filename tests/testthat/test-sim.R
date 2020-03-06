
test_that("sim works",{
   model <- lm(mpg ~ hp + disp + wt+ qsec, mtcars) 
   testset <- makeTestSet(mtcars,list(
      hp = seq(min(mtcars$hp),max(mtcars$hp),length.out = 200)
   ), mean)
   dataset <- cbind(testset,sim(testset,model,n=50))

   expect_equal(nrow(dataset),200)
   expect_equal(ncol(dataset),ncol(testset)+3)

   expect_true(all(dataset$sim_lower < dataset$sim_mean))
   expect_true(all(dataset$sim_upper > dataset$sim_mean))
   expect_lt(
      abs(mean(dataset$sim_mean)-mean(mtcars$mpg)),
      1)
})
