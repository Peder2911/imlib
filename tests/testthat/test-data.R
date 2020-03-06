test_that("Testdata is created properly.", {
   variables <- list(
      hp = quantile(mtcars$hp,c(0,1)), 
      disp = quantile(mtcars$disp ,c(0,0.25,0.5,0.75,1))
   )
   nPermutations <- purrr::reduce(sapply(variables,length), `*`)

   ts <- makeTestSet(mtcars,variables,mean)

   expect_equal(nPermutations, nrow(ts))
   expect_equal(length(unique(ts$hp)),2)
   expect_equal(length(unique(ts$disp)),5)
})

test_that("Testdata raises proper exceptions", {
   variables <- list(
      foo = c(0,1),
      bar = c(0,1) 
   )
   nPermutations <- purrr::reduce(sapply(variables,length), `*`)

   expect_error(makeTestSet(mtcars,variables,mean),
      "Missing variables:\nfoo\nbar")

})
