# Use this file to create tests/debug your functions

# Source the functions
source("FunctionsLM.R")

#load required testing package
if (!require(testthat)) install.packages('testthat')

#initialize some data
n=50 #sample size
p=2 #number of predictors
X=matrix(rnorm(n*p), n, p) #setup a design matrix with some data
beta = rep(0.3, p) #setup an actual beta vector
sigma = 0.5 #any sigma value for standard deviation

#test generateY

#test with default seed
test_generateY_defaultseed = test_that("Test generateY with default seed", {
  Ytest1 = generateY(X, beta, sigma)
  #make sure the size of Y is as expected
  expect_length(Ytest1,n)
  #call again and verify the same result
  Ytest2 = generateY(X, beta, sigma)
  #check whether both are equal
  expect_equal(Ytest1, Ytest2)
})


#test with custom seed
test_generateY_customseed = test_that("Test generateY with custom seed", {
  Ytest1 = generateY(X, beta, sigma, seed=5050501)
  #make sure the size of Y is as expected
  expect_length(Ytest1,n)
  
  #call again and verify the same result
  Ytest2 = generateY(X, beta, sigma)
  #expect all to be unequal
  expect_false(isTRUE(all.equal(Ytest1, Ytest2)))
  
  #this should pass now since we are using the same seed
  Ytest3 = generateY(X, beta, sigma, seed=5050501)
  #both must be equal here
  expect_equal(Ytest1, Ytest3)
  
  #create a small dataset and betas
  Xsimple = matrix(c(1,1,2,2), nrow=2, ncol=2, byrow = TRUE)
  betasimple = c(0.5, 0.5)
  seed = 1001
  set.seed(1001)
  noise = rnorm(2, 0, sigma)
  Yexpect = as.matrix(c(1+noise[1], 2+noise[2]))
  expect_equal(Yexpect, generateY(Xsimple, betasimple, sigma, seed))
  
})

#test generateY with bad inputs, all should error out
test_generateY_uneq_inputs = test_that("Test generateY with unequal input", {
  #try an X with one less column than what beta has.
  Xbad=matrix(rnorm(n*(p-1)), n, p-1)
  expect_error(generateY(Xbad, beta, sigma))
  #try bad beta with p-1 predictors but X has p columns
  betabad = rep(0.3, p-1)
  expect_error(generateY(X, betabad, sigma))
  
})

#test with matrices
test_generateY_matrixinputs = test_that("Test generateY with matrix inputs", {
  #generate using matrix
  beta_matrix = as.matrix(beta, nrow=p, ncol=1)
  Ytest1 = generateY(X, beta_matrix, sigma)
  #make sure the size of Y is as expected
  expect_length(Ytest1,n)
  #generate using vector
  Ytest2 = generateY(X, beta, sigma)
  #make sure both match
  expect_equal(Ytest1, Ytest2)
})

#test calculateBeta
test_calculateBeta = test_that("Test calculateBeta", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  #check we get the expected number of betas
  expect_length(beta_LS, p)
}
)

test_calculateBeta_uneq_inputs = test_that("Test calculateBeta with unequal inputs",{
  #generate some Y
  Ytest = generateY(X, beta, sigma)
  #setup a bad X with 1 fewer row
  Xbad=matrix(rnorm((n-1)*p, n-1, p))
  #this should fail because of mismatched dimensions
  expect_error(calculateBeta(Xbad, Ytest))
  
  #try the vector way too
  Xbadvec = as.vector(Xbad)
  #this should fail because of mismatched dimensions
  expect_error(calculateBeta(Xbadvec, Ytest))
})

#test with a Y vector instead
test_calculateBeta_vectorinput = test_that("Test calculateBeta with vector input", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #convert to vector to test for this input type
  Yvector = as.vector(Ytest)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  beta_LS_vector = calculateBeta(X,Yvector)
  #check we get the expected number of betas
  expect_length(beta_LS, p)
  #both should be equal, independent of input.
  expect_equal(beta_LS, beta_LS_vector)
})


#test calculateEstimationError
test_calculateEstimationError_vectors = test_that("Test calculateBeta with vectors", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error = calculateEstimationError(beta, beta_LS)
  #check error is >=0
  expect_gte(error, 0)
  #create some betas
  betas1 = c(3,3,2)
  betas2 = c(3,3,-1)
  #check the error value
  expect_equal(calculateEstimationError(betas1, betas2), 9)
}) 

#test calculateEstimationError with vectors that have same values
test_calculateEstimationError_testsamevalues = test_that("Test calculateBeta with vectors with same values", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error = calculateEstimationError(beta_LS, beta_LS)
  #check error is 0
  expect_equal(error,0)
}) 

#test calculateEstimationError with matrices
test_calculateEstimationError_testsmatrices = test_that("Test calculateEstimationError with matrices", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error_matrix = calculateEstimationError(as.matrix(beta, nrow=1, ncol=p), as.matrix(beta_LS, nrow=1, ncol=p))
  error_matrix1 = calculateEstimationError(as.matrix(beta, nrow=1, ncol=p), beta_LS)
  error_vector = calculateEstimationError(beta, beta_LS)
  #check error is >=0
  expect_gte(error_matrix,0)
  #both should be equal, independent of input.
  expect_equal(error_matrix, error_vector)
  expect_equal(error_matrix1, error_vector)
  
  #create some betas
  betas1 = c(3,3,2)
  betas2 = matrix(c(3,3,0), nrow=3, ncol=1)
  #check the error value
  expect_equal(calculateEstimationError(betas1, betas2), 4)
  
  #now flip over the matrix
  betas2 = matrix(c(3,3,0), nrow=1, ncol=3)
  #check the error value
  expect_equal(calculateEstimationError(betas1, betas2), 4)
  
}) 

#test calculateEstimationError with unequal lengths
test_calculateEstimationError_uneq_len = test_that("Test calculateEstimationError with unequal lengths",{
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  #compute the error but drop a value
  expect_error(calculateEstimationError(beta[-1], beta_LS))
})

#test calculateEstimationError with mixed inputs
test_calculateEstimationError_mixedinputs = test_that("Test calculateEstimationError with mixed inputs", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error_vectors = calculateEstimationError(beta, beta_LS)
  error_matrix_vector = calculateEstimationError(as.matrix(beta, nrow=1, ncol=p), beta_LS)
  error_vector_matrix = calculateEstimationError(beta, as.matrix(beta_LS, nrow=1, ncol=p))
  #check for equality of the different pairs of errors
  expect_equal(error_vectors, error_matrix_vector)
  expect_equal(error_vectors, error_vector_matrix)
})

#test calculatePredictionError
test_calculatePredictionError = test_that("Test calculatePredictionError with vectors", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error = calculatePredictionError(Ytest, X, beta_LS)
  #check error is >=0
  expect_gte(error, 0)
}) 

#test calculatePredictionError
test_calculatePredictionError = test_that("Test calculatePredictionError with equal data", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error = calculatePredictionError(Ytest, X, beta)
  #check error is >=0
  expect_gte(error, 0)
}) 

#test calculatePredictionError
test_calculatePredictionError_uneq_data = test_that("Test calculatePredictionError with unequal lengths", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  Xbad=matrix(rnorm((n-1)*p), n-1, p)
  expect_error(calculatePredictionError(Ytest, xbad, beta_LS))
  
  #create another xbad this time with fewer predictors
  Xbad = matrix(rnorm(n*(p-1)), n, p-1)
  expect_error(calculatePredictionError(Ytest, Xbad, beta_LS))
}) 

#test with matrix inputs
test_calculatePredictionError_testsmatrices = test_that("Test calculatePredictionError with matrices", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error = calculatePredictionError(Ytest, X, as.matrix(beta_LS))
  #check error is >=0
  expect_gte(error,0)
}) 

#test with mixed inputs
test_calculatePredictionError_mixedinputs = test_that("Test calculatePredictionError with mixed inputs", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method
  beta_LS = calculateBeta(X,Ytest)
  error_vectors = calculatePredictionError(Ytest, X, beta)
  error_vector_matrix = calculatePredictionError(as.vector(Ytest), X, as.matrix(beta, nrow=p, ncol=1))
  #must be equal, independent of inputs
  expect_equal(error_vectors, error_vector_matrix)
})

#compare with lm
test_calculateBeta_lmcomparison = test_that("Compare with lm coefficients", {
  #generate some Y values
  Ytest = generateY(X, beta, sigma)
  #compute betas using LS method but also use an intercept column
  ones = rep(1, n)
  beta_LS = round(calculateBeta(cbind(ones,X),Ytest),2)
  rownames(beta_LS) = NULL
  #fit a linear model using lm
  mod = lm(Ytest ~ X)
  lm_beta =  round(coef(summary(mod))[,1],2)
  names(lm_beta) = NULL
  #we must get the same betas from lm and our implementation
  expect_equal(as.numeric(beta_LS), lm_beta, tolerance = 1e-4)
  
  #check for MSPE
  xdf = data.frame(X)
  Y = generateY(X, beta, sigma)
  mod = lm(Y ~ 0 + ., data = xdf)
  testdf = data.frame(X)
  pred = predict(mod, testdf)
  #compute mspe for lm
  lmmspe = sum((Ytest-pred)^2)
  #compute mspe for our code
  mspe = calculatePredictionError(Y, X, beta_LS = calculateBeta(X,Y))
  expect_equal(lmmspe, mspe)
})
