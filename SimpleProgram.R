# Generate data from linear regression model and calculate the least squares vector of coefficients
#####################################################################################################

# Source your functions
source("FunctionsLM.R")

# Model parameters
p = 10 # number of covariates
sigma = 2 # noise standard deviation
beta = c(2, rep(0, p-1)) # true vector of coefficients

# Training data generator
n = 100 # sample size for training data
X = matrix(rnorm(n * p), n, p) # n by p matrix of predictors
# [ToDo] Use generateY function to generate Y for training data with default seed
Y = generateY(X, beta, sigma)

# [ToDo] Use calculateBeta function to calculate beta_LS
beta_LS = calculateBeta(X,Y)

# [ToDo] Use calculateEstimationError to assess the estimation error measured by squared eucledian distance - ||beta - beta_LS||_2^2. Report the error in the comments.
MSE = calculateEstimationError(beta, beta_LS)
#The estimation error is 0.9446748

# Testing data generator
n = 200 # sample size for testing data
Xtest = matrix(rnorm(n * p), n, p) # n by p matrix of covariates
# [ToDo] Use generateY function to generate Ytest for testing data with seed = 678910
Ytest = generateY(Xtest, beta, sigma, seed = 678910)

# [ToDo] Use calculatePredictionError to asses the prediction error on Ytest. Report the error in the comments.
MSPE = calculatePredictionError(Ytest, Xtest, beta_LS)
#The prediction error is 1198.694.

# [ToDo] Use calculatePredictionError to asses the prediction error on Ytest based only on the first covariate. Report the error in the comments.
# Hint: to avoid error of non-conformable arguments, use Xtest[, 1, drop = FALSE]
MSPE_OneCovariate = calculatePredictionError(Ytest, Xtest[, 1, drop = FALSE], beta_LS[1])
#the prediction error is 1057.855