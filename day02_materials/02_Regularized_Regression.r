
# Package "glmnet" contains the LASSO function
# install.packages( 'glmnet' )
library( glmnet )

# Package "glmnetUtils" allows the use of R formulas for
# specifying glmnet models (as opposed to converting to matrices)
#library(devtools)
#install_github("hong-revo/glmnetUtils")
library(glmnetUtils)

# Metapackage "tidyverse" imports libraries 
# for data manipulation (dplyr) and plotting (ggplot2)
library( tidyverse )

# Package "skimr" has excellent descriptive statistics
# function skim_to_wide()
library( skimr )

# Package "GGally" has ggplot2-style scatterplot matrices
library( GGally )

# Package tictoc has functions to time function calls
library( tictoc )

# Library car has variance inflation factor function (vif())
library( car )

library( broom )

options(repr.plot.width=4, repr.plot.height=3)

uniform_noise <- function( min, max ) { 
    runif( n=200, min, max )
}

gaussian_noise <- function( c=1 ) { 
    rnorm( n=200 ) * c
}

set.seed( 42 )

GenerateFake <- function (){
    Y <- uniform_noise( -10, 10 ) + 7
    X1 <- Y + gaussian_noise(1)
    X2 <- Y + gaussian_noise(2)
    X3 <- Y + gaussian_noise(4)
    trash1 <- gaussian_noise(10)
    trash2 <- gaussian_noise(10)
    trash3 <- gaussian_noise(10)
    fake_data <- data.frame( Y, X1, X2, X3, trash1, trash2, trash3 )
    return( fake_data )
}

fake_data <- GenerateFake()

library( rsample )

dim( fake_data )

skim_to_wide( fake_data )

library( GGally )

options(repr.plot.width=6, repr.plot.height=6)

ggpairs( fake_data, aes( alpha=0.1) )

model0 <- lm( Y ~ 1, fake_data )

summary( model0 )

model1 <- lm( Y ~ X1, fake_data )

summary( model1 )

model1 <- lm( Y ~ ., fake_data )

summary( model1 )

vif( model1 )

glmnet_lm_result <- glmnet( Y ~ ., data=fake_data )

print( glmnet_lm_result )

options(repr.plot.width=6, repr.plot.height=4)
plot( glmnet_lm_result, xvar='lambda', label=TRUE )

tic()
glmnet_cv_result <- cv.glmnet( Y ~ ., data=fake_data )
toc()

print( glmnet_cv_result )

glmnet_cv_result$lambda.min

options(repr.plot.width=6, repr.plot.height=5)

log( glmnet_cv_result$lambda.min )

plot( glmnet_cv_result )
