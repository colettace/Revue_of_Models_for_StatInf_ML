
# Package "glmnet" contains the LASSO function
# install.packages( 'glmnet' )
library( glmnet )

# Package "mice" is for imputation of missing data
#install.packages( 'mice' )
library( mice )

# Metapackage "tidyverse" imports libraries 
# for data manipulation (dplyr) and plotting (ggplot2)
library( tidyverse )

# Package "readxl" has the read_excel() function
library( readxl )

# Package "skimr" has excellent descriptive statistics
# function skim_to_wide()
library( skimr )

# Package "GGally" has ggplot2-style scatterplot matrices
library( GGally )

# Package "ggfortify" has ggplot2-style regression diagnostic plots
#install.packages( 'ggfortify' )
library( ggfortify )

# Package tictoc has functions to time function calls
library( tictoc )

# Package "glmnetUtils" allows the use of R formulas for
# specifying glmnet models (as opposed to converting to matrices)
#library(devtools)
#install_github("hong-revo/glmnetUtils")
library(glmnetUtils)

# Package "broom" for tidy() function for pulling
# regression coefficients from glmnet models
library( broom )

# Package "rsample" for train/test split utilities
library( rsample )

data <- read_excel( 'NYTowns.xlsx' )

data %>% dim 

data <- data %>% 
    #select( -starts_with( 'Anc') ) %>%
    select( -c( 'GEO_ID', "GEO_NAME", "NAME") )

# A Jupyter Notebook-specific directive
# sets the maximum number of rows to something above what is needed
# to show everything
options( repr.matrix.max.rows=300 )

skim_to_wide( data ) %>% select( -type, -n )

data %>% is.na %>% sum 

tic()
imputation_model <- mice( data, method='cart', m=1, maxit=1 )
toc()

#glimpse( imputation_model )

imputed_data <- mice::complete( imputation_model )

imputed_data %>% is.na %>% sum

set.seed( 42 )
data_splitter <- initial_split( imputed_data, prop=0.8 )
train_data <- training( data_splitter )
test_data <- testing( data_splitter )

glmnet_lm_result <- glmnet( Penetration ~ ., data=train_data )

print( glmnet_lm_result )

options(repr.plot.width=6, repr.plot.height=4)
plot( glmnet_lm_result, xvar='lambda', label=TRUE )
# can also use ggfortify version, which doesn't work very well with many variables
#options(repr.plot.width=30, repr.plot.height=30 )
#autoplot( glmnet_lm_result, label=FALSE )
#plot_glmnet( glmnet_lm_result, xvar='lambda', label=TRUE )

glmnet_lm_result %>%                 # Take the glmnet result object
    coef %>%                         # get all the coefficients for all 100 models (lambdas)
    t %>%                            # transpose so the variables are in the columns
    as.matrix %>% as.data.frame %>%  # convert to something the tidyverse can manipulate
    map_int( ~ sum( . != 0 ) ) %>%   # For each column, return a count
    sort( decreasing = TRUE ) %>%    # sort from highest to lowest
    head( 40 )  %>%                  # show the top 20 used variables
    print

tic()
glmnet_cv_result <- cv.glmnet( Penetration ~ ., data=train_data )
toc()

print( glmnet_cv_result )

glmnet_cv_result$lambda.min

options(repr.plot.width=6, repr.plot.height=5)

log( glmnet_cv_result$lambda.min )

plot( glmnet_cv_result )

lm_coefs <- glmnet_cv_result %>% 
    coef( s = "lambda.min" ) %>%    # Get the betas from the best model
    tidy %>%                        # Put betas into a data.frame
    select( -column ) %>%           # prune unimportant column named "column"
    arrange( desc( abs( value ) ) ) # Sort by absolute value

head( lm_coefs, 30 )

dim( train_data )

lm_ypred <- predict( 
    glmnet_cv_result, test_data, s="lambda.min" ) %>%
    as.numeric # convert predictions to R vector from R matrix

length(lm_ypred)

all_pred_results <- tibble( y=test_data$Penetration, glmnet_linear=lm_ypred )

summary( lm( glmnet_linear ~ y, all_pred_results) )

ggplot( all_pred_results, aes( y, glmnet_linear ) ) + 
    geom_point( alpha=0.3 )

ggplot( all_pred_results, aes( y, glmnet_linear ) ) + 
    geom_point( alpha=0.3 ) + 
    scale_x_continuous(trans = 'log10') + 
    scale_y_continuous(trans = 'log10') +
    annotation_logticks()

tic()
glmnet_cv_result2 <- cv.glmnet( Penetration ~ ., data=train_data, family='poisson' )
toc()

plot( glmnet_cv_result2 )

lm2_ypred <- predict( 
    glmnet_cv_result2, test_data, s="lambda.min", type = "response" ) %>%
    as.numeric # convert to R vector from R matrix

length( lm2_ypred )

all_pred_results <- all_pred_results %>%
    mutate( glmnet_poisson = lm2_ypred )

summary( lm( glmnet_poisson ~ y, all_pred_results) )

all_pred_results %>% 
    gather( key='model', value='Y_pred', -y ) %>%
    ggplot( aes( y, Y_pred, color=model) ) + geom_point()

all_pred_results %>% 
    gather( key='model', value='Y_pred', -y ) %>%
    ggplot( aes( y, Y_pred, color=model) ) + geom_point() +
    scale_x_continuous(trans = 'log10') + 
    scale_y_continuous(trans = 'log10') +
    annotation_logticks()

# remove row marked '(Intercept)'
interesting_linear_vars <- c( setdiff( lm_coefs[1:15,'row'],  '(Intercept)'), 'Penetration' )

interesting_linear_vars

train_data_subset <- train_data %>% select( interesting_linear_vars )

skim_to_wide( train_data_subset )  %>% select( -type, -n )

options(repr.plot.width=10, repr.plot.height=10)
ggpairs( train_data_subset, aes( alpha=0.001 ) )

model0 <- lm( Penetration ~ ., data=train_data_subset )

summary( model0 )

options( repr.plot.width=7.5, repr.plot.height=5 )

autoplot( model0, which = 1:6, ncol=3, alpha=0.1)

all_pred_results <- all_pred_results %>%
    mutate( lm = predict( model0, test_data ) )

all_pred_results %>% 
    gather( key='model', value='Y_pred', -y ) %>%
    ggplot( aes( y, Y_pred, color=model) ) + geom_point() #+
    #scale_x_continuous(trans = 'log10') + 
    #scale_y_continuous(trans = 'log10') +
    #annotation_logticks()

glm_coefs <- glmnet_cv_result2 %>% 
    coef( s = "lambda.min" ) %>%    # Get the betas from the best model
    tidy %>%                        # Put betas into a data.frame
    select( -column ) %>%           # prune unimportant column named "column"
    arrange( desc( abs( value ) ) ) # Sort by absolute value

glm_coefs %>% head(15)

# remove row marked '(Intercept)'
interesting_poisson_vars <- c( setdiff( glm_coefs[1:15,'row'],  '(Intercept)'), 'Penetration' )

data_subset <- train_data %>% select( interesting_poisson_vars )

model1 <- glm( Penetration ~ ., data=data_subset, family='quasipoisson' )

summary( model1 )

glm_preds <- as.numeric( predict( model1, test_data, type='response' ) )

all_pred_results <- all_pred_results %>%
    mutate( glm =  glm_preds )

summary( lm( glm ~ y, all_pred_results ) )

all_pred_results %>% 
    gather( key='model', value='Y_pred', -y ) %>%
    ggplot( aes( y, Y_pred, color=model) ) + geom_point() #+
    #scale_x_continuous(trans = 'log10') + 
    #scale_y_continuous(trans = 'log10') +
    #annotation_logticks()

autoplot( model1, which = 1:6, ncol=3, alpha=0.1)

log_transform_vars <- c( "MortageLT500", "CommuteAtHome", 'BadPlumbing', "BoatRVVan", 'JobAgriculture', "LessEq2Rooms" )

transformed_data_subset <- data_subset %>%
    mutate_at( log_transform_vars, ~ log( . + 1) )

skim_to_wide( transformed_data_subset )

skim_to_wide( data_subset )

model2 <- glm( Penetration ~ ., data=transformed_data_subset, family='quasipoisson' )

summary( model2 )

autoplot( model2, which = 1:6, ncol=3)

anova( model1, model2)
