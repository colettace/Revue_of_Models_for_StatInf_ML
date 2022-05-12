#install.packages( "tictoc" )
library( tictoc )

# This took ~ 7 minutes for me to complete in Google Colab
# tic()
# install.packages( c(
#     "tidymodels",
#     "GGally",
#     "ggfortify",
#     "ROCit",
#     "ranger",
#     "xgboost"
# ))
# toc()

library( tidyverse )
library( tidymodels )
library( GGally )
library( ggfortify )
library( ROCit )

funkydata <- read_csv( 'https://raw.githubusercontent.com/colettace/Revue_of_Models_for_StatInf_ML/master/day04_materials/funkydata.csv' )

dim( funkydata )

head( funkydata )

funkydata$Y <- factor( funkydata$Y  )

options(repr.plot.width=10, repr.plot.height=10)
funkydata %>% ggpairs( aes( color=Y ) )

read_csv( 'https://raw.githubusercontent.com/colettace/Revue_of_Models_for_StatInf_ML/master/day04_materials/unequal_variance_data.csv' ) %>%
    mutate( Y=factor(Y) ) %>%
    ggpairs( aes( color=Y, alpha=0.1 ) )

set.seed( 42 )
data_splitter <- initial_split( funkydata, prop=0.8 )
train_data <- training( data_splitter )
test_data <- testing( data_splitter )

dim( train_data )

dim( test_data )

model0 <- glm( Y ~ 1, train_data, family='binomial' )

summary( model0 )

model1 <- glm( Y ~ Gaussian1 + Gaussian2, train_data, family='binomial' )

summary( model1 )

anova( model0, model1 )

augmented_funky1 <- augment( model1 )

augmented_funky1 %>% head 

options(repr.plot.width=4, repr.plot.height=3)

augmented_funky1 %>% ggplot( aes( x=.fitted, fill=Y) ) + geom_density( alpha=0.5 )

Ypred_test <- predict( model1, test_data )

qplot( Ypred_test, geom='density', fill=test_data$Y, alpha=0.5 )

summary( model1 )

coef( model1 )

exp( coef( model1 ) )

train_data %>% 
    select( Gaussian1, Gaussian2, Y ) %>%
    ggpairs( aes( fill=Y ) )

Ypred_test <- predict( model1, test_data )

head( Ypred_test )

Ypred_test <- predict( model1, test_data, type='response' )

head(Ypred_test)

length( Ypred_test )

head( Ypred_test )

mean( as.numeric( Ypred_test > 0.5 ) == test_data$Y )

# In class activity 1: How to get test prediction accuracy?

# Homework: How to get four-square confusion matrix of TP/FP/FN/TN?

ROCit_obj <- rocit( score = Ypred_test, class = test_data$Y )

options(repr.plot.width=4, repr.plot.height=4)

plot(ROCit_obj)

glm_fit <- logistic_reg() %>%
    set_engine( "glm" ) %>% 
    fit( Y ~ Gaussian1 + Gaussian2, train_data )

glm_fit

glm_test_predictions <- glm_fit %>%
    predict( test_data ) %>%
    bind_cols( test_data )

sample_n( glm_test_predictions, 7 )

glm_test_predictions %>%
    yardstick::metrics( Y, .pred_class )

rf_fit <- rand_forest() %>%
    set_engine( "ranger" ) %>%
    set_mode( "classification" ) %>% 
    fit( Y ~ Gaussian1 + Gaussian2, train_data )

rf_fit

rf_test_predictions <- rf_fit %>%
    predict( test_data ) %>%
    bind_cols( test_data )

sample_n( rf_test_predictions, 7 )

rf_test_predictions %>%
    yardstick::metrics( Y, .pred_class )

xgb_fit <- boost_tree() %>%
    set_engine( "xgboost" ) %>%
    set_mode( "classification" ) %>% 
    fit( Y ~ Gaussian1 + Gaussian2, train_data )

xgb_fit

xgb_test_predictions <- xgb_fit %>%
    predict( test_data ) %>%
    bind_cols( test_data )

sample_n( xgb_test_predictions, 7 )

xgb_test_predictions %>%
    yardstick::metrics( Y, .pred_class )

mean( as.character(xgb_test_predictions$.pred_class) == 
 as.character(xgb_test_predictions$Y) )
