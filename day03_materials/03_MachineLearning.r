
library( tidyverse )

library( tidymodels )

library( GGally )

library( ggfortify )

funkydata <- read_csv( 'funkydata.csv')

funkydata$Y <- factor( funkydata$Y  )

funkydata %>% ggpairs( aes( color=Y ) )

options(repr.plot.width=10, repr.plot.height=10)

read_csv( 'unequal_variance_data.csv' ) %>%
    mutate( Y=factor(Y) ) %>%
    ggpairs( aes( color=Y, alpha=0.1 ) )

set.seed( 42 )
data_splitter <- initial_split( funkydata, prop=0.8 )
train_data <- training( data_splitter )
test_data <- testing( data_splitter )

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

coef( model1 )

exp( coef( model1 ) )

Ypred_test <- predict( model1, test_data )

head( Ypred_test )

Ypred_test <- predict( model1, test_data, type='response')

head(Ypred_test)

# In class activity 1: How to get test prediction accuracy?

# Homework: How to get four-square confusion matrix of TP/FP/FN/TN?

install.packages( "ROCit" )

library(ROCit)

ROCit_obj <- rocit( score = Ypred_test, class = test_data$Y )

options(repr.plot.width=4, repr.plot.height=4)

plot(ROCit_obj)

test_predictions <- logistic_reg() %>%
    set_engine( "glm" ) %>% 
    fit( Y ~ Gaussian1 + Gaussian2, train_data ) %>%
    predict( test_data ) %>%
    bind_cols( test_data )

head( test_predictions )

#model_metrics <- metric_set( accuracy )
#test_predictions %>% 
#    model_metrics

rand_forest_model <- rand_forest() %>%
    set_engine( "ranger" )

rf_fit <- rand_forest_model %>% 
    fit( Y ~ Gaussian1 + Gaussian2, train_data )

rf_fit

#rf_fit %>%    predict( test_data )

test_predictions <- boost_tree() %>%
    set_engine( "xgboost" ) %>% 
    fit( Y ~ Gaussian1 + Gaussian2, train_data ) %>%
    predict( test_data ) %>%
    bind_cols( test_data )

test_predictions
