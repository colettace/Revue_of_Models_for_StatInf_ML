
library( readr )
library( dplyr )
library( GGally )

unequal_var_data <- read_csv( "unequal_variance_data.csv" ) %>% 
    mutate( Y = factor( Y ) )

library( skimr )

skim_to_wide( unequal_var_data )

ggpairs( unequal_var_data, aes( alpha=0.1, color=Y ) )

library(h2o)

h2o.init()

unequal_var_data = as.h2o( unequal_var_data )

class( unequal_var_data )

summary( unequal_var_data )

unequal_splits <- h2o.splitFrame( unequal_var_data, ratios=c(0.5) )

unequal_train <- unequal_splits[[1]]
unequal_test <- unequal_splits[[2]]

dim( unequal_train )

dim( unequal_test )

library(tictoc)

tic()
aml_results <- h2o.automl(
    # x is omitted since we want to use all the columns except "Y" as predictors
    y = 'Y',
    training_frame = unequal_train,
    leaderboard_frame = unequal_test,
    max_runtime_secs = 120,
    exclude_algos = 'GBM',
)
toc()

dim( aml_results@leaderboard )

as.data.frame( aml_results@leaderboard  )

h2o.performance( aml_results@leader )

getParms( aml_results@leader )
# or a synonym:
# aml_results@leader@parameters

Y_pred <- as.data.frame( predict( aml_results@leader, unequal_test ) )

Y_pred

test_data <- as.data.frame( unequal_test )

dim( test_data )

dim( Y_pred )

library( tibble )

prob1 <- Y_pred$p1

aug <- test_data %>% 
    cbind( prob1 ) %>%
    mutate( Y=if_else( as.character( Y ) == '-1', 0, 1 ) ) %>%
    mutate( prob1_round=round( prob1 ) )

aug <- test_data %>% 
    cbind( prob1 ) %>%
    mutate( Y=if_else( as.character( Y ) == '-1', 0, 1 ) ) %>%
    mutate( prob1_round=round( prob1 ) )

aug$class <- 'LC'

aug[ (aug$Y == 1) & (aug$prob1_round == 1), 'class' ] <- 'HC'
aug[ (aug$Y == 1) & (aug$prob1_round == 0), 'class' ] <- 'HI'
aug[ (aug$Y == 0) & (aug$prob1_round == 1), 'class' ] <- 'LI'

aug$class <- factor( aug$class )

options( repr.plot.width=10, repr.plot.height=10 )

aug %>% 
    select( -Y, -prob1, -prob1_round ) %>%
    ggpairs( aes( color=class, alpha=0.1))


