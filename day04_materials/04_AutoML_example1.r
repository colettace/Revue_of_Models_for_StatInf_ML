
library( readr )
library( dplyr )
library( GGally )

funky_data <- read_csv( "funkydata.csv" ) %>% 
    mutate( Y = factor( Y ) )

library( skimr )

skim_to_wide( funky_data )

ggpairs( funky_data, aes( alpha=0.1, color=Y ) )

library(h2o)

h2o.init()

funky_data = as.h2o( funky_data )

class( funky_data )

summary( funky_data )

funky_splits <- h2o.splitFrame( funky_data, ratios=c(0.5) ) 

funky_train <- funky_splits[[1]]
funky_test <- funky_splits[[2]]

dim( funky_train )

dim( funky_test )

library(tictoc)

tic()
aml_results <- h2o.automl(
    # x is omitted since we want to use all the columns except "Y" as predictors
    y = 'Y',
    training_frame = funky_train,
    leaderboard_frame = funky_test,
    max_runtime_secs = 360, # Default time is one hour!!
    exclude_algos = 'GBM',
)
toc()

dim( aml_results@leaderboard )

head( aml_results@leaderboard, 20 )

getParms( aml_results@leader )
# or a synonym:
# aml_results@leader@parameters

as.data.frame( predict( aml_results@leader, funky_test ) )
