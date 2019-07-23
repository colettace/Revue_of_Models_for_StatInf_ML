
library( tidyverse )

library( broom )

options(repr.plot.width=4, repr.plot.height=3)

uniform_noise <- function() { 
    runif( n=100, min=-1, max=1 )
}

qplot( uniform_noise() )

gaussian_noise <- function() {
    rnorm( n=100 ) 
}

qplot( gaussian_noise() )

qplot( x=gaussian_noise(), y=gaussian_noise() )

y <- 7 + gaussian_noise()

qplot( y )

mean( y )

library( skimr )

skim_to_wide( y )

model0 <- lm( y ~ 1 )

summary( model0 )

glance( model0 )

tidy( model0 )

t.test( y )

glance( t.test( y ) )

model2 <- lm( y ~ x - 1, blob )

summary( model2 )

anova( model1, model2 )

fake_data_A <- data.frame( X1=uniform_noise(), Y=gaussian_noise(), Class=rep(0,100) )
fake_data_B <- data.frame( X1=uniform_noise(), Y=gaussian_noise()+1, Class=rep(1,100) )
fake_data_all <- rbind( fake_data_A, fake_data_B )
fake_data_all$Class <- as.factor( fake_data_all$Class )

skim_to_wide( fake_data_all )

sample_n( fake_data_all, 5)

ggplot( fake_data_all, aes( x=X1, y=Y, color=Class) ) + geom_point()

ggplot( fake_data_all, aes( x=X1, y=Y, color=Class ) ) + 
    geom_boxplot() +
    labs( x='class' )

model0 <- lm( Y ~ 1, fake_data_all )

summary( model0 )

model1 <- lm( Y ~ X1, fake_data_all )

summary( model1 )

model2 <- lm( Y ~ Class, fake_data_all )

summary( model2 )

confint( model2 ) 

t.test( Y ~ Class, fake_data_all )

anova( model0, model1, model2 )

fake_data_all <- fake_data_all %>%
    mutate( X2 = as.numeric( Class ) + rnorm( n=200 ) )

fake_data_all %>% sample_n( 5 )

fake_data_all %>% 
    ggplot( aes( x=X2, y=Y, color=Class ) ) +
    geom_point()

model0 <- lm( Y ~ 1, fake_data_all )

summary( model0 )

model1 <- lm( Y ~ X2, fake_data_all )

summary( model1 )

model2 <- lm( Y ~ X1 + X2, fake_data_all )

summary( model2 )

anova( model0, model1, model2)

fake_data_all <- fake_data_all %>%
    mutate( X3 = as.numeric( Class ) + rnorm( n=200 ) ) %>%
    select( Class, Y, X1, X2, X3 ) # reorder 

options(repr.plot.width=4, repr.plot.height=3)

fake_data_all %>% 
    ggplot( aes( x=X2, y=X3, color=Class ) ) +
    geom_point()

library( GGally )

options(repr.plot.width=5, repr.plot.height=5)

ggpairs( fake_data_all, aes( color=Class, alpha=0.1) )

model0 <- lm( Y ~ 1, fake_data_all )

model1 <- lm( Y ~ X2, fake_data_all )

model3 <- lm( Y ~ X2 + X3, fake_data_all )

summary( model3 )
