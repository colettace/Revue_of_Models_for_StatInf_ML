.Library

# install.packages( "plotly" )
# install.packages( "magrittr" )
# install.packages( "lmerTest" )

library( tidyverse )

# Import this to use the "double-ended pipe" i.e., %<>%
library( magrittr )

# # Download data time series data of topic mentions per year

# files <- fs::dir_ls( path = "pubmed_data", glob = "*csv" )

# as.character( files )

# length( files )

# PreviewFile <- function( filepath )
# {
#     filepath %>%
#         readLines %>%
#         head( 10 ) %>%
#         paste( collapse="\n" ) %>%
#         cat
# }

# PreviewFile( 'pubmed_data/anova.csv' )

# PreviewFile( 'pubmed_data/artificial_intelligence.csv'  )

# # Load time series data into a single data frame

# data <- readr::read_csv( files, id="Topic", skip=1 )

# glimpse( data )

# sample_n( data, 10 )

# ## Make the topics look pretty

# PrettifyTopic <- function( raw )
# {
#     filenames <- str_split_fixed( raw, "/", n=2 ) [,2]
#     #head( filenames )
#     raw_topics <- str_split_fixed( filenames, "\\.", n=2 )[,1]
#     #sample( raw_topics, 5 )
#     topics <- str_replace_all( raw_topics, "_", " " )
#     #sample( topics, 5 )
#     cap_topics <- str_to_title( topics )
#     #sample( cap_topics, 5 )
#     return( cap_topics )
# }

# data$Topic <- PrettifyTopic( data$Topic )

# data %>%
#     write_csv( "pubmed_data_science_mentions.csv" )

data <- readr::read_csv( "pubmed_data_science_mentions.csv" )

sample_n( data, 10 )

dim( data )

data %<>%
    filter( Year > 1975 ) %>%
    filter( Count >= 10 )

dim( data )

sample_n( data, 10 )

data$Topic %>% unique %>% sort

library( lubridate )

today <- ymd( "2022-04-15" )

first_of_year <- ymd( "2022-01-01" )

n_days <- interval( first_of_year, today ) %>%
    as.numeric( 'days' )

n_days

multiplier <- 365 / n_days

multiplier

Extrapolate_Count <- function( val ) {
    return( as.integer( round( val * multiplier ) ) )
}

head( data[ data$Year == 2022, ] )

data[ data$Year == 2022, ]$Count %<>%
    Extrapolate_Count

head( data[ data$Year == 2022, ] )

options(
    repr.plot.width=10,
    repr.plot.height=6,
    repr.plot.res=300
)

data %>%
    ggplot( aes( x=Year, y=Count, color=Topic)  ) +
    geom_line() + theme(legend.position="bottom")
    #geom_smooth(method = "loess")

data %<>%
    mutate(
        log10_Count = log( Count, base=10 )
    )

head( data )

options(
    repr.plot.width=10,
    repr.plot.height=6,
    repr.plot.res=300
)

data %>%
    ggplot(
        aes( x=Year, y=log10_Count )
    ) +
    geom_line( aes( color=Topic ) ) + 
    geom_smooth( method = "lm" ) +
    theme( legend.position="bottom" )

# library(rbokeh)

# fig <- rbokeh::figure(width = 1200, height = 600) %>%
#   ly_lines(
#       x=Year,
#       y=log_mentions,
#       color=Topic,
#       hover=Topic,
#       data=data)#, legend = "lowess")

# fig

library( plotly )

fig <- data %>%
    plot_ly(
        x = ~Year,
        y = ~log10_Count,
        color = ~Topic,
        type = 'scatter',
        mode = 'lines'
    )

fig

glimpse( data )

ReparameterizeXaxis <- function( df, Xvar_name )
# Add three columns to the data frame representing
# the three time reparameterization options.
{
    X <- df[[ Xvar_name ]]
    first_year <- min( X ) 
    last_year <- max( X )
    mean_year <- mean( c( first_year, last_year ) )
    df %>%
        mutate(
            centered_time = X - mean_year,
            entry_time = X - first_year,
            exit_time = X - last_year,
        ) %>%
        return
}

min( data$Year )

max( data$Year )

mean( c( min( data$Year ), max( data$Year ) ) )

data %>%
    ReparameterizeXaxis( "Year" ) %>%
    plot_ly(
        x = ~centered_time,
        #x = ~entry_time,
        #x = ~exit_time,
        y = ~log10_Count,
        color = ~Topic,
        type = 'scatter',
        mode = 'lines'
    )

LM_model <- data %>%
    ReparameterizeXaxis( "Year" ) %>%
    lm( log10_Count ~ 1 + centered_time, data=. )

summary( LM_model )

sd( residuals( LM_model ) )

summary( residuals( LM_model ) )

# 2017 Paper for lmerTest has over 9,000 citations!
library( lmerTest )

LMixed_model <- data %>%
    ReparameterizeXaxis( "Year" ) %>%
    #lmer( log10_Count ~ 1 + centered_time + (1 + centered_time | Topic), data=. ) # Parameterize using correlated slopes/ints
    lmer( log10_Count ~ 1 + centered_time + (1 + centered_time || Topic), data=. ) # Parameterize using UNcorrelated slopes/ints

# compare to that of straight LM above
sd( residuals( LMixed_model ) )

# compare to that of straight LM above
summary( residuals( LMixed_model ) )

LMixed_model
#summary( LMixed_model )

# Here's a differen't way to represent the outputs:
# library( broom.mixed )
# LMixed_model %>% tidy

fixef( LMixed_model )

10**fixef( LMixed_model )

( 10**fixef( LMixed_model )[[ 'centered_time' ]] ) * 100

LMixed_model %>%
    ranef() %>%
    pluck( "Topic" ) %>%
    head

model2002to2012 <- data %>%
    filter( Year >= 2002, Year <= 2012 ) %>%
    ReparameterizeXaxis( "Year" ) %>%
    lmer( log10_Count ~ 1 + centered_time + (1 + centered_time | Topic), data=. )
    #lmer( log10_Count ~ 1 + exit_time + (1 + exit_time | Topic), data=. )

model2012to2022 <- data %>%
    filter( Year >= 2012, Year <= 2022 ) %>%
    ReparameterizeXaxis( "Year" ) %>%
    lmer( log10_Count ~ 1 + centered_time + (1 + centered_time | Topic), data=. )
    #lmer( log10_Count ~ 1 + exit_time + (1 + exit_time | Topic), data=. )

model2002to2012
#summary( model2002to2012 )

model2012to2022
#summary( model2012to2022 )

GrabRanefs <- function( model, ranef_name ) {
    ranefs <- model %>%
        ranef %>%
        pluck( "Topic" ) %>%
        rownames_to_column
    ranef_estimates <- ranefs[[ ranef_name ]]
    names( ranef_estimates ) <- ranefs$rowname
    return( ranef_estimates )
}

model2002to2012

old_trajectories <- GrabRanefs( model2002to2012, "centered_time" )
#old_trajectories <- GrabRanefs( model2002to2012, "exit_time" )

head( old_trajectories )

new_trajectories <- GrabRanefs( model2012to2022, "centered_time" )
#new_trajectories <- GrabRanefs( model2012to2022, "exit_time" )

head( new_trajectories )

all_slopes <- data.frame(
        old_trajectories,
        new_trajectories
    )

all_slopes <- ( 10**all_slopes ) * 100

all_slopes %>%
    rownames_to_column( "Topic" ) %>%
    plot_ly(
        x = ~old_trajectories,
        y = ~new_trajectories,
        color = ~Topic,
        type = 'scatter'
    ) %>%
    layout( 
        title = 'Decade-to-decade differences in rates of change of data science topic mentions in PubMed',
        xaxis = list( title = '2002-12 % increase relative to baseline' ), 
        yaxis = list( title = '2012-22 % increase relative to baseline' )
    )

model2002to2012 <- data %>%
    filter( Year >= 2002, Year <= 2012 ) %>%
    ReparameterizeXaxis( "Year" ) %>%
    lmer( log10_Count ~ 1 + centered_time + (1 + centered_time | Topic), data=. )
    #lmer( log10_Count ~ 1 + exit_time + (1 + exit_time | Topic), data=. )

model2012to2022 <- data %>%
    filter( Year >= 2012, Year <= 2022 ) %>%
    ReparameterizeXaxis( "Year" ) %>%
    lmer( log10_Count ~ 1 + centered_time + (1 + centered_time | Topic), data=. )
    #lmer( log10_Count ~ 1 + exit_time + (1 + exit_time | Topic), data=. )

old_intercepts <- GrabRanefs( model2002to2012, "(Intercept)" )

head( old_intercepts )

new_intercepts <- GrabRanefs( model2012to2022, "(Intercept)" )

head( new_intercepts )

all_intercepts <- data.frame(
        old_intercepts,
        new_intercepts
    )

#all_intercepts <- ( 10**all_intercepts )

all_intercepts %>%
    rownames_to_column( "Topic" ) %>%
    plot_ly(
        x = ~old_intercepts,
        y = ~new_intercepts,
        color = ~Topic,
        type = 'scatter'
    ) %>%
    layout( 
        title = 'Decade-to-decade change in relative importances of data science topic, measured by mentions in PubMed',
        xaxis = list( title = '2002-12 relative importance (log scale)' ), 
        yaxis = list( title = '2012-22 relative importance (log scale)' )
    )


