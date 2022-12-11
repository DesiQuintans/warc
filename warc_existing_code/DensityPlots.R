#________________________________________#
#DensityPlots.R
#Author: AvH based on
#       (https://drsimonj.svbtle.com/quick-plot-of-all-variables)
#Modified by: AVH
#Description: Function plotting mutliple density
#              plots for selected continuous variables in a study
#Input: NA
#Output:NA
#
#________________________________________#

#' Plot densities
#'
#' A function that returns Histogram overlaid with kernel density for continuous variables
#'
#' @param df The dataset that you want the statistics for
#' @param vars List of variables you want plotted in the order you want them plotted
#' @param graphlabels Actual text you want shown under each graph e.g. SBP at baseline (mmHg)
#' @param xmin  Minimum value for histogram
#' @param xmax  Maximum value for histogram
#' @param barwidth How wide do you want histogram bars along x axis?
#' @param xlab_space Space between xaxis labels
#'
#' @examples


DensityPlots<- function(df,
                        vars,
                        graphlabels,
                        xmin,
                        xmax,
                        barwidth,
                        xlab_space)
{

    df1<- df[,vars]

    df2<- df1 %>%
        select_if(~is.numeric(.x)) %>%        # Keep only numeric columns
        gather()                              # Convert to key-value pairs
    keylabs <- lapply(graphlabels, function(x) {paste0("`",x,"`")})

    df2 %>% mutate(key=factor(key,levels=vars,labels=keylabs)) %>%   #Make sure the order is correct
        ggplot(aes(value)) +                  # plot the values
        scale_x_continuous(breaks= seq(xmin,xmax,xlab_space)) +
        geom_histogram(breaks=seq(xmin,xmax,barwidth),aes(y=..density..),      # Histogram with density instead of count on y-axis
                       colour="black",
                       fill="peachpuff") +

        facet_wrap(~ key, # In separate panels
                   ncol=2,
                   scales = "free", #let density scales vary
                   strip.position =  "bottom", # flip the facet labels along the x axis
                   labeller = label_parsed # redefine the text that shows up for the facets
        ) +
        xlab(NULL) + # remove the word "values")


        geom_density(colour="chocolate3",lwd=1)  +    # Overlay with transparent density plot
        theme_minimal() +
        theme(strip.background = element_blank(), # remove the background
              strip.placement = "outside") # put labels outside text

}



# if you want different conditions by treatment.factor use this:
#geom_histogram(aes(y = ..density.., color = treatment.factor, fill = treatment.factor),  alpha = 0.4, position = "identity") +
#geom_density(aes(color = treatment.factor), size =1)
