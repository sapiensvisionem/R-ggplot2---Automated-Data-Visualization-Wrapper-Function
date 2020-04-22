library(vcd)
library(hexbin)
library(ggrepel)
library(ggplot2)
library(ggExtra)
library(viridis)
library(gridExtra)
library(tidyverse)
library(hrbrthemes)
library(wesanderson)

options(scipen=999)
####### theme options
# theme_gray()
# theme_bw()
# theme_linedraw()
# theme_light()
# theme_dark()
# theme_minimal()
# theme_classic()
# theme_void()
# theme_ipsum()
theme <- theme_classic()
theme_set(theme)

######## color theme
scale_fill_gradient(low="blue", high="red")
scale_color_gradient2(midpoint=mid, low="blue", mid="white",
                      high="red", space ="Lab" )
scale_color_gradientn(colours = rainbow(5))
scale_color_manual(values=wes_palette(n=3, name="GrandBudapest")) # Moonrise1, Royal1, Zissou, FantasticFox, Darjeeling, Rushmore
scale_color_brewer(palette="Dark2") # 

opts <- options()  # save old options

options(ggplot2.continuous.colour="viridis") # magma, plasma, inferno, cividis
options(ggplot2.continuous.fill = "viridis")
scale_fill_viridis(discrete = TRUE, alpha=0.6)

###############################
# numeric vs numeric
numeric_numeric_dist <- function(df, v1, v2, v3=NULL, v4=NULL) {
  
  g <- ggplot(df, aes_string(v1,v2))
  if (is.null(c(v3,v4))) {
    # scatterplot with smoothing
    (gg <- 
       g +
       geom_point() +
       geom_smooth(method='lm', se=F) +
       labs(title="Scatterplot"))
    
    g +
      geom_point() +
      geom_smooth(method='loess', se=F) +
      labs(title="Scatterplot")
    
    # add marginal rugs
    gg +
      geom_rug()
    
    # add marginal histograms
    ggMarginal(gg, type='histogram', fill='transparent')
    
    # add marginal boxplots
    ggMarginal(gg, type='boxplot', fill='transparent')
    
    # jitterplot
    g +
      geom_jitter() + 
      labs(title='Jitter Plot')
    
    # countplot
    g +
      geom_count() + 
      labs(title='Counts Plot')
    
    # 2-d density plots
    g +
      geom_bin2d()
    
    g +
      geom_hex()
    
    
  } else if (!is.null(v3) & is.null(v4)) {
    g +
      geom_point(aes_string(col=v3)) +
      geom_smooth(method='loess', se=F) +
      labs(title="Scatterplot")
    
  } else {
    g +
      geom_point(aes_string(col=v3, size=v4)) +
      geom_smooth(method='loess', se=F) +
      labs(title="Scatterplot")
  }
  
}
#########################
# categorical vs numeric
categorical_numeric_dist <- function(df, v1, v2, v3=NULL) { #v1 is categorical
  g <- ggplot(df, aes(v1,v2, fill=v3))
  # boxplot
  g + geom_boxplot(fill=v1) +
    labs(title='Box Plot')
  
  # violin plot
  g + geom_violin(trim=F) +
    labs(title='Violin Plot')
    
  
  # box-violin plot
  g + geom_violin() +
    geom_boxplot()
  
  # tufte boxplot
  g + geom_tufteboxplot() +
    labs(title='Tufte Box Plot')
  
  # violin - jitter
  g + geom_violin() + 
    geom_jitter()
}

############################
# Categorical vs Numeric Summary

categorical_numeric_summary <- function(table, v1, v2=Freq) {
  
  # Pie Chart
  ggplot(table, aes(x="", y=Freq, fill=v1)) +
    geom_bar(width=1, stat='identity') +
    theme(axis.line = element_blank(),
          plot.title = element_text(hjust=0.5)) +
    labs(fill='class',
         x=NULL,
         y=NULL,
         title="Pie Chart of Class") + 
    coord_polar(theta = "y", start=0)
  
  # Column Chart
  ggplot(table, aes(v1, Freq)) +
    geom_bar(stat='identity', width=0.5) +
    labs(title="Column Chart") + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  
  # Dot Plot
  ggplot(df, aes(x=v1, y=Freq)) +
    geom_point() +
    geom_segment(aes(x=v1,
                     xend=v1,
                     y=min(Freq),
                     yend=max(Freq)),
                 linetype='dashed',
                 size=0.1) +
    labs(title='Dot Plot') +
    coord_flip()
  
  # Waffle Chart
  nrows <- 10
  df <- expand.grid(y = 1:nrows, x = 1:nrows)
  
  table$category <- factor(rep(names(table$v1), df))
  
  ggplot(df, aes(x=x, y=y, fill=category)) +
    geom_tile(color = "black", size = 0.5) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
    scale_fill_brewer(palette = "Set3") +
    labs(title="Waffle Chart") + 
    theme(panel.border = element_rect(size = 2),
          plot.title = element_text(size = rel(1.2)),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.position = "right")
    
 
  
}


########################
# Categorical vs Categorical

categorical_categorical_dist <- function(df, v1, v2, v3=NULL) {
  
  g <- ggplot(df, aes(v1,v2))
  
  # jitterplot
  g + geom_jitter(aes(col=v1))
  
  # column charts
  tbl <- count(df,v1,v2)
  tbl <- mutate(tbl, v1=reorder(v1,-n,sum),
                v2=reorder(v2, -n, sum))
  ggplot(tbl) +
    geom_col(aes(x=v1, y=n, fill=v2), position='dodge') +
    facet_grid(~v3)
  
  # mosaic plot
  mosaic(~v1+v2+v3, df)
  
  ggplot(df) + 
    geom_mosaic(aes(x=product(v1,v2), fill=v3),
                divider = ddecker()) +
    theme(axis.text.x = element_text(angle=15, hjust=1))
  
  
}

numeric_distribution <- function(df,v1, v2=NULL) {
  g <- ggplot(df, aes(v1))
  
  # density plot 
  
  g + geom_density(aes(fill=v2), alpha=.8) +
    labs(title='Density Plot')
  
  # histogram
  
  g + geom_histogram(aes(fill=v2))
  
}







#############################
# how to conditionally add elements to ggplot depending on the data 
# if the length of string is long, or there are a lot of dates, this needs to be done
# theme(axis.text.x = element_text(angle=65, vjust=0.6))