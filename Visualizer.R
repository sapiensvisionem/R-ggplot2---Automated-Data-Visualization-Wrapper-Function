library(vcd) # mosaic plot
library(GGally) # correlation heatmap
library(hexbin) # 2-d density
library(gpairs) # correlation heatmap
library(ellipse) # correlation heatmap
library(ggrepel) # non-overlapping texts
library(ggplot2) # main visualization library
library(ggExtra) # marginal plots
library(viridis) # color scheme
library(corrgram) # correlation heatmap
library(corrplot) # correlation heatmap
library(reshape2) # correlation heatmap
library(gridExtra) # marginal plots
library(hrbrthemes) # theme options
library(wesanderson) # color scheme
library(RColorBrewer) # color scheme


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

univariate_visualizer <- function(df, col, theme) {
  # set the theme
  theme_set(theme)
  # check data type of col
  var <- df[,col]
  # if numeric,
  if (class(var) == 'numeric') {
    
    # density plot 
    
    vis1 <- 
      ggplot(df, aes_string(col)) + 
      geom_density(color='cyan') +
      labs(title='Density Plot',
           subtitle=col)
  
    # histogram
    
    vis2 <- 
      ggplot(df, aes_string(col, fill = cut(var, 100))) + 
      geom_histogram(show.legend = FALSE) +
      labs(title='Histogram',
           subtitle=col) +
      scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
    
    # histogram + density
    
    vis3 <- 
      ggplot(df, aes_string(col)) + 
      geom_histogram(aes(y = ..density..), show.legend = FALSE, color = "cyan", fill = "white", alpha = 0.4, position = "identity") +
      geom_density(color='grey', size=1) +
      labs(title='Histogram',
           subtitle=col)
    
    output <- list(vis1,vis2, vis3)
    return(output)
  } 
  # if categorical,
  else if (class(var) == 'factor') {
    
    # barplot
    
    df2 <- as.data.frame.table(table(var))
    vis4 <- 
      ggplot(df2, aes(var, Freq)) +
      geom_bar(stat="identity", width = 0.5, fill="cyan") + 
      labs(title="Bar Chart",
           x=col,
           y='Freq',
           subtitle=col) +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    
    # dot plot
    
    vis5 <- 
      ggplot(df2, aes(x=var, y=Freq)) +
      geom_point() +
      geom_segment(aes(x=var,
                       xend=var,
                       y=min(Freq),
                       yend=max(Freq)),
                   linetype='dashed',
                   size=0.1) +
      labs(title='Dot Plot', subtitle=col) +
      coord_flip()  
    
    # pie chart
    
    df$new_col <- var
    vis6 <-
      ggplot(df, aes(x = "", fill = new_col)) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill=col, 
           x=NULL, 
           y=NULL, 
           title="Pie Chart", 
           subtitle=col) +
      coord_polar(theta = "y", start=0)
    return(list(vis4, vis5, vis6))
    
    # waffle plot
    
    if (length(var)>100) {
      nrows <- 10
      df <- expand.grid(y = 1:nrows, x = 1:nrows)
      categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
      df$category <- factor(rep(names(categ_table), categ_table))  
      
      vis7 <-
        ggplot(df, aes(x = x, y = y, fill = category)) + 
        geom_tile(color = "black", size = 0.5) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
        scale_fill_brewer(palette = "Set3") +
        labs(title="Waffle Chart", subtitle=col) + 
        theme(panel.border = element_rect(size = 2),
              plot.title = element_text(size = rel(1.2)),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.title = element_blank(),
              legend.position = "right")
      return(list(vis4, vis5, vis6, vis7))
    }
  } else {
    return('Correctly specify data type of your variable as either numeric or factor')
  }
}

mtcars$mpg2 <- factor(mtcars$mpg)
univariate_visualizer(mtcars, 'mpg', theme_minimal())
univariate_visualizer(mtcars, 'mpg2', theme_minimal())


bivariate_visualizer <- function(df, col1, col2, theme) {
  # set the theme
  theme_set(theme)
  
  # check data type of col
  var <- df[,col]
  
  # if numeric,
  if (class(var) == 'numeric')
  
  # numeric vs numeric
  
    ## scatterplot
    
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
    
    ## add marginal rugs
    gg +
      geom_rug()
    
    ## add marginal histograms
    ggMarginal(gg, type='histogram', fill='transparent')
    
    ## add marginal boxplots
    ggMarginal(gg, type='boxplot', fill='transparent')
    
    ## jitterplot
    g +
      geom_jitter() + 
      labs(title='Jitter Plot')
    
    ## countplot
    g +
      geom_count() + 
      labs(title='Counts Plot')
    
    ## 2-d density plots
    g +
      geom_bin2d()
    
    g +
      geom_hex()
    
  
  # numeric vs categorical
  
    ## boxplot
    
    g <- ggplot(df, aes(v1,v2, fill=v3))
    g + geom_boxplot(fill=v1) +
      labs(title='Box Plot')
    
    ## violin plot
    
    g + geom_violin(trim=F) +
      labs(title='Violin Plot')
    
    ## box-violin plot
    
    g + geom_violin() +
      geom_boxplot()
    
    ## tufte-boxplot
    
    g + geom_tufteboxplot() +
      labs(title='Tufte Box Plot')
    
    ## violin-jitterplot
    
    g + geom_violin() + 
      geom_jitter()
    
    ## histogram per color
    
    vis2 <- 
      ggplot(df, aes_string(col, fill = cut(var, 100))) + 
      geom_histogram(show.legend = FALSE) +
      labs(title='Histogram',
           subtitle=col) +
      scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
    
    ## density per color
    
    vis1 <- 
      ggplot(df, aes_string(col)) + 
      geom_density(color='cyan') +
      labs(title='Density Plot',
           subtitle=col)
  
  # categorical vs categorical
  
    ## jitterplot
    
    g <- ggplot(df, aes(v1,v2))
    g + geom_jitter(aes(col=v1))
    
    ## column charts
    
    tbl <- count(df,v1,v2)
    tbl <- mutate(tbl, v1=reorder(v1,-n,sum),
                  v2=reorder(v2, -n, sum))
    ggplot(tbl) +
      geom_col(aes(x=v1, y=n, fill=v2), position='dodge') +
      facet_grid(~v3)
    
    ## mosaic plot
    
    mosaic(~v1+v2+v3, df)
    
    ggplot(df) + 
      geom_mosaic(aes(x=product(v1,v2), fill=v3),
                  divider = ddecker()) +
      theme(axis.text.x = element_text(angle=15, hjust=1))
}



multivariate_visualizer <- function(df, col1, col2, col3, col4=NULL) {
  # numeric x3
  ## perspective plot
  
  # numeric x2 categorical x1
  ## scatterplot with color/size
  
  # numeric x1 categorical x2
  ## violin plot
  ## boxplot
  ## column chart
  
  # categorical x3
  ## balloon plot
  ## column chart
  ## mosaic plot
  ## correspondance analysis
}


# need to specify the date column and a vector of columns
# data needs to be in long format
timeseries_visualizer <- function(df, date, var) {
  # overlain
  
  # faceted
  
  # smoothed
}


# Correlation Visualizer only takes in all numeric data

correlation_visualizer <- function(df) {
  cormat <- round(cor(df), 2)
  
  ## base R heatmap
  vis1 <-
    heatmap(as.matrix(df), scale='column', xlab="variable", Colv = NA, Rowv = NA, col=colorRampPalette(brewer.pal(8, "PiYG"))(25))
  
  ## corrplot
  vis2 <-
    corrplot(cormat, order="hclust")
  
  ## gpairs
  vis3 <-
    gpairs(df)
  
  ## ggpairs
  vis4 <-
    ggpairs(df, progress=F) 
  
  ## ggcorr
  vis5 <-
    ggcorr(df, method = c("everything", "pearson")) 
  
  ## plotcorr
  colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
  vis6 <-
    plotcorr(cormat, col=colorfun((cormat+1)/2),  diag=T)
  
  ## corrgram
  vis7 <-
    corrgram(df, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)
  
  vis8 <-
    corrgram(df, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt)
  
  return(list(vis1,vis2,vis3,vis4,vis5,vis6,vis7,vis8))
}

correlation_visualizer(mtcars)
corrplot(round(cor(mtcars),2), main='nah')

