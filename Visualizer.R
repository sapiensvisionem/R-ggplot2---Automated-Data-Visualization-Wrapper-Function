library(vcd) # mosaic plot
library(GGally) # correlation heatmap
library(hexbin) # 2-d density
library(gpairs) # correlation heatmap
library(ellipse) # correlation heatmap
library(ggrepel) # non-overlapping texts
library(ggplot2) # main visualization library
library(ggpmisc) # time series peaks and valleys
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
# theme_excel()
# theme_economist()
# theme_fivethirtyeight()
# theme_tufte()
# theme_gdocs()
# theme_wsj()
# theme_calc()
# theme_hc()
# theme_article()
# theme_pubclean()
# theme_bigstarsr()

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
    
    ggplot(df, aes_string(col1, col2)) +
      geom_boxplot(fill='cyan') +
      labs(title='Box Plot')
    
    ## violin plot
    
    ggplot(df, aes_string(col1, col2)) + 
      geom_violin(trim=F) +
      labs(title='Violin Plot')
    
    ## lollipop
    ggplot(cty_mpg, aes(x=make, y=mileage)) + 
      geom_point(size=3) + 
      geom_segment(aes(x=make, 
                       xend=make, 
                       y=0, 
                       yend=mileage)) + 
      labs(title="Lollipop Chart", 
           subtitle="Make Vs Avg. Mileage", 
           caption="source: mpg") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
    
    ## dotplot
    ggplot(cty_mpg, aes(x=make, y=mileage)) + 
      geom_point(col="tomato2", size=3) +   # Draw points
      geom_segment(aes(x=make, 
                       xend=make, 
                       y=min(mileage), 
                       yend=max(mileage)), 
                   linetype="dashed", 
                   size=0.1) +   # Draw dashed lines
      labs(title="Dot Plot") +  
      coord_flip()
    
    ## box-violin plot
    
    ggplot(df, aes_string(col1, col2)) + 
      geom_violin() +
      geom_boxplot()
    
    ## box-dot plot
    ggplot(df, aes_string(col1, col2)) +
      geom_boxplot() +
      geom_dotplot(binaxis='y',
                   stackdir='center',
                   fill='red') + 
      labs(title="box and dot plot") + 
      theme
    
    ## tufte-boxplot
    
    ggplot(df, aes_string(col1, col2)) + 
      geom_tufteboxplot() +
      labs(title='Tufte Box Plot')
    
    ## violin-jitterplot
    
    g + geom_violin() + 
      geom_jitter()
    
    ## histogram per color
    
    vis2 <- 
      ggplot(df, aes_string(col1)) + 
      geom_histogram(aes_string(fill=col2)) +
      labs(title='Histogram') 
    
    ## density per color
    
    vis5 <- 
      ggplot(df, aes_string(col1)) + 
      geom_density(aes_string(fill=col2)) +
      labs(title='Density Plot')
  
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
    
    g <- ggplot(mpg, aes(manufacturer))
    g + geom_bar(aes(fill=class), width = 0.5) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
      labs(title="Categorywise Bar Chart", 
           subtitle="Manufacturer of vehicles", 
           caption="Source: Manufacturers from 'mpg' dataset")
    
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
# also date information needs to be in R's date format. Use lubridate library.
# ingests three columns: date, variable, and value

# date_format: day, week, month, year
timeseries_visualizer <- function(df, date, var, value, date_format, theme) {
  g <- ggplot(df, aes_string(x=date, y=value))
  date_format <- ifelse(date_format=='year',
                        '%Y',
                        ifelse(date_format=='month',
                               '%M', 'd'))
  x_scale <- scale_x_date(date_labels = date_format) # date_breaks
  x_ticks <- theme(axis.text.x=element_text(angle=45, hjust=1)) 
  color_scheme <- scale_fill_viridis(discrete=T) 
  basic <- g + x_scale + x_ticks + color_scheme + theme
    
  # area chart
  basic +
    geom_area(aes(color=var, fill=var)) +
    labs(title='Time Series', 
         subtitle='Area Chart',
         x=paste('Date in', date_format),
         y=col)  
  
  # overlain (different color)
  basic +
    geom_line(color=var) +
    geom_point(shape=21, color='black', fill='#69b3a2') +
    labs(title='Time Series', 
         subtitle='Area Chart',
         x=paste('Date in', date_format),
         y=col)  
    
  ## forecast
  ggseasonplot
  ts_plot() # https://cran.r-project.org/web/packages/TSstudio/vignettes/Plotting_Time_Series.html
  ts_plot(df, type='multiple',
          title,
          Xtitle,
          Ytitle,
          line.mode,
          dash)
  
  # faceted
  basic + 
    geom_point()
  
  # peaks and valleys
  ggplot(lynx, as.numeric = FALSE) + geom_line() + 
    stat_peaks(colour = "red") +
    stat_peaks(geom = "text", colour = "red", 
               vjust = -0.5, x.label.fmt = "%Y") +
    stat_valleys(colour = "blue") +
    stat_valleys(geom = "text", colour = "blue", angle = 45,
                 vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
    ylim(-500, 7300)
    
  # smoothed
  basic + 
    stat_smooth(color=var, fill=var, method='loess')
    
  # calendar
  # Create Month Week
  df$yearmonth <- as.yearmon(df$date)
  df$yearmonthf <- factor(df$yearmonth)
  df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
  df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "VIX.Close")]
  ggplot(df, aes(monthweek, weekdayf, fill = VIX.Close)) + 
    geom_tile(colour = "white") + 
    facet_grid(year~monthf) + 
    scale_fill_gradient(low="red", high="green") +
    labs(x="Week of Month",
         y="",
         title = "Time-Series Calendar Heatmap", 
         subtitle="Yahoo Closing Price", 
         fill="Close")


}
# http://homepage.divms.uiowa.edu/~luke/classes/STAT4580-2020/timeseries.html


# Correlation Visualizer only takes in all numeric data

correlation_visualizer <- function(df) {
  cormat <- round(cor(df), 2)
  
  ## base R heatmap
  vis1 <-
    heatmap(as.matrix(df), scale='column', xlab="variable", Colv = NA, Rowv = NA, col=colorRampPalette(brewer.pal(8, "PiYG"))(25))
  
  ## corrplot
  vis2 <-
    corrplot(cormat, order="hclust")
  
  ## ggcorrplot
  ggcorrplot(cormat, hc.order=T,
             type = "lower", 
             lab = TRUE, 
             method="circle", 
             colors = c("tomato2", "white", "springgreen3"),
             ggtheme=theme)
  
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

