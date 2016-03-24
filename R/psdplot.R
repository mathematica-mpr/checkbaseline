#### PROPENSITY SCORE DENSITY PLOT
psdplot <- function(matched_dta, trt_var, distance = distance, mytitle = "Propensity score distribution") {

  (min_x <- signif(min(matched_dta$distance),1))
  (max_x <- signif(max(matched_dta$distance),1))
  xvals  <- signif(seq(min_x, max_x, .1),1)
  dplot.df <- matched_dta
  dplot.df$thistrt <- factor(matched_dta[[trt_var]])
  levels(dplot.df$thistrt)[levels(dplot.df$thistrt)=="1"] <- "Treatment group"
  levels(dplot.df$thistrt)[levels(dplot.df$thistrt)=="0"] <- "Comparison group"


 # density plot overlapped
 #plotpsmdist <- ggplot(dplot.df, aes(x=distance, fill=thistrt)) + #aes(y=..count.., x=distance, fill=thistrt))
 #   geom_density(alpha=.5) +
 #   geom_histogram(alpha = .5, binwidth = .01) +

 # histograms stacked
  plotpsmdist <- ggplot(dplot.df, aes(x=distance, fill=thistrt)) +
  geom_histogram(binwidth = .01) +
  facet_grid(thistrt ~ .) +
   #theme(strip.text=element_blank(), strip.background=element_blank()) + #<-removes facets label
    theme(strip.background=element_rect(fill="white", colour="white", size=2)) +
    xlab("Propensity scores") +
    ylab("Count of observations") +
    scale_x_continuous(breaks = xvals,
                       limits = c(min_x-.1,max_x+.1)) +
    scale_fill_manual(values = c("#34B6E4","#002E5F"),
                      labels = c("Comparison group","Treatment group")) +
    theme(legend.position = "right",
          legend.title = element_text(size = 2, colour = "white"),
          legend.key = element_blank(), #<-removes boxes around key symbols
          axis.ticks = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour = "gray60", linetype = 3),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour = "gray60", linetype = 3),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    guides(fill = FALSE) #<-removes legend

  return(plotpsmdist)
}

