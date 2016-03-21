#### STANDARDIZED BIAS PLOT WITHOUT ARROWS [i.e. no matching]
sbplot_nm <- function(plot.df = non_matched.tb) {

  #print("Starting standardized bias plot for non-matched data")
  plot.df$NameNumber <- as.numeric(rownames(plot.df))
  min_x = round(min(plot.df$Standardized.bias),1) -.1
  max_x = round(max(plot.df$Standardized.bias),1) +.1
  range = max(abs(min_x),abs(max_x))
  ncovs <- length(plot.df$Name)
  #print(paste0("ncovs = ", ncovs, ";range = ", -range, ",", range))
  sbplot <- ggplot(data = plot.df, aes(x = Standardized.bias, y = NameNumber)) +
    geom_point(size = 4) +
    xlab("Standardized bias") +
    scale_x_continuous(breaks = c(-.9, -.7, -.5, -.25, -.05, .05, .25, .5, .7, .9),
                       limits = c(-range,range)) +
    ylab(" ") +
    scale_y_continuous(breaks = c(1:ncovs),
                       labels = as.character(unique(plot.df$Name)),
                       limits = c(0.5,ncovs+.5)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "top",
          legend.title = element_text(size = 2, colour = "white"),
          legend.key = element_blank(), #<-removes boxes around key symbols
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour = "gray30", linetype = 2 ),
          panel.border = element_blank()) +
    guides(fill = FALSE) + #<-removes rectangles dataset legend
    geom_vline(xintercept = 0, linetype = "longdash", colour="gray") +
    annotate("text", x = -0.28, y = 3.2, label = "Unbalanced", size = 5, colour = "gray45") +
    annotate("text", x = -0.16, y = 2.5, label = "Good balance", size = 5, colour = "gray55") +
    annotate("text", x =  0.00, y = 0.7, label = "Great balance", size = 5, colour = "gray65") +
    annotate("rect", xmin=c(-range,0.25),xmax=c(-0.25,range), ymin=0.5, ymax=ncovs + .5, alpha=0.2, fill="#6C6F70") +
    annotate("rect", xmin=c(-0.25,0.05), xmax=c(-0.05,0.25) , ymin=0.5, ymax=ncovs + .5, alpha=0.2, fill="#D7D3C8") +
    annotate("rect", xmin=c(-0.05)     , xmax=c(-0.05)      , ymin=0.5, ymax=ncovs + .5, alpha=0.2, fill="#C7BE71")
  #print(sbplot)

}

#### STANDARDIZED BIAS PLOT WITH ARROWS [i.e. with matched and un-matched data]*
sbplot_wm <- function(plot.df, mytitle = "Standardized Bias") {

  #print("Starting standardized bias plot for matched data")

  # obtain parameters
  min_x = round(min(plot.df$Standardized.bias),1) -.1
  max_x = round(max(plot.df$Standardized.bias),1) +.1
  range <- max(abs(min_x),abs(max_x))
  ncovs <- length(unique(plot.df$Name))

  # arrows
  plot.df <- arrange(plot.df, Matching, Name) %>%
    tbl_df %>%
    group_by(Matching) %>%
    mutate(NameNumber = row_number())
  plot.df <- arrange(plot.df, Matching, NameNumber)

  plot.df <- merge(
    plot.df,
    plot.df[plot.df$Matching == 'None', c('NameNumber', 'Standardized.bias')],
    by = c('NameNumber'),
    suffixes = c('', '_unmatched'),
    all = TRUE)

  plot.df$dirsign <- ifelse(
    plot.df$Matching == 'Matched',
    -.01 * sign(plot.df$Standardized.bias_unmatched - plot.df$Standardized.bias),
    NA)

  #plot.df$imputed <- factor(plot.df$imputed, levels = c("Non-imputed", "Imputed"))

  mypretty_plot <- ggplot(data = plot.df, aes(x = Standardized.bias, y = NameNumber, colour = Matching)) +
    geom_point(size = 4) +
    #facet_grid(.~imputed) +
    xlab("Standardized bias") +
    scale_colour_manual(values = c(None = "#34B6E4", Matched = "#002E5F"),
                        labels = c(None = "Before matching", Matched = "After matching"),
                        limits = c("None","Matched")) +
    ylab(" ") +
    scale_y_continuous(breaks = c(1:ncovs),
                       labels = as.character(unique(plot.df$Name)),
                       limits = c(0.5,ncovs+.5)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "top",
          legend.title = element_text(size = 2, colour = "white"),
          legend.key = element_blank(), #<-removes boxes around key symbols
          axis.ticks = element_blank(),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour = "gray30", linetype = 2 ),
          panel.border = element_blank(),
          strip.background = element_rect(fill="white", color="white")) +
    guides(fill = FALSE) + #<-removes rectangles dataset legend
    geom_vline(xintercept = 0, linetype = "longdash", colour="gray") +
    annotate("text", x = -0.32 + .01, y = ncovs - 0.5, label = "Unbalanced", size = 8, colour = "gray45") +
    annotate("text", x = -0.17 , y = ncovs/2 + .2, label = "Good balance", size = 8, colour = "gray55") +
    annotate("text", x =  0.00, y = 0.7, label = "Great balance", size = 8, colour = "gray65") +
    annotate("rect", xmin=c(-range,0.25),xmax=c(-0.25,range), ymin=0.5, ymax=ncovs + .5, alpha=0.2, fill="#6C6F70") +
    annotate("rect", xmin=c(-0.25,0.05), xmax=c(-0.05,0.25) , ymin=0.5, ymax=ncovs + .5, alpha=0.2, fill="#D7D3C8") +
    annotate("rect", xmin=c(-0.05)     , xmax=c(-0.05)      , ymin=0.5, ymax=ncovs + .5, alpha=0.2, fill="#C7BE71") +
    ggtitle(mytitle) +
    geom_segment(
      data = plot.df[plot.df$Matching == 'Matched', ],
      aes(
        x = Standardized.bias_unmatched,
        xend = Standardized.bias,
        yend = NameNumber),
      colour = "#34B6E4",
      arrow = arrow(length = unit(0.3, "cm")))

  return(mypretty_plot)

}
