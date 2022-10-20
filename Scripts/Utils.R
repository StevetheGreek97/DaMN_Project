## Project: DaMN project

## Title: Nanoplastics modulate the outcome of a zooplankton-microparasite 
## interaction.

## Script purpose: Required function to perform statistical analysis & visualize 
## the data

## Date: 15-Oct-2022

## Authors: Stylianos Mavrianos, 
##          Florent Manzi, 
##          Ramsy Agha, 
##          Noemi Azoubib, 
##          Charlotte Schampera, 
##          Justyna Wolinska1

## Corresponding author: florent.vmanzi@gmail.com

#_______________________________________________________________________________
get_mean <- function(data, geom, size, pos = 'dodge') {
  
  return (stat_summary(data = data, 
                       fun.data = "mean_se",
                       geom = geom,
                       size = size,
                       width = 0.2,
                       pch = 21,
                       colour = "black", 
                       position = pos,
)
  )
  
}



get_errorbar <- function(data, width, colour) {
  return ( stat_summary(data = data,
                        fun.data = "mean_se",
                        geom = "errorbar",
                        width = width,
                        position = position_dodge(width = 0.2),
                        colour = colour, 
                        alpha = 1)
  )
  
  
}


get_lines <- function(data) {
  return (stat_summary(data = data, fun.data = "mean_se",
                                   geom = "line",
                                   position = position_dodge(width=0.6),
                                   colour = "black",
                                   alpha = 0.3))
  
}


make_barplot <- function(data, x, y, fill, group, lab_x, lab_y, title) {
  return  (ggplot(data,
                  aes(
                    x = {{x}}, 
                    y = {{y}}, 
                    fill = {{fill}}, 
                    group = {{group}} 
                  )   ) + 
             coord_cartesian(ylim=c(0, 1)) +
             scale_y_continuous(breaks=seq(0, 1, 0.25)) +
             scale_fill_grey(start = 1, end = 0) +
             get_mean(data, "bar", 0.5) +
             get_errorbar(data = data,
                          width =  0.05, 
                          colour = "darkgrey") +
             labs( x = lab_x,
                   y = lab_y,
                   title =  title) + 
             theme_bw() +
             theme(panel.grid.major = element_line(),
                   panel.grid.minor = element_blank(),
                   plot.title = element_text(hjust = 0.5, size = 15),
                   legend.title = element_blank(), 
                   legend.position = "none",
                   axis.text = element_text(size = 13),
                   axis.title = element_text(size = 15))
           )}

make_scatterplot <- function(data, x, y, fill, group, lab_x, lab_y, title, lab_fill = NULL) {
  plot <- ggplot(data,
                  aes(
                    x = {{x}}, 
                    y = {{y}}, 
                    fill = {{fill}}, 
                    group = {{group}} 
                  )   ) + 
      
      geom_jitter(width = 0.1, alpha = 0.5, pch = 20, size = 0.7) +
      scale_fill_grey(start = 1, end = 0) +
      get_errorbar(data = data, 
                   width = 0.1, 
                   colour = 'black') +
      get_lines(data) +
      get_mean(data, 
               geom = "point", 
               size = 1.5, 
               pos = position_dodge(width = 0.2)) +
      labs(x = lab_x,
           y = lab_y,
           fill = lab_fill,
           title = title
           ) +
      theme_bw() +
      theme(panel.grid.major = element_line(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 15),
            legend.title = element_blank(), 
            legend.position = "none",
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 15)
            )
    return (plot)
}

save_plot <- function(name, plot, width = 4.5, height = 4) {
  ggsave(filename = name, 
         plot = plot, 
         path = '../Plots',
         width = width, 
         height = height, 
         units = 'in', 
         dpi = 600, 
         compression = 'lzw')
}
get_CI <-function(data, var, inf, np = NULL, inplace = F) {
  
  
  mask <-var[data$Inf_treatment== inf & data$NP_treatment== np]
  
  if (inplace == T) {
    mask <- var[data$Inf_treatment== inf]
  }
  
  mean <- mean(mask)
  stddev <- sd(mask)
  len <- length(mask)
  
  return (c(qnorm(0.975)*stddev/sqrt(len), mean))
}
