# Plotting culmen length as a linear regression of culmen depth

plot_culmen_figure <- function(logpenguins) {
  logpenguins %>%
    ggplot(aes(x = culmen_depth_mm, y = logculmenlength, colour = species)) +
    geom_point(position = position_jitter(seed = 0, height = 0.01), alpha = 0.6) +
    geom_smooth(method = "lm") +
    labs(title = "Log of culmen length plotted against culmen depth", 
         x = "Culmen depth (mm)", y = "Log of culmen length (mm)", colour = "Penguin Species") +
    scale_colour_manual(values = met.brewer("Archambault", 3),
                        labels=c("Adelie", "Chinstrap", "Gentoo")) +
    theme_bw()
}

# setting the seed value = 0 so that the jitter is reproducible

## Saving a png image

save_culmenplot_png <- function(penguins_flippers, 
                                   filename, size, res, scaling){
   agg_png(filename, width   =  size, 
                     height  =  size, 
                     units   =  "cm", 
                     res     =  res, 
                     scaling =  scaling)
   culmenplot <- plot_culmen_figure(penguins_filtered)
   print(culmenplot)
   dev.off()
 }

## Saving a svg

 save_culmenplot_svg <- function(penguins_filtered, 
                                   filename, size, scaling){
     size_inches = size/2.54
 svglite(filename, width   = size_inches, 
                       height  = size_inches, 
                       scaling = scaling)
     culmenplot <- plot_culmen_figure(penguins_filtered)
     print(culmenplot)
     dev.off()
 }