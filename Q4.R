# Question 4: Run a statistical test on the Palmer Penguins dataset and produce a figure to explain it.

## Load libraries

source("functions/libraries.R")

## Load cleaning and plotting functions 

source("functions/cleaning.R")
source("functions/plotting.R")

# Loading and cleaning the data

penguins_raw <- read.csv("data_raw/penguins_raw.csv")
penguins_clean <- cleaning(penguins_raw) 

## Checking that the data are clean
names(penguins_clean)

## Saving the cleaned data
write.csv(penguins_clean, "data_clean/penguins_clean.csv")

## Removing NA values
penguins_filtered <- remove_empty_culmen(penguins_clean)

## Checking the cleaned data
summary(penguins_filtered)

# Statistical testing

## Creating a linear model

penguins_culmen <- lm(culmen_depth_mm ~ culmen_length_mm, penguins_filtered)

# First, checking the assumption that data are normally distributed
plot(penguins_culmen, which=2)

# This assumption is met, as the points tend to fall on or close to the line.

# Secondly, checking the assumption that there is homogeneity of variance
plot(penguins_culmen, which=1)

# This assumption is not met, as residuals do not exhibit homogeneity of variance and are clustered. I will therefore log the data and plot qqplots to check assumptions. 

logpenguins <- mutate(penguins_filtered, logculmenlength = log10(penguins_filtered$culmen_length_mm)) #creating a new data set with the logged values, to not alter the original data set for reproducibility  
logculmenmodel <- lm(logculmenlength ~ culmen_depth_mm, data = logpenguins) #making a new linear model with the logged culmen length value 

# Checking assumptions 
plot(logculmenmodel, which=2) #The assumption of normality is met.

plot(logculmenmodel, which=1) #Residuals are distributed more evenly than the original dataset. Real data is unlikely to exactly match our assumptions, but the residuals have a more homogeneous variance so I will use the logged dataset. 

## The logged plot matches both assumptions of normality and homogeneous variance. For this reason, I will proceed by using the new mutated 'logpenguin' dataset. I created a new dataset for reproducibility, as to not alter the original data.

# Examining the summary of the linear model 

summary(logculmenmodel)

#Very low adjusted R-squared value of 0.05901, so only around 5.9% of variation in logged culmen length is 'explained' by culmen depth
# So it is unlikely to be that biologically important. 
#However the p-value is 3.28e-06 so there is a significant correlation? between the two variables. 

## Plotting the data

colorblind.friendly("Archambault") #checking the palette is colour-blind friendly for plotting

culmenplot <- plot_culmen_figure(logpenguins)
culmenplot 

## Saving the figure

# Saving the figure as a svg to prevent pixellation and allow zooming

save_culmenplot_svg(logpenguins, "figures/culmen_vector.svg",
                    size = 25, scaling = 1.5)