# Set-up ----
setwd("~/QM 2")

# Turning off scientific notation
options(scipen = 999)

# Loading required libraries
library(foreign)
library(ggplot2)
library(effects)

# Font package
install.packages('showtext')
library(showtext)
font_add_google("Roboto")
showtext_auto()

# Loading data
G16_data <- read.csv ("Group16-2.csv")

# Removing the District of Columbia from analyses ----
states <- G16_data[-c(9), ]
View (states)

# As the District of Columbia is not a state, it is inappropriate to consider it for 'state level' analysis. Therefore, the first step we might take is to remove the District of Columbia from further analyses - considering geographical and population size, it might be more appropriate to consider the district of columbia at a county level, rather than at a 'state' geographical hierarchy for analysis.

# Assumptions of multiple regression: Linearity and variance of residuals (homoskedascity) ----
# Republican vote share
repub_poly <- ggplot (states,
        aes(x = republican_pct_2020,
            y = deaths_per_100k,
            label = state)) +
  geom_text (alpha = 1, size = 8) +
  geom_smooth (method = 'lm',
               formula = y ~ x + I(x^2),
               alpha = 0.2) + # Adding (+\beta x^2)
  theme_classic() +
  theme(plot.title = element_text(colour = 'black', size = 45, face = 'bold',
                                  family = 'Roboto'),
        plot.subtitle = element_text(colour = 'grey30', size = 25, face = 'italic',
                                     family = 'Roboto'),
        plot.caption = element_text(colour = 'grey30', size = 25, face = 'italic',
                                    family = 'Roboto'),
        axis.title = element_text(size = 30,
                                  face = 'bold',
                                  family = 'Roboto'),
        axis.text = element_text(size = 30,
                                 family = 'Roboto',
                                 colour = 'black'),
        legend.position = 'none') +
  labs (title = "Republican Vote Share and Gun Violence Mortality Rate",
        subtitle = "Polynomial regression",
        caption = "Produced using data from the Gun Violence Association (2021) and MIT Election Data and Science Lab (2022)",
        x = "Republican Vote Share [2020]",
        y = "Gun Violence Mortality Rate [2021]")

ggsave(repub_poly, file = "repub_poly.png", dpi = 300)

# 'White Alone' ethnicity
white_poly <- ggplot (states,
        aes(x = white_alone_pct,
            y = deaths_per_100k,
            label = state)) +
  geom_text (alpha = 1, size = 8) +
  geom_smooth (method = 'lm',
               formula = y ~ x + I(x^2),
               alpha = 0.2) + # Adding (+\beta x^2)
  theme_classic() +
  theme(plot.title = element_text(colour = 'black', size = 45, face = 'bold',
                                  family = 'Roboto'),
        plot.subtitle = element_text(colour = 'grey30', size = 25, face = 'italic',
                                     family = 'Roboto'),
        plot.caption = element_text(colour = 'grey30', size = 25, face = 'italic',
                                    family = 'Roboto'),
        axis.title = element_text(size = 30,
                                  face = 'bold',
                                  family = 'Roboto'),
        axis.text = element_text(size = 30,
                                 family = 'Roboto',
                                 colour = 'black'),
        legend.position = 'none') +
  labs (title = "'White Alone' Ethnicity Percentage and Gun Violence Mortality Rate",
        subtitle = "Polynomial regression",
        caption = "Produced using data from the Gun Violence Association (2021) and US Decennial Census (2020)",
        x = "'White Alone' Ethnicity Percentage [2020]",
        y = "Gun Violence Mortality Rate [2021]")

ggsave(white_poly, file = "white_poly.png", dpi = 300)

# Comparing models
## Original regression model
model1 <- lm (deaths_per_100k ~ republican_pct_2020 + white_alone_pct + household_less_than_10k, data = states)
summary (model1)

## Polynomial regression model
model2 <- lm (deaths_per_100k ~ republican_pct_2020 + I(republican_pct_2020^2) + white_alone_pct + I(white_alone_pct^2) + household_less_than_10k, data = states)
summary (model2)

# Forest plots and nice regression tables
library(jtools)
library(huxtable)
library(broom.mixed)
library(modelsummary)
install.packages("modelsummary")
install.packages('huxtable')
install.packages('broom.mixed')

# forest plots
plot_summs(model1, model2, model.names = c("Without Polynomial Terms", "With Polynomial Terms"))

# nice regression table 2
msummary(list(model1, model2))

# Examining residual values for original and updated model
car::residualPlots (model1, grid = FALSE, main = "Original Model Residual Plots")
car::residualPlots (model2, grid = FALSE, main = "Model Including Polynomial Terms Residual Plots")

# Distribution of gun violence mortality rate
GVMR_dist <- ggplot (states, aes (x = deaths_per_100k)) +
  geom_density (fill = '#0072C6') + 
  geom_vline(aes (xintercept = mean(deaths_per_100k)), 
             color="#D55E00", linetype="dashed", size=1) +
  theme_classic() +
  theme(plot.title = element_text(colour = 'black', size = 45, face = 'bold',
                                  family = 'Roboto'),
        plot.subtitle = element_text(colour = 'grey30', size = 25, face = 'italic',
                                     family = 'Roboto'),
        plot.caption = element_text(colour = 'grey30', size = 25, face = 'italic',
                                    family = 'Roboto'),
        axis.title = element_text(size = 30,
                                  face = 'bold',
                                  family = 'Roboto'),
        axis.text = element_text(size = 30,
                                 family = 'Roboto',
                                 colour = 'black'),
        legend.position = 'none') +
  labs (title = "Distribution of Gun Violence Mortality Rate",
        caption = "Produced using data from the Gun Violence Association (2021)",
        x = "Gun Violence Mortality Rate [2021]",
        y = "Density")
ggsave(GVMR_dist, file = "GVMR_dist.png", dpi = 300)

# Examining transformations of the gun violence mortality rate
car::symbox (~deaths_per_100k, data = states, na.rm = TRUE)

# Plotting the distribution of the log transformed gun violence mortality rate
logGVMR_dist <- ggplot (states, aes (x = log(deaths_per_100k))) +
  geom_density (fill = '#0072C6') + 
  geom_vline(aes (xintercept = mean(log(deaths_per_100k))), 
             color="#D55E00", linetype="dashed", size=1) +
  theme_classic() +
  theme(plot.title = element_text(colour = 'black', size = 45, face = 'bold',
                                  family = 'Roboto'),
        plot.subtitle = element_text(colour = 'grey30', size = 25, face = 'italic',
                                     family = 'Roboto'),
        plot.caption = element_text(colour = 'grey30', size = 25, face = 'italic',
                                    family = 'Roboto'),
        axis.title = element_text(size = 30,
                                  face = 'bold',
                                  family = 'Roboto'),
        axis.text = element_text(size = 30,
                                 family = 'Roboto',
                                 colour = 'black'),
        legend.position = 'none') +
  labs (title = "Distribution of Logaritmic Transformed Gun Violence Mortality Rate",
        caption = "Produced using data from the Gun Violence Association (2021)",
        x = "Log Gun Violence Mortality Rate [2021]",
        y = "Density")
ggsave(logGVMR_dist, file = "logGVMR_dist.png", dpi = 300)

## Final regression model
final_model <- lm (log(deaths_per_100k) ~ republican_pct_2020 + I(republican_pct_2020^2) + white_alone_pct + I(white_alone_pct^2) + household_less_than_10k, data = states)
summary (final_model)

# Calculating exponential of slope coefficients
exp(coef(final_model))

# Residual plot
car::residualPlots (final_model, grid = FALSE, main = "Final Model (log dependent variable and polynomial terms) Residual Plots")

# Assumptions of multiple regression: Normality of residuals ----
# List of model residuals
resids <- resid(final_model)

# Adding model residuals to dataframe
states$fm_residuals <- resids

# Normal probability plot of final model residuals (final model)
qq_fm <- qplot(sample = fm_residuals, data = states)

# Formatted qq plot
norm_plot_fm <- qq_fm +
  stat_qq_line(colour = "red") +
  theme_classic() +
  theme(plot.title = element_text(colour = 'black', size = 45, face = 'bold',
                                  family = 'Roboto'),
        plot.subtitle = element_text(colour = 'grey30', size = 25, face = 'italic',
                                     family = 'Roboto'),
        plot.caption = element_text(colour = 'grey30', size = 17.5, face = 'italic',
                                    family = 'Roboto'),
        axis.title = element_text(size = 30,
                                  face = 'bold',
                                  family = 'Roboto'),
        axis.text = element_text(size = 30,
                                 family = 'Roboto',
                                 colour = 'black'),
        legend.position = 'none') +
  labs (title = "Normal Probability Plot of Final Model Residuals",
        caption = "Produced using data from the Gun Violence Association (2021), MIT Election Data and Science Lab (2022), US Decennial Census (2020) and American Community Survey 5-Year Estimates (2020)",
        x = "Theoretical Quantiles",
        y = "Sample Quantiles")
ggsave(norm_plot_fm, file = "norm_plot_fm.png", dpi = 300)

# Distribution of final model residuals
fm_hist <- ggplot (states, aes (x = resids)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density (fill = '#0072C6', colour = '#0072C6', alpha = 0.25) + 
  theme_classic() +
  theme(plot.title = element_text(colour = 'black', size = 45, face = 'bold',
                                  family = 'Roboto'),
        plot.subtitle = element_text(colour = 'grey30', size = 25, face = 'italic',
                                     family = 'Roboto'),
        plot.caption = element_text(colour = 'grey30', size = 17.5, face = 'italic',
                                    family = 'Roboto'),
        axis.title = element_text(size = 30,
                                  face = 'bold',
                                  family = 'Roboto'),
        axis.text = element_text(size = 30,
                                 family = 'Roboto',
                                 colour = 'black'),
        legend.position = 'none') +
  labs (title = "Final Model Residuals",
        caption = "Produced using data from the Gun Violence Association (2021), MIT Election Data and Science Lab (2022), US Decennial Census (2020) and American Community Survey 5-Year Estimates (2020)",
        x = "Final Model Residual Values",
        y = "Density")
ggsave(fm_hist, file = "fm_hist.png", dpi = 300)

# Here, residual values are compared to what we would expect if residuals were normally distributed. As such, we see a relatively symmetric distribution, however with somewhat 'fat tails'. This indicates that there are greater residual values at the extreme ends of the distribution than would be expected (if the residuals followed a normal distribution). However, as we have already attempted to address other assumptions (including transformations and polynomial terms), there is a limited spectrum of potential fixes we could add to our model to address this without adding further complexity to an already complex model. As such, we must accept that the final model presented is likely to be inefficient in its provided estimations. 


# Assumptions of multiple regression: Multicollinearity  ----
# Variance inflation factor for original model
library(car)
vif(model1)

# Tolerance statistic
1/(vif(model1))

# Model evaluation AND INTERPRETATION ----
## log regression model without polynomial terms
final_model_no_poly <- lm (log(deaths_per_100k) ~ republican_pct_2020 + white_alone_pct + household_less_than_10k, data = states)
summary (final_model_no_poly)
summary (final_model)

# Analysis of variance test for the final model and a model that does not include polynomial terms
anova (final_model_no_poly, final_model)

# Pretty regression table
msummary(list(final_model_no_poly, final_model))

# Geographically Weighted Regression set-up ----
#Installing packages
install.packages ("rgdal")
install.packages ("rgeos")

# Loading relevant libraries
library("sp")
library("rgdal")
library("rgeos")
library("tmap")
library("spgwr")
library("grid")
library("gridExtra")
library("ggplot2")
library('RColorBrewer')

#Load United States shapefile
states_dc <- readOGR ("~/QM 2", "states")

# Joining dataframe to shapefile
g16.states.50 <- merge (states_dc, states, by.x = "STATE_ABBR", by.y = "code")

# GWR: Creating a SpatialPolygonsDataFrame for GWR ----
# Assigning model residuals to an object so they may be mapped
resids <- residuals(final_model)

# Adding an 'na' value for the district of columbia, as it is not included in analyses
resids <- c(resids[1:27], NA, resids[28:50])

# Creating a duplicate SPDF object to preserve the original for later use
fm.resids <- g16.states.50

# Joining residuals to duplicate SPDF
fm.resids@data <- cbind (g16.states.50@data, resids)

# Checking left join implemented correctly
View (fm.resids@data)

# Creating an object for assessing the index position of the 'District of Columbia'
polygons <- fm.resids

# Finding the index of the polygon corresponding to the District of Columbia using the 'STATE_NAME' column
dc_index <- match("District of Columbia", polygons@data$STATE_NAME)

# Use the index to extract the polygon corresponding to the District of Columbia
dc_polygon <- polygons[dc_index,]
print(dc_polygon)

# Creating a spatialpolygonsdataframe with the district of Columbia polygon removed
fm.resids.NO_DC <- fm.resids
fm.resids.NO_DC <- fm.resids.NO_DC[-28, ]
# GWR: Mapping residual values ----
# Map of final model residual values
resid_map <- tm_shape(fm.resids.NO_DC) +
  tm_fill('resids', midpoint = 0, style = 'quantile', palette = 'BrBG', title = '', textNA = "District of Columbia") +
  tm_layout (frame = TRUE, title = "Final Model Residuals", title.size = 5,
             title.position = c(0.65,0.95), title.fontfamily = 'Roboto', title.fontface = 'bold',
             legend.show = TRUE, legend.title.size = 3, legend.title.fontfamily = 'Roboto',
             legend.title.fontface = 'bold', legend.text.size = 1.5, legend.position = c(0.85,0.62)) +
  tm_borders (col = 'black', alpha = 0.1)
tmap_save(resid_map, "resid_map.png")

# GWR: Creating a SPDF containing GWR results ---- 
# Calculating an adaptive kernel bandwidth to utilise in a geographically weighted regression
adaptive_bw <- gwr.sel (log(deaths_per_100k) ~ republican_pct_2020 + I(republican_pct_2020^2) + white_alone_pct + I(white_alone_pct^2) + household_less_than_10k, data = fm.resids.NO_DC, adapt = TRUE)

# Creating a GWR object
g16.gwr <- gwr (log(deaths_per_100k) ~ republican_pct_2020 + I(republican_pct_2020^2) + white_alone_pct + I(white_alone_pct^2) + household_less_than_10k, data = fm.resids.NO_DC, adapt = adaptive_bw, se.fit = TRUE)

# Examining the results
g16.gwr

# Storing GWR output results to a data frame object
GwrDataFrame <- as.data.frame (g16.gwr$SDF)

# Creating an object for global GWR R-squared value
gwr_global_r2 <- round((1 - (g16.gwr$results$rss/g16.gwr$gTSS)), 2)

# Binding the outputs to the SpatialPolygonsDataFrame so they may be mapped
g16.gwr.map <- fm.resids.NO_DC
g16.gwr.map@data <- cbind (fm.resids.NO_DC@data, as.matrix(GwrDataFrame))

# Checking column names
names (g16.gwr.map@data)

# Renaming the correlation coefficient columns (prior to renaming they are simply named as the variable, thus creating duplicate variable names in the dataframe)
names (g16.gwr.map@data) [22:26] <- c("repub_corr", "repub_poly_corr", "white_corr", "white_poly_corr",
                                      "household_corr")

# Checking the renaming has worked correctly
names (g16.gwr.map@data)
# GWR: Maps ----
# Map of local r-squared
r2_map <- tm_shape(g16.gwr.map) +
  tm_fill('localR2', midpoint = 0, style = 'quantile', palette = 'Blues', title = '', textNA = "District of Columbia") +
  tm_layout (frame = TRUE, title = "GWR Local R-Squared", title.size = 5,
             title.position = c(0.65,0.95), title.fontfamily = 'Roboto', title.fontface = 'bold',
             legend.show = TRUE, legend.title.size = 3, legend.title.fontfamily = 'Roboto',
             legend.title.fontface = 'bold', legend.text.size = 1.5, legend.position = c(0.85,0.62)) +
  tm_borders (col = 'black', alpha = 0.1)
tmap_save(r2_map, "r2_map.png")

# Map of Republican Vote Share % Coefficient
repub_map <- tm_shape(g16.gwr.map) +
  tm_fill('repub_corr', midpoint = 0, style = 'quantile', palette = 'BrBG', title = '', textNA = "District of Columbia") +
  tm_layout (frame = TRUE, title = "Republican Vote Share GWR Coefficient", title.size = 5,
             title.position = c(0.4,0.95), title.fontfamily = 'Roboto', title.fontface = 'bold',
             legend.show = TRUE, legend.title.size = 3, legend.title.fontfamily = 'Roboto',
             legend.title.fontface = 'bold', legend.text.size = 1.5, legend.position = c(0.85,0.62)) +
  tm_borders (col = 'black', alpha = 0.1)
tmap_save(repub_map, "repub_map.png")

# Map of Republican Vote Share % Polynomial Term Correlation Coefficient
repub_poly_map <- tm_shape(g16.gwr.map) +
  tm_fill('repub_poly_corr', midpoint = 0, style = 'quantile', palette = 'BrBG', title = '', textNA = "District of Columbia") +
  tm_layout (frame = TRUE, title = "Republican Vote Share Quadratic Term GWR Coefficient", title.size = 3.5,
             title.position = c(0.4,0.95), title.fontfamily = 'Roboto', title.fontface = 'bold',
             legend.show = TRUE, legend.title.size = 3, legend.title.fontfamily = 'Roboto',
             legend.title.fontface = 'bold', legend.text.size = 1.5, legend.position = c(0.825,0.62)) +
  tm_borders (col = 'black', alpha = 0.1)
tmap_save(repub_poly_map, "repub_poly_map.png")

# Map of 'White Alone' ethinicity population proportion correlation coefficent
white_map <- tm_shape(g16.gwr.map) +
  tm_fill('white_corr', midpoint = 0, style = 'quantile', palette = 'PuOr', title = '', textNA = "District of Columbia") +
  tm_layout (frame = TRUE, title = "'White Alone' Ethnicity GWR Coefficient", title.size = 5,
             title.position = c(0.4,0.95), title.fontfamily = 'Roboto', title.fontface = 'bold',
             legend.show = TRUE, legend.title.size = 3, legend.title.fontfamily = 'Roboto',
             legend.title.fontface = 'bold', legend.text.size = 1.5, legend.position = c(0.85,0.62)) +
  tm_borders (col = 'black', alpha = 0.1)
tmap_save(white_map, "white_map.png")

# Map of 'Households Earning Less Than $10,000 per year' correlation coefficent
household_map <- tm_shape(g16.gwr.map) +
  tm_fill('household_corr', midpoint = 0, style = 'quantile', palette = 'RdBu', title = '', textNA = "District of Columbia") +
  tm_layout (frame = TRUE, title = "Proportion of Households Earning Less Than $10k GWR Coefficient", title.size = 3,
             title.position = c(0.38,0.95), title.fontfamily = 'Roboto', title.fontface = 'bold',
             legend.show = TRUE, legend.title.size = 3, legend.title.fontfamily = 'Roboto',
             legend.title.fontface = 'bold', legend.text.size = 1.5, legend.position = c(0.85,0.62)) +
  tm_borders (col = 'black', alpha = 0.1)
tmap_save(household_map, "household_map.png")


