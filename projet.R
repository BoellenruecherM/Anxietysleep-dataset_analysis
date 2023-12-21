#######################################################################
#
# Programming Skills Project: Dataset Analysis on Sleep Anxiety During Covid  
# 
# Context:
# We inspect the influence of the Covid-19 pandemic on global dynamics and its notable effects on
# mental health, particularly anxiety and sleep patterns. The study, that we based on, compared
# total and partial confinement in two zones, considering age and gender as additional variables.
# The collected data is available in an R package (AnxietySleep).
# 
# Objectives:
# 1. Import and preprocess the dataset, ensuring data integrity and handling missing values.
# 2. Conduct univariate analysis, providing numeric summaries, and graphical representations
#    for key variables such as age, categories of age, sex, zone, Beck global index, and Pitts global
#    index.
# 3. Perform multivariate analysis, specifically focusing on anxiety analysis using the Beck
#    inventory and sleep quality analysis using the Pitts inventory. This includes descriptive
#    analysis and plots.
# 4. Explore the relationship between anxiety and sleep quality using a mosaic plot.
#
# Author: M. Böllenrücher, F. Crettol
#
# Date and location: 22-12-2023, St. Gallen
#######################################################################


# Setup: Libraries and Data Loading -------------------------------------

### Load Packages ### 
library(AnxietySleep)
library(ggplot2)
library(plotly)
library(ggpubr)
library(ggmosaic)
library(tidyverse)

### Import Data ###
data("anxiety")  # Load anxiety dataset from the AnxietySleep package
df <- as.data.frame(anxiety)  # Convert the dataset to a data frame 


# Database structure and manipulation -------------------------------------

# The dataset comprises 7 variables and 617 observations. Beck Anxiety Inventory scores, for 
# instance, are summed to categorize anxiety levels: 0–7 (no anxiety), 8–15 (mild anxiety),
# 16–25 (moderate anxiety), and 26–63 (severe anxiety). Similarly, Pittsburgh Sleep Quality Index
# scores are summed to classify sleep quality as ≤5 (good) or >5 (poor). Additionally, four factors
# ---id, sex, zone, and cat_age---are featured. Nominal variables like sex and zone require no
# adjustments, with sex having "Male" and "Female" levels, and zone divided
# into "PZ" (partially confined) and "CZ" (totally confined).

### Structure and missing values ###
any(is.na(df))  # Check if there is missing values, as it returns FALSE, there are none

### Inspect the structure ###
str(df)
summary(df)

### Manipulation ###
df$id <- as.character(df$id)  # Convert id to character type

# Recode cat_age, beck_global and pits_global as ordered factor with specification about the order
# of the variable.
# Recode cat_age as an ordered factor with specific levels and order.
df$cat_age <- factor(df$cat_age, ordered = TRUE,
                     levels = c("18-25","26-40","41-50",">50"))

# We order the following variables by using cut() and creating intervals through "breaks"
# (e.g. [8,16), meaning the interval stops at 15), because in the dataset they were integer and not
# nominal variables as they are in the Beck Anxiety Inventory and Pittsburgh Sleep Quality Index.
# Plus, we assign a specific range to these intervals through "labels", we use the nomenclature
# "-inf" to refers to 0.
df$beck_global <- cut(df$beck_global, breaks=c(-Inf, 8, 16, 26, 63),
                      labels = c("0-7", "8-15", "16-25", "26-63"),
                      ordered_result = "TRUE")

df$pits_global <- cut(df$pits_global, breaks=c(-Inf, 6, Inf),
                      labels = c("≤5", ">5"),
                      ordered_result = "TRUE")

### Inspect the structure after manipulations ###
str(df)
summary(df)


# Univariate analysis  ----------------------------------------------------

# The univariate analysis will consist of two components: a numerical examination of various 
# variables and their graphical representation.

##### Numerical examination #####

### Age variable ###
# We examine the "age" variable, paying attention to its Variance, Standard deviation, and
# Interquartile range as it is a quantitative variable.
summary(df$age)
var(df$age)
sd(df$age)
IQR(df$age)

# We compute tables for the following qualitative variables, giving the percentage of participants
# by category with the proportions() function, and then returning the category with the biggest
# number of occurrences using a special indexing. The sort() function allows to sort the table and 
# we specify by decreasing chronology and then indexing [1]. With the names() function, we return
# the name of the category.

### Categories of age variable ###
round(proportions(table(df$cat_age)), 3)  # Compute and round categorical variable percentages
names(sort(table(df$cat_age), decreasing = TRUE))[1]  # Identify most frequent category in variable

### Sex variable ###
round(proportions(table(df$sex)), 3)
names(sort(table(df$sex), decreasing = TRUE))[1]

### Zone variable ###
round(proportions(table(df$zone)), 3)
names(sort(table(df$zone), decreasing = TRUE))[1]

### Beck variable ###
round(proportions(table(df$beck_global)), 3)
names(sort(table(df$beck_global), decreasing = TRUE))[1]

### Pits variable ###
round(proportions(table(df$pits_global)), 3)
names(sort(table(df$pits_global), decreasing = TRUE))[1]

##### Graphical representation #####

# We design bar charts for the following variables. The ggplot() function defines a plot object to
# which layers are added using "+". The mapping function is always defined as aes() to map x and y
# axis. The geom() function represents the geometrical object, here bar, to represent data in bar
# charts. Additional layers improve readability, including y-axis limits, a specific theme, and
# titles for the chart and axes. Moreover, ggplotly() is used to convert static ggplot2
# visualizations into interactive plots using the plotly library.

### Age variable ###

bar_age_ly <- ggplotly(
  ggplot(df, aes(x = age)) +
    geom_bar(color="black", fill="lightblue") +
    ylim(0, 40) +
    labs(title = "Bar plot of the age",
         x = "Age", y = "Count") +
    theme_minimal()
)

### Categories of age variable ###

bar_catage_ly <- ggplotly(
  ggplot(df, aes(x=cat_age))+
    geom_bar(color="black", fill="lightblue")+
    ylim(0, 220) +
    labs(title = "Bar plot of the categories of age",
         x="Categories of age", y ="Count")+
    theme_minimal()
)

### Sex variable ###

bar_sex_ly <- ggplotly(
  ggplot(df, aes(x=sex))+
    geom_bar(color="black", fill="lightblue")+
    ylim(0, 500) +
    labs(title = "Bar plot of the sex",
         x="Sex", y ="Count")+
    theme_minimal()
)

### Zone variable ###

bar_zone_ly <- ggplotly(
  ggplot(df, aes(x=zone))+
    geom_bar(color="black", fill="lightblue")+
    ylim(0, 350) +
    labs(title = "Bar plot of the zones",
         x="Zones", y ="Count")+
    theme_minimal()
)

### Beck variable ###

bar_beck_ly <- ggplotly(
  ggplot(df, aes(x=beck_global)) +
    geom_bar(color="black", fill="lightblue") +
    ylim(0, 400) +
    labs(title = "Bar plot of the Beck global index",
         x= "Beck global",y = "Count") +
    theme_minimal()
)

### Pits variable ###

bar_pits_ly <- ggplotly(
  ggplot(df, aes(x=pits_global)) +
    geom_bar(color="black", fill="lightblue") +
    ylim(0, 450) +
    labs(title = "Bar plot of the Pitts global index",
         x= "Pitts global",y = "Count") +
    theme_minimal()
)

### Plots of all univariate variables ###

# For increasing the readability and a better communication of outcomes, we display individual bar
# charts for each variable in a single graph.

subplot(bar_age_ly,  # Combine the bar charts using the subplot() function
        bar_sex_ly,
        bar_zone_ly,
        bar_catage_ly,
        bar_beck_ly,
        bar_pits_ly,
        nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.05) |>  # title displays the axis titles 
  layout(title = "Univariate plots")  # Customize the layout of the combined plot


# Multivariate analysis  --------------------------------------------------

# Given that the objective is to elucidate the influence of confinement on anxiety levels and sleep 
# quality, we sequentially delve into multivariate analyses for these two variables (anxiety and sleep).
# The intention is to display the associations between age, gender, and confinement zone with anxiety 
# and sleep. Therefore, we start by making cross tables of all the variables to get an initial overview 
# of the situation. Next, a numerical analysis is performed to extend the graphical conclusions and 
# calculate the proportions of individuals in the different modalities of the variables. We then 
# perform a graphical representation of the variables. Finally, we look at the Relation between
# anxiety and sleep.

##### Anxiety analysis: Beck inventory #####

### Descriptive analysis ###

# The first tables will show the number of occurrences for the beck global index depending on each
# three variables: age, gender, and confinement zone. The second tables will display the relative
# frequency for each three categorical variables.

table(df$beck_global, df$cat_age)
table(df$beck_global, df$sex)
table(df$beck_global, df$zone)

round(proportions(table(df$beck_global, df$cat_age)),3)
round(proportions(table(df$beck_global, df$sex)),3)
round(proportions(table(df$beck_global, df$zone)),3)

# We summarize the occurrences of the three categorical variables in one table using ftable()

ftable(df$beck_global, df$zone, df$sex, df$cat_age)

### Plots ###

# As for the univariate graphical representation, we design a bar chart, but this time we add a 
# layer with the function facet_grid() that generates a subplot grid, exploring relationships 
# between Beck inventory scores (beck_global), "sex", and categorical variables "zone" and 
# "cat_age." Each facet corresponds to a unique combination of "zone" and "cat_age," displaying bars  
# for different levels of the "sex" variable with "fill = sex" in distinct colors with
# "scale_fill_manual". Specifying the "position = dodge" enables to have distinct bars for the levels
# of the sex instead of one bar combining the two.

ggplotly(
  ggplot(df, aes(x=beck_global, fill = sex))+
    geom_bar(position = "dodge")+
    facet_grid(vars(zone), vars(cat_age)) +
    labs(title = "Plot of the Beck inventory with respect to zone, categories of age and sex",
         x="Beck global", y ="Count")+ 
    scale_fill_manual(values=c("lightgreen","orange"))
)

##### Sleep quality analysis: Pitts inventory #####

### Descriptive analysis ###

# The first tables will show the number of occurrences for the Pitts global index depending on each
# three variables: age, gender, and confinement zone. The second tables will display the relative
# frequency for each three categorical variables. Lastly, we summarize the occurrences of the three 
# categorical variables in one table using ftable().

table(df$pits_global, df$cat_age)
table(df$pits_global, df$sex)
table(df$pits_global, df$zone)

round(proportions(table(df$pits_global, df$cat_age)),3)
round(proportions(table(df$pits_global, df$sex)),3)
round(proportions(table(df$pits_global, df$zone)),3)

ftable(df$pits_global, df$zone, df$sex, df$cat_age)

### Plots ###

# It's the same plot as for beck_global variable

ggplotly(
  ggplot(df, aes(x=pits_global, fill = sex))+
    geom_bar(position = "dodge")+
    facet_grid(vars(zone), vars(cat_age)) +
    labs(title = "Plot of the Pitts inventory with respect to zone, categories of age and sex",
         x="Pitts global", y ="Count")+ 
    scale_fill_manual(values=c("lightgreen","orange"))
)

##### Relation between anxiety and sleep #####

# Finally, we overlap both index and as before we create two tables, the first one 
# displaying the occurrences and the second one the relative frequency

table(df$beck_global, df$pits_global)
round(proportions(table(df$beck_global, df$pits_global)),3)

# To round off the project, we show the relationship between the Pitts and Beck indexes, with a
# mosaic plot. This type of plot displays the relative proportions of categories within each
# variable and the interactions between them using tiled rectangles. Here, we do not take the
# aesthetic mapping at the global level (as before in ggplot), but we take it locally in
# "geom_mosaic". It is necessary as the goal is to overlap both indexes via the argument product()
# and display the different colors (fill =) for the respective levels of the pits_global variable.
# In line with this, we do not want to show the levels on the y-axis and rely on a color legend
# instead. For this purpose, we add a layer by stating a blank element for the y-axis text.

ggplotly(
  ggplot(df) +
    geom_mosaic(aes(x = product(pits_global, beck_global), fill = pits_global)) +
    labs(title = "Mosaic plot between anxiety and sleep",
         x= "Beck global",y = "Pitts global") + 
    theme_minimal() +
    theme(axis.text.y = element_blank())
)

