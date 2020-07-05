library(ggplot2)
library(plyr)
library(dplyr)
library(Hmisc)
library(stringr)
#library(ggmap)
library(ggthemes)
library(viridis)
library(rgdal)
library(plotly)
library(hrbrthemes)
library(packcircles)
library(ggiraph)
library(devtools)


demographicsData = read.csv('./dataInput/deaths.csv', as.is = TRUE)

# there is a '.' and '90+' in the files so we have to convert them into readible variables.
for (i in 1:length(demographicsData$age)){
  if (demographicsData$age[i] == '.'){
    demographicsData$age[i] <- 'Unknown'
  } else if (demographicsData$age[i] == '90+'){ # will just change 90+ to 90 temporarily for easier to caluclate, then
    # convert later into 90+
    demographicsData$age[i] <- '90'
  }
}
# turn all column into numeric for comparisons later
demographicsData$age <- as.numeric(demographicsData$age)
# get a short name for column 'age'
age <- demographicsData$age
for (i in 1:length(age)){
  if (!is.na(age[i])){
    if (age[i] >= 0 && age[i] < 20){
      age[i] <- '0-19'
    } else if (age[i] >= 20 && age[i] < 30){
      age[i] <- '20-29'
    } else if (age[i] >= 30 && age[i] < 40){
      age[i] <- '30-39'
    } else if (age[i] >= 40 && age[i] < 50){
      age[i] <- '40-49'
    } else if (age[i] >= 50 && age[i] < 60){
      age[i] <- '50-59'
    } else if (age[i] >= 60 && age[i] < 70){
      age[i] <- '60-69'
    } else if (age[i] >= 70 && age[i] < 80){
      age[i] <- '70-79'
    } else if (age[i] >= 80 && age[i] < 90){
      age[i] <- '80-89'
    } else if (age[i] >= 90){
      age[i] <- '90+'
    }
  } else{
    age[i] <- 'Unknown'
  }
}
# Convert back into the data frame
demographicsData$age <- age
# Count how many deaths in each group of age

## AGE
ageFreq <- table(demographicsData$age)
ageFreq <- as.data.frame(ageFreq)
names(ageFreq)[1] <- 'Age'

age_plot <- ggplot(data = ageFreq) +
  geom_bar(aes(x = Age, y = Freq, text = paste0('<b>Age: </b>',Age,'<br>','<b>Number of Deaths: </b>',Freq)), stat = 'identity', fill ='orange', color = 'black')+
  geom_text(aes(x = Age, y = Freq, label = paste0('<b>',Freq)),position=position_stack(vjust=0.5)) +
  labs(title = 'Number of Deaths by Age', y = 'Number of Deaths') + guides(fill = FALSE)
age_fig <- ggplotly(age_plot, tooltip = 'text')
age_fig

sexFreq <- table(demographicsData$sex)
sexFreq <- as.data.frame(sexFreq)
names(sexFreq)[1] <- 'Sex'

# SEX
sex_plot <- ggplot(data = sexFreq) +
  geom_bar(aes(x = Sex, y = Freq, text = paste0('<b>Sex: </b>',Sex,'<br>','<b>Number of Deaths: </b>',Freq)), stat = 'identity', fill ='orange', color = 'black')+
  geom_text(aes(x = Sex, y = Freq, label = paste0('<b>',Freq)),position=position_stack(vjust=0.5)) +
  labs(title = 'Number of Deaths by Sex', y = 'Number of Deaths') + guides(fill = FALSE)
sex_fig <- ggplotly(sex_plot, tooltip = 'text')
sex_fig


# RACE
raceFreq <- table(demographicsData$race)
raceFreq <- as.data.frame(raceFreq)
names(raceFreq)[1] <- 'Race'

# Generate the layout
packing <- circleProgressiveLayout(raceFreq$Freq, sizetype='area')
raceFreq <- cbind(raceFreq, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
dat.gg
# Make the plot with a few differences compared to the static version:
race_plot <- ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = paste0(raceFreq$Race[id],'<br>',"<b>Number of Deaths: </b>",raceFreq$Freq[id]), data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  #geom_text(data = raceFreq, aes(x, y, label = gsub("Race", "", Race)), size=5, color="black") +
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  labs(title = 'Number of Deaths by Race') + guides(fill = FALSE) +
  coord_equal() + theme_void() +
  theme(plot.title = element_text(face = "bold")) + theme(plot.title = element_text(hjust=0.4)) 
# Turn it interactive
race_fig <- ggiraph(ggobj = race_plot, width_svg = 7, height_svg = 7)
race_fig

### Factors Analysis

factors <- demographicsData[,c(1,2,3)]
factors_analysis <- count(factors, vars = c('age','race','sex'))

factors_plot <- ggplot(factors_analysis, aes(x = race, y = age, size = freq, fill = sex)) +
  geom_point(shape = 21, aes(text = paste('<b>Number of Deaths: </b>',freq))) + theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle('Distribution of COVID-19 Deaths in GA by Race, Age & Sex') +
  scale_fill_manual(values = c('violetred1','steelblue','purple')) +
  labs(x = 'Race', y = 'Age') +  scale_size(range = c(3,10)) + 
  theme(axis.text.x = element_text(size = 8))  + guides(fill = FALSE, size = FALSE)

factors_fig <- ggplotly(factors_plot, tooltip = 'text', height = 520) %>%
  layout(autosize=TRUE)
factors_fig
