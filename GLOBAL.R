
library(dplyr)
library(plotly)
library(tidyverse)
library(maps)

my_data<- USArrests 

states<- rownames(my_data)
states
my_data<-my_data %>% 
  mutate(State=states)



p1<-my_data %>% 
  plot_ly() %>% add_histogram(~Rape)

#Boxplot
  p2<-my_data %>% 
  plot_ly() %>% 
  add_boxplot(~Rape)

  
#Choices for Selectinput- NB: with States column
c1 <- my_data %>% 
  select(-State) %>% names()

#Choices for Selectinput- NB: without States  and Urban Population column
c2 <- my_data %>% 
  select(-"State",-"UrbanPop") %>% 
  names()


state_map %>% 
  str()
state_map<- map_data("state")
my_data1<- my_data %>% 
  mutate(State= tolower(State))

merged<- right_join(my_data1, state_map, by= c("State"= "region"))

###adding state abbreviations and center locations for each states. create a dataframe
st = data.frame(abb= state.abb, stname= tolower(state.name), x=state.center$x, y= state.center$y)
new_join = left_join(merged, st, by=c("State" = "stname"))
