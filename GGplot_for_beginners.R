#This script will take you through how to modify plots made with the ggplot2 package
#This script is not an exhaustive list of what you can do with ggplot, but this should get you started!
#The example file to use with this script is title 'R_Example_meeting'

#set working directory
#open libraries
library(ggplot2)

# Let's start with a basic plot
P1 = ggplot(R_Example_meeting, aes())
P1

# Let's specify we want Depth on our Y axis
P1 = ggplot(R_Example_meeting, aes( ,Depth))
P1

# Let's add in some data. We have many options.
# The ones I use most often are geom_point(), geom_path(), and geom_line()
P1 = ggplot(R_Example_meeting, aes( ,Depth)) + 
  geom_path(aes(x = DIC_mM)) +
  geom_point(aes(x = Temp))
P1

# Let's change our X axis title
P1 = ggplot(R_Example_meeting, aes( ,Depth)) +
  geom_path(aes(x = DIC_mM)) +
  geom_point(aes(x = Temp)) +
  scale_x_continuous(name = "Brownie Lake")
P1

# ...and let's rename our Y axis
P1 = ggplot(R_Example_meeting, aes( ,Depth)) +
  geom_path(aes(x = DIC_mM)) +
  geom_point(aes(x = Temp)) +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_continuous(name = "Depth (m)")
P1

# Every good graph needs a title
P1 = ggplot(R_Example_meeting, aes( ,Depth)) +
  geom_path(aes(x = DIC_mM)) +
  geom_point(aes(x = Temp)) +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_continuous(name = "Depth (m)") +
  labs(title="Nick's pretty graph")
P1

# Let's center the title
P1 = ggplot(R_Example_meeting, aes( ,Depth)) +
  geom_path(aes(x = DIC_mM)) +
  geom_point(aes(x = Temp)) +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_continuous(name = "Depth (m)") +
  labs(title="Nick's pretty graph") + theme(plot.title = element_text(hjust = 0.5))
P1

# Let's change the background theme to white
P1 = ggplot(R_Example_meeting, aes( ,Depth)) +
  geom_path(aes(x = DIC_mM)) +
  geom_point(aes(x = Temp)) +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_continuous(name = "Depth (m)") +
  labs(title="Nick's pretty graph") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic()
P1

# Wait, now our title is not centered. Why? Because the last command was a theme command and GGplot follows the last command. 
# I will reverse the commands to center the title while keeping the new theme
P1 = ggplot(R_Example_meeting, aes( ,Depth)) +
  geom_path(aes(x = DIC_mM)) +
  geom_point(aes(x = Temp)) +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_continuous(name = "Depth (m)") +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1

# Let's add some color to our variables so we can set them apart
# Depending on where you use 'color' and the value, you get different outcomes
# try adding ,color=Date inside the aes()...see how it changes the color
P1 = ggplot(R_Example_meeting, aes( ,y=Depth)) +
  geom_point(aes(x = DIC_mM), color='steelblue') +
  geom_point(aes(x = Temp), color='thistle') +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_continuous(name = "Depth (m)") +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1

# Let's fix the y axis. 
# First, the maximum depth of our data set is 13 m. Let's get our y axis to reflect that
P1 = ggplot(R_Example_meeting, aes(x=,y=Depth)) +
  geom_point(aes(x = DIC_mM), color='steelblue') +
  geom_point(aes(x = Temp), color='thistle') +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_continuous(name = "Depth (m)", breaks = seq(0,13,1)) +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1

# But wait, we want 1 to be at the top, mirroring the top of the water column. 
# To do this, we change out "_continuous" with "_reverse" for the y axis
P1 = ggplot(R_Example_meeting, aes(x=,y=Depth)) +
  geom_point(aes(x = DIC_mM), color='steelblue') +
  geom_point(aes(x = Temp), color='thistle') +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_reverse(name = "Depth (m)", breaks = seq(0,13,1)) +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1

# Let's say I want to add a line connecting my dots
# Use the geom_path or geom_line command
# I also changed the points for DIC to be colored by month and made the line color grey
P1 = ggplot(R_Example_meeting, aes(x=,y=Depth)) +
  geom_point(aes(x = DIC_mM, color=Date)) +
  geom_path(aes(x = DIC_mM), color='grey') +
  geom_point(aes(x = Temp), color='thistle') +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_reverse(name = "Depth (m)", breaks = seq(0,13,1)) +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1 

# NOTICE: Using the geom_path, I have missing intervals. These are NA's in my excel sheet
# We can omit the NA "values" and connect our dots using the data= na.omit() function
P1 = ggplot(R_Example_meeting, aes(x=,y=Depth)) +
  geom_point(aes(x = DIC_mM, color=Date)) +
  geom_path(aes(x = DIC_mM), color='grey', data = na.omit(R_Example_meeting)) +
  geom_point(aes(x = Temp), color='thistle') +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_reverse(name = "Depth (m)", breaks = seq(0,13,1)) +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1

# Let's change all the lines to a different style
P1 = ggplot(R_Example_meeting, aes(x=,y=Depth)) +
  geom_point(aes(x = DIC_mM, color=Date)) +
  geom_path(aes(x = DIC_mM), color='grey', data = na.omit(R_Example_meeting), linetype=5) +
  geom_point(aes(x = Temp), color='thistle') +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_reverse(name = "Depth (m)", breaks = seq(0,13,1)) +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1 

# Let's get new symbols
# Let's change the symbols according to the sampling date for DIC and to squares for Temp
P1 = ggplot(R_Example_meeting, aes(x=,y=Depth)) +
  geom_point(aes(x = DIC_mM, color=Date, shape= Date)) +
  geom_path(aes(x = DIC_mM), color='grey', data = na.omit(R_Example_meeting), linetype=5) +
  geom_point(aes(x = Temp), color='thistle', shape=15) +
  scale_x_continuous(name = "Brownie Lake") +
  scale_y_reverse(name = "Depth (m)", breaks = seq(0,13,1)) +
  labs(title="Nick's pretty graph") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1

# Let's change the size of the dots for both DIC and Temp.
# To do this, use the size=# command
# I'm also changing the x axis title
P1 = ggplot(R_Example_meeting, aes(x=,y=Depth)) +
  geom_point(aes(x = DIC_mM, color=Date, shape=Date), size=2) +
  geom_path(aes(x = DIC_mM), color='grey', data = na.omit(R_Example_meeting), linetype=5) +
  geom_point(aes(x = Temp), color='thistle', shape=15, size=2) +
  scale_x_continuous(name = "DIC (mM) and Temp (C)") +
  scale_y_reverse(name = "Depth (m)", breaks = seq(0,13,1)) +
  labs(title="Nick's pretty graph", caption="Swanner et al. (2019)") + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
P1 

# Let's modify our graph's title and other elements
Modified <- P1 + theme(plot.title = element_text(size=20, color="hotpink2", family = "Times"), 
           axis.title.x = element_text(size=15, face="bold"),
           axis.title.y = element_text(size=15, face="bold"),
           axis.text.x = element_text(size=12),
           axis.text.y = element_text(size = 12),
           plot.caption = element_text(size = 10))
Modified

# To change the order of the axis legend
Modified + scale_color_discrete(breaks=c("April_2017", "May_2017", "July_2017", "September_2017"))

# Let's modify the legend
CoolPlot = Modified + scale_color_manual(name="Field Campaign", labels=c("April '17", "July '17", "May '17", "September '17"), 
          values=c("red", "orange", "green", "purple")) +
          scale_shape_manual(name="Field Campaign", labels=c("April '17", "July '17", "May '17", "September '17"), values=c("April_2017"="circle", "July_2017"="triangle", "May_2017"="square", "September_2017"="plus"))
CoolPlot + scale_col

### FACETING (see also facet_grid tutorial)
# Let's group our data by a specific function. In our case, let's do by date
# Facet Wrap
P2 = CoolPlot + facet_wrap(.~Date) 
P2

# Facet Grid
# If you have two discreet variables (e.g. Sampling Month and Year), you could make a grid like facet_grid(Sampling Month ~ Year)
# This would group Sampling month in rows and Year in columns
P3 = CoolPlot + facet_grid(.~Date) + labs(subtitle="Brownie Lake - 2017")
P3

# Modify labels when facetting
# First, create a character vector
Months <- c('April_2017'="April", 'July_2017'="July", 'May_2017'="May", 'September_2017'="September")

# Second, Use vector as a labeler
P2 = CoolPlot + facet_wrap(.~Date, labeller = as_labeller(Months)) + labs(subtitle="Brownie Lake - 2017")
P2

# Let's spread out our panels a little, edit our facet labels, make our axis text black, and remove our legend
P4 = P2 + theme(panel.spacing = unit(2, "lines"), axis.text.y.left = element_text(color = "black"), 
                axis.text.x.bottom = element_text(color = "black"), 
                strip.text = element_text(color="black", face = "bold", size = 15), legend.position = "none")
P4

# Save the graph using the export button OR use ggsave (see facet grid tutorial on that)

#####

