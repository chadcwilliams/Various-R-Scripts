#### Load required libraries ####
library(ggplot2) #used for plotting

#### Create functions ####

#Function to create simulated data
create_data = function(n){ #Create function, n = number of datapoints
  data = data.frame( #Create dataframe
    scores= c(rnorm(n,1,.5),rnorm(n,1.5,.5)), #Randomize data for two conditions
    condition = c(rep(-.5,n),rep(.5,n)), #Create a coding variable
    subject = 1:n)} #Create a subject ID variable

#Sticky plot function
sticky_plot = function(data){ #Create function, data = data of interest as determined by the create_data function
  jitter = runif(dim(data)[1], -0.02, 0.02) #Add custom jitter values
  ggplot(aes(x=condition+jitter,y=scores),data=data)+ #Create ggplot
    geom_violin(aes(x=condition,fill=as.factor(condition)),alpha=.5,color='white')+ #Create violin distributions
    geom_rect(mapping=aes(xmin=-.5,xmax=.5,ymin=min(scores),ymax=max(scores)),fill='white')+ #Cut distributions in half
    geom_line(aes(group=subject),color='grey90',alpha=.8)+ #Create lines between points for each subject
    geom_point(color=c(rep('#DCA076',1,dim(data)[1]/2),rep('#B5C3CA',1,dim(data)[1]/2)),alpha=.8)+ #Insert each participants data points
    stat_summary(aes(x=condition,y=scores),fun=mean, geom='line',color='black')+ #Create grand averaged effects line
    stat_summary(aes(x=condition),fun=mean, geom='point',color='black')+ #Insert points for each conditions mean
    stat_summary(aes(x=condition),fun.data=mean_cl_normal, geom='errorbar',width=.1,color='black')+ #Add errorbars to the means
    scale_x_continuous(breaks = c(-.5,.5), labels = c('',''))+ #Remove labels
    scale_fill_manual(values=c("#DCA076", "#B5C3CA"),labels = c("Congruent", "Incongruent"))+ #Determine condition colours and names
    labs(x='',y='Reaction Time (s)')+ #Determine y axis label, remove x axis label
    theme_classic()+ #Remove grid background
    theme(legend.title = element_blank(), #Remove legend title
          text = element_text(size=12), #Determine font size
          axis.ticks = element_blank(), #Remove axis tick marks
          axis.line = element_blank())} #Remove axis lines

#### Create plots ####
sticky_plot(create_data(100)) #Use the create_data function (n=100) within the sticky_plot function
