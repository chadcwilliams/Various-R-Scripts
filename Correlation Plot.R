Data = data.frame(
  Correlation = c(-.4,-.2,.2,.7,-.55,.6,-.35,-.15,.15,.55,-.4,.65),
  Group = sort(rep(c(1,1.1),6)),
  Variable = rep(1:6,2)
)

brew_colour = c("#d73027", "#c13a3c", "#ab4551", "#8a5472", "#71608a", "#5b6b9f", "#4575b4")


library(ggplot2)
ggplot(aes(x=Variable,y=Group,colour=Correlation),data=Data)+
  geom_point(size=abs(Data$Correlation)*50)+
  geom_point(size=50,shape = 21,colour = 'grey')+
  geom_point(size=50*.5,shape = 21,colour = 'grey')+
  geom_point(size=50*.3,shape = 21,colour = 'grey')+
  geom_point(size=50*.1,shape = 21,colour = 'grey')+
  scale_color_gradient2(midpoint = 0, low = '#d73027', mid = "white", high = '#4575b4', limits = c(-.7,.7))+
  scale_y_continuous(breaks=c(1,1.1),labels = c('A','B'), limits = c(0.95,1.15))+
  scale_x_continuous(breaks=c(1:6),labels = c('1','2','3','4','5','6'), limits = c(0.5,6.5))+
  theme_classic()+theme(text = element_text(size=30))
  