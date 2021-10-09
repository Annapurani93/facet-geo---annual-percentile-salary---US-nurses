library(tidytuesdayR)
library(tidyverse)
library(geofacet)
library(ggrepel)
library(reshape2)
library(RColorBrewer)
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)

tuesdata$nurses->nurses

glimpse(nurses)

nurses%>%select(State,Year,`Annual 10th Percentile`, `Annual 25th Percentile`, 
                `Annual 75th Percentile`, `Annual 90th Percentile`)%>%
  distinct(State,Year,.keep_all = TRUE)->salary1
salary1%>%group_by(State,Year)%>%
  arrange(State,Year)->salary1
data.frame(salary1)->salary1
colnames(salary1)<-c("State","Year","10th Percentile","25th Percentile","75th Percentile","90th Percentile")
salary1


melt(salary1, id.vars = c("State","Year"),
     measure.vars = c("10th Percentile","25th Percentile","75th Percentile","90th Percentile"),
     value.name = "Salary")->salary12

salary12%>%group_by(State,Year,variable)->salary12
salary12<-data.frame(salary12)
colnames(salary12)<-c("State","Year","Percentile","Value")
salary12
salary12$Value<-(salary12$Value)/1000


ggplot(salary12, aes(x=Year,y=Value, colour=Percentile))+
  facet_geo(~State,grid = "us_state_grid3")+
  geom_line()+
  scale_color_brewer(palette="RdBu", name="Annual, Percentile Salary (in thousand dollars):")+
  theme(panel.spacing.x = unit(2.5, "lines"))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  theme(panel.background = element_rect(colour = "black"))+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "white",fill=NA))+
  theme(panel.grid=element_blank())+
  theme(axis.title = element_blank())+
  theme(legend.background = element_rect(fill = "black", colour = "white"),
        legend.text = element_text(colour = "white"),
        legend.position = "top",
        legend.title = element_text(colour = "white"))+
   theme(plot.background=element_rect(fill = "black"))+
  theme(panel.background = element_rect(fill = "black"))+
  theme(axis.text = element_text(colour = "white"),
        strip.text = element_text(colour = "white"))+
  labs(title="THE VARIATION IN THE ANNUAL PERCENTILE SALARY OF THE US NURSES",
       subtitle = "The annual percentile salary (in thousand dollars) of the nurses for each US State has been plotted for the last two decades (1998 to 2020)",
       caption = "Data: TidyTuesday|Design: @annapurani93")+
  theme(plot.title = element_text(face = "bold",size = 24, colour = "white", hjust=0.5),
        plot.subtitle = element_text(size = 16, colour = "white", hjust=0.5),
        plot.caption = element_text(size=10,colour = "white"))->mlp
  
mlp

ggsave("mlp.pdf",mlp,width = 25,height=20,dpi=500)
ggsave("mlp.png",mlp,width = 25,height=20,dpi=500)



