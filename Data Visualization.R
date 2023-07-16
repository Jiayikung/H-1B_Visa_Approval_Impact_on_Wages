#Data Visualization
#install.packages("plotly")
library(ggplot2)
library(plotly)
library(tidyverse)

# Figure.1
# Applicants with STEM, Healthcare, and others career categories generally have higher wage and job opening numbers
df1 %>%
  ggplot(aes(x=total_positions, y=prevailing_wage, color=category))+
  geom_point(size = 5)+
  labs(x="Total number of job openings", y="Mean hourly wage")+
  theme_classic()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 15))+
  guides(color = guide_legend(override.aes = list(size = 10)))
summary(plot1)

Mean <- df1 %>%
  group_by(category) %>%
  summarise( Mean = mean(prevailing_wage)) %>%
  arrange(desc(Mean))

#plot2
Mean %>%
  ggplot(aes(x = category, y=Mean, fill=category)) +
  geom_bar(stat = 'identity')+
  scale_x_discrete(limits = Mean$category)+
  labs(x="Job Category", y="Mean hourly wage")+
  theme_classic()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'))

install.packages('gridExtra')
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

posi2 <- df1 %>%
  group_by(category) %>%
  summarise( posi2 = sum(total_positions))

certified_share <- df1 %>%
  group_by(category) %>%
  summarise( certified_share = mean(certi_dummy))

df5 = Mean %>%
  mutate(posi2) %>%
  mutate(certified_share)

df6 <- left_join(Mean, posi2, by = "category") %>%
  left_join(certified_share, by = "category")

df7 <- left_join(Wage, fivcountfiv, by = "location") %>%
  left_join(certified_share, by = "location")

certified_share <- df1 %>%
  group_by(location) %>%
  summarise(certified_share = mean(certi_dummy))

Wage <- df1 %>%
  group_by(location) %>%
  summarise(Wage = mean(prevailing_wage))

fiv <- df1 %>%
  group_by(location) %>%
  summarise(fiv = sum(category == "Arts, Media & Entertainment"|
                        category == "Business, Finance, and Management"|
                        category == "Education & Social Services"|
                        category == "Healthcare & Life Sciences"|
                        category == "Legal and Compliance"))

fivcountfiv <- df1 %>%
  group_by(location) %>%
  summarise(fivcountfiv = sum(category == "Arts, Media & Entertainment" |
                                category == "Business, Finance, and Management" |
                                category == "Education & Social Services" |
                                category == "Healthcare & Life Sciences" |
                                category == "Legal and Compliance") / n())

library(gapminder)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
install.packages('gapminder')
install.packages('hrbrthemes')

library(ggplot2)

#plot3
df6 %>%
  ggplot(aes(x = posi2, y=Mean, size=certified_share, color=category)) +
  geom_point(alpha=2)+
  labs(x="Total number of job openings", y="Mean hourly wage")+
  theme_classic()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 15))+
  guides(color = guide_legend(override.aes = list(size = 10)))

#plot3.1
Mean %>%
  ggplot(aes(x = posi, y=Mean, size=cer, color=category)) +
  geom_point(alpha=0.5)+
  scale_size(range = c(.1, 24), name="certified share")+
  labs(x="Mean of job meanings", y="Mean hourly wage")+
  theme_classic()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 15))+
  guides(color = guide_legend(override.aes = list(size = 10)))

#plot4
df4 <- data.frame(Wage)
install.packages('ggrepel')
library(ggrepel)
ggplot(df7, aes(x = fivcountfiv, y=Wage, size=certified_share, color=location)) +
  geom_point(alpha=1)+
  labs(x="Share of the top five highest-paying career categories", y="Mean hourly wage")+
  theme_classic()+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 11))+
  guides(color = guide_legend(override.aes = list(size = 5))) 

# variable correlation in heat map (It seems we don't need this...)
install.packages("corrplot")
library (corrplot)
corr <- cor(df1[c("total_positions","prevailing_wage","foreign_wage_level")])
corrplot(corr, method="circle")

df1 %>%
  ggplot(aes(x=category, y=prevailing_wage, fill=certi_dummy)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  labs(title="Figure 2. Wage distribution for H1B workers by career categories",x="categories",
       y="Hourly wage")

ggplot(data=Group_project_group_9_ver_2_,aes(x=case_status, y=prevailing_wage)) +
  geom_point() +
  labs(title="Case status v Salary", caption="Data collected by Kaggle user Abeyer",
       x="Case status",
       y="Salary") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=19),
        axis.text.x = element_text(size= 13, color = 'black'),
        axis.text.y = element_text(size = 13, color = 'black'),
        axis.title.x = element_text(size= 14, color = 'black'),
        axis.title.y = element_text(size = 14, color = 'black'))

# create a bar plot
ggplot(df1, aes(x = location, y = certi_dummy)) +
  geom_bar(stat = "identity", fill = "location") +
  labs(title = "Number of Certified Cases by Region", 
       x = "Region", y = "Number of Certified Cases")

df1 %>%
  ggplot(aes(x=location, y=certi_dummy, fill=location))+
  geom_bar(stat='identity')+
  scale_x_discrete(limits = df1$certi_dummy)

ggplot(Group_project_group_9_ver_2_, aes(x = location, fill = as.factor(certi_dummy))) +
  geom_bar(stat = "count", position = "fill")

