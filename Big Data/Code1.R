# install.packages("GGally")
# install.packages("data.table")
# install.packages("microbenchmark")
# install.packages("parallel")
# install.packages("lme4")
#install.packages("patchwork")
#install.packages("hrbrthemes")
# install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("hrbrthemes")
#install.packages("viridis")

library(hrbrthemes)
library(viridis)
library(data.table)
library(tidyverse)
library(microbenchmark)
library(lme4)
library(tidyverse)
library(dplyr)
library(GGally)
library(parallel)
library(ggplot2)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(reshape2)
library(corrplot)




#Sequential Processing#~~~~~~~~~~~~
library(data.table)
library(tidyverse)
library(microbenchmark)

df2 <- function(i){
  list.files(path = "C:/Users/ASUS/Documents/Area-Lvl Grocery/", pattern = ".csv") %>%
    map_df(~fread(.))
}

mbm2 <- microbenchmark(lapply(1:100, df2))

mbm2
autoplot(mbm2)

#Sequential Processing#~~~~~~~~~~~~

#Parallel Processing#~~~~~~~~~~~~
library(parallel)

df3 <- function(i){
  list.files(path = "C:/Users/ASUS/Documents/Area-Lvl Grocery 3/", pattern = ".csv") %>%
    map_df(~fread(.))
}

df4 <- function(i){
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(lme4))
  parLapply(cl, 1:100, df3)
  stopCluster(cl)
}

mbm4 <- microbenchmark(lapply(1:100, df3), df4)

mbm4
library(ggplot2)
autoplot(mbm4)
#Parallel Processing#~~~~~~~~~~~~

#Sequential + Parallel#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install and load required packages
# install.packages('tidyverse')
# install.packages('microbenchmark')
# install.packages("doParallel")
# install.packages("magrittr")
library(parallel)
library(doParallel)
library(microbenchmark)
library(readr)
library(dplyr)
library(ggplot2)

#Big Data Project/Project
"C:/Users/ASUS/Downloads/Area-Lvl Grocery/"
"C:/Users/ASUS/Documents/Area-Lvl Grocery/"

# Setting the working directory to the grocery dataset.
setwd("C:/Users/ASUS/Documents/Area-Lvl Grocery/")

# function to Load data in sequential.
sequential <- function() {
  list_CSV_files <- list.files(path="C:/Users/ASUS/Documents/Area-Lvl Grocery/")
  foreach(i=1:2)  %do% {
    dfpar <- readr::read_csv(list_CSV_files) #, id="file_name"
  }
}

# Set no of cores and create cluster to be used for parallel.
no_of_cores <- detectCores() - 1
cl <- makeCluster(no_of_cores, type="PSOCK")
registerDoParallel(cl)

# Assigning function to load data in parallel
parallel <- function() {
  list_CSV_files <- list.files(path="C:/Users/ASUS/Documents/Area-Lvl Grocery/")
  foreach(i=1:2)  %dopar% {
    dfpar <- readr::read_csv(list_CSV_files) #, id="file_name"
  }
}

# Call the functions and time them using microbenchmark.
mbm <- microbenchmark(sequential(),parallel())

stopCluster(cl)

mbm
# plot graph of time taken for parallel and sequential function.
autoplot(mbm)

#Sequential + Parallel#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

year_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/year_msoa_grocery.csv")
new_year_msoa <- year_msoa %>% select(1,159,160:162,163,165,166)

#structure & summary#
str(new_year_msoa)
summary(new_year_msoa)
#structure & summary#

#mean#
meandairy <- mean(new_year_msoa$f_dairy)
print(meandairy)

meaneggs <- mean(new_year_msoa$f_eggs)
print(meaneggs)

meanfats <- mean(new_year_msoa$f_fats_oils)
print(meanfats)

meanfish <- mean(new_year_msoa$f_fish)
print(meanfish)

meanfruits <- mean(new_year_msoa$f_fruit_veg)
print(meanfruits)

meanmeat <- mean(new_year_msoa$f_meat_red)
print(meanmeat)

meanpoultry <- mean(new_year_msoa$f_poultry)
print(meanpoultry)

#std#
sddairy <- sd(new_year_msoa$f_dairy)
print(sddairy)

sdeggs <- sd(new_year_msoa$f_eggs)
print(sdeggs)

sdfats <- sd(new_year_msoa$f_fats_oils)
print(sdfats)

sdfish <- sd(new_year_msoa$f_fish)
print(sdfish)

sdfruits <- sd(new_year_msoa$f_fruit_veg)
print(sdfruits)

sdmeat <- sd(new_year_msoa$f_meat_red)
print(sdmeat)

sdpoultry <- sd(new_year_msoa$f_poultry)
print(sdpoultry)

#median#
mediandairy <- median(new_year_msoa$f_dairy)
print(mediandairy)

medianeggs <- median(new_year_msoa$f_eggs)
print(medianeggs)

medianfats <- median(new_year_msoa$f_fats_oils)
print(medianfats)

medianfish <- median(new_year_msoa$f_fish)
print(medianfish)

medianfruits <- median(new_year_msoa$f_fruit_veg)
print(medianfruits)

medianmeat <- median(new_year_msoa$f_meat_red)
print(medianmeat)

medianpoultry <- median(new_year_msoa$f_poultry)
print(medianpoultry)


#visualisation using histogram#
hist(new_year_msoa$f_dairy)

hist(new_year_msoa$f_eggs)

hist(new_year_msoa$f_fats_oils)

hist(new_year_msoa$f_fish)

hist(new_year_msoa$f_fruit_veg)

hist(new_year_msoa$f_meat_red)

hist(new_year_msoa$f_poultry)


# Visualization for Mean and Standard Deviation
std_df <- data.frame(meanValue = c(mean(new_year_msoa$f_dairy), 
                                   mean(new_year_msoa$f_eggs), 
                                   mean(new_year_msoa$f_fats_oils),
                                   mean(new_year_msoa$f_fish),
                                   mean(new_year_msoa$f_fruit_veg),
                                   mean(new_year_msoa$f_meat_red),
                                   mean(new_year_msoa$f_poultry)),
                     
                     stdValue = c(sd(new_year_msoa$f_dairy), 
                                  sd(new_year_msoa$f_eggs), 
                                  sd(new_year_msoa$f_fats_oils),
                                  sd(new_year_msoa$f_fish),
                                  sd(new_year_msoa$f_fruit_veg),
                                  sd(new_year_msoa$f_meat_red),
                                  sd(new_year_msoa$f_poultry)),
                     
                     Category=c("dairy","eggs","fats & oil", "fish", "fruits & veg", 
                                "red meat", "poultry"))

library(ggplot2)

ggplot(std_df, aes(x=Category, y=meanValue)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour='black') +
  geom_errorbar(aes(ymin=meanValue-stdValue, ymax=meanValue+stdValue), width=.2)

#Correlation#

#dairy correlation#
library("ggpubr")
ggscatter(new_year_msoa, x = "f_dairy", y = "f_eggs", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "dairy", ylab = "eggs")

ggscatter(new_year_msoa, x = "f_dairy", y = "f_fats_oils", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "dairy", ylab = "fats_oils")

ggscatter(new_year_msoa, x = "f_dairy", y = "f_fish", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "dairy", ylab = "fish")

ggscatter(new_year_msoa, x = "f_dairy", y = "f_fruit_veg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "dairy", ylab = "fruits & veg")

ggscatter(new_year_msoa, x = "f_dairy", y = "f_meat_red", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "dairy", ylab = "red meat")

ggscatter(new_year_msoa, x = "f_dairy", y = "f_poultry", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "dairy", ylab = "poultry")

#egg correlation#
#dairy alrd done above#
ggscatter(new_year_msoa, x = "f_eggs", y = "f_fats_oils", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "eggs", ylab = "fats_oils")

ggscatter(new_year_msoa, x = "f_eggs", y = "f_fish", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "eggs", ylab = "fish")

ggscatter(new_year_msoa, x = "f_eggs", y = "f_fruit_veg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "eggs", ylab = "fruits & veg")

ggscatter(new_year_msoa, x = "f_eggs", y = "f_meat_red", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "eggs", ylab = "red meat")

ggscatter(new_year_msoa, x = "f_eggs", y = "f_poultry", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "eggs", ylab = "poultry")

#fats&oil correlation#
ggscatter(new_year_msoa, x = "f_fats_oils", y = "f_fish", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fats_oils", ylab = "fish")

ggscatter(new_year_msoa, x = "f_fats_oils", y = "f_fruit_veg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fats_oils", ylab = "fruits & veg")

ggscatter(new_year_msoa, x = "f_fats_oils", y = "f_meat_red", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fats_oils", ylab = "red meat")

ggscatter(new_year_msoa, x = "f_fats_oils", y = "f_poultry", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fats_oils", ylab = "poultry")

#fish correlation#
ggscatter(new_year_msoa, x = "f_fish", y = "f_fruit_veg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fish", ylab = "fruits & veg")

ggscatter(new_year_msoa, x = "f_fish", y = "f_meat_red", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fish", ylab = "red meat")

ggscatter(new_year_msoa, x = "f_fish", y = "f_poultry", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fish", ylab = "poultry")

#fruits & veg correlation#
ggscatter(new_year_msoa, x = "f_fruit_veg", y = "f_meat_red", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fruits & veg", ylab = "red meat")

ggscatter(new_year_msoa, x = "f_fruit_veg", y = "f_poultry", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "fruits & veg", ylab = "poultry")

#red meat correlation#
ggscatter(new_year_msoa, x = "f_meat_red", y = "f_poultry", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "red meat", ylab = "poultry")

ggpairs(data = new_year_msoa, columns = 2:8,)
#Correlation#

#Regression#
# Create the predictor and response variable.

#dairy regression#
plot(y,x,col = "blue",main = "Dairy & Eggs Regression",
     abline(lm(new_year_msoa$f_dairy~new_year_msoa$f_eggs)),cex = 1.3,pch = 16,xlab = "dairy",ylab = "eggs")

plot(y,x,col = "blue",main = "Dairy & Fats and Oil Regression",
     abline(lm(new_year_msoa$f_dairy~new_year_msoa$f_fats_oils)),cex = 1.3,pch = 16,xlab = "dairy",ylab = "fats & oil")

plot(y,x,col = "blue",main = "Dairy & Fish Regression",
     abline(lm(new_year_msoa$f_dairy~new_year_msoa$f_fish)),cex = 1.3,pch = 16,xlab = "dairy",ylab = "fish")

plot(y,x,col = "blue",main = "Dairy & Fruits and Veggies Regression",
     abline(lm(new_year_msoa$f_dairy~new_year_msoa$f_fruit_veg)),cex = 1.3,pch = 16,xlab = "dairy",ylab = "fruits & veg")

plot(y,x,col = "blue",main = "Dairy & Red Meat Regression",
     abline(lm(new_year_msoa$f_dairy~new_year_msoa$f_meat_red)),cex = 1.3,pch = 16,xlab = "dairy",ylab = "red meat")

plot(y,x,col = "blue",main = "Dairy & Poultry Regression",
     abline(lm(new_year_msoa$f_dairy~new_year_msoa$f_poultry)),cex = 1.3,pch = 16,xlab = "dairy",ylab = "poultry")


#egg regression#
plot(y,x,col = "blue",main = "Egg & Fats and Oil Regression",
     abline(lm(new_year_msoa$f_eggs~new_year_msoa$f_fats_oils)),cex = 1.3,pch = 16,xlab = "Egg",ylab = "fats & oil")

plot(y,x,col = "blue",main = "Egg & Fish Regression",
     abline(lm(new_year_msoa$f_eggs~new_year_msoa$f_fish)),cex = 1.3,pch = 16,xlab = "Egg",ylab = "fish")

plot(y,x,col = "blue",main = "Egg & Fruits and Veggies Regression",
     abline(lm(new_year_msoa$f_eggs~new_year_msoa$f_fruit_veg)),cex = 1.3,pch = 16,xlab = "Egg",ylab = "fruits & veg")

plot(y,x,col = "blue",main = "Egg & Red Meat Regression",
     abline(lm(new_year_msoa$f_eggs~new_year_msoa$f_meat_red)),cex = 1.3,pch = 16,xlab = "Egg",ylab = "red meat")

plot(y,x,col = "blue",main = "Egg & Poultry Regression",
     abline(lm(new_year_msoa$f_eggs~new_year_msoa$f_poultry)),cex = 1.3,pch = 16,xlab = "Egg",ylab = "poultry")


#fats&oil regression#
plot(y,x,col = "blue",main = "Fats and Oil & Fish Regression",
     abline(lm(new_year_msoa$f_fats_oils~new_year_msoa$f_fish)),cex = 1.3,pch = 16,xlab = "fats & oil",ylab = "fish")

plot(y,x,col = "blue",main = "Fats and Oil & Fruits and Veggies Regression",
     abline(lm(new_year_msoa$f_fats_oils~new_year_msoa$f_fruit_veg)),cex = 1.3,pch = 16,xlab = "fats & oil",ylab = "fruits & veg")

plot(y,x,col = "blue",main = "Fats and Oil & Red Meat Regression",
     abline(lm(new_year_msoa$f_fats_oils~new_year_msoa$f_meat_red)),cex = 1.3,pch = 16,xlab = "fats & oil",ylab = "red meat")

plot(y,x,col = "blue",main = "Fats and Oil & Poultry Regression",
     abline(lm(new_year_msoa$f_fats_oils~new_year_msoa$f_poultry)),cex = 1.3,pch = 16,xlab = "fats & oil",ylab = "poultry")


#fish regression#
plot(y,x,col = "blue",main = "Fish & Fruits and Veggies Regression",
     abline(lm(new_year_msoa$f_fish~new_year_msoa$f_fruit_veg)),cex = 1.3,pch = 16,xlab = "fish",ylab = "fruits & veg")

plot(y,x,col = "blue",main = "Fish & Red Meat Regression",
     abline(lm(new_year_msoa$f_fish~new_year_msoa$f_meat_red)),cex = 1.3,pch = 16,xlab = "fish",ylab = "red meat")

plot(y,x,col = "blue",main = "Fish & Poultry Regression",
     abline(lm(new_year_msoa$f_fish~new_year_msoa$f_poultry)),cex = 1.3,pch = 16,xlab = "fish",ylab = "poultry")


#fruits & veg regression#

plot(y,x,col = "blue",main = "Fruits and Veggies & Red Meat Regression",
     abline(lm(new_year_msoa$f_fruit_veg~new_year_msoa$f_meat_red)),cex = 1.3,pch = 16,xlab = "fruits & veg",ylab = "red meat")

plot(y,x,col = "blue",main = "Fruits and Veggies & Poultry Regression",
     abline(lm(new_year_msoa$f_fruit_veg~new_year_msoa$f_poultry)),cex = 1.3,pch = 16,xlab = "fruits & veg",ylab = "poultry")

#red meat regression#
plot(y,x,col = "blue",main = "Red Meat & Poultry Regression",
     abline(lm(new_year_msoa$f_fruit_veg~new_year_msoa$f_poultry)),cex = 1.3,pch = 16,xlab = "red meat",ylab = "poultry")

#Regression#

#Hypothesis Testing#~~~~~~~~~~~~

#January#
jan_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Jan_msoa_grocery.csv")
my_jan_attributes <- jan_msoa %>% select(158:174) %>% slice(0:30)
my_jan_attributes_witharea <- jan_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)
new_jan_attributes <- jan_msoa %>% select(159,160:162,163,165,166) %>% slice(0:30)

ggplot()+
  geom_line(my_jan_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_jan_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_jan_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_jan_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_jan_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_jan_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_jan_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for January")


#February#
feb_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Feb_msoa_grocery.csv")
#my_feb_attributes <- jan_msoa %>% select(158:174) %>% slice(0:30)
my_feb_attributes_witharea <- jan_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)
#new_feb_attributes <- jan_msoa %>% select(159,160:162,163,165,166) %>% slice(0:30)

ggplot()+
  geom_line(my_feb_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_feb_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_feb_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_feb_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_feb_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_feb_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_feb_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for February")

#March#
mar_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Mar_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
#my_mar_attributes <- mar_msoa %>% select(158:174) %>% slice(0:30) #Test #works
my_mar_attributes_witharea <- mar_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)
#new_mar_attributes <- mar_msoa %>% select(159,160:162,163,165,166) %>% slice(0:30)


#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_mar_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_mar_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_mar_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_mar_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_mar_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_mar_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_mar_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for March")

#April#
apr_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Apr_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
#my_apr_attributes <- apr_msoa %>% select(158:174) %>% slice(0:30) #Test #works
my_apr_attributes_witharea <- apr_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)
#new_apr_attributes <- apr_msoa %>% select(159,160:162,163,165,166) %>% slice(0:30)


#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_apr_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_apr_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_apr_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_apr_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_apr_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_apr_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_apr_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for April")

#May#
may_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/May_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
my_may_attributes <- may_msoa %>% select(158:174) %>% slice(0:30) #Test #works
my_may_attributes_witharea <- may_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)
new_may_attributes <- may_msoa %>% select(159,160:162,163,165,166) %>% slice(0:30)


#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_may_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_may_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_may_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_may_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_may_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_may_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_may_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for May")

#June#
jun_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Jun_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
#my_may_attributes <- may_msoa %>% select(158:174) %>% slice(0:30) #Test #works
my_jun_attributes_witharea <- jun_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)
#new_may_attributes <- may_msoa %>% select(159,160:162,163,165,166) %>% slice(0:30)


#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_jun_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_jun_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_jun_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_jun_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_jun_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_jun_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_jun_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for June")

#July#
jul_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Jul_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196

my_jul_attributes_witharea <- jul_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)

#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_jul_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_jul_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_jul_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_jul_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_jul_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_jul_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_jul_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for July")

#August#
aug_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Aug_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
my_aug_attributes_witharea <- aug_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)


#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_aug_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_aug_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_aug_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_aug_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_aug_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_aug_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_aug_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for August")

#September#
sep_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Sep_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196

my_sep_attributes_witharea <- sep_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)

#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_sep_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_sep_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_sep_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_sep_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_sep_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_sep_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_sep_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for September")

#October#
oct_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Oct_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
my_oct_attributes_witharea <- oct_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)

#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_oct_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_oct_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_oct_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_oct_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_oct_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_oct_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_oct_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for October")

#November#
nov_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Nov_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
my_nov_attributes_witharea <- nov_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)

#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_nov_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_nov_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_nov_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_nov_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_nov_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_nov_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_nov_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for November")

#December#
dec_msoa <- read.csv("C:/Users/ASUS/Documents/Area-Lvl Grocery/Dec_msoa_grocery.csv")

#area_id column is 0
#beer - wine = 158-174
#male & female = 195 & 196
my_dec_attributes_witharea <- dec_msoa %>% select(1,159,160:162,163,165,166) %>% slice(0:10)

#testing graph 2  group=1  ** Use this one
ggplot()+
  geom_line(my_dec_attributes_witharea, mapping = aes(y=f_dairy,x= area_id,colour="dairy", group=1),size=1)+
  geom_line(my_dec_attributes_witharea, mapping = aes(y=f_eggs,x= area_id,colour="eggs", group=2),size=1)+
  geom_line(my_dec_attributes_witharea, mapping = aes(y=f_fats_oils,x= area_id,colour="fats&oil", group=3),size=1)+
  geom_line(my_dec_attributes_witharea, mapping = aes(y=f_fish,x= area_id,colour="fish", group=4),size=1)+
  geom_line(my_dec_attributes_witharea, mapping = aes(y=f_fruit_veg,x= area_id,colour="fruits&veg", group=5),size=1)+
  geom_line(my_dec_attributes_witharea, mapping = aes(y=f_meat_red,x= area_id,colour="red meat", group=6),size=1)+
  geom_line(my_dec_attributes_witharea, mapping = aes(y=f_poultry,x= area_id,colour="poultry", group=7),size=1)+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Food Attributes Values",)+
  
  theme(
    axis.title.y = element_text(color = "Black", size=13),
  ) +
  ggtitle("Area Against Multiple Food Attributes for December")

#Hypothesis Testing#~~~~~~~~~~~~
