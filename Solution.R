
#Creating vector with all required packages
required_packages <- c("readxl", "dplyr", "ggplot2", "viridis")

#Create a vector which contains packag names, which are not installed in the machine yet
not_installed_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

#Check wether all required packages installed, if not, install them

if(length(not_installed_packages) == 0){
  
  print("All required packages are installed")
  
} else {
  
  for(i in 1:length(not_installed_packages)){
    
    install.packages(not_installed_packages[i])
  }
}

#Load all the required packages for this assignment
for(i in 1:length(required_packages )){
  
  library(required_packages[i], character.only = T)
}
##############

#Read in dataset tabs
Level_Start <- read_excel(file.choose(), sheet = "Level Start")

Spenders <- read_excel(file.choose(), sheet = "Spenders")

Session_Start <- read_excel(file.choose(), sheet = "Session Start")

#Understand the data
glimpse(Level_Start)

summary(Level_Start)

glimpse(Spenders)

summary(Spenders)

glimpse(Session_Start)

summary(Session_Start)


#Clean & Transform data for further analysis
#Correct colnames
colnames(Spenders) <- c("user_id", "event_date", "event_time", "level", "session_nb", "pack_name", "money_spent", "money_currency")

colnames(Level_Start) <- c("user_id", "event_date", "event_time", "level_id", "session_nb")

#Transform data types as required
Level_Start$user_id <- as.factor(Level_Start$user_id)

Level_Start$event_date <- as.Date(Level_Start$event_date)

Level_Start[,c(4,5)] <- sapply(Level_Start[,c(4,5)], as.integer)

Spenders[,c(1,6,8)] <- as.data.frame(sapply(Spenders[,c(1,6,8)], as.factor))

Spenders$event_date <- as.Date(Spenders$event_date)

Spenders[,c(4,5)] <- sapply(Spenders[,c(4,5)], as.integer)

Session_Start[,c(1,4,6)] <- as.data.frame(sapply(Session_Start[,c(1,4,6)], as.factor))

Session_Start$event_date <- as.Date(Session_Start$event_date)

Session_Start$session_nb <- as.integer(Session_Start$session_nb)

#Arrange data by event_time

Session_Start <- Session_Start %>%
                  arrange(event_time)

Level_Start <- Level_Start %>%
                arrange(event_time)

Spenders <- Spenders %>%
              arrange(event_time)

##############

#1.	From the given Dataset, determine and analyze Day 1, Day 3, Day 7, Day 15 and Day 30 Retention.
#(Refer to Session start tab)

#Day N retention also known as classic retention or Retention by Day, is the percent of 
#new users who come back on a specific day.


#Find the "day 0"
day0 <- min(Session_Start$event_date)

#Calculate number of users on "day 0"
day0_users <- Session_Start %>%
                subset(subset = event_date == day0) %>%
                select(user_id) %>%
                collect %>% .[["user_id"]] %>%
                unique %>%
                droplevels
#Findout the total number of users, who return on specific day              
day_wise_no_of_return_users <- c()

for(day in 1:30){
  day_wise_no_of_return_users <- append(day_wise_no_of_return_users,
                                        Session_Start %>%
                                          select(user_id, event_date) %>%
                                          filter(event_date == day0 + day & user_id %in% day0_users) %>%
                                          unique %>%
                                          nrow(), 
                                        after = length(day_wise_no_of_return_users))
    
}

#Create retention rate data.frame
classic_retention_rate_df <- data.frame(Day = 1:30,
                                        NO_Return_Users = day_wise_no_of_return_users,
                                        Day0_users =  rep(length(day0_users),30))

classic_retention_rate_df <- classic_retention_rate_df %>%
                              mutate(Retention_Rate = NO_Return_Users / Day0_users)

#Day 1, Day 3, Day 7, Day 15 and Day 30 Retention rate
classic_retention_rate_df[c(1,3,7,15,30),c(1,4)]

#Visualize the classic retention rate
ggplot(classic_retention_rate_df, aes(x = Day, y = Retention_Rate)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(0,30), breaks =  seq(0,30,1)) +
  scale_y_continuous(limits = c(0,1), breaks =  seq(0,1,0.05)) +
  theme_bw()
  
##############

#2.	Create a Funnel of level progression and determine if any areas of the game are
# acting as blockers. (Refer to Level Start tab). Also, identify how many times levels are replayed.

total_players <- length(unique(Level_Start$user_id))

level_finishers <- Level_Start %>% 
                            select(user_id, level_id) %>% 
                            unique %>%
                            group_by(user_id) %>%
                            summarise(high_level_played = max(level_id))
                           
total_finishers <- c()

for(level in 1:100){
  
  total_finishers <- append(total_finishers, sum(level_finishers$high_level_played >= level), after = length(total_finishers))

  }

level_progression <- data.frame(progression_rate = total_finishers / total_players, level = 1:100) 

#Level pregression funnel
ggplot(level_progression, aes(x = level, y = progression_rate)) +
  geom_col(col = "blue", fill = "yellow") +
  scale_x_continuous(limits = c(0,100), breaks =  seq(0,100,2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), breaks =  seq(0,1,0.05), expand = c(0,0)) +
  theme_light()

session_stat_level_wise <- Level_Start %>%
  select(-3) %>%
  group_by(level_id) %>%
  summarise(tot_no_of_times_plyd = n(),
            max_sessions = max(session_nb),
            min_sessions = min(session_nb),
            avg_session = mean(session_nb),
            median_session = median(session_nb),
            std_deviation = sd(session_nb))

#Total number of times level played including all the players
ggplot(session_stat_level_wise, aes(x = level_id, y = tot_no_of_times_plyd)) +
  geom_point(col = "green", size = 3) +
  geom_line(col = "red") +
  geom_text(aes(label = tot_no_of_times_plyd),size = 3, hjust = 0.5, vjust = 2, position = "stack") +
  scale_x_continuous(limits = c(0,100), breaks =  seq(0,100,2), expand = c(0,0)) +
  theme_light()

session_stat_of_every_level_per_user <- Level_Start %>%
  select(-3) %>%
  group_by(user_id, level_id) %>%
  summarise(tot_no_of_times_plyd = n(),
            max_sessions = max(session_nb),
            min_sessions = min(session_nb),
            avg_session = mean(session_nb),
            median_session = median(session_nb),
            std_deviation = sd(session_nb))

#Levelwise maximum number of sessions played
ggplot(session_stat_of_every_level_per_user, aes(x = level_id, y = max_sessions )) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_x_continuous(limits = c(0,100), breaks =  seq(0,100,2))+
  theme_light()


###############

#3.	What percentage of users make first time purchases between levels 1 to 7?
#(Refer to Spenders tab)

total_spending_users <- length(unique(Spenders$user_id))

tot_users_purchase_btw_l1_l7 <- Spenders %>%
                                          filter(level >= 1  & level <= 7) %>%
                                          select(user_id) %>%
                                          unique() %>%
                                          summarise(total = n()) %>%
                                          pull()


#Calculate purchase rate of  users, who make first time purchases between levels 1 to 7

purchase_rate_btw_l1_l7 <- tot_users_purchase_btw_l1_l7 / total_spending_users * 100

purchase_rate_btw_l1_l7
###############

#4.	What percentage of users make a repeat purchase?
#   At what level maximum repeat purchases happen? (Refer to Spenders tab)

users_not_repeated_purchase <- Spenders %>%
                                    group_by(user_id) %>%
                                    summarise(total = n()) %>% 
                                    filter(total == 1) %>%
                                    select(user_id) %>%
                                    collect() %>% .[["user_id"]] %>%
                                    unique %>%
                                    droplevels

#Calculate percentage of users make a repeat purchase

not_repeated_users <- length(users_not_repeated_purchase)

repeated_users <- total_spending_users - not_repeated_users

repeated_users_percentage <- repeated_users / total_spending_users * 100

repeated_users_percentage

#Calculate level at which maximum repeat purchases happen

Spenders %>%
  subset(subset = !user_id  %in% users_not_repeated_purchase) %>%
  select(user_id, level) %>%
  unique %>%
  group_by(level) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

###############

#5.	Which packs generate the maximum revenue? (Refer to Spenders tab)

Spenders$money_currency[Spenders$money_currency == "$ AUD"] <- "AUD"

Spenders$money_currency <- droplevels(Spenders$money_currency)

#Conversion rates as per google

one_inr <- 0.015 #USD

one_euro <- 1.23 #USD

one_cad <- 0.79 #USD 

one_gbp <- 1.41 #USD

one_nzd <- 0.72 #USD

one_rub <- 0.016 #USD

one_aud <- 0.77 #USD

Spenders$money_spent[Spenders$money_currency == "INR"] <- Spenders$money_spent[Spenders$money_currency == "INR"] * one_inr

Spenders$money_spent[Spenders$money_currency == "CAD"] <- Spenders$money_spent[Spenders$money_currency == "CAD"] * one_cad

Spenders$money_spent[Spenders$money_currency == "EUR"] <- Spenders$money_spent[Spenders$money_currency == "EUR"] * one_euro

Spenders$money_spent[Spenders$money_currency == "GBP"] <- Spenders$money_spent[Spenders$money_currency == "GBP"] * one_gbp

Spenders$money_spent[Spenders$money_currency == "NZD"] <- Spenders$money_spent[Spenders$money_currency == "NZD"] * one_nzd

Spenders$money_spent[Spenders$money_currency == "RUB"] <- Spenders$money_spent[Spenders$money_currency == "RUB"] * one_rub

Spenders$money_currency <- "USD"

Spenders %>%
  group_by(pack_name) %>%
  summarise(total = sum(money_spent)) %>%
  arrange(desc(total))

###########################







