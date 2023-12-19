library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(lattice)
library(caret)

Games <- read_csv('C:\\Users\\Dan\\OneDrive\\University\\Anul 3\\Analiza datelor\\Set\\jocuri_video.csv')
Games <- data.frame(Games)

Games <- filter(Games, !is.na(name))
Games$publisher <- tolower(Games$publisher)

index_ea <- grep(pattern = "electronic arts", x = Games$publisher)
table(Games$publisher[index_ea])

index_activision <- grep(pattern = "activision", x = Games$publisher)
table(Games$publisher[index_activision])

index_sony <- grep(pattern = "sony", x = Games$publisher)
table(Games$publisher[index_sony])

index_ubisoft <- grep(pattern = "ubisoft", x = Games$publisher)
table(Games$publisher[index_ubisoft])

Games$Publisher_Company <- Games$publisher
Games$Publisher_Company <- sub("^Electronic Arts.*", "Electronic Arts", Games$Publisher_Company)
Games$Publisher_Company <- sub("^Activision.*", "Activision", Games$Publisher_Company) 
Games$Publisher_Company <- sub("sony.*", "sony", Games$Publisher_Company) 
Games$Publisher_Company <- sub("^Ubisoft.*", "Ubisoft", Games$Publisher_Company)

Sales_per_publisher <- Games %>%
  group_by(Publisher_Company) %>%
  summarise(ttl_sales = sum(global_sales)) %>%
  arrange(desc(ttl_sales))

Top10_Sales_publisher <- Sales_per_publisher[1:10,]

Top10_Sales_publisher$Publisher_Company <- factor(Top10_Sales_publisher$Publisher_Company,
                                                  levels = rev(Top10_Sales_publisher$Publisher_Company[order(Top10_Sales_publisher$ttl_sales)]),ordered = TRUE)

Top10 <- ggplot(Top10_Sales_publisher) + 
  geom_bar(mapping = aes(x = reorder(Publisher_Company,ttl_sales),y = ttl_sales, fill = Publisher_Company)
           ,stat = "identity") 
Top10 + labs(x = "Publishers", y = "Total Global Sales") + 
  theme(legend.position = "top")+
  guides(fill = guide_legend(title = "Publisher")) + coord_flip() 

Games  %>%
  filter(year_of_release != "N/A", year_of_release < 2017)  %>%
  group_by(year_of_release)  %>%
  summarise(num_of_games = n_distinct(name))  -> Games_Year

ggplot(Games_Year) +
  geom_point(mapping = aes(x=year_of_release, y=num_of_games),stat = 'identity') +
  geom_line(mapping = aes(x=year_of_release, y=num_of_games),stat = 'identity') + 
  labs(x = "Year", y = "Number of Games")

top5 <- Games %>% 
  filter(Publisher_Company %in% c("nintendo","electronic arts","sony","ubisoft",
                                  "activision"), year_of_release != "N/A", year_of_release < 2017) %>%
  group_by(Publisher_Company,year_of_release) %>% 
  summarise(num_of_games = n_distinct(name), .groups = 'drop')

top5$year <- as.numeric(as.character(top5$year_of_release))

top5$Publisher_Company <- factor(top5$Publisher_Company, levels = c("nintendo", "electronic arts", "activision", "sony", "ubisoft"),
                                 ordered = TRUE)

ggplot(top5) +
  geom_line(mapping = aes(x=year, y=num_of_games,color = Publisher_Company),stat = 'identity')+
  labs(x = "Year", y = "Number of Games")

Games %>%
  group_by(platform)  %>%
  summarise(n = n())  %>%
  arrange(desc(n))  %>%
  head(n = 10)


result <- Games %>%
  filter(Publisher_Company == "ubisoft", year_of_release >= 2000, year_of_release <= 2010) %>%
  group_by(Year = year_of_release) %>%
  summarise(Total = n(), 
            PS2 = sum(platform == "PS2"), p_PS2 = PS2 / Total * 100,
            PS3 = sum(platform == "PS3"), p_PS3 = PS3 / Total * 100,
            DS = sum(platform == "DS"), p_DS = DS / Total * 100,
            Wii = sum(platform == "Wii"), p_Wii = Wii / Total * 100,
            p_4 = (PS2 + PS3 + DS + Wii) / Total * 100)

ggplot(result, aes(x = Year)) +
  geom_line(aes(y = p_PS2, color = "PS2"), linewidth = 1) +
  geom_line(aes(y = p_PS3, color = "PS3"), linewidth = 1) +
  geom_line(aes(y = p_DS, color = "DS"), linewidth = 1) +
  geom_line(aes(y = p_Wii, color = "Wii"), linewidth = 1) +
  geom_line(aes(y = p_4, color = "Overall"), linewidth = 1) +
  labs(title = "Procentajul de jocuri lansate pe platforme în funcție de an",
       x = "Anul de lansare",
       y = "Procentajul de jocuri") +
  scale_color_manual(values = c("PS2" = "blue", "PS3" = "red", "DS" = "green", "Wii" = "purple", "Overall" = "black")) +
  theme_minimal() + scale_x_continuous(breaks = seq(min(result$Year), max(result$Year), by = 1))



result_ea <- Games %>%
  filter(Publisher_Company == "electronic arts", year_of_release >= 2000, year_of_release <= 2010) %>%
  group_by(Year = as.integer(year_of_release)) %>%
  summarise(Total = n(), 
            PS2 = sum(platform == "PS2"), p_PS2 = PS2 / Total * 100,
            PS3 = sum(platform == "PS3"), p_PS3 = PS3 / Total * 100,
            DS = sum(platform == "DS"), p_DS = DS / Total * 100,
            Wii = sum(platform == "Wii"), p_Wii = Wii / Total * 100,
            p_4 = (PS2 + PS3 + DS + Wii) / Total * 100)

ggplot(result_ea, aes(x = Year)) +
  geom_line(aes(y = p_PS2, color = "PS2"), size = 1) +
  geom_line(aes(y = p_PS3, color = "PS3"), size = 1) +
  geom_line(aes(y = p_DS, color = "DS"), size = 1) +
  geom_line(aes(y = p_Wii, color = "Wii"), size = 1) +
  geom_line(aes(y = p_4, color = "Overall"), size = 1) +
  labs(title = "Procentajul de jocuri lansate pe platforme de către Electronic Arts în funcție de an",
       x = "Anul de lansare",
       y = "Procentajul de jocuri") +
  scale_color_manual(values = c("PS2" = "blue", "PS3" = "red", "DS" = "green", "Wii" = "purple", "Overall" = "black")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(result_ea$Year), max(result_ea$Year), by = 1))

top3_genres <- Nintendo_sales_Genre[1:3, ]

# Diagrama pentru vânzările pe genuri (limitată la primele 3 genuri)
ggplot(top3_genres, aes(x = genre, y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Vânzări Nintendo pe Primele 3 Genuri",
       x = "Genul Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal()

ggplot(Nintendo_sales_Genre_Name[1:10, ], aes(x = reorder(name, -sales), y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Cele Mai Bine Vândute 10 Jocuri Nintendo pe Genuri",
       x = "Numele Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top3_genres_activision <- Activision_sales_Genre[1:3, ]

# Diagrama pentru vânzările pe genuri (limitată la primele 3 genuri)
ggplot(top3_genres_activision, aes(x = genre, y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Vânzări Activision pe Primele 3 Genuri",
       x = "Genul Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal()

ggplot(Activision_sales_Genre_Name[1:10, ], aes(x = reorder(name, -sales), y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Cele Mai Bine Vândute 10 Jocuri Activision pe Genuri",
       x = "Numele Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top3_genres_sony <- Sony_sales_Genre[1:3, ]

# Diagrama pentru vânzările pe genuri (limitată la primele 3 genuri)
ggplot(top3_genres_sony, aes(x = genre, y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Vânzări Sony pe Primele 3 Genuri",
       x = "Genul Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal()

# Diagrama pentru cele mai bine vândute 10 jocuri Sony pe genuri
ggplot(Sony_sales_Genre_Name[1:10, ], aes(x = reorder(name, -sales), y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Cele Mai Bine Vândute 10 Jocuri Sony pe Genuri",
       x = "Numele Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



top3_genres_ubisoft <- Ubisoft_sales_Genre[1:3, ]

# Diagrama pentru vânzările pe genuri (limitată la primele 3 genuri)
ggplot(top3_genres_ubisoft, aes(x = genre, y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Vânzări Ubisoft pe Primele 3 Genuri",
       x = "Genul Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal()

# Diagrama pentru cele mai bine vândute 10 jocuri Ubisoft pe genuri
ggplot(Ubisoft_sales_Genre_Name[1:10, ], aes(x = reorder(name, -sales), y = sales, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Cele Mai Bine Vândute 10 Jocuri Ubisoft pe Genuri",
       x = "Numele Jocului",
       y = "Vânzări Globale (mil. $)",
       fill = "Genul Jocului") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


Games %>%
  group_by (year_of_release, genre)  %>%
  summarise(Top_Sales_Genre = sum(global_sales))  %>%
  arrange(year_of_release, desc(Top_Sales_Genre)) %>%
  filter(year_of_release != "N/A", year_of_release < 2017)  -> Year_Genre 

as.numeric(as.character(Year_Genre$year_of_release)) -> Year_Genre$year_of_release

df0 <- NULL

for (p in (unique(Year_Genre$year_of_release))){
  df <- Year_Genre %>%
    filter(year_of_release == p, Top_Sales_Genre == max(Top_Sales_Genre))
  df0 <- rbind(df0, df)
} 

ggplot(df0) +
  geom_bar(aes(x = year_of_release, y = Top_Sales_Genre, fill = genre), stat = "identity")+
  labs(x = "Year", y = "1st Genre Based on the Global Sales") 


Games%>%
  filter(year_of_release != "N/A", year_of_release < 2017) %>%
  group_by(year_of_release, genre)  %>%
  summarise(Genre_Year_Games = n()) %>%
  group_by(year_of_release)  %>%
  filter(Genre_Year_Games ==max(Genre_Year_Games))  %>%
  select(year_of_release, genre, Genre_Year_Games)  %>%
  arrange((year_of_release))  -> Year_Genre_Games

as.numeric(as.character(Year_Genre_Games$year_of_release)) -> Year_Genre_Games$year_of_release

ggplot(Year_Genre_Games) +
  geom_bar(aes(x = year_of_release, y = Genre_Year_Games, fill = genre), stat = "identity")+
  labs(x = "Year", y = "1st Genre Based on the Number of Games")

NAsales_Genre <- Games %>%
  group_by(genre)  %>%
  filter(genre != "Misc") %>%
  summarise(NAsales = sum(na_sales))  %>%
  arrange(desc(NAsales)) %>%
  mutate(p_NASales = NAsales/sum(NAsales)*100) %>%
  arrange(desc(p_NASales))

EUsales_Genre <- Games %>%
  group_by(genre)  %>%
  filter(genre != "Misc") %>%
  summarise(EUsales = sum(eu_sales))  %>%
  arrange(desc(EUsales)) %>%
  mutate(p_EUSales = EUsales/sum(EUsales)*100)

JPsales_Genre <- Games %>%
  group_by(genre)  %>%
  filter(genre != "Misc") %>%
  summarise(JPsales = sum(jp_sales))  %>%
  arrange(desc(JPsales)) %>%
  mutate(p_JPSales = JPsales/sum(JPsales)*100)

merge(NAsales_Genre, EUsales_Genre, by = "genre") ->t

merge(t, JPsales_Genre, by = "genre") -> Genre_Region

Genre_Region$NAsales <- NULL
Genre_Region$EUsales <- NULL
Genre_Region$JPsales <- NULL

Genre_Region_long <- gather(Genre_Region, Region, Percentage_Region, p_NASales:p_JPSales, factor_key=TRUE)

Genre_Region_long$Genre <- factor(Genre_Region_long$genre, 
                                  levels = c("Strategy", "Adventure", "Puzzle","Simulation",
                                             "Fighting", "Role-Playing", "Racing", "Platform",
                                             "Shooter", "Sports", "Action"), ordered = TRUE)

names <- c("p_NASales" = "North America Sales", 
           "p_EUSales" = "Europe Sales", 
           "p_JPSales" = "Japan Sales")  
ggplot(Genre_Region_long) +
  geom_bar(aes(x= genre, y = Percentage_Region, fill = genre), stat = "identity") +
  facet_grid(~Region, labeller = as_labeller(names))+
  labs(x = "Percentage Distribution by Region", y = "Genre") +
  coord_flip()+
  guides(fill=FALSE)



sales <- read_csv('C:\\Users\\Dan\\OneDrive\\University\\Anul 3\\Analiza datelor\\Set\\jocuri_video.csv')
colSums(is.na(games))
sales <- sales[complete.cases(sales), ]
colSums(is.na(sales))
str(sales)
unique(sales$year_of_release)
sales <- sales[sales$year_of_release != "N/A", ]
unique(sales$year_of_release)
sales$year_of_release <- as.integer(sales$year_of_release)
sum(sales$publisher=="N/A")
sales <- sales[sales$publisher != "N/A", ]
sum(sales$developer == "N/A")
sum(sales$rating == "N/A")

sales$user_score <- as.numeric(sales$user_score)
summary(sales$user_score)
sales$user_score <- sales$user_score * 10
sales$platform <- as.factor(sales$platform)
sales$genre <- as.factor(sales$genre)

sales <- sales %>% mutate(rating = ifelse(rating == "AO", "M", rating))
sales <- sales %>% mutate(rating = ifelse(rating == "K-A", "E", rating))
sales <- sales %>% mutate(rating = ifelse(rating == "RP", "E", rating))
sales %>% count(rating)

set.seed(123)
test_index <- createDataPartition(sales$global_sales, p = 0.8, list = FALSE)
train_set <- sales[-test_index, ]
test_set <- sales[test_index, ]

totalData <- rbind(train_set, test_set)

model_lm <- train(log(global_sales) ~ critic_score + 
                    user_score + genre + 
                    year_of_release, method = "lm", data = train_set)
model_lm

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

test_set$predicted_lm <- predict(model_lm, test_set)
rmse_results <- data.frame(Method = "Linear regression", 
                           RMSE = RMSE(log(test_set$global_sales), test_set$predicted_lm))

rmse_results
ggplot(test_set) + 
  geom_point(aes(log(global_sales), predicted_lm)) + 
  geom_line(aes(log(global_sales), log(global_sales))) + 
  xlab("Actual values") + ylab("Predicted values") + 
  labs(caption = 
         paste("R-squared", 
               format(model_lm$results$Rsquared, 
                      digits = 2)))
