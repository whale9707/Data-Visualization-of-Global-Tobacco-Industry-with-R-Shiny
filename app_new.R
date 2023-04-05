library(devtools)
library(knitr)
library(leaflet)
library(shiny)
library(raster)
library(RColorBrewer)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(readxl)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinydashboard)
library(shinyWidgets)
library(maps)
library(plotrix)
library(tidyr)
library(stringr)
library(tidytext)
library(RTextTools)
library(SnowballC)
library(quanteda)
library(magrittr)
library(tidyverse)
library(tm)
library(wordcloud)
library(widyr)
library(igraph)
library(ggraph)
library(shinythemes)
library(networkD3)
library(rgeos)
library(sp)
library(Rcpp)
library(sp)
library(osmdata)
library(magrittr)
library(ggmap)

# load data in overview
sales <- read.csv('Datasets/sales_per_day.csv')
final_dataset <- read.csv("Datasets/final_dataset.csv")
nytimes_tobacco_control <- read_excel("Datasets/nytimes_tobacco_control.xlsx")
reddit_tobacco_control <- read_excel("Datasets/reddit_tobacco_control.xlsx")
mpower_overview <- read.csv("Datasets/mpower_overview.csv")
names(mpower_overview)[3] <- 'HelpToQuit'
names(mpower_overview)[4] <- 'EnforceBansTobaccoAd'
retail_price <- read.csv("Datasets/retail_price.csv")
names(retail_price)[3] <- 'CigarPricePPP$'
taxes <- read.csv("Datasets/taxes.csv")
names(taxes)[3] <- 'taxes%'

world <- map_data("world")
df <- read_csv("Datasets/tobacco_use_ww.csv")
df <- df%>%
  filter(Gender == 'Both sexes')%>%
  select(Location, Year,Value)
colnames(df)[which(names(df) == "Value")] <- "tobacco_use"
colnames(df)[which(names(df) == "Location")] <- "Country"

# preprocess data in residents tab
# For sales
sales <- sales %>%
  filter(Year >= 2000)


# For cost benefit analysis
final <- final_dataset %>% 
  select(Country, Year, harmvalue, econvalue)

avg_harm <- final%>%
  select(Country, Year, harmvalue)%>%
  group_by(Year)%>%
  summarize(mean_harm = mean(harmvalue))

avg_econ <- final%>%
  select(Country, Year, econvalue)%>%
  group_by(Year)%>%
  summarize(mean_econ = mean(econvalue))

merged_df <- left_join(as.data.frame(avg_harm), as.data.frame(avg_econ), by = "Year")

# For Current Policy
price_taxes <- left_join(retail_price, taxes, by=c("Country","Year"))
price_taxes["taxes$"] <- round(retail_price$`CigarPricePPP$`*taxes$`taxes%`, 2)

stop_smoking_byh <- left_join(price_taxes, mpower_overview, by=c("Country", "Year"))

stop_smoking_byh <- na.omit(stop_smoking_byh)

group_by(stop_smoking_byh, Country) %>%
  count(n())

avearge_change <- stop_smoking_byh %>% 
  group_by(Year) %>%
  summarise(mean_quit = mean(HelpToQuit), mean_ad = mean(EnforceBansTobaccoAd))
avearge_change

country_geocode <- force(world.cities) %>%
  filter(capital == 1) %>%
  rename(Country = country.etc) %>%
  arrange(Country)

stop_smoking_byh[stop_smoking_byh=="Democratic Republic of the Congo"] <- "Congo Democratic Republic"
stop_smoking_byh[stop_smoking_byh=="Bolivia (Plurinational State of)"] <- "Bolivia"
stop_smoking_byh[stop_smoking_byh=="Brunei Darussalam"] <- "Brunei"
stop_smoking_byh[stop_smoking_byh=="Cabo Verde"] <- "Cape Verde"
stop_smoking_byh[stop_smoking_byh=="Czechia"] <- "Czech Republic"
stop_smoking_byh[stop_smoking_byh=="Eswatini"] <- "Swaziland"
stop_smoking_byh[stop_smoking_byh=="Lao People's Democratic Republic"] <- "Laos"
stop_smoking_byh[stop_smoking_byh=="Micronesia (Federated States of)"] <- "Micronesia"
stop_smoking_byh[stop_smoking_byh=="Republic of Korea"] <- "Korea South"
stop_smoking_byh[stop_smoking_byh=="Russian Federation"] <- "Russia"
stop_smoking_byh[stop_smoking_byh=="Saint Vincent and the Grenadines"] <- "Saint Vincent and The Grenadines"
stop_smoking_byh[stop_smoking_byh=="Republic of Moldova"] <- "Moldova"
stop_smoking_byh[stop_smoking_byh=="Syrian Arab Republic"] <- "Syria"
stop_smoking_byh[stop_smoking_byh=="Timor-Leste"] <- "East Timor"
stop_smoking_byh[stop_smoking_byh=="United Kingdom of Great Britain and Northern Ireland"] <- "UK"
stop_smoking_byh[stop_smoking_byh=="United States of America"] <- "USA"
stop_smoking_byh[stop_smoking_byh=="United Republic of Tanzania"] <- "Tanzania"
stop_smoking_byh[stop_smoking_byh=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
stop_smoking_byh[stop_smoking_byh=="Viet Nam"] <- "Vietnam"

country_geocode <- force(world.cities) %>%
  filter(capital == 1) %>%
  rename(Country = country.etc) %>%
  arrange(Country)


stop_smoking_geo <- left_join(stop_smoking_byh, country_geocode, by = "Country") %>%
  select(-pop, -name, -capital)

# check whether there are null values on longitude and latitude
stop_smoking_geo %>% 
  filter_all(any_vars(is.na(long))) %>%
  group_by(Country) %>%
  summarise(price = sum(`CigarPricePPP$`))

stop_smoking_geo$HelpToQuit[stop_smoking_geo$HelpToQuit == 1] <- "Data not reported"
stop_smoking_geo$HelpToQuit[stop_smoking_geo$HelpToQuit == 2] <- "None"
stop_smoking_geo$HelpToQuit[stop_smoking_geo$HelpToQuit == 3] <- "NRT* and/or some cessation services** (neither cost-covered)"
stop_smoking_geo$HelpToQuit[stop_smoking_geo$HelpToQuit == 4] <- "NRT* and/or some cessation services** (at least one of which is cost-covered)"
stop_smoking_geo$HelpToQuit[stop_smoking_geo$HelpToQuit == 5] <- "National quitline, and both NRT* and some cessation services** cost-covered"

stop_smoking_geo$EnforceBansTobaccoAd[stop_smoking_geo$EnforceBansTobaccoAd == 1] <- "Data not reported"
stop_smoking_geo$EnforceBansTobaccoAd[stop_smoking_geo$EnforceBansTobaccoAd == 2] <- "Complete absence of ban, or ban that does not cover national television (TV), radio and print media"
stop_smoking_geo$EnforceBansTobaccoAd[stop_smoking_geo$EnforceBansTobaccoAd == 3] <- "Ban on national TV, radio and print media only"
stop_smoking_geo$EnforceBansTobaccoAd[stop_smoking_geo$EnforceBansTobaccoAd == 4] <- "Ban on national TV, radio and print media as well as on some but not all other forms of direct* and/or indirect** advertising"
stop_smoking_geo$EnforceBansTobaccoAd[stop_smoking_geo$EnforceBansTobaccoAd == 5] <- "Ban on all forms of direct* and indirect** advertising."

stop_smoking_geo <- na.omit(stop_smoking_geo) %>%
  rename(c("Help To Quit Policy" = HelpToQuit, "Tobacco Ads Policy" = EnforceBansTobaccoAd, "Adjusted Average Cigarette Price in Dollars$" = "CigarPricePPP$", "Average Cigarette Taxes in Percentage%" = "taxes%"))

stop_smoking_geo <- na.omit(stop_smoking_geo)


# For New York Times Sentiment
nytimes_abstract <- tibble(text = str_trim(gsub("[A-Z]{2,}", "",
                                                gsub("[[:punct:]]","",     
                                                     gsub("[[:digit:]]+","", 
                                                          gsub("\n", "", 
                                                               gsub("\r", "", nytimes_tobacco_control$abstract))))))) %>%
  drop_na() %>%
  mutate(row_num = 1:523)

cleaned_stem_nytimes <- nytimes_abstract %>%
  drop_na() %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart"))%>%
  mutate(stem = wordStem(word)) 

cleaned_word_nytimes_count <- cleaned_stem_nytimes %>%
  group_by(word) %>%
  mutate(count = 1) %>%
  summarize(freq = sum(count)) %>%
  arrange(-freq)

cleaned_stem_nytimes_count <- cleaned_stem_nytimes %>%
  group_by(stem) %>%
  mutate(count = 1) %>%
  summarize(freq = sum(count)) %>%
  arrange(-freq)

pos <- read.table("positive-words.txt", as.is=T)
neg <- read.table("negative-words.txt", as.is=T)

sentiment <- function(words){
  require(quanteda)
  tok <- quanteda::tokens(words)
  pos.count <- sum(tok[[1]]%in%pos[,1])
  neg.count <- sum(tok[[1]]%in%neg[,1])
  out <- round((pos.count - neg.count)/(pos.count+neg.count), 2)
  return(out)
}

output <- vector()
for (i in (1:523)) {
  output[i] <- sentiment(nytimes_abstract$text[i])
  #output[is.na(output)] <- 0
}

tone_output_df <- as.data.frame(output) %>%
  mutate(row_num = 1:523) %>%
  merge(nytimes_abstract, "row_num") %>%
  select(1:2) %>%
  drop_na()

positive_tone <- tone_output_df %>%
  filter(output > 0) %>%
  merge(nytimes_abstract, "row_num")

positive_stem_nytimes <- positive_tone %>%
  drop_na() %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart"))

pos_df <- as.data.frame(pos)
names(pos_df)[1] <- "word"

pos_words <- left_join(pos_df, positive_stem_nytimes, by = "word") %>% drop_na()

negative_tone <- tone_output_df %>%
  filter(output < 0) %>%
  merge(nytimes_abstract, "row_num")

negative_stem_nytimes <- negative_tone %>%
  drop_na() %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart"))

neg_df <- as.data.frame(neg)
names(neg_df)[1] <- "word"

neg_words <- left_join(neg_df, negative_stem_nytimes, by = "word") %>% drop_na()

positve_tone_text1<-paste(pos_words$word, collapse=" ")
negative_tone_text1<-paste(neg_words$word, collapse=" ")

positive_negative_dtm2 <- DocumentTermMatrix(VCorpus(VectorSource(c(positve_tone_text1, negative_tone_text1))))

positive_negative_m2 <- as.matrix(positive_negative_dtm2)

positive_negative_m2 <- t(positive_negative_m2)

colnames(positive_negative_m2)[1] <- "positive"
colnames(positive_negative_m2)[2] <- "negative"

# For Reddit Sentiment
reddit_text <- tibble(text = str_trim(gsub("[A-Z]{2,}", "",
                                           gsub("[[:punct:]]","",     
                                                gsub("[[:digit:]]+","", 
                                                     gsub('http\\S+\\s*'," ",
                                                          gsub("\n", " ", 
                                                               gsub("\r", " ", reddit_tobacco_control$text)))))))) %>%
  select(1) %>%
  drop_na() %>%
  mutate(row_num = 1:86)

cleaned_stem_reddit <- reddit_text %>%
  drop_na() %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart"))%>%
  mutate(stem = wordStem(word)) 

cleaned_word_reddit_count <- cleaned_stem_reddit %>%
  group_by(word) %>%
  mutate(count = 1) %>%
  summarize(freq = sum(count)) %>%
  arrange(-freq)

cleaned_stem_reddit_count <- cleaned_stem_reddit %>%
  group_by(stem) %>%
  mutate(count = 1) %>%
  summarize(freq = sum(count)) %>%
  arrange(-freq)

output_reddit <- vector()
for (i in (1:86)) {
  output_reddit[i] <- sentiment(reddit_text$text[i])
}

tone_output_df_reddit <- as.data.frame(output_reddit) %>%
  mutate(row_num = 1:86) %>%
  merge(reddit_text, "row_num") %>%
  select(1:2) %>%
  drop_na() 

positive_tone_reddit <- tone_output_df_reddit %>%
  filter(output_reddit > 0) %>%
  merge(reddit_text, "row_num") 

positive_word_reddit <- positive_tone_reddit %>%
  drop_na() %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart"))

pos_words_reddit <- left_join(pos_df, positive_word_reddit, by = "word") %>% drop_na()

negative_tone_reddit <- tone_output_df_reddit %>%
  filter(output_reddit < 0) %>%
  merge(reddit_text, "row_num") 

negative_word_reddit <- negative_tone_reddit %>%
  drop_na() %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(source = "smart"))

neg_words_reddit <- left_join(neg_df, negative_word_reddit, by = "word") %>% drop_na()

positve_tone_text_reddit <-paste(pos_words_reddit$word, collapse=" ")
negative_tone_text_reddit<-paste(neg_words_reddit$word, collapse=" ")

positive_negative_dtm_reddit <- DocumentTermMatrix(VCorpus(VectorSource(c(positve_tone_text_reddit, negative_tone_text_reddit))))

positive_negative_m_reddit <- as.matrix(positive_negative_dtm_reddit)
positive_negative_m_reddit <- t(positive_negative_m_reddit)

colnames(positive_negative_m_reddit)[1] <- "positive"
colnames(positive_negative_m_reddit)[2] <- "negative"

# For NYT Network
nytime_text2 <- tibble(text = str_trim(gsub("[A-Z]{2,}", "",
                                            gsub("[[:punct:]]","",     
                                                 gsub("[[:digit:]]+","", 
                                                      gsub('http\\S+\\s*'," ",
                                                           gsub("\n", " ", 
                                                                gsub("\r", " ", nytimes_tobacco_control$abstract)))))))) %>%
  select(1) %>%
  drop_na() %>%
  mutate(row_num = 1:523)

nytimes_word_pairs <- nytime_text2 %>% 
  drop_na() %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  pairwise_count(word,row_num, sort = TRUE, upper = FALSE)

# For Reddit Network
reddit_text2 <- tibble(text = str_trim(gsub("[A-Z]{2,}", "",
                                            gsub("[[:punct:]]","",     
                                                 gsub("[[:digit:]]+","", 
                                                      gsub('http\\S+\\s*'," ",
                                                           gsub("\n", " ", 
                                                                gsub("\r", " ", reddit_tobacco_control$text)))))))) %>%
  select(1) %>%
  drop_na() %>%
  mutate(row_num = 1:86)

reddit_word_pairs <- reddit_text2 %>% 
  drop_na() %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  pairwise_count(word,row_num, sort = TRUE, upper = FALSE) 

# For Use

world <- map_data("world")
df <- read_csv("Datasets/tobacco_use_ww.csv")
df <- df%>%
  filter(Gender == 'Both sexes')%>%
  select(Location, Year,Value)
colnames(df)[which(names(df) == "Value")] <- "tobacco_use"
colnames(df)[which(names(df) == "Location")] <- "Country"

df_2000 <- df %>% 
  filter(Year == 2000)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2005 <- df %>% 
  filter(Year == 2005)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2010 <- df %>% 
  filter(Year == 2010)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2013 <- df %>% 
  filter(Year == 2013)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2014 <- df %>% 
  filter(Year == 2014)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2015 <- df %>% 
  filter(Year == 2015)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2016 <- df %>% 
  filter(Year == 2016)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2017 <- df %>% 
  filter(Year == 2017)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)
df_2018 <- df %>% 
  filter(Year == 2018)%>%
  select(Country,tobacco_use)%>%
  group_by(Country)

data_2018 <- merge(df_2018, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)
data_2017 <- merge(df_2017, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)

data_2016 <- merge(df_2016, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)

data_2015 <- merge(df_2015, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)

data_2014 <- merge(df_2014, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)

data_2013 <- merge(df_2013, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)

data_2010 <- merge(df_2010, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)

data_2005 <- merge(df_2005, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)

data_2000 <- merge(df_2000, 
                   map_data("world"), by.x = "Country", by.y = "region") %>%
  arrange(Country, order)



plot_2018 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2018,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2018")

plot_2017 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2017,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2017")


plot_2016 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2016,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2016")


plot_2015 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2015,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2015")


plot_2014 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2014,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2014")


plot_2013 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2013,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2013")


# world map
plot_2010 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2010,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2010")


plot_2005 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2005,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2005")


plot_2000 <- ggplot() + 
  # world map
  geom_polygon(data = map_data("world"), 
               aes(x=long, y = lat, group = group),
               fill = "lightgrey") +
  # custom map
  geom_polygon(data = data_2000,
               aes(x=long, y = lat, group = group, fill = tobacco_use))+
  labs(fill = "Tobacco Use Prevalence", title = "Prevalence of Global Tobacco Use 2003")


carol_year <- tibble::tribble(
  ~carolyear,
  "2018", 
  "2017", 
  "2016",
  "2015",
  "2014",
  "2013",
  "2010",
  "2005",
  "2000"
)




# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Tobacco Control Research",
  theme = shinytheme("flatly"),
  tabPanel(
    "Project Overview",
    titlePanel(div(
      windowTitle = "TobaccoBackground",
      img(src = "intro.jpeg", width = "100%", class = "bg")
    )),
    htmlOutput("intro"),
    uiOutput("tab1"),
    uiOutput("tab2"),
    uiOutput("tab3"),
    uiOutput("tab4")
  ),
  tabPanel(
    "Tobacco Background",
    titlePanel(div(
      windowTitle = "TobaccoBackground",
      img(src = "background.jpeg", width = "100%", class = "bg")
    )),
    tags$br(),
    h4("Tobacco background provides a thorough overview of different perspectives in the global tobacco industry and public health issues related to smoking. There are 3 separate plots in this section and each one showcased an interactive visualization by year."),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        tags$br(),
        selectInput(
          'sales_ctry',
          label = 'Country',
          choices = sales$Entity,
          selected = sales$Entity[0]
        )
      ),
      
      mainPanel(
        h2("Consumption -  Cigarette Number"),
        h4("An interactive line graph where users can choose their own countries of interest and the value for any particular year for a specific country will be shown when the mouse stops at the point on the line graph. It shows the average number of cigarettes consumed per smoker in each country from 2000 to 2014. For most countries, there is a downward trends in the number of cigaretts consumed."),
        plotlyOutput("sales"),
        tags$br(),
        tags$br()
      )
    ),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        tags$br(),
        selectInput(inputId = "carolyear",
                    label = "Choose a year:",
                    choices = (carol_year$carolyear))
      ),
      
      mainPanel(
        h2("Consumption - Tobacco Usage Penetration"),
        h4("A world map that indicates the prevalence of tobacco consumption in each country from 2000 to 2010. The greater percentage the smoking population, the lighter the color will be."),
        imageOutput("carolmap"),
        tags$br(),
        tags$br()
      )
    ),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        tags$br(),
        selectInput(
          'death_ctry',
          label = 'Country',
          choices = (sort(unique(final_dataset$Country))),
          selected = sort(unique(final_dataset$Country))[0]
        )
      ),
      
      mainPanel(
        h2("Death Rate"),
        h4("An interactive line graph where users can choose their own countries of interest and the value for any particular year for a specific country will be shown when the mouse stops at the point on the line graph. It shows the number of deaths attributed to tobacco consumption (directly and indirectly) with disability adjusted years per 10,000 individuals in each year. We are really happy to see that for most countries, the death rate is decreasing."),
        plotlyOutput("death"),
        tags$br(),
        tags$br()
      )
    )
  ),
  tabPanel(
    "Global Tobacco Control Policy",
    h2("Global Tabacco Control Policy Map"),
    tags$br(),
    h4("This is an interactive world map of the changes in Global Tobacco Control policies in 188 countries from 2008 to 2018 bi-annually. Users can choose the year of their own interest. The information for any particular year and the policies for each country will be shown when click on the bubble."),
    h4("The sizes of the bubbles means the adjusted price of cigarettes in dollars. The larger bubbles, the higher price, and vice versa. "),
    tags$br(),
    selectInput("year", "Please choose a year:",
                c('2018' = 2018,
                  '2016' = 2016,
                  '2014' = 2014,
                  '2012' = 2012,
                  '2010' = 2010,
                  '2008' = 2008)),
    leafletOutput("distPlot",height = "600px"),
    tags$br(),
    h4("Takeaway:"),
    h4("From the map, we could clearly see the price of cigarettes and the taxes of cigarettes have increasing tendencies through 2008 to 2018, which means the governments are trying to increase the price and taxes to decrease the purchasing behaviors in cigarettes. "),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        h4("Tobacco Control Policies Overview"),
        h5("1. Average Cigarette Price. Note that this is adjusted by the Purchasing Power Parity to make the values consistent across countries with different economic standards."),
        h5("2. Taxes (%). Percentage of tax imposed on tobacco products."),
        h5("3. HelpToQuit. Information from countries on the availability and non-availability of tobacco cessation aids is assessed to determine the comparative level of assistance countries provide to help tobacco users quit."),
        h5(" ·  1 = Data not reported"),
        h5(" ·  2 = None"),
        h5(" ·  3 = NRT* and/or some cessation services** (neither cost-covered)"),
        h5(" ·  4 = NRT* and/or some cessation services** (at least one of which is cost-covered)"),
        h5(" ·  5 = National quitline, and both NRT* and some cessation services** cost-covered"),
        h5("4. EnforceBansTobaccoAd. Enforcement of bans on tobacco advertising."),
        h5(" ·  1 = Data not reported"),
        h5(" ·  2 = Complete absence of ban or ban that does not cover national television (TV), radio and print media"),
        h5(" ·  3 = Ban on national TV, radio and print media only"),
        h5(" ·  4 = Ban on national TV, radio and print media as well as on some but not all other forms of direct* and/or indirect** advertising"),
        h5(" ·  5 = Ban on all forms of direct* and indirect** advertising.")
      ),
      mainPanel(
        h3("Introduction to Global Tobacco Control Policies"),
        plotlyOutput("encoded_policies"),
        h4("Takeaways:"),
        h4("This line chart shows the tendencies of encoded Help To Quit policy and encoded Tobacco Advertisement policy. From the left side we could know while the encoded number increased, which means the policy is improved. So the overall tendency shows the the Help to Quit policy is more supportive and the Tobacco Advertisement policy is more strict. ")
    )),
    tags$br(),
    tags$hr(),
    h3("Detail Information for Tobacco Control Policies"),
    sidebarLayout(
      sidebarPanel(
        selectInput("year2", "Please choose a year:",
                    c('2018' = 2018,
                      '2016' = 2016,
                      '2014' = 2014,
                      '2012' = 2012,
                      '2010' = 2010,
                      '2008' = 2008)),
        selectInput("field", "Please choose a specific field:",
                    c("Adjusted Average Cigarette Price (Dollars)" = "Adjusted Average Cigarette Price in Dollars$",
                      "Average Cigarette Taxes in Percentage" = "Average Cigarette Taxes in Percentage%",
                      "Tobacco Ads Policy" = "Tobacco Ads Policy",
                      "Help To Quit Policy" = "Help To Quit Policy"))
      ),
      mainPanel(
        dataTableOutput("smoking_table"))
    
  )
),
  
  tabPanel(
    "Cost Benefit Analysis",
    titlePanel(div(
      windowTitle = "TobaccoBackground",
      img(src = "tobacco.jpeg", width = "100%", class = "bg"),
      tags$br()
    )),
    h2("Analysis for Tobacco Harm and Economic Benefit"),
    h4("The team has developed models to derive harm value (which includes variables that can bring impact on human health) and economics value (which includes variables that generate economic benefits to the country). The formula we used for deriving harm and economic value are as shown below."),
    HTML('<center><img src="formula2.jpeg"></center>'),
    HTML('<center><img src="formula1.jpeg""></center>'),
    h4("The two values are modeled in ways that are backed up by economic theories and we make sure that these values are comparable to each other. For example, we take log on the tax revenue derived in the EconValue formula to ensure that it follows the Law of Diminishing Marginal Returns. For the HarmValue, we use a power scalar greater than 1 to indicate the polynomial growth of the impact of death rate as a 80% will definitely raise much more concern compared to a 10% death rate."),
    h4("As seen from the result shown in the graph, we can see that the HarmValue has been decreasing over the policy years we studied. Besides, the EconValue that are produced by tobacco tax revenue have been gradually increasing over the years. All these results demonstrate the effectiveness of current global tobacco control policies in terms of balancing well between the trade-offs between health impact and economic benefit."),
    tags$br(),
    tags$hr(),
    h4("Here, we use an interactive line graph to demonstrate the results for our cost-benefit analysis during the policy year studied. Details on the models we build will be described in detail below the graph."),
    plotlyOutput("cost_benefit")
  ),
  tabPanel(
    "Text Exploration",
    h2("Text Analysis for Tobacco Control Policy on New York Time and Reddit"),
    tags$br(),
    h4("Here, we show the textual-based analysis for tobacco control policy by using sentiment score comparison, common word extraction and word pairs co-occurrence analysis between New York Times and Reddit. For New York Times, we tokenized the abstracts of news articles filtered by keywords “tobacco control policy” from 2008 to 2018 and for Reddit, we extracted posts with the same time horizon."),
    h4("For the New York Times text analysis, we selected data from 2008 to 2018 from New York Times API, and there were 529 articles related to tobacco control policies after we cleaned the data set. "),
    h4("For the Reddit text analysis, we web-scraped data from Reddit and selected data from 2008 to 2018, and there were 104 useful comments related to tobacco control policies after we cleaned the data set. "),
    h4("The boxplots show the sentiment analysis, and the summary statistics will be revealed when users move the mouse on the plots. For New York Times, the sentiment score is a wider range than Reddit, whereas it is more evenly distributed between positive words and negative words in NY times. In Reddit, the posts tend to contain more negative sentiment."),
    h4("The word clouds show the most common positive and negative words in the two platforms. For example, cancer is the common negative word meaning that all users like to link tobacco with cancer."),
    h4("The network analysis shows the co-occurrence of word pairs. For example, in Reddit the word health likes to co-exist with policy in a post. And in the NY times, smoking is likely to occur in the post with the public."),
    tags$br(),
    splitLayout(
      HTML('<center><img src="NYT.jpeg", width = "30%"></center>'),
      HTML('<center><img src="reddit.jpg", width = "30%"></center>')
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Sentiment Score Distribution",
        splitLayout(
          plotlyOutput("NYT_ss_box"),
          plotlyOutput("reddit_ss_box")
        )
      ),
      tabPanel(
        "Word Cloud",
        splitLayout(
          plotOutput("NYT_wordcloud", width = "800px", height="1000px"),
          plotOutput("reddit_wordcloud", width = "800px", height="1000px")
        ),
        splitLayout(
          h4("New York Times", align = 'center'), 
          h4("Reddit", align = 'center')
        )
      ),
      tabPanel(
        "Network Analysis",
        splitLayout(
          forceNetworkOutput("NYT_force"),
          forceNetworkOutput("reddit_force")
        ),
        splitLayout(
          h4("New York Times", align = 'center'), 
          h4("Reddit", align = 'center')
        )
      )
    )
  ),
  
  tabPanel(
    "Appendix",
    htmlOutput("appendix")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sales <- renderPlotly({
    selected_sales<-sales %>% 
      filter(Entity == input$sales_ctry)
    plot_ly(selected_sales, x = ~Year, y = ~amt, type = 'scatter', mode = 'lines+markers', width = 4) %>%
      layout( yaxis = list(title = "Average Number of Cigrattes Consumed per Smoker"))
  })
  
  output$death <- renderPlotly({
    selected_df<-final_dataset %>% 
      filter(Country==input$death_ctry)
    selected_df <- selected_df[order(selected_df$Year, decreasing = FALSE), ]
    plot_ly(selected_df, x = ~Year, y = ~death_rate, type = 'scatter', mode = 'lines+markers') %>%
      layout(yaxis = list(title = 'Death Number'), legend = list(title=list(text='<b> Death Number</b>')))
  })
  
  output$distPlot <- renderLeaflet({
    smoking_country_data = stop_smoking_geo[stop_smoking_geo$Year == input$year, ]
    pal <- colorQuantile("Reds", stop_smoking_geo$`taxes$`, n = 3)
    m3 <- leaflet(smoking_country_data) %>%
      addTiles() %>% 
      setView(34.5085, 8.7832, zoom = 2) %>%
      addCircles(data = smoking_country_data, 
                 ~long, ~lat, 
                 weight = smoking_country_data$"Adjusted Average Cigarette Price in Dollars$" * 2,
                 color = ~pal(smoking_country_data$`taxes$`),
                 fillOpacity = 0.7,
                 opacity = 0.7,
                 popup = paste0(
                   'Country: ', smoking_country_data$Country, "<br>",
                   'Year: ', smoking_country_data$Year, "<br>",
                   'Average Price($): ', smoking_country_data$"Adjusted Average Cigarette Price in Dollars$", "<br>",
                   'Average Tax(%): ', smoking_country_data$"Average Cigarette Taxes in Percentage%", "<br>",
                   'Ban Tobacco Ad: ', smoking_country_data$"Tobacco Ads Policy", "<br>",
                   'Help to quit: ', smoking_country_data$"Help To Quit Policy"
                 )
      ) %>%
      addLegend(pal = pal, values = ~stop_smoking_geo$`taxes$`,title = "Average Cigarette Taxes in Percentage%", opacity = 1)  
    
    
  })
  
  output$encoded_policies <- renderPlotly({
    quit_change <- 
      plot_ly(avearge_change, x = ~Year, y = ~mean_quit, type = "scatter", name = 'Help To Quit Policy', mode = 'lines+markers') %>% 
      add_trace(y = ~mean_ad, name = 'Tobacco Advertisement Policy', mode = 'lines+markers') %>%  
      layout(title = 'The Average Changes in Encoded Policies',
             xaxis = list(title = 'Year'),
             yaxis = list (title = 'Encoded Policy'))
    quit_change 
  })
  
  
  output$smoking_table <- renderDataTable({
    smoking_country_data =  stop_smoking_geo[stop_smoking_geo$Year == input$year2, ]
    plot_data <- na.omit(smoking_country_data)
    plot_data <- plot_data[c(1:200), c("Country", input$field)]
    plot_data
  })
  
  output$cost_benefit <- renderPlotly({
    plot_ly(merged_df, x = ~Year) %>%
      add_trace(y = ~mean_harm, name ="harm value", type = "scatter", mode="line") %>%
      add_trace(y = ~mean_econ, name = "econ value", mode = "line") %>%
      layout(title = "Cost Benefit Analysis by Year", yaxis = list(title = 'Value'))
  })
  
  output$NYT_sentiment_score <- renderPlot({
    ggplot(tone_output_df, aes(x=output))+
      geom_bar(fill = 'royalblue2')+
      theme_minimal() +  
      labs(title="New York Times Sentiment Score and Counts", x ="Sentiment Score", y = "Count")
  })
  
  output$NYT_ss_box <- renderPlotly({
    plot_ly() %>%
      add_trace(data = tone_output_df, y = ~output, 
                type = "box", fillcolor = "royalblue2", 
                name = 'New York Times')%>%
      layout(yaxis = list(title = 'Sentiment Score'))
  })
  
  output$reddit_sentiment_score <- renderPlot({
    ggplot(tone_output_df_reddit, aes(x=output_reddit))+
      geom_bar(fill = 'royalblue2')+
      theme_minimal() +  
      labs(title="Reddit Sentiment Score and Counts", x ="Sentiment Score", y = "Count")
  })
  
  output$reddit_ss_box <- renderPlotly({
    plot_ly() %>%
      add_trace(data = tone_output_df_reddit, y = ~output_reddit, 
                type = "box", fillcolor = "royalblue2", 
                name = 'Reddit')%>%
      layout(yaxis = list(title = 'Sentiment Score'))
  })
  
  output$NYT_wordcloud <- renderPlot({
    comparison.cloud(positive_negative_m2, colors = c("orange", "grey"), scale=c(4, 0.1), title.size = 3.5, max.words = 100, fixed.asp=TRUE)
  })
  output$reddit_wordcloud <- renderPlot({
    comparison.cloud(positive_negative_m_reddit, colors = c("orange", "grey"), scale=c(7, 0.3), title.size = 3.5, max.words = 100, fixed.asp=TRUE)
  })
  
  output$reddit_word_pairs <- renderPlot({
    reddit_word_pairs %>% 
      filter(n >= input$reddit_pairs) %>%
      graph_from_data_frame() %>% 
      ggraph(layout = "fr") + 
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "tomato") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, 
                     point.padding = unit(0.2, "lines")) +
      theme_void()
  })
  
  url1 <- a("Hanzhang Hu", href = "https://www.linkedin.com/in/hanzhang-hu-9a36701a0")
  url2 <- a("Dan Li", href = "https://www.linkedin.com/in/jessica-li-43484095")
  url3 <- a("Pengyun Li", href = "https://www.linkedin.com/in/pengyun-li/")
  url4 <- a("Rui Lu", href = "https://www.linkedin.com/in/rui-lu-98765b19a/")
  
  output$tab1 <- renderUI({
    tagList("Contact", url1, ": hh2921@columbia.edu")
  })
  output$tab2 <- renderUI({
    tagList("Contact", url2, ": dl3466@columbia.edu")
  })
  output$tab3 <- renderUI({
    tagList("Contact", url3, ": pl2799@columbia.edu")
  })
  output$tab4 <- renderUI({
    tagList("Contact", url4, ": rl3191@columbia.edu")
  })
  
  
  output$intro <- renderUI({
    HTML(paste("<h1>About Us</h1>",
               "<h4> We are a team of master’s students at Columbia University in the City of New York in the Quantitative Methods in Social Sciences (QMSS) Program. This Shiny Dashboard serves as the platform for our GR5063 Data Visualization Final Project (Group C). Our team members include Hanzhang Hu, Dan Li, Pengyun Li, and Rui Lu.  </h4>",
               "<h1>Main Objective</h1>",
               "<h4>Tobacco consumption has been one of the biggest public health threats in the world. Recent studies conducted by the World Health Organization (WHO) and the Institute for Health Metrics and Evaluation have shown that around 7.2 million people die prematurely every year from smoking. However, despite the severe harms of tobacco usage, it is impractical to simply ban tobacco production as tobacco generates significant economic benefits such as agricultural employment, tax revenue, foreign exchange earnings, etc. Therefore, policymakers in different countries need to find a balance point between the harms of tobacco consumption on public health and the economic benefits generated from the tobacco industry.</h4>",
               "<h4>Our team aims to utilize various data visualization techniques on tobacco industry and policy datasets to see the effectiveness and public sentiment of global tobacco control policies from 2008 to 2018.</h4>",
               "<h4>Click the tabs on the top to explore!</h4>",
               "<h1>Contact Us</h1>",
               sep ="<br/>"))
  })
  
  output$conclusion <- renderUI({HTML(paste("<h1>Conclusion</h1>",
                                            "<h4>As seen from the results for cost benefit analysis, we can see that the negative impact of tobacco on public health has been decreasing over the policy years we studied. Besides, the benefits that are produced by tobacco tax revenue and production have been gradually increasing over the years. All these results demonstrate the effectiveness of current global tobacco control policies in terms of balancing well between the trade-offs between health impact and economic benefit. </h4>",
                                            "<h4>Our further analysis on public sentiment towards tobacco control shows a more negative sentiment score in New York Times and a more positive score in Reddit. This can be attributed to the fact that in news articles, writers usually need to point out the harms of tobacco consumption to support the government tobacco control policy, leading to more negative words to be extracted. On the counterpart, in Reddit, the public do not need to summarize anything but explicitly express their opinions on the tobacco control policies, and their overall positive attitudes towards the policy lead to more positive words to be extracted for our sentiment score calculation. Therefore, our findings from the sentiment analysis match the results from the cost benefit analysis, which demonstrated that the global tobacco control policies are both necessary and effective.</h4>"
                                            ,sep ="<br/>"))
  })
  
  output$appendix <- renderUI({HTML(paste("<h1>Data Sources</h1>",
                                          "<h4>1. tobacco_use_ww.csv</h4>",
                                          "<h5>The percentage of the population aged 15 years and over who currently use any tobacco product (smoked and/or smokeless tobacco) on a daily or non-daily basis. Tobacco products include cigarettes, pipes, cigars, cigarillos, water pipes (hookah, shisha), bidis, kretek, heated tobacco products, and all forms of smokeless (oral and nasal) tobacco. Tobacco products exclude e-cigarettes (which do not contain tobacco), “e-cigars”, “e-hookahs”, JUUL and “e-pipes”.</h5>",
                                          "<h5>Source: https://www.who.int/</h5>",
                                          "<h4>2. sales_per_day.csv</h4>",
                                          "<h5>Figures include manufactured cigarettes, as well as estimated number of hand-rolled cigarettes, per adult (ages 15+) per day. The number of cigarettes smoked per person per day for both males and females has been averaged across all years in which multiple estimates were provided in the ISS dataset for the United States, to arrive at one estimate for each year.</h5>",
                                          "<h5>Source: http://www.pnlee.co.uk/ISS.htm</h5>",
                                          "<h4>3. death_rates_smoking_age.csv</h4>",
                                          "<h5>Death rates are measured as the number of early deaths due to smoking per 100,000 individuals in a given demographic group. </h5>",
                                          "<h5>Source: http://ghdx.healthdata.org/gbd-results-tool</h5>",
                                          "<h4>4. stop_smoking.csv</h4>",
                                          "<h5>This dataset presents indicators that contribute to an individual to stop smoking. These contributions can be very direct, like offering government help, or less direct like increasing the tax and price of cigars and banning Tobacco advertisements. </h5>",
                                          "<h5>Source: https://apps.who.int/gho/data/node.home</h5>",
                                          "<h4>5. nytimes_tobacco_control.xlsx</h4>",
                                          "<h5>This dataset used New York Times’ API to gain the abstracts NYT articles from related to tobacco control policies from 2008 to 2018, which stays same with our overall interested years. </h5>",
                                          "<h4>6. reddit_tobacco_control.xlsx</h4>",
                                          "<h5>This dataset used web-scaping to gain the posts on the Reddit that related to tobacco control policy topics from 2008 to 2018, which stays same with our overall interested years. </h5>",
                                          sep ="<br/>"))
  })
  
  
  output$NYT_force <- renderForceNetwork({
    threshold <- 3
    
    network <-  nytimes_word_pairs %>%
      filter(n > threshold) %>%
      graph_from_data_frame(directed = FALSE)
    
    V(network)$degree <- strength(graph = network)
    E(network)$width <- E(network)$n/max(E(network)$n)
    
    network.D3 <- igraph_to_networkD3(g = network)
    network.D3$nodes %<>% mutate(Degree = V(network)$degree)
    network.D3$links$Width <- 10*E(network)$width
    
    forceNetwork(
      Links = network.D3$links, 
      Nodes = network.D3$nodes, 
      Source = 'source', 
      Target = 'target',
      NodeID = 'name',
      Group = "Degree", 
      opacity = 0.9,
      Value = 'Width',
      Nodesize = 'Degree', 
      # We input a JavaScript function.
      linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
      fontSize = 12,
      zoom = TRUE, 
      opacityNoHover = 1
    )
  })
  
  output$reddit_force <- renderForceNetwork({
    threshold <- 20
    
    network <-  reddit_word_pairs %>%
      filter(n > threshold) %>%
      graph_from_data_frame(directed = FALSE)
    
    V(network)$degree <- strength(graph = network)
    E(network)$width <- E(network)$n/max(E(network)$n)
    
    network.D3 <- igraph_to_networkD3(g = network)
    network.D3$nodes %<>% mutate(Degree = V(network)$degree)
    network.D3$links$Width <- 10*E(network)$width
    
    forceNetwork(
      Links = network.D3$links, 
      Nodes = network.D3$nodes, 
      Source = 'source', 
      Target = 'target',
      NodeID = 'name',
      Group = "Degree", 
      opacity = 0.9,
      Value = 'Width',
      Nodesize = 'Degree', 
      # We input a JavaScript function.
      linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
      fontSize = 12,
      zoom = TRUE, 
      opacityNoHover = 1
    )
    
  })
  
  ############### carolmap
  output$carolmap <- renderPlot({
    switch(input$carolyear,
           "2018" = plot_2018,
           "2017" = plot_2017,
           "2016" = plot_2016,
           "2015" = plot_2015,
           "2014" = plot_2014,
           "2013" = plot_2013,
           "2010" = plot_2010,
           "2005" = plot_2005,
           "2000" = plot_2000)
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
