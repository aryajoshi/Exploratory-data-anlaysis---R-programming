---
  title: "Spotify dataset"
output: html_document
---
  
  ```{r setup, include=FALSE}
# **********************************************************************************
# Exploratory data analysis on spotify 
# **********************************************************************************


library(tidyverse)
library(funModeling)
library(Hmisc)
require(lattice)
library(dplyr)
install.packages("ggpubr")
install.packages("corrplot")
require(corrplot)
require(ggpubr)
require(ggplot2)
library(RColorBrewer)


# 1 Data extraction 
# 2 Data cleaning 
# 3 Diving deep into data and finfind patterns
# Identifying the features for machie learning
# Algorith Selection 
# Algorithm implementation 


# Loading dataset in a dataframe using tidyverse
spotify <- read_csv("spotify_data.csv")

spotify

glimpse(spotify)
status(spotify)

# Check NA values 
spotify %>% is.na() %>% colSums()

# Remove all the NA values from the dataset 
# spotify = spotify$track_name %>% na.omit()
# spotify = spotify$track_artist %>% na.omit()
# spotify = spotify$track_album_name %>% na.omit()


# Removing  NA values from  the datasets.
spotify<- na.omit(spotify) 



# removing unnecessary columns form the dataset -
spotify$track_id <- NULL
spotify$track_album_id <- NULL
spotify$playlist_id <- NULL


# Checking all the genre
spotify %>% distinct(playlist_genre)
# There are 5 genre _ pop, rap, rock, latin, r&b, edm

# Checking all the sub genre
spotify %>% distinct(playlist_subgenre)
# dance pop	, post-teen pop, electropop	, indie poptimism	, hip hop	, southern hip hop	,gangster rap	, trap, album rock	# classic rock, permanent wave, hard rock, tropical, latin pop, reggaeton, latin hip hop, urban contemporary,
# hip hop, new jack swing, neo soul.


# List of top 5 artist that gave  high valence albums.(happy songs )
spotify %>% select(track_artist,track_album_name,mode, danceability, valence) %>% arrange(desc(spotify$valence)) %>% head(5)

# List of top 5 artist that gave low valence albums.(Sad songs )
spotify %>% select(track_artist,track_album_name,mode, danceability, valence) %>% arrange(spotify$valence) %>% head(5)

#  List of tracks that were performed live (Value above 0.8 means there was a strong precense of people )
spotify %>% filter(liveness > 0.8) %>% select(track_artist,track_album_name, liveness) 

# Most danceable songs 
spotify %>% select(track_artist, track_name, danceability) %>% arrange(desc(spotify$danceability))
# 	If Only I Could (feat. Steve Lucas) - Liem Remix by Fusion groove orchestra has the highest danceability.

# Most


# Assigning all the numeric columns to a variable "numeric" for further analysis.
nums <- unlist(lapply(spotify, is.numeric))
numeric <- spotify[ , nums]


# Constructing Correlation-Matrix of numeric dataframe

numeric.rcorr = rcorr(as.matrix(numeric))
numeric.rcorr

numeric.cor = cor(numeric, method = c("spearman"))
corrplot(numeric.cor)

# Major insights from corelation-matrix  -
# Most positivley correlated columns are loudness and energy 
# Most negatively correlated columns are energy and acousticness
# Valence and danceability are the second most correlated


# <---------- Checking how data is spread ------------------------------------->
# Here we are considering the spread of genre and sub-genre so that in future when we use classiying algorithm, We can make assumptions whether algorithm will be biased or no, when predicting the class of a song.
# Constructing Bar-plot for genre
# Classes are evenly distributed in genre  - they have enough value so there are low chances of biased results
ggplot(spotify , aes(x=as.factor(playlist_genre), fill=as.factor(playlist_genre) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")

# Constructing Bar-plot for sub-genre
# RcolorBrewer library is used and as there are lot of sub-genre we need to set sace_fill as manual
# Classes are evenly distributed in subgenre  - they have enough value so there are low chances of biased results
nb.cols <- 24 # No of columns are specified first
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols) #  Setting parameters
# Actual plotting
ggplot(spotify , aes(x=as.factor(playlist_subgenre), fill=as.factor(playlist_subgenre) )) + 
  geom_bar( ) +
  scale_fill_manual(values = mycolors) +
  theme(legend.position="none")

#Checking distribution of all the columns in numeric dataframe.
hist(numeric)

# Plotting box-plot to get a sense of outliers.
b <- boxplot(numeric)
# Column time-duration and tempo have most of the outliers

#Analysing variable instrumentalness
spotify$instrumentalness
numeric %>% distinct(instrumentalness)
hist(numeric$instrumentalness, breaks = 5)
# Most of the songs fall in a value between 0 and 0.2 which means, most songs have vocals

# We’ve already determined that energy,valence and danceability are positively correlated; but this time, let’s see how these variables are distributed over xyz songs.
correlated_density <- ggplot(spotify) +
  geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
  geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
  geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Valence and Danceability") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Energy, Valence and Danceability") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

correlated_density


boxplot(spotify$instrumentalness)



# Most popular song is Dance Moneky by Tones and I 
spotify %>% select(track_popularity, track_name, valence, track_artist, acousticness) %>% arrange(desc(track_popularity) ) %>% head(100)
# Most popular song does not have highest valence



# Analysing Tones and I artist overall to see what makes them popular 
spotify %>% filter(track_artist == "Tones and I" ) %>% select(track_name, track_popularity, track_artist)





correlated_density <- ggplot(spotify) +
  geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
  geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
  geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Valence and Danceability") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Energy, Valence and Danceability") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

correlated_density


loudness_density <- ggplot(spotify) +
  geom_density(aes(loudness, fill ="loudness")) + 
  scale_x_continuous(name = "Loudness") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Loudness") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Paired")

loudness_density

# Converting key and mode to factors for better analysis (Categories )
spotify$key <- as.factor(spotify$key)
spotify$mode <- as.factor(spotify$mode)


# Visualising keys distribution 
ggplot(spotify, aes(x=key, fill=key)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Sound Keys Counts (Pitch Class notation)") +
  scale_x_discrete(labels = c('C','C#','D','D#','E','F','F#','G','G#','A','A#','B'))


# Analysing the mode by track modality in terms of major and minor 
ggplot(spotify, aes(x=mode, fill=mode)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Trak Modality") +
  scale_x_discrete(labels = c('Minor','Major'))

# Analysing speechiness 
# Categorising speechiness 
spotify$speechiness[spotify$speechiness < 0.33]  = "Music and non-speec tracks"
spotify$speechiness[spotify$speechiness >=  0.33 & spotify$speechiness  <= 0.66 ] = "Music and speech"
spotify$speechiness[spotify$speechiness > 0.66] = "Entirely spoken words"


spotify <- table(spotify$speechiness)
barplot(counts, main="Spotify",
        xlab="Speechiness")


ggplot(,aes(x=rhythm, fill= rhythm)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Musical Tempo Bands") +
  scale_x_discrete(labels = c('Presto', 'Allegro', 'Andante','Adagio', 'Lenght'))







# Analysing tempo variable 
# Dividing Tempo Feature in Bands
# Length: very slow (20 bpm)
# Adagio: slow and majestic (66 to 76 bpm)
# Andante: at the pace, quiet, a little vivacious (76 to 108 bpm)
# Allegro: animated and fast. (110 to 168 bpm).
# Presto: very fast (168 to 200 bpm).

q <- sum(spotify$tempo > 168)
w <- sum(spotify$tempo >= 110 & spotify$tempo <= 168)
e <- sum(spotify$tempo >= 76 & spotify$tempo <= 108)
r <- sum(spotify$tempo >= 66 & spotify$tempo <= 76)
t <- sum(spotify$tempo < 65)

presto <- integer(q) 
allegro <- integer(w) + 1
andante <- integer(e) + 2
adagio <- integer(r) + 3
lenght <- integer(t) + 4 

rhythm = c(presto, allegro, andante, adagio, lenght)
rhythm <- as.factor(rhythm)
genres <- data.frame(rhythm)

# Plotting temo column 
ggplot(genres,aes(x=rhythm, fill= rhythm)) +
  geom_bar() +
  theme(legend.position='none') +
  labs(title = "Musical Tempo Bands") +
  scale_x_discrete(labels = c('Presto', 'Allegro', 'Andante','Adagio', 'Lenght'))
# Most songs fall into adagio and andante which means they are slow and majestic

# Important 
# Artist that gave most albums  
spotify %>%
  group_by(track_artist) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  slice(1:10) %>%
  ggplot(., aes(reorder(track_artist, +freq), freq))+
  geom_bar(stat = "identity", fill = "royalblue1", col = "grey10")+
  coord_flip()+
  labs(x = "" ,y = "Top 10 Artists", title = "Artist with most songs released")+
  geom_text(aes(label = freq, y = freq/2))
# Martin Garrix has the highest frequency in the dataset.

spotify %>% filter(track_artist == 'Martin Garrix') %>% select(track_name,track_artist)
