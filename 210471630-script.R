#install.packages("readr")
#install.packages("tidyr")
#install.packages("dplyr")
library(readr)
library(tidyr)
library(dplyr)

#Load the uncorrected data frame
setwd("/cloud/project/Spotify_datasets_2024")
spotify_raw <- 
read.table("Spotify_Messy_210471630.txt", sep= "\t", header=T)

#View function used to look at data
View(spotify_raw) 

# Correcting issues in the initial data frame

# 1) Verify column names of dataframe
colnames(spotify_raw)

# Correcting column names
spotify_final <- spotify_raw %>% 
  rename(track_name = TTrack_nameff5) %>% 
  #rename column
  separate_wider_delim("danceability.energy","_",names=c("danceability","energy"))%>%
  #Separate fused columns
  pivot_longer(cols = c(22:26), names_to = "playlist_genre") %>%
  #Combine multiple columns to generate 2 columns; playlist_genre and playlist_subgenre
  rename(playlist_subgenre = value) %>%
  #Rename column
  filter(playlist_subgenre != "")
  #Filter out incorrect columns generated from pivot_longer

# Verify corrected column names
colnames(spotify_final)

# 2) Identify which artists with known typos are in the subset
#Artists are:Shakira, Bad Bunny Taylor Swift, Janis Joplin, and The Four Owls

# Extracting artists using first and last letter of their name
# Note multiple combinations were tried
grep_out1 <- grep(pattern = '^S.*a', x = spotify_raw$track_artist)
spotify_raw$track_artist[grep_out1] #no typos found

grep_out2 <- grep(pattern = 'B.*y', x = spotify_raw$track_artist)
spotify_raw$track_artist[grep_out2] #Bad Sunny is a typo of Bad Bunny

grep_out3 <- grep(pattern = 'T.*t', x = spotify_raw$track_artist)
spotify_raw$track_artist[grep_out3] #none identified

grep_out4 <- grep(pattern = 'J.*n', x = spotify_raw$track_artist)
spotify_raw$track_artist[grep_out4] #none identified

grep_out5 <- grep(pattern = 'T.*s', x = spotify_raw$track_artist)
spotify_raw$track_artist[grep_out5] #none identified

# 3) Fixing typos identified in four columns: track_artist, mode, track album_release_date and playlist_genre
spotify_final <- spotify_final %>%
  mutate(track_artist = gsub("Sunny", "Bunny", track_artist)) %>%
  #Fix typo in artist name
  mutate(mode = as.numeric(gsub("T", "", mode))) %>%
  #Remove Ts present in mode column; this column should be 1 or 0
  mutate(track_album_release_date = gsub("^75", "2019", track_album_release_date))%>%
  #Correct incorrect album release date
  mutate(playlist_genre = gsub("r.b", "r&b", playlist_genre))
  #Rename r.b to r&b

# 4) Moving newly made columns appropriately
spotify_final<- spotify_final %>% 
  relocate(playlist_subgenre, .before=danceability)%>%
  relocate(playlist_genre, .before=playlist_subgenre)

# 5) Deleting incorrect id columns
spotify_final <- spotify_final %>%
  select(-playlist_id)%>%
  select(-track_album_id)

# 6) Verifying variable types (structure of columns)
str(spotify_final)

# Correcting incorrect variables:danceability and energy
spotify_final <- spotify_final %>% 
  mutate(danceability = as.numeric(spotify1$danceability),
         energy = as.numeric(spotify1$energy))

# Verify corrected structure
str(spotify_final)

# 7) For my colleague who will compare danceability scores across the playlist, we need to make playlist_name a factor column
spotify_final <- spotify_final %>% 
  mutate(playlist_name = as.factor(playlist_name))

# Compiling entire code into one function
Correct_spotify_table <- spotify_raw%>% 
  rename(track_name = TTrack_nameff5) %>% 
  separate_wider_delim("danceability.energy","_",names=c("danceability","energy"))%>%
  pivot_longer(cols = c(22:26), names_to = "playlist_genre") %>%
  rename(playlist_subgenre = value) %>%
  filter(playlist_subgenre != "")%>%
  mutate(track_artist = gsub("Sunny", "Bunny", track_artist)) %>%
  mutate(mode = as.numeric(gsub("T", "", mode))) %>%
  mutate(track_album_release_date = gsub("^75", "2019", track_album_release_date))%>%
  mutate(playlist_genre = gsub("r.b", "r&b", playlist_genre))%>% 
  relocate(playlist_subgenre, .before=danceability)%>%
  relocate(playlist_genre, .before=playlist_subgenre)%>%
  select(-playlist_id)%>%
  select(-track_album_id)%>% 
  mutate(danceability = as.numeric(spotify1$danceability),
         energy = as.numeric(spotify1$energy))%>% 
  mutate(playlist_name = as.factor(playlist_name))

# Viewing and verifying  the code
View(Correct_spotify_table)
str(Correct_spotify_table)

# Create final file
write.table(Correct_spotify_table, "210471630 - file.txt",
            sep="\t",
            col.names = T,
            row.names = F,
            quote = F)

# Check if it works
check <- read.table("210471630 - file.txt", sep= "\t", header=T)
View(check)

