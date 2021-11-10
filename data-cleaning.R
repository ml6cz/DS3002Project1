library(dplyr)
# Read in the kaggle dataset on Spotify data from https://www.kaggle.com/iamsumat/spotify-top-2000s-mega-dataset
df<-read.csv("Spotify-2000.csv")

# Rename columns
df <- df %>%
  rename(SongGenre = Top.Genre,
         BPM = Beats.Per.Minute..BPM.,
         Loudness = Loudness..dB.,
         DurationSeconds = Length..Duration.)

# Drop index and duration columns
df <- df[ , !(names(df) %in% c("Index", "DurationSeconds"))]

# Create a new metric on how upbeat a song is based on other Spotify metrics
df$upBeat <- df$BPM/3 + df$Energy + df$Danceability + df$Valence + (100 - df$Acousticness)

# Remove rows with null values, none were found though
df <- na.omit(df)

# Print the names of the column in df for viewing
colnames(df)

# Number of columns (16 columns)
ncol(df)

# Number of rows (1994 rows)
nrow(df)

# Write to a csv file
write.csv(df,"Spotify-2000-cleaned.csv", row.names=FALSE)