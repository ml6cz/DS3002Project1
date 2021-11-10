#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
library(DT)
library(jsonlite)

df <- read.csv("Spotify-2000-cleaned.csv") 

# Define UI
ui <- fluidPage(
    setBackgroundColor(  
        color = c("#ffb8cf", "#b8cdff"),
        gradient = "radial",
        direction = c("top", "left")
    ),

    # Application title
    titlePanel("Spotify Data"),
    tags$h2("DS 3002: Project 1"),
    tags$h3("Megan Lin (ml6cz)\n\n"),
    
    tags$h3("Artists\n"),
    
    # Initial artist evaluation
    fluidRow(
        column(3,
               selectInput(inputId = "Artist",label = "Select an artist",
                           choices = unique(df[c("Artist")]),
                           selected=levels(df$type)[1])
                ),
        column(6, 
               tabsetPanel(
                   tabPanel("Artist's Work", DT::dataTableOutput("table")),
                   tabPanel("Discography Popularity", plotOutput("plot")),
                   tabPanel("Basic Stats", verbatimTextOutput("stats"))
                   )
                )
            ),
    fluidRow(column(12, verbatimTextOutput("dimensions"))),
    
    tags$h3("Songs\n"),
    
    # Song evaluation
    sidebarLayout(
        sidebarPanel(
            downloadButton("downloadData", "Download")
        ),
        
        mainPanel(
            fluidRow(
                column(4,verbatimTextOutput("artist_name")),
                column(8,tableOutput("similar_songs"))
            )
        )
    )
)

#Filter the table by the selected artist, then enable selection of single rows (aka a single song) for further evaluation
server <- function(input, output) {
    df <- read.csv("Spotify-2000-cleaned.csv")
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <-df %>% filter(Artist == input$Artist)
        return(data)
    }), selection = 'single')

    output$plot <- renderPlot({
        artist <- df %>% filter(Artist == input$Artist)
        ggplot(artist, aes(x = Popularity)) + 
            geom_histogram(aes(y=..density..), fill="red", alpha = 0.2)+
            geom_density(color="darkblue", fill="lightblue", alpha = 0.4) +
            xlim(0,100)
    })
    
    output$stats <- renderPrint({
        artist <- df %>% filter(Artist == input$Artist)
        summary(artist)
    })
    
    output$dimensions <- renderText({
        artist <- df %>% filter(Artist == input$Artist)
        paste(input$Artist, " has ", nrow(artist), " songs. We have metrics for ", ncol(df), " features of each song including the song title.")
    })
    
    output$artist_name <- renderPrint({
        artist <- df %>% filter(Artist == input$Artist)
        
        # Must convert artist name and song name to characters from their natural factor forms
        artist$Artist<-as.character(artist$Artist)
        artist$Title<-as.character(artist$Title)
        
        s = input$table_rows_selected
        row<-artist[s, ]
        
        if (length(s)) {
            cat('Similar songs to "')
            cat(row$Title)
            cat('" by ')
            cat(row$Artist)
        }
    })
        
    # Find similar songs based on similarity in energy, danceability, valence, and acousticness
    output$similar_songs <- renderTable({
        artist <- df %>% filter(Artist == input$Artist)
        
        # Must convert artist name and song name to characters from their natural factor forms
        artist$Artist<-as.character(artist$Artist)
        artist$Title<-as.character(artist$Title)
        
        s = input$table_rows_selected
        if (is.integer(s)) {
            row<-artist[s, ]
            base_artist <-as.character(row$Artist)
            base_title <-as.character(row$Title)
            base_year <-as.character(row$Year)
            base_energy <- rep(as.numeric(row$Energy), times = nrow(df))
            base_danceability <- rep(as.numeric(row$Danceability), times = nrow(df))
            base_valence <- rep(as.numeric(row$Valence), times = nrow(df))
            base_acousticness <- rep(as.numeric(row$Acousticness), times = nrow(df))
            
            #Calculate the similarity scores
            scores <- df %>% 
                select(Artist, Title, SongGenre, Energy, Danceability, Valence, Acousticness)
            
            scores$SimilarityScore <-abs(scores$Energy - base_energy) + abs(scores$Danceability - base_danceability) + abs(scores$Valence - base_valence) + abs(scores$Acousticness - base_acousticness)
            
            # Sort the scores in ascending order
            attach(scores)
            sorted <- scores[order(SimilarityScore),]
            detach(scores)
            # Return the top 10 most similar scoring songs, with the original song as reference
            return(head(sorted, 11))
        }
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('recommended_songs-', input$Artist, '.json', sep='')
        },
        content = function(file) {
            
            artist <- df %>% filter(Artist == input$Artist)
            artist$Artist<-as.character(artist$Artist)
            artist$Title<-as.character(artist$Title)
            
            s = input$table_rows_selected
            if (is.integer(s)) {
                row<-artist[s, ]
                base_artist <-as.character(row$Artist)
                base_title <-as.character(row$Title)
                base_year <-as.character(row$Year)
                base_energy <- rep(as.numeric(row$Energy), times = nrow(df))
                base_danceability <- rep(as.numeric(row$Danceability), times = nrow(df))
                base_valence <- rep(as.numeric(row$Valence), times = nrow(df))
                base_acousticness <- rep(as.numeric(row$Acousticness), times = nrow(df))
                
                #Calculate the similarity scores
                scores <- df %>% 
                    select(Artist, Title, SongGenre, Energy, Danceability, Valence, Acousticness)
                
                scores$SimilarityScore <-abs(scores$Energy - base_energy) + abs(scores$Danceability - base_danceability) + abs(scores$Valence - base_valence) + abs(scores$Acousticness - base_acousticness)
                
                # Sort the scores in ascending order
                attach(scores)
                sorted <- scores[order(SimilarityScore),]
                detach(scores)
                sorted <-(head(sorted, 21))
                json <- toJSON(sorted)
                write(json, paste('recommended_songs-', input$Artist, '.json'))
            }
        }
    )
}
# Run the application 
shinyApp(ui = ui, server = server)
