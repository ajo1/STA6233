
library(shiny)
library(ggplot2)
library(stats)
library(data.table)
library(plotly)

load(url("https://raw.githubusercontent.com/ajo1/STA6233/master/lyrics_final.rdata"))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Song Lyrics"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            sliderInput("year",
                        "Year:",
                        min = 1960,
                        max = 2010,
                        value = 1960,
                        step = 5,
                        animate = animationOptions(500))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Aggregate", plotlyOutput(outputId = "FreqPlot")),
                tabPanel("Percent", plotlyOutput(outputId = "PcntPlot")),
                tabPanel("Sentiment", plotlyOutput(outputId = "SentPlot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    song_table_final_2 <- song_table_final[, .SD[1:20], by=Year]
    
    output$FreqPlot <- renderPlotly({
        
        ggplot(data=subset(song_table_final_2, Year == input$year), aes(x=reorder(Var1,-Freq),y=Freq)) + 
            geom_bar(stat="identity", color="black") + theme_light() + 
            ggtitle("Most frequent lyrics by year") + xlab("Word") + ylab("Count") + 
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))
        
    })
    
    output$PcntPlot <- renderPlotly({
        
        ggplot(data=subset(song_table_final_2, Year == input$year), aes(x=reorder(Var1,-pcnt),y=pcnt)) + 
            geom_bar(stat="identity", color="black") + theme_light() + 
            ggtitle("Most frequent lyrics by year") + xlab("Word") + ylab("Count") + 
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))
        
    })
    
    sent <- song_table_final[complete.cases(song_table_final), ]
    sent_agg <- aggregate(sent$Freq, by=list(Sentiment=sent$sentiment, Year=sent$Year), FUN=sum)
    
    output$SentPlot <- renderPlotly({
        
        ggplot(data=subset(sent_agg, Year == input$year), aes(x=Sentiment,y=x)) + 
            geom_bar(stat="identity", color="black") + theme_light() + 
            ggtitle("Most frequent lyrics by year") + xlab("Word") + ylab("Count") + 
            theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
