
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(RColorBrewer)
library(summarytools)
library(htmlTable)
library(htmlwidgets)
library(tidyr)
library(dplyr)
library(shinyjs)
library(sqldf)
library(devtools)
library(plotly)
library(sunburstR)
library(plotme)
library(wordcloud)
library(shinyWidgets)


gross_df <- read.csv("disney.csv", header=TRUE,
                    stringsAsFactors = FALSE,na.string = "")

gross_df <- na.omit(gross_df)


na_count <-sapply(gross_df, function(y) sum(length(which(is.na(y)))))
print(na_count)


gross_int <- unlist(lapply(gross_df$Total_gross, function(x) 
  gsub("\\$|\\,", "", x)))


gross_df$Total_gross <- as.numeric(gross_int)

inflation_gross_int <- unlist(lapply(gross_df$Inflation_adjusted_gross, function(x) 
  gsub("\\$|\\,", "", x)))


gross_df$Inflation_adjusted_gross <- as.numeric(inflation_gross_int)

head(gross_df)

gross_df <- select(gross_df, -Index)

gross_df = gross_df %>%
  mutate(
    Decade = case_when(
      startsWith(as.character(Year),"193") ~ "1930s",
      startsWith(as.character(Year),"194") ~ "1940s",
      startsWith(as.character(Year),"195") ~ "1950s",
      startsWith(as.character(Year),"196") ~ "1960s",
      startsWith(as.character(Year),"197") ~ "1970s",
      startsWith(as.character(Year),"198") ~ "1980s",
      startsWith(as.character(Year),"199") ~ "1990s",
      startsWith(as.character(Year),"200") ~ "2000s",
      startsWith(as.character(Year),"201") ~ "2010s",
    )
  )

server = function(input,output,session){
  
  #sunburst - top 10 movies in each ratings
  #searching by genre and rating, total gross
  #wordcloud - movie title
  
 
  genre_top_5_movies <- sqldf("select Movie_title, Genre,Total_gross from
                          (select Movie_title, Genre,Total_gross,
                          rank() over(partition by Genre 
                          order by Total_gross desc)
                          as rank from gross_df) temp
                          where temp.rank <= 3 and 
                              Genre in ('Adventure','Action',
                              'Comedy','Drama','Musical','Romantic Comedy',
                              'Western','Thriller/Suspense')")  
  
  devtools::install_github("yogevherz/plotme", force= TRUE)
  
  sunburst =  genre_top_5_movies %>% count(Genre,Movie_title, wt = Total_gross) %>% 
    count_to_sunburst(fill_by_n=TRUE)
  
  
  output$sunburst_movies <- renderUI({
    sunburst
  })
 
  
  #top 5 genres, based on num of movies

  movie_title_frequency <- unlist(lapply(gross_df$Movie_title, function(x) 
    strsplit(x, " +")[[1]]))
  
  
  movie_title_freq_df <- data.frame(table(movie_title_frequency))
  
  colnames(movie_title_freq_df) <- c("Movie_title_keywords","Count")
  
  title_word_freq <- sqldf("select Movie_title_keywords,
                          sum(Count) as Total_Frequency from movie_title_freq_df
                          where Movie_title_keywords not in ('And','&','a','A','to',
                          'in','and')
                          group by Movie_title_keywords
                          having Total_Frequency < 20
                          order by Total_Frequency desc limit 50")
 
  output$movie_word_cloud <- renderPlot({
    wc_color = brewer.pal(8,"Set2")
    if(input$color == "Accent"){
      wc_color = brewer.pal(8,"Accent")
    }
    else{
      wc_color = brewer.pal(8,"Dark2")
    }
    wordcloud(words = title_word_freq$Movie_title_keywords, 
              freq = title_word_freq$Total_Frequency,
              min.freq = input$wordfreq,
              max.words = input$maxword,
              main = "Popular keywords for the movie genres",
              scale=c(3.5,0.25),
              colors = wc_color,
              random.order = input$random, rot.per = .30)
  })
  
  output$movies_table <- renderDataTable({
    movies = gross_df %>% 
      filter(Genre %in% input$genre,
             MPAA_rating %in% input$rating,
                                 Total_gross <= input$total_gross,
                                 Year <= input$year) %>%
      select(Movie_title,Release_date,Genre,Year,MPAA_rating,Total_gross)


  })
  

  decade_top_5_movies <- sqldf("select Movie_title, Decade,Total_gross from
                          (select Movie_title, Decade,Total_gross,
                          rank() over(partition by Decade 
                          order by Total_gross desc)
                          as rank from gross_df) temp
                          where temp.rank <= 5")
  
  print(decade_top_5_movies)
  
  decade_top_gross_movies =  decade_top_5_movies %>% 
    count(Decade,Movie_title,wt = Total_gross) %>% 
    count_to_treemap(fill_by_n=TRUE)
  
  output$treemap_movies <- renderUI({
    decade_top_gross_movies
  })
  
  # Filter rows with no blank values in genre column

  df_filtered <- gross_df
  df_filtered$Genre <- as.factor(df_filtered$Genre)
  df_filtered$MPAA_rating <- as.factor(df_filtered$MPAA_rating)
  df_filtered$Season <- as.factor(df_filtered$Season)
  df_filtered$Month <- as.factor(df_filtered$Month)
  
  # creating the summary for variables
  output$summary<-renderUI({
    print(dfSummary(subset(df_filtered))
          ,method = 'render',headings = FALSE, bootstracp.css = FALSE)
  })
  
  options(scipen=999)
  # create the scatter plot
  output$scatter_plot <- renderPlot({
    ggplot(df_filtered, aes(x = Total_gross, y = Inflation_adjusted_gross,color=Genre)) +
      geom_point() +
      labs(x = "Total Gross", y = "Inflation-adjusted Gross")+
      scale_color_brewer(palette = 'Set1')+scale_colour_discrete(na.translate = F)+
      theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(text=element_text(size=14,  family="Comic Sans MS"))
  })
  
  # Line graph
  output$line_graph <- renderPlot({
    df_filtered %>%
      ggplot(aes(x = Year)) +
      geom_line(aes(y = Total_gross, color = "Total Gross")) +
      geom_line(aes(y = Inflation_adjusted_gross, color = "Inflation")) +labs(x = "Year", y = "Amount (USD)", color = "", title = "Total Gross and Inflation Over Time") + 
      scale_color_manual(name = "", 
                      values = c("Total Gross" = "#42d6a4", "Inflation" = "#c780e8"), 
                      labels = c("Total Gross", "Inflation")) +
      theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(text=element_text(size=14,  family="Comic Sans MS"))
  })
  
  # Bar graph
  output$barplot <- renderPlot({
    df_filtered %>%
      group_by(MPAA_rating) %>%
      summarise(n = n(),total_gross = sum(Total_gross)) %>%
      ggplot(aes(x = n, y = MPAA_rating, fill= MPAA_rating)) +
      geom_bar(stat = "identity") +  
      scale_fill_manual(values = c("#F8B195", "#F67280", "#C06C84", "#6C5B7B", "#355C7D"),
                        name = "Rating",
                        labels = c("G: General Audience.Suitable for all ages",
                                   "Not Rated: Some films are not submitted for a rating, or the rating process is not yet complete",
                                   "PG: Parental Guidance Suggested. Some material may not be suitable for children",
                                   "PG-13: Parents Strongly Cautioned. Some material may be inappropriate for children under 13",
                                   "R: Restricted. Children under 17 require accompanying parent or adult guardian")) + 
      labs(x = "Count of Movies", y = "Rating", fill = "Rating")+
      theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(text=element_text(size=15,  family="Comic Sans MS"))
  })
  
  # Donut chart
  output$donut_chart <- renderPlot({
    df_filtered %>%
      group_by(Season) %>% summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100) %>%
      ggplot(aes(x = Season, y = percentage, fill= Season)) +geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(y = percentage/2, label = sprintf("%.2f%%", round(percentage, 2))), color = "white", size = 5) +
      scale_fill_brewer(palette = "Dark2")+
      labs(title = "Percentage of Movies Released by Season")+
      theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(text=element_text(size=15,  family="Comic Sans MS"))
  })
  

}

title <- tags$a(
  href="https://www.disneystudios.com/#/",
  tags$img(src="https://i.insider.com/5f577f8be6ff30001d4e76a8?width=750&format=jpeg&auto=webp",
           height = '60', width = '100'))

frow1 <- fluidRow(
  tags$img(src="https://resizing.flixster.com/4QmGJFR39jMm8uYSeMv4Aop8w6I=/206x305/v2/https://flxt.tmsimg.com/assets/p8815512_p_v8_ax.jpg",
           height = '200', width = '170'),
  tags$img(src="https://lumiere-a.akamaihd.net/v1/images/p_findingnemo_19752_05271d3f.jpeg?region=0%2C0%2C540%2C810",
           height = '200', width = '170'),
  tags$img(src="https://prod-ripcut-delivery.disney-plus.net/v1/variant/disney/764F2A9B9CD5A8601381942F2A8C582ED593E88B0492E6776783FC64408E8ADC/scale?width=1200&aspectRatio=1.78&format=jpeg",
           height = '200', width = '170'),
  tags$img(src="https://lumiere-a.akamaihd.net/v1/images/p_aladdin1992_20486_174ba005.jpeg",
           height = '200', width = '170'),
  tags$img(src="https://lumiere-a.akamaihd.net/v1/images/p_piratesofthecaribbean_thecurseoftheblackpearl_19642_d1ba8e66.jpeg",
           height = '200', width = '170'),
  tags$img(src="https://lumiere-a.akamaihd.net/v1/images/p_toystory2_19639_4eca9113.jpeg",
           height = '200', width = '170'),
  tags$img(src="https://kbimages1-a.akamaihd.net/9493a870-bd6a-41ba-ae3f-794a4bfc77d1/1200/1200/False/walt-disney-s-the-jungle-book-1.jpg",
           height = '200', width = '170')
  
)

frow2 <- fluidRow(
  tags$img(src="https://prod-ripcut-delivery.disney-plus.net/v1/variant/disney/CF05493C681BE56C7D085E1131EE47D12D9BC505411E1D77BB2164D84AE0CC48/scale?width=1200&aspectRatio=1.78&format=jpeg",
           height = '200', width = '170'),
  tags$img(src="https://resizing.flixster.com/4QmGJFR39jMm8uYSeMv4Aop8w6I=/206x305/v2/https://flxt.tmsimg.com/assets/p8815512_p_v8_ax.jpg",
           height = '200', width = '170'),
  tags$img(src="https://m.media-amazon.com/images/M/MV5BMjE5MzcyNjk1M15BMl5BanBnXkFtZTcwMjQ4MjcxOQ@@._V1_.jpg",
           height = '200', width = '170'),
  tags$img(src="https://lumiere-a.akamaihd.net/v1/images/p_aladdin1992_20486_174ba005.jpeg",
           height = '200', width = '170'),
  tags$img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTqugf4WRAeOvFF0_j96PKjbjJo6Tu2JQ9EyA&usqp=CAU",
           height = '200', width = '170'),
  tags$img(src="https://lumiere-a.akamaihd.net/v1/images/p_cinderella_20490_a7c83808.jpeg",
           height = '200', width = '170'),
  tags$img(src="https://lumiere-a.akamaihd.net/v1/images/p_ratatouille_19736_0814231f.jpeg",
           height = '200', width = '170')
  
)




header <- dashboardHeader(title =  title)  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("About", tabName = "introduction", icon = icon("info")),
              menuItem("Overview", tabName = "overview", icon = icon("list-alt")),
              menuItem("Total Gross vs Adj Gross", tabName = "scatterplot", icon = icon("usd")),
              menuItem("No. of Movies by Ratings", tabName = "bar_graph", icon = icon("star")),
              menuItem("Percentage of Movies by Seasons", tabName = "donut_chart", icon = icon("dashboard")),
              menuItem("Top Movies by Decade", tabName = "treemap", icon = icon("tree")),
              menuItem("Top Movies by Genre", tabName = "popular_movies", icon = icon("signal")),
              menuItem("Popular Movie title Keywords", tabName = "word_cloud", icon = icon("cloud")),
              menuItem("Movie Explorer", tabName = "search", icon = icon("search"))
              
  )
)


# combine the two fluid rows to make the body
body <- dashboardBody(  h2('Disney Movies'),
                        useShinyjs(),
                        shinydashboard::tabItems(
                          shinydashboard::tabItem(
                          tabName = "introduction",
                          tags$img(src="https://w0.peakpx.com/wallpaper/677/925/HD-wallpaper-disney-infinity-buzz-lightyear-elsa-frozen-figurine-in-white-background-movies.jpg",height="500px",width="auto"),h1("Disney Dashboard"),
                          p("The Disney Movies dataset contains information about various movies released between 1937 and 2016, including their titles, release dates, production budgets, worldwide gross earnings, and more."),
                          p("This dataset can be useful for analyzing the performance of various Disney movies over time and for identifying trends in the industry."),
                          p("Navigate through the tabs to view different data visualizations.")
                        ),
                        shinydashboard::tabItem(
                          
                          tabName = "overview",
                          
                          sidebarLayout(
                            sidebarPanel(
                              width=4,
                              tags$img(src="https://images.saymedia-content.com/.image/ar_4:3%2Cc_fill%2Ccs_srgb%2Cfl_progressive%2Cq_auto:eco%2Cw_1200/MTc0MzA1MTE0NDk1MjY0NjM2/top-10-disney-movie-favourites.jpg",height = '500', width = '400')
                            ),
                            mainPanel(
                              htmlOutput("summary")
                              
                            )
                          )
                        ),
                        shinydashboard::tabItem(
                          tabName = "scatterplot",
                          plotOutput("scatter_plot")
                        ),
                        shinydashboard::tabItem(
                          tabName = "bar_graph",
                          plotOutput("barplot"),
                          tags$img(src="https://files.kstatecollegian.com/2022/04/WhyDisneyMoviesareGreatforEveryone6Reasons.jpg",height = '300', width = '1100')
                        ),
                        shinydashboard::tabItem(
                          tabName = "donut_chart",
                          plotOutput("donut_chart")
                        ),
                          shinydashboard::tabItem(
                            tabName = "treemap",
                            h3("Top 5 Highest-Grossing Movies by Decade",
                               align="center"),
                            htmlOutput("treemap_movies", height = 1000),
                            frow1
                           
                          ),
                          shinydashboard::tabItem(
                            tabName = "popular_movies",
                            h3("Top 3 Highest-Grossing Movies by Genre",align="center"),
                            htmlOutput("sunburst_movies", height = 1000),
                            frow2
                            
                          ),
                          shinydashboard::tabItem(
                            tabName = "word_cloud",
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("wordfreq",
                                            "Select the min frequency of the word",
                                            1,50,40),
                                sliderInput("maxword",
                                            "Select the max number of the words",
                                            1,50,40),
                                checkboxInput("random","Random Order?"),
                                radioButtons("color","Select the word color theme",
                                             c("Accent","Dark"),
                                             selected ="Dark")
                              ),
                              mainPanel(
                                #h2("Popular Genres"),
                               #plotOutput("genre_word_cloud"),
                                h2("Popular Keywords for Movies"),
                                plotOutput("movie_word_cloud")
                                
                              )
                            )
                            
                            
                          ),
                          shinydashboard::tabItem(
                            tabName = "search",
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("genre", "Genre",
                                                   c("Action","Adventure","Musical",
                                                     "Drama", "Comedy","Documentary"
                                                     ,"Horror","Romantic Comedy",
                                                     "Thriller/Suspense","Western"),
                                                   selected="Adventure"),
                                checkboxGroupInput("rating", "MPAA_Rating",
                                                   c("G","PG","PG-13",
                                                     "R", "Not Rated"),
                                                   selected="G"),
                                sliderInput("year",
                                            "Release Year",
                                            1937, 2016,2000),
                                sliderInput("total_gross",
                                            "Total Gross",
                                            0, 1000000000,500000000)
                                
                                
                              ),
                              mainPanel(
                                h2("Movies based on your choices"),
                                dataTableOutput("movies_table")
                              )
                            )
                            
                          )
                        ) 
                        
)

ui <- dashboardPage(title = 'Best movies by Disney Productions', 
                    header, sidebar, body, skin='red')

shinyApp(ui = ui, server = server)

