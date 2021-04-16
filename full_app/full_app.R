library(shiny)
library(tidyverse)
library(teamcolors)
library(ggimage)
library(ggrepel)
library(magick)
library(grid)
library(ggpmisc)
library(extrafont)
library(lubridate)
loadfonts(device = "win", quiet = TRUE)

### GLOBAL VARIABLES

lee_logos <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/logos.csv") %>%
    select(team, team_logo)
nfl_colors_df <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/teamcolors.csv") %>%
    select(team, color)


nfl_df <-  lee_logos %>% 
    left_join(., nfl_colors_df, by = c("team")) 

logo_grobs <- image_read(nfl_df$team_logo)
# create grob for each logo
nfl_grobs <- list()
for (i in 1:nrow(nfl_df)) {
    nfl_grobs[[i]] <- rasterGrob(image = logo_grobs[i])
}
nfl_df$logo_grob <- nfl_grobs



ui <- fillPage(
    
    # avoid greying out plot while recalculating
    tags$style(type="text/css",
               ".recalculating {opacity: 1.0;}"
    ),
    
    # Application title
    titlePanel("NFL Draft Sentiment"),
    plotOutput("fullMaPlot", height = "90%")

    )

        



server <- function(input, output, session) {
    
    # reactive vals
    obj <- reactiveValues()
    obj$n <- 0
    obj$last_exec <- Sys.time()
    
    observe({
        # redo every 5 seconds
        invalidateLater(5000)
        if (Sys.time() - obj$last_exec >= 5){
            # load in reactive data
            comment_data <- as_tibble(isolate(fileReaderData())) 
            
            ### X = SHARE OF POSITIVE COMMENTS, Y =  TOTAL COMMENTS
             obj$scatter_table <- left_join(
                dplyr::select(nfl_teams,team),
                filter(comment_data, as.numeric(Sys.time())  <= 3600) %>%
                mutate(sentiment = case_when(
                    sentiment >= 3 ~ "Positive",
                    sentiment == 2 ~ "Neutral",
                    sentiment <= 1 ~ "Negative"
                )) %>%
                group_by(team) %>%
                add_count() %>%
                filter(sentiment != "Neutral") %>%
                group_by(team, n, sentiment) %>%
                summarise(n2 = n()) %>%
                mutate(pos_freq = n2 / sum(n2)) %>%
                filter(sentiment == "Positive") %>%
                dplyr::select(team, n, pos_freq)) %>%
                mutate(
                    n = replace_na(n,0),
                    pos_freq = replace_na(pos_freq,0)
                )
                
            
            
            # only in window
            comment_data <- comment_data %>%
                filter(as.numeric(Sys.time()) - timestamp <= 180)
            
            # get moving average
            var_time <- as.character(5 * obj$n)
            mean_sent <- left_join(
                dplyr::select(nfl_teams,team),
                (comment_data %>%
                group_by(team) %>%
                summarise(sentiment = mean(sentiment)))
            ) %>%
                mutate(sentiment = replace_na(sentiment,2),
                       time = var_time)
            
            # put in tables
            if (obj$n == 0){
                # moving average table
                obj$ma_table <- mean_sent

                # update n
                obj$n <- 1
            } else {
                # moving average table
                obj$ma_table <- rbind(obj$ma_table,mean_sent)
                
                # update n
                obj$n <- obj$n + 1
            }
            obj$last_exec <- Sys.time()
            
            # put in plot table
            curr_time <- max(as.numeric(obj$ma_table$time))
            obj$plot_table <- filter(obj$ma_table, as.numeric(time) >= (obj$n * 5) - 600) %>% mutate(time = as.numeric(time) - curr_time) %>%
                left_join(nfl_teams)
            
        }
        
    })
    
    
    output$fullMaPlot <- renderPlot({
        req(obj$ma_table)
        curr_time <- max(as.numeric(obj$ma_table$time))
        # plot
        p <- obj$ma_table %>% 
            mutate(time = as.numeric(time) - curr_time) %>%
            left_join(nfl_teams) %>%
            ggplot(aes(x=time,y=sentiment,group=team)) +
            geom_line(aes(color=primary),size=1) +
            #geom_image(data=filter(obj$plot_table,time==0),aes(image=logo), hjust=5, asp = 5) +
            #geom_text_repel(data=filter(obj$plot_table,time==0),aes(label=team),direction = "y", hjust = 0) +
            geom_grob(data=filter(obj$plot_table,time==0),aes(label = logo_grob, time, sentiment),vp.height = .5, vp.width = .5) +
            scale_color_identity() +
            scale_y_continuous(limits = c(0,4)) + 
            geom_hline(yintercept = 2, linetype="longdash") +
            #scale_y_continuous(limits = 0,4, breaks = seq(0,4)) +
            facet_wrap(~team, ncol=4) +
            labs(
                title = "Recent rolling average of /r/NFL comment sentiment by division",
                caption = "Senti",
                y = "mean sentiment (prior 2 mins)",
                x = ""
            ) +
            theme(
                legend.position = "none",
                text = element_text(family="Roboto")
            )
        print(p)
    })
    
    
    fileReaderData <- reactiveFileReader(500, session,
                                         "D:/Dropbox/nfl-draft-sentiment/data/comments.csv", read.csv, sep="\t", stringsAsFactors = FALSE, quote="")
}

# Run the application 
shinyApp(ui = ui, server = server)
