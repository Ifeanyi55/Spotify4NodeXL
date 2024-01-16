library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(spotifyr)
library(SpotifyNetwork)
library(spsComps)
library(searcher)
library(reactable)
library(bslib)
library(dplyr)

# set global options
options(
  shiny.browser = T,
  spinner.color = "#16F529",
  spinner.color.background = "#FFFFFF",
  spinner.size = 2
)

# create theme
my_theme <- bs_theme(
  bg = "#fdfefe",
  fg = "black",
  primary = "red",
  base_font = font_google("Michroma"),
  "font-size-base" = "0.75rem",
  version = 5,
  "navbar-bg" = "#16F529"
)

# define UI for application that gets Spotify network data
ui <- navbarPage(
  title = strong("Spotify Data Importer"), id = "navbar",
  # useShinyalert(),
  windowTitle = "Spotify Data Importer",
  
  # add telemetry javascript elements
  header = use_telemetry(),
  
  footer = h5(strong(tagList(h5(span("Spotify Data Importer for",style="color:green"), a("NodeXL", href = "https://www.nodexl.com/"))))),
  theme = my_theme, collapsible = T, setBackgroundImage(src = "music.jpg"),
  # add marquee visual element
  tags$html(HTML("<marquee direction = right scrollamount = '12' style = 'color:green; font-size:17px;'><strong>Spotify Data Importer For <span style='color:red'>NodeXL</span></strong></marquee>"),
),  
  # Add social media icons to navbar
  tags$style(".navbar-nav.socialmedia { display: flex; justify-content: right; flex-direction:row; padding: 5px; font-size: 20px; }"),
  tags$div(class = "navbar-nav socialmedia",
           tags$a(href = "https://www.github.com/Ifeanyi55", target = "_blank", icon("github", lib = "font-awesome"))),
  
  tabPanel(
    id = "tabOne", value = "oneTab", title = strong("Home"), icon = icon("home"),
    sidebarLayout(
      sidebarPanel(
        id = "side", width = 3, h4(strong("Credentials")), hr(),
        textInput("client_id", strong("Enter Client ID")),
        textInput("client_secret", strong("Enter Client SECRET")), br(),
        actionButton("validate", strong("Authenticate"), icon = icon("caret-right")), br(),hr(),
        textOutput("valout")
      ),
      mainPanel(
        id = "main", width = 8,
        span(style = "color:blue;",h5(strong(p(id = "dateclock")))),br(),
        h2(strong("Welcome to the NodeXL Spotify Data Importer!")),
        p(h5(strong("Before you begin scraping data, you will need to complete the steps below:"))), hr(),
        p(h5(strong(tagList("STEP 1: Go to", a("https://developer.spotify.com/dashboard/", href = "https://developer.spotify.com/dashboard/"), "and login with your credentials")))), br(),
        p(h5(strong("STEP 2: Create an app, and give it a name and a description"))), br(),
        p(h5(strong("STEP 3: Get the generated client ID and client Secret, and return here"))), br(),
        p(h5(strong("STEP 4: Enter the client ID and client Secret in the 'Credentials' box"))), br(),
        p(h5(strong("STEP 5: Click the 'Authenticate' button"))), br(),
        p(h5(strong("STEP 6: Wait for the system to authenticate your credentials and print out a reference ID"))), br(),
        p(h5(strong("Great! You can now proceed to scraping network data via the Spotify API."))),
        actionButton("nextTab", strong("Proceed")), hr()
      )
    )
  ),
  tabPanel(
    id = "tabA", value = "Atab", title = strong("Network Data"), icon = icon("table"),

    # load and run the CSS script
    includeCSS("style.css"),
    # load and run the javascript script
    includeScript("JSCode.js"),
    sidebarLayout(
      sidebarPanel(
        width = 2, id = "sidebar",
        actionButton("info", strong("Info"), icon = icon("info")), br(), br(), br(), tags$a(img(src = "spotify.png"), href = "https://open.spotify.com/"), br(), hr(),
        textInput("id", strong("Enter Artist's Spotify Id")),
        actionButton("run", strong("Related Data"), icon = icon("caret-right")),br(),hr(),br(),
        h5(strong("Collaborators Data")),
        actionButton("fetch", strong("Collab Data"), icon = icon("caret-right")),hr()
      ),
      
      mainPanel(
        # go to top button
        spsGoTop(id = "up",right = "3%",bottom = "10%",icon = icon("arrow-up",color = "green")),
        textInput("search", span(strong("Search Box"),style = "color:white;"), placeholder = "Search Google", width = "150px"),actionButton("search_bttn", strong("Search")),hr(),
        fluidRow(column(12, h3(strong(span(style = "color:white;text-align:center;",h4("Related Artists Network Data")))))),
        fluidRow(
          column(12, withSpinner(reactableOutput("network_data",width = 1000,height = 400), type = 1)),
        ),
        fluidRow(
          column(6, downloadButton("down_csv", strong("Download CSV"), icon = icon("download"))),
          column(6, downloadButton("down_graphml", strong("Download GraphML"), icon = icon("download")))
        ),br(),br(),br(),
        fluidRow(column(12, h3(strong(span(style = "color:white;text-align:center;",h4("Artists Collaboration Network Data")))))),
        fluidRow(
          column(12,withSpinner(reactableOutput("collabs_data", width = 1000, height = 400),type = 1))
        ),
        downloadButton("down_flat",strong("Download CSV"),icon = icon("download")),
        br(), hr(), uiOutput("out")
      )
    )
  ),
  tabPanel(
    id = "tabB", value = "Btab", title = strong("80s Hits"), icon = icon("music"),
    sidebarLayout(sidebarPanel = "", mainPanel(tags$iframe(
      style = "border-radius:12px",
      src = "https://open.spotify.com/embed/playlist/37i9dQZF1DXb57FjYWz00c?utm_source=generator",
      width = "1350px",
      height = "550px",
      frameBorder = "0",
      allowfullscreen = "",
      allow = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading = "lazy"
    )))
  ),
  tabPanel(
      id = "tabD", strong("NodeXL YouTube"), icon = icon("youtube"),
      sidebarLayout(sidebarPanel(id = ""), mainPanel(
        tags$iframe(
          width = "620",
          height = "350",
          src = "https://www.youtube.com/embed/xKhYGRpbwOc",
          title = "YouTube video player",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = T
        ), hr(),
        tags$iframe(
          width = "620",
          height = "350",
          src = "https://www.youtube.com/embed/Gs4NPuKIXdo",
          title = "YouTube video player",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = T
        ), hr(),
        tags$iframe(
          width = "620",
          height = "350",
          src = "https://www.youtube.com/embed/J1W5uqAyHTg",
          title = "YouTube video player",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = T
        ), hr(),
        tags$iframe(
          width = "620",
          height = "350",
          src = "https://www.youtube.com/embed/zEgrruOITHw",
          title = "YouTube video player",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = T
        ), hr(),
        tags$iframe(
          width = "620",
          height = "350",
          src = "https://www.youtube.com/embed/pwsImFyc0lE",
          title = "YouTube video player",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = T
        ), hr(),
        tags$iframe(
          width = "620",
          height = "350",
          src = "https://www.youtube.com/embed/mjAq8eA7uOM",
          title = "YouTube video player",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = T
        )
      ))
    )
  )


# define server logic required to get Spotify's network data
server <- function(input, output, session) {
  
  # set up Spotify API credentials environment
  authentication <- function(id, secret) {
    client_ID <- id
    client_secret <- secret

    # authenticate the spotify client side
    Sys.setenv(SPOTIFY_CLIENT_ID = client_ID)
    Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

    access_token <- get_spotify_access_token()
  }

  client_id <- reactive(as.character(input$client_id))
  client_secret <- reactive(as.character(input$client_secret))
  authenticate <- reactive(authentication(
    client_id(),
    client_secret()
  ))
  validation <- eventReactive(input$validate, {
    authenticate()
  })

  output$valout <- renderText({
    validation()
  })

  # update nav bar page using the value of the tabPanel
  # (i.e. value = "Atab")
  observeEvent(input$nextTab, {
    updateNavbarPage(session, inputId = "navbar", selected = "Atab")
  })

  
  
  # activate the search box
  search_input <- reactive(input$search)

  searchGoogle <- reactive(search_google(search_input(), rlang = F))

  search_result <- eventReactive(input$search_bttn, {
    searchGoogle()
  })

  output$out <- renderUI({
    search_result()
  })

  # render spotify
  # renderUI({
  #   tags$iframe(src = "https://open.spotify.com/")
  # })


  # turn text input into a reactive object
  id_input <- reactive({
    as.character(input$id)
  })

  # wrap SpotifyNetwork functions in reactive wrappers
  # related_network <- reactive({
  #   related_artists_network(id_input())
  # })
  # artist_plot <- reactive({
  #   artists_popularity(id_input())
  # })
  nodes_table <- reactive({
    related_artists_nodes(id_input())
  })
  edges_table <- reactive({
    related_artists_edges(id_input())
  })

  # create event reactive element for each output
  # network_react <- eventReactive(input$run, {
  #   related_network()
  # })
  # artist_react <- eventReactive(input$run, {
  #   artist_plot()
  # })
  nodes_react <- eventReactive(input$run, {
    nodes_table()
  })
  edges_react <- eventReactive(input$run, {
    edges_table()
  })
  
  # function to wrangle nodes & edges data into a NodeXL flat file
  create_flatTabler <- function(dfN,dfE){
    
    
    # scrape data from Vertex1
    # as.vector(data,mode) converts the returned list into a vector
    popularity <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex1"]])[[3]])
    popularity <- as.vector(popularity,"numeric")
    
    
    followers <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex1"]])[[4]])
    followers <- as.vector(followers,"numeric")
    
    
    profile <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex1"]])[[5]])
    profile <- as.vector(profile,"character")
    
    
    images <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex1"]])[[6]])
    images <- as.vector(images,"character")
    
    
    genre <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex1"]])[[7]])
    genre <- as.vector(genre,"character")
    
    
    # scrape data from Vertex2
    # as.vector(data,mode) converts the returned list into a vector
    popularityB <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex2"]])[[3]])
    popularityB <- as.vector(popularityB,"numeric")
    
    
    followersB <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex2"]])[[4]])
    followersB <- as.vector(followersB,"numeric")
    
    
    profileB <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex2"]])[[5]])
    profileB <- as.vector(profileB,"character")
    
    
    imagesB <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex2"]])[[6]])
    imagesB <- as.vector(imagesB,"character")
    
    
    genreB <- apply(dfE,1,function(df) subset(dfN,dfN$name == df[["Vertex2"]])[[7]])
    genreB <- as.vector(genreB,"character")
    
    
    # assign scraped data for Vertex1 to new columns
    dfE$`Vertex1 popularity` <- popularity
    dfE$`Vertex1 followers` <- followers
    dfE$`Vertex1 profile` <- profile
    dfE$`Vertex1 images` <- images
    dfE$`Vertex1 genre` <- genre
    
    # assign scraped data for Vertex2 to new columns
    dfE$`Vertex2 popularity` <- popularityB
    dfE$`Vertex2 followers` <- followersB
    dfE$`Vertex2 profile` <- profileB
    dfE$`Vertex2 images` <- imagesB
    dfE$`Vertex2 genre` <- genreB
    
    dfE$`Edge Weight` <- round(dfE$`Vertex1 popularity`/dfE$`Vertex2 popularity`,2)
    
    dfE <- dfE |> relocate(`Edge Weight`,.after = Vertex2)
    
    return(dfE)
    
    
  }
  
  
  # parse edges_react to function
  flat_file <- reactive({create_flatTabler(nodes_react(),edges_react())})
  
  # add edge metadata
  # flat_file()["Edge Weight"] <- reactive({round(flat_file()$`Vertex1 popularity`/flat_file()$`Vertex2 popularity`,2)})
  # 
  # flat_file() <- flat_file() |>
  # reactive({relocate(`Edge Weight`,.after = Vertex2)})

  # create flat file event reactive object
  flat_react <- eventReactive(input$run,{
    flat_file()
  })
  
  # # render outputs
  # output$network <- renderVisNetwork({
  #   network_react()
  # })
  # 
  # output$plot <- renderPlotly({
  #   artist_react()
  # })

  # function to download nodes data file
  output$down_csv <- downloadHandler(
    filename = function() {
      paste("Related_artists", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(flat_react(), file)
    }
  )

  output$network_data <- renderReactable({
    
        tryCatch(
          {
            reactable(flat_react(),
                      theme = reactableTheme(
                        highlightColor = "#00e600",
                        borderColor = "#00e600",
                        borderWidth = 3
                      ),
                      outlined = T,
                      bordered = T,
                      filterable = T,
                      striped = T,
                      compact = T,
                      highlight = T,
                      defaultColDef = colDef(
                        align = "center",
                        headerStyle = list(background = "#00e600")
                      ),
                      paginationType = "simple"
            )
            
          },
          error = function(e){
            message("There was an error!")
            print(e)
          }
        )
  })
  
  # function to generate GraphML file
  create_graphml <- function(nodes,edges){
    
    # create new graph object
    graph <- graph_from_data_frame(edges)
    
    
    # add attributes to graph nodes
    {
      V(graph)$Name <- nodes$name
      V(graph)$Popularity <- nodes$popularity
      V(graph)$Followers <- nodes$followers
      V(graph)$Profile <- nodes$profile
      V(graph)$Images <- nodes$images
      V(graph)$Genre <- nodes$genre
    }
    
    return(graph)
    
  }
  
  # make function reactive
  graphml_react <- reactive({
    create_graphml(nodes_react(),
                   edges_react())})
  
  # make function event reactive so that it is triggered
  # by run action button
  graphmlReact <- eventReactive(input$run,{
    graphml_react()
  })

  
  # write GraphML download function
  output$down_graphml <- downloadHandler(
    filename = function(){
      paste("Related_artists",".graphml",sep = "")
    },
    content = function(file){
      write_graph(graphmlReact(),file,format = "graphml")
    }
  )
  
  # import get_artists_collaborators() function
  source("Collaborators.R")
  
  # wrap in reactive wrappers
  artists_collaborations <- reactive({get_artists_collaborators(id_input())})
  
  # make event reactive
  collabs_react <- eventReactive(input$fetch,{artists_collaborations()})
  
  output$collabs_data <- renderReactable({
    tryCatch(
      {
        reactable(collabs_react(),
                  theme = reactableTheme(
                    highlightColor = "#3498DA",
                    borderColor = "#3498DA",
                    borderWidth = 3
                  ),
                  outlined = T,
                  bordered = T,
                  filterable = T,
                  striped = T,
                  compact = T,
                  highlight = T,
                  defaultColDef = colDef(
                    align = "center",
                    headerStyle = list(background = "#3498DA")
                  ),
                  paginationType = "simple"
        )
      },
      error = function(e){
        message("There was an error!")
        print(e)
      }
    )
  })
  
  # activate download button
  output$down_flat <- downloadHandler(
    filename = function(){
      paste("CollabData",".csv",sep = "")
    },
    content = function(file){
      write.csv(collabs_react(),file)
    }
  )

  # function to download edges data file
  # output$down_edges <- downloadHandler(
  #   filename = function() {
  #     paste("Edges", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(edges_react(), file)
  #   }
  # )


  # output$edges <- renderReactable({
  #   reactable(edges_react(),
  #     theme = reactableTheme(
  #       highlightColor = "#00FFAB",
  #       borderColor = "#00FFAB"
  #     ),
  #     outlined = T,
  #     bordered = T,
  #     filterable = T,
  #     striped = T,
  #     compact = T,
  #     highlight = T,
  #     defaultColDef = colDef(
  #       align = "center",
  #       headerStyle = list(background = "#00FFAB")
  #     ),
  #     paginationType = "simple"
  #   )
  # })

  observeEvent(input$info, {
    shinyalert(
      title = "About Software", closeOnEsc = T, confirmButtonCol = "#006400",
      imageUrl = "spotify.png",
      closeOnClickOutside = T,
      confirmButtonText = "Got It", showConfirmButton = T,
      animation = "pop", timer = 20000,
      text = "The Spotify Data Importer allows a user to
              query the Spotify API and get network data 
              of artists that are related to a particular artist. You can also get data of artists that have collaborated together. 
      
              It typically takes between 16 and 20 
              seconds to get a response from the Spotify server for related
              artists network data and more than that for artists collaboration
              network data."
  )
  })
  
  
    observeEvent(input$fetch,{
      shinyalert(
        closeOnEsc = T,
        confirmButtonText = "Got It!",
        confirmButtonCol = "#3498DA",
        closeOnClickOutside = T,
        showConfirmButton = T,
        animation = "slide-from-top",
        cancelButtonText = T,
        timer = 10000,
        text = "This may take a while to run, sorry!"
        
      )
    })
    
  
}

  

# Run the application
shinyApp(ui = ui, server = server)
