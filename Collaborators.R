# {
#   # load packages
#   suppressPackageStartupMessages(library(dplyr))
#   suppressPackageStartupMessages(library(spotifyr))
# 
#   # Set up environment
#   client_ID <- "bc0b388b3801497f8162615befb50a43"
#   client_secret <- "512e20aa79ff4a228cc4e95ab46a45fd"
# 
#   Sys.setenv(SPOTIFY_CLIENT_ID = client_ID)
#   Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
# 
#   access_token <- get_spotify_access_token()
# }


get_artists_collaborators <- function(spotify_artist_id) {
  # related artists nodes function
  get_Nodes <- function(artist_id) {
    # get artists related to main artist
    related_artists <- get_related_artists(
      id = artist_id,
      include_meta_info = TRUE
    )

    # get other artists that are related to the
    # artists that are related to the main artist
    other_related <- c()
    for (i in 1:nrow(related_artists$artists)) {
      result <- get_related_artists(
        id = related_artists$artists[["id"]][i],
        include_meta_info = TRUE
      )
      other_related <- append(other_related, result)
    }

    images <- c()
    for (i in other_related) { # this loops through the list
      for (k in 1:nrow(i)) { # this loops through each table in list
        image_urls <- i$images[[k]]$url[2] # the third image is collected per row in each table
        images <- append(images, image_urls)
      }
    }

    genre <- c()
    for (i in (other_related)) { # this loops through each list
      for (j in 1:nrow(i)) { # this loops through each table in list
        result <- i$genres[[j]][2] # this collects the 2nd item in the vector of genres
        genre <- append(genre, result)
      }
    }


    nodes <- data.frame(
      name = tolower(c(
        other_related[[1]]$name,
        other_related[[2]]$name,
        other_related[[3]]$name,
        other_related[[4]]$name,
        other_related[[5]]$name,
        other_related[[6]]$name,
        other_related[[7]]$name,
        other_related[[8]]$name,
        other_related[[9]]$name,
        other_related[[10]]$name,
        other_related[[11]]$name,
        other_related[[12]]$name,
        other_related[[13]]$name,
        other_related[[14]]$name,
        other_related[[15]]$name,
        other_related[[16]]$name,
        other_related[[17]]$name,
        other_related[[18]]$name,
        other_related[[19]]$name,
        other_related[[20]]$name
      )),
      id = c(c(
        other_related[[1]]$id,
        other_related[[2]]$id,
        other_related[[3]]$id,
        other_related[[4]]$id,
        other_related[[5]]$id,
        other_related[[6]]$id,
        other_related[[7]]$id,
        other_related[[8]]$id,
        other_related[[9]]$id,
        other_related[[10]]$id,
        other_related[[11]]$id,
        other_related[[12]]$id,
        other_related[[13]]$id,
        other_related[[14]]$id,
        other_related[[15]]$id,
        other_related[[16]]$id,
        other_related[[17]]$id,
        other_related[[18]]$id,
        other_related[[19]]$id,
        other_related[[20]]$id
      )),
      popularity = c(c(
        other_related[[1]]$popularity,
        other_related[[2]]$popularity,
        other_related[[3]]$popularity,
        other_related[[4]]$popularity,
        other_related[[5]]$popularity,
        other_related[[6]]$popularity,
        other_related[[7]]$popularity,
        other_related[[8]]$popularity,
        other_related[[9]]$popularity,
        other_related[[10]]$popularity,
        other_related[[11]]$popularity,
        other_related[[12]]$popularity,
        other_related[[13]]$popularity,
        other_related[[14]]$popularity,
        other_related[[15]]$popularity,
        other_related[[16]]$popularity,
        other_related[[17]]$popularity,
        other_related[[18]]$popularity,
        other_related[[19]]$popularity,
        other_related[[20]]$popularity
      )),
      followers = c(c(
        other_related[[1]]$followers.total,
        other_related[[2]]$followers.total,
        other_related[[3]]$followers.total,
        other_related[[4]]$followers.total,
        other_related[[5]]$followers.total,
        other_related[[6]]$followers.total,
        other_related[[7]]$followers.total,
        other_related[[8]]$followers.total,
        other_related[[9]]$followers.total,
        other_related[[10]]$followers.total,
        other_related[[11]]$followers.total,
        other_related[[12]]$followers.total,
        other_related[[13]]$followers.total,
        other_related[[14]]$followers.total,
        other_related[[15]]$followers.total,
        other_related[[16]]$followers.total,
        other_related[[17]]$followers.total,
        other_related[[18]]$followers.total,
        other_related[[19]]$followers.total,
        other_related[[20]]$followers.total
      )),
      profile = c(c(
        other_related[[1]]$external_urls.spotify,
        other_related[[2]]$external_urls.spotify,
        other_related[[3]]$external_urls.spotify,
        other_related[[4]]$external_urls.spotify,
        other_related[[5]]$external_urls.spotify,
        other_related[[6]]$external_urls.spotify,
        other_related[[7]]$external_urls.spotify,
        other_related[[8]]$external_urls.spotify,
        other_related[[9]]$external_urls.spotify,
        other_related[[10]]$external_urls.spotify,
        other_related[[11]]$external_urls.spotify,
        other_related[[12]]$external_urls.spotify,
        other_related[[13]]$external_urls.spotify,
        other_related[[14]]$external_urls.spotify,
        other_related[[15]]$external_urls.spotify,
        other_related[[16]]$external_urls.spotify,
        other_related[[17]]$external_urls.spotify,
        other_related[[18]]$external_urls.spotify,
        other_related[[19]]$external_urls.spotify,
        other_related[[20]]$external_urls.spotify
      )),
      images = images,
      genre = genre
    )

    ## Remove duplicate nodes and labels in data frame

    nodes_df <- distinct(nodes, name, id, popularity, profile,
      images, genre, followers,
      .keep_all = T
    )


    return(nodes_df)
  }

  # get related artists nodes
  related_artists <- get_Nodes(artist_id = spotify_artist_id)

  # get related artists data
  artist_related_artists <- function(related_artist) {
    related_artists_data <- list()

    for (i in 1:nrow(related_artist)) {
      # Get the artist ID from the second column of related_artists
      artist_id <- related_artist[[2]][i]

      # Retrieve the artist's albums using the artist ID
      result <- get_artist_albums(artist_id, limit = 50)

      # Create a data frame from the result
      related_artists_albums <- data.frame(result)

      # Add the data frame to the list
      related_artists_data[[i]] <- related_artists_albums
    }

    return(related_artists_data)
  }

  related_artists_data <- artist_related_artists(related_artist = related_artists)

  # get the artists collaborators
  get_collaborators <- function(data, artist_name) {
    artists <- c() # initialize empty vector
    # outer loop loops through the length of artists list
    for (i in 1:length(data$artists)) {
      # inner loop loops through the length of individual
      # "name" column in artists list
      for (j in 1:length(data$artists[[i]][3][, ])) {
        # scrapes the artist names
        result <- data$artists[[i]][3][j, ]
        # appends the names to "artists" vector
        artists <- append(artists, result)
      }
    }

    artists <- unique(artists) # removes duplicate names
    artists <- tolower(artists) # turns to lowercase
    # turns the search artist's name to NA
    artists <- gsub(tolower(artist_name), NA, artists)
    artists <- na.omit(artists) # remove NA from vector

    return(artists)
  }

  # function that gets the collaborators data
  collab_df <- function(related_artists_data, artist_data) {
    collaborators <- c()
    artists_list <- c()
    for (i in 1:length(related_artists_data)) {
      result <- get_collaborators(related_artists_data[[i]],
        artist_name = artist_data[[1]][i]
      )

      collaborators <- c(collaborators, result)
      artists_list <- c(artists_list, rep(artist_data[[1]][i], times = length(result)))
    }

    artists_collaborators <- data.frame(artists = artists_list, collaborators = collaborators)

    return(artists_collaborators)
  }

  # application of the function
  collabs <- collab_df(
    related_artists_data = related_artists_data,
    artist_data = related_artists
  )

  # get attribute data for each collaborator
  attribute_data <- list()
  for (i in 1:nrow(collabs)) {
    attribute_data[[i]] <- search_spotify(collabs$collaborators[[i]],
      type = "artist",
      include_meta_info = T
    )
  }

  # collect attributes of collaborators
  {
    name <- c()
    id <- c()
    popularity <- c()
    followers <- c()
    profile <- c()
    images <- c()
    genre <- c()

    for (i in 1:length(attribute_data)) {
      name <- c(name, attribute_data[[i]][[1]][[2]][5][[1]][1])
      id <- c(id, attribute_data[[i]][[1]][[2]][3][[1]][1])
      popularity <- c(popularity, attribute_data[[i]][[1]][[2]][6][[1]][1])
      followers <- c(followers, attribute_data[[i]][[1]][[2]][11][[1]][1])
      profile <- c(profile, attribute_data[[i]][[1]][[2]][9][[1]][1])
      images <- c(images, attribute_data[[i]][[1]][[2]][4][[1]][1])
      genre <- c(genre, attribute_data[[i]][[1]][[2]][1][[1]][1])
    }
  }

  # loop through images list and store converted
  # data frames in a list
  images_df_list <- list()
  for (i in 1:length(images)) {
    images_df_list[[i]] <- list2DF(images[[i]])
  }

  # loop through the list of data frames & extract
  # the image urls
  images_vec <- c()
  for (i in 1:length(images_df_list)) {
    images_vec <- c(images_vec, images_df_list[[i]]$url[[1]][1])
  }

  len_diff_img <- name |>
    length() - images_vec |>
    length()

  # add a repetition of the last 6 urls to the vector
  # so that its length is equal to the length of other
  # attribute vectors
  images_vec <- c(
    images_vec,
    rep(images_vec[tail(length(images_vec))], times = len_diff_img)
  )

  # get genre data
  genre_vec <- c()
  for (i in 1:length(genre)) {
    genre_vec <- c(genre_vec, genre[[i]][1])
  }

  music_genres <- c()
  for (m in 1:length(genre_vec)) {
    music_genres <- c(music_genres, genre_vec[[m]])
  }

  len_diff_gnr <- name |>
    length() - music_genres |>
    length()

  music_genres <- c(
    music_genres,
    rep(music_genres[tail(length(music_genres))], times = len_diff_gnr)
  )

  # create collaborators data frame
  collaborators_df <- data.frame(
    name = name,
    id = id,
    popularity = popularity,
    followers = followers,
    profile = profile,
    images = images_vec,
    genre = music_genres
  )

  # filter out 2Pac
  collaborators_df <- collaborators_df |>
    filter(name != "2Pac")

  # rename columns in collabs
  colnames(collabs) <- c("Vertex1", "Vertex2")

  # grab Vertex1 attributes
  popularity <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      related_artists,
      related_artists$name == collabs$Vertex1[[i]][1]
    )[[3]]
    popularity <- c(popularity, result)
  }

  followers <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      related_artists,
      related_artists$name == collabs$Vertex1[[i]][1]
    )[[4]]
    followers <- c(followers, result)
  }

  profile <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      related_artists,
      related_artists$name == collabs$Vertex1[[i]][1]
    )[[5]]
    profile <- c(profile, result)
  }

  images <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      related_artists,
      related_artists$name == collabs$Vertex1[[i]][1]
    )[[6]]
    images <- c(images, result)
  }

  genre <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      related_artists,
      related_artists$name == collabs$Vertex1[[i]][1]
    )[[7]]
    genre <- c(genre, result)
  }

  # convert "names" in collaborators_df to lowercase
  collaborators_df$name <- tolower(collaborators_df$name)

  # filter out "various artists" from collabs
  collabs <- collabs |>
    filter(Vertex2 != "various artists")

  # check if name in Vertex2 is an English character
  ascii_check <- c()
  for (i in 1:nrow(collabs)) {
    ascii_check <- c(ascii_check, collabs$Vertex2[[i]][1] |> stringi::stri_enc_isascii())
  }

  # append check result to collabs dataframe
  collabs$ASCII <- ascii_check

  # filter out non-English characters
  collabs <- collabs |>
    filter(ASCII != FALSE)

  # delete ASCII column
  collabs$ASCII <- NULL

  # delete rows from Vertex1 attributes to equal
  # collabs rows
  popularity <- popularity[-c(1 + length(popularity):nrow(collabs))]

  followers <- followers[-c(1 + length(followers):nrow(collabs))]

  profile <- profile[-c(1 + length(profile):nrow(collabs))]

  images <- images[-c(1 + length(images):nrow(collabs))]

  genre <- genre[-c(1 + length(genre):nrow(collabs))]

  # grab Vertex2 attributes
  popularityB <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      collaborators_df,
      collaborators_df$name == collabs$Vertex2[[i]][1]
    )[[3]]
    popularityB <- c(popularityB, result)
  }

  followersB <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      collaborators_df,
      collaborators_df$name == collabs$Vertex2[[i]][1]
    )[[4]]
    followersB <- c(followersB, result)
  }

  profileB <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      collaborators_df,
      collaborators_df$name == collabs$Vertex2[[i]][1]
    )[[5]]
    profileB <- c(profileB, result)
  }

  imagesB <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      collaborators_df,
      collaborators_df$name == collabs$Vertex2[[i]][1]
    )[[6]]
    imagesB <- c(imagesB, result)
  }

  genreB <- c()
  for (i in 1:nrow(collabs)) {
    result <- filter(
      collaborators_df,
      collaborators_df$name == collabs$Vertex2[[i]][1]
    )[[7]]
    genreB <- c(genreB, result)
  }

  # delete rows from Vertex2 attributes to equal
  # collabs rows
  popularityB <- popularityB[-c(1 + length(popularityB):nrow(collabs))]

  followersB <- followersB[-c(1 + length(followersB):nrow(collabs))]

  profileB <- profileB[-c(1 + length(profileB):nrow(collabs))]

  imagesB <- imagesB[-c(1 + length(imagesB):nrow(collabs))]

  genreB <- genreB[-c(1 + length(genreB):nrow(collabs))]

  # create flat file of collaborators
  {
    collabs$`Vertex1 popularity` <- popularity
    collabs$`Vertex1 followers` <- followers
    collabs$`Vertex1 profile` <- profile
    collabs$`Vertex1 images` <- images
    collabs$`Vertex1 genre` <- genre

    collabs$`Vertex2 popularity` <- popularityB
    collabs$`Vertex2 followers` <- followersB
    collabs$`Vertex2 profile` <- profileB
    collabs$`Vertex2 images` <- imagesB
    collabs$`Vertex2 genre` <- genreB
  }



  return(collabs)
  
  
}

# test
# tictoc::tic()
# steve_wonder_collab_network <- get_artists_collaborators(spotify_artist_id = "7guDJrEfX3qb6FEbdPA5qi")
# tictoc::toc()
# 
# 
# steve_wonder_collab_network |> View()
# 
# tictoc::tic()
# billie_eilish_collab_network <- get_artists_collaborators(spotify_artist_id = "6qqNVTkY8uBg9cP3Jd7DAH")
# tictoc::toc()
# 
# billie_eilish_collab_network |> View()
# write.csv(billie_eilish_collab_network,file = "billie_eilish_collab_network.csv")
# 
# tictoc::tic()
# madonna_collab_network <- get_artists_collaborators(spotify_artist_id = "6tbjWDEIzxoDsBA1FuhfPW")
# tictoc::toc()
# 
# madonna_collab_network |> View()
# 
# tictoc::tic()
# diana_ross_collab_network <- get_artists_collaborators(spotify_artist_id = "3MdG05syQeRYPPcClLaUGl")
# tictoc::toc()
# 
