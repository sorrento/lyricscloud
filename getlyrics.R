# NOTE: you can be banned if get more than 300 songs a day

# LIBRARIES -------------------------------------------------------------------------------------------------------

  library(data.table)
  library(dplyr)
  library(rvest) 

# FUNCTIONS ------------------------------------------------------------------------------------------------------------

  get.cloud <- function(i){
    require(wordcloud)
    # i <- 6
    set.seed(1234)
    bandas <- best$banda %>% unique
    d <- best[banda == bandas[i]][n>2]
    
    wordcloud(
      words        = d$word,
      freq         = (d$tf_idf),
      min.freq     = 1,
      max.words    = 100,
      random.order = FALSE,
      rot.per      = 0.35,
      colors       = brewer.pal(8, "Dark2"),
      
    )
    
  }
  
  get.song.urls <- function(url.artist){
    # h <- html(url.artist)
    h <- xml2::read_html(url.artist, options=c('RECOVER'))
    banda <- h %>% html_nodes('h1') %>% html_text() %>% stringr::str_remove(' lyrics')
    
    song.nodes <- h %>% html_nodes("table") %>% .[6] %>% html_nodes('td') %>% html_nodes('a') 
    dt <- data.table(link = song.nodes %>% html_attr('href'),
                     name = song.nodes %>% html_text())[order(name)][,.SD[1], name]
    dt$banda <- banda
    
    return(dt)
  }
  
  get.lyric <- function(url){
    texto <- NA
    try(
      texto <- xml2::read_html(paste0('http://www.mldb.org/', url)) %>% html_node('p.songtext') %>% html_text()
    )
    
    return(texto)
  }
  
  process.banda <- function(url.artist){
    dt.urls <- get.song.urls(url.artist)
    try(
      dt.urls[, letra := lapply(link, get.lyric)]
    )
    banda <- dt.urls[1]$banda
    saveRDS(dt.urls, paste0('lyrics/', banda, '.RDS'))
    
    return(dt.urls)
  }

# GET LYRICS ----------------------------------------------------------------------------------------------------------

  process.banda('http://www.mldb.org/artist-25-metallica.html')
  process.banda('http://www.mldb.org/artist-39-the-beatles.html')
  a <- process.banda('http://www.mldb.org/artist-1498-queen.html')
  a <- process.banda('http://www.mldb.org/artist-600-pearl-jam.html')
  
  # a <- process.banda('http://www.mldb.org/artist-3370-soundgarden.html')
  # a <- process.banda('http://www.mldb.org/artist-40-pink-floyd.html')
  # a <- process.banda('http://www.mldb.org/artist-54-madonna.html')
  # a <- process.banda('http://www.mldb.org/artist-746-radiohead.html')

# PROCESS LYRICS WITH TF_IDF ------------------------------------------------------------------------------------------------
  
  library(tidytext)
  
  archivos <- list.files('lyrics/')
  dt <- NULL
  for(a in archivos){
    dt <- rbind(dt, readRDS(paste0('lyrics/', a)))
  }
  
  
  dt[, texto := unlist(letra)]
  dt$letra <- NULL


  # removing duplicated songs:
    dt[, i := 1:.N]
    # canciones repetidas con nombres levemente distintas
    borrar <- c(118,122, 127,143,157,163,166,167,174,186,192,203,207,208,224,225,232,240,265,305,372,464,821,834,840,
                851,890,907,910)
    dt <- dt[!(i %in% borrar)]
    saveRDS(dt,'lyrics_sin_reps_5_bandas.RDS')

  # Stopwords: specially band members
  stops <- c('NA', 'freddie', 'mercury', 'hetfield','brian', '<NA>', 'chris', 'cornell', 'ulrich', 'x2', 'x3','x4', 'recorded',
             'hammett', 'instrumental', 'solo', 'alt', 'chorus', 'original', 'alt', 'written', 'lyrics', 'kim', 'thayil', 'roger',
             'taylor', 'written', 're', 've','s', 't', '2x', 'originally', 'polly', "i’ve", "you're", 'don', "you’re", 
             'cameron', 'shepherd', 'ron', 'll', 'music', 'motorhead', 'x5', 'x7', 'x8', 'x6', 'deacon', 'john', 'queen', 
             'george', '3x', 'cos','wooh','haa','ha','ooh', '1979')

  # TF IDF
    buf <- dt %>% unnest_tokens(word, texto, ) %>% as.data.table
    buf <- buf[,.N, .(banda, name, word)] # we count only once each word in a song
    buf <- buf[!(word %in% stops)][!is.na(word)]
    book_words <- buf[, .(n = .N), .(word, banda)]
    book_words[, total := .N, banda]

    book_words <- book_words %>%
      bind_tf_idf(word, banda, n) %>% as.data.table %>% setorder(banda, -tf_idf)
    
  # filter the top 200 words by band
  (best <- book_words[, .SD[1:200], banda])

# WORDCLOUD -------------------------------------------------------------------------------------------------------
  
  # save to a file
  for(i in 1:6){
    filename <- paste0('images/', stringr::str_pad(i, 2, pad = "0"))
    png(paste0(filename, ".png"),
        width = 500, height = 500, res = 70)
    get.cloud(i)
    # print(g)
    dev.off()
  }