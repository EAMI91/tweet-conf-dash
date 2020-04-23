tweetExplorerUI <- function(id, tweet_div_id = "tweetExplorer-tweet", collapsed = FALSE, status = "info") {
  ns <- NS(id)
  fluidRow(
    column(
      12,
      column(
        12,
        class = "col-md-push-9 col-md-3",
        box(
          width = "6 col-md-12",
          status = status,
          solidHeader = TRUE,
          title = "Options",
          collapsible = TRUE,
          collapsed = collapsed,
          selectInput(
            ns('view'),
            'Grupo de tuits',
            c('Popular',
              "Multimedia",
              "Todos")
          ),
          uiOutput(ns('help_text')),
          uiOutput(ns('filters'))
        ),
        column(
          width = 6,
          class = "col-md-12",
          id = tweet_div_id,
          uiOutput(ns("tweet"))
        )
      ),
      box(
        width = "12 col-md-pull-3 col-md-9",
        status = "primary",
        DT::dataTableOutput(ns('tweets')))
    )
  )
}

tweetExplorer <- function(input, output, session, all_tweets, tzone = "America/New_York") {
  ns <- session$ns

  top_hashtags <- reactive({
    all_tweets() %>%
      select(hashtags) %>%
      unnest() %>%
      count(hashtags, sort = TRUE) %>%
      slice(1:25) %>%
      pull(hashtags)
  })

  tweets <- reactive({
    tip_words <- "(TIL|DYK|[Tt]ip|[Ll]earned|[Uu]seful|[Kk]now|[Tt]rick)"
    session_words <- "([Aa]vailable|[Oo]nline|[Ll]ink|[Ss]lide|[Ss]ession)"

    all_tweets <- all_tweets() %>%
      mutate(
        relates_tip = str_detect(text, tip_words),
        relates_session = str_detect(text, session_words),
        relates_github = str_detect(text, "[Gg]it[Hh]ub") | str_detect(urls_expanded_url, 'github.com'),
        hashtags = map(hashtags, tolower)
      )

    x <- switch(
      input$view,
      'Popular' = all_tweets %>%
        arrange(desc(retweet_count + favorite_count),
                -map_int(mentions_screen_name, length)),
      'Multimedia' = all_tweets %>% filter(!is_retweet, !is.na(media_url)),
      all_tweets
    )

    if (input$view %in% c('Todos', 'Popular')) {
      if (length(input$filter_binary)) {
        for (filter_binary in input$filter_binary) {
          x <- switch(
            filter_binary,
            # 'Not Retweet' = filter(x, !is_retweet),
            'RT con comentario' = filter(x, !is_quote),
            'Tiene multimedia' = filter(x, !is.na(media_url)),
            'Tiene Link' = filter(x, !is.na(urls_url)),
            "Retuiteados" = filter(x, retweet_count > 0),
            "Favoritos" = filter(x, favorite_count > 0),
            x
          )
        }
      }
      if (length(input$filter_hashtag)) {
        x <- filter(x, !is.null(hashtags))
        for (filter_hashtag in input$filter_hashtag) {
          x <- filter(x, map_lgl(hashtags, function(h) filter_hashtag %in% h))
        }
      }
    }
    x
  })

  output$help_text <- renderUI({
    req(input$view)
    switch(
      input$view,
      'Popular' = helpText(HTML("&#x1F4AF;"),  "Más populares (retuits + favs) primero"),
      'Multimedia' = helpText(HTML("&#x1F4F8;"),  "Tuits con multimedia"),
      'Todos' = helpText(HTML("&#x1F917;"), "Todos los tuits"),
      NULL
    )
  })

  hashtags_related <- reactive({
    req(input$view %in% c('Todos', 'Popular'))
    if (is.null(input$filter_hashtag) || input$filter_hashtag == '') return(top_hashtags())
    limit_to_tags <- related_hashtags %>%
      filter(tag %in% input$filter_hashtag) %>%
      pull(related) %>%
      unique()
    top_hashtags() %>%
      filter(`Top 10 Hashtags` %in% c(limit_to_tags, input$filter_hashtag)) %>%
      pull(`Top 10 Hashtags`)
  })

  output$filters <- renderUI({
    selected_hashtags <- isolate(input$filter_hashtag)
    selected_binary <- isolate(input$filter_binary)
    if (input$view %in% c('Todos', 'Popular')) {
      tagList(
        checkboxGroupInput(ns('filter_binary'), 'Tweet Filters',
                           choices = c("Sin citas", "Tiene multimedia", "Tiene link", "Retuits", "Favoritos"),
                           selected = selected_binary,
                           inline = TRUE),
        selectizeInput(ns('filter_hashtag'), 'Hashtags', choices = c("", hashtags_related()), selected = selected_hashtags,
                       multiple = TRUE, options = list(plugins = list('remove_button')), width = "100%")
      )
    }
  })

  output$tweets <- DT::renderDataTable({
    tweets() %>%
      select(created_at, screen_name, text, retweet_count, favorite_count, mentions_screen_name) %>%
      mutate(created_at = strftime(created_at, '%F %T', tz = tzone),
             mentions_screen_name = map_chr(mentions_screen_name, paste, collapse = ', '),
             mentions_screen_name = ifelse(mentions_screen_name == 'NA', '', mentions_screen_name))
  },
  selection = 'single',
  style = "bootstrap",
  rownames = FALSE,
  colnames = c("Fecha", "Usuario", "Tuit", "RT", "Fav", "Menciones"),
  filter = 'top',
  options = list(lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5)
  )

  output$tweet <- renderUI({
    req(tweets())
    x <- if (!is.null(input$tweets_rows_selected)) {
      tweets() %>%
        slice(input$tweets_rows_selected) %>%
        pmap_chr(get_tweet_blockquote) %>%
        HTML()
    } else {
      HTML('<blockquote>Escoja un tuit de la tabla...</blockquote>')
    }

    tags$div(class = "tweet-item", x)
  })
}
