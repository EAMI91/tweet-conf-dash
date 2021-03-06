pager2 <- function (id, n_items, page_break = 20)
{
  shiny::callModule(pagerModule2, id, page_break = page_break,
                    n_items = n_items)
}

pagerModule2 <- function (input, output, session, page_break = 20, n_items)
{
  ns <- session$ns
  if (!inherits(n_items, "reactive"))
    n_items <- reactiveVal(n_items)
  if (!inherits(page_break, "reactive"))
    page_break <- reactiveVal(page_break)
  pages <- rep(0L, 999)
  s_page <- reactiveVal(1L)
  n_pages <- reactive({
    ceiling(n_items()/page_break())
  })
  at_first <- reactive({
    s_page() == 1
  })
  at_last <- reactive({
    s_page() == n_pages()
  })
  observe({
    pgs <- sort(names(input))
    if (!length(pgs))
      return(NULL)
    pgs <- pgs[grepl("\\d", pgs)]
    pg <- purrr::map_int(pgs, ~input[[.]] %||% 0L)
    s_pg <- which(pages[1:length(pg)] != pg)
    if (!length(s_pg))
      return(NULL)
    s_page(s_pg)
    pages[s_pg] <- pg[s_pg]
  })
  observe({
    req(n_items() > page_break())
    req(s_page())
    shinyjs::toggleClass("page_extra_prev_li", class = "disabled",
                         condition = at_first())
    shinyjs::toggleState("page_extra_prev", condition = !at_first())
  })
  observe({
    req(n_items() > page_break())
    req(s_page())
    shinyjs::toggleClass("page_extra_next_li", class = "disabled",
                         condition = at_last())
    shinyjs::toggleState("page_extra_next", condition = !at_last())
  })
  output$pager_buttons <- renderUI({
    req(n_items() > page_break())
    req(s_page())
    page_start <- if (s_page() <= 3)
      1L
    else min(n_pages() - 4L, s_page() - 2L)
    page_end <- if (s_page() <= 3)
      min(5L, n_pages())
    else min(n_pages(), s_page() + 2L)
    bttns <- purrr::map(page_start:page_end, pager_button2,
                        ns = ns, s_page = s_page())
    prev <- pager_button_prev_next2("prev", ns, at_first())
    nxt <- pager_button_prev_next2("next", ns, at_last())
    tags$nav(`aria-label` = "Pagination Navigation", tags$ul(class = "pagination",
                                                             prev, if (page_start > 1L)
                                                               pager_button_dots2(), bttns, if (page_end < n_pages())
                                                                 pager_button_dots2(), nxt))
  })
  observeEvent(input$page_prev, {
    if (at_first())
      return()
    s_page(s_page() - 1L)
  })
  observeEvent(input$page_next, {
    if (at_last())
      return()
    s_page(s_page() + 1L)
  })
  observeEvent(input$page_extra_prev, {
    if (at_first())
      return()
    s_page(s_page() - 1L)
  })
  observeEvent(input$page_extra_next, {
    if (at_last())
      return()
    s_page(s_page() + 1L)
  })
  page_indices <- function(s_page, n_items, page_break) {
    idx_pg <- 1:page_break + (s_page - 1) * page_break
    idx_pg[idx_pg <= n_items]
  }
  return(reactive({
    page_indices(s_page(), n_items(), page_break())
  }))
}
pager_button_dots2 <- function ()
{
  tags$li(class = "page-item disabled", tags$a(href = "#",
                                               class = "page-link", style = "padding: 6px 8px;", HTML("&#x22ef;")))
}
pager_button2 <- function (n, ns, s_page = 1)
{
  tags$li(class = paste("page-item", if (n == s_page)
    "active"), actionLink(ns(sprintf("page_%03d", n)), n,
                          class = "page-link"))
}
pager_button_prev_next2 <- function (goto = c("prev", "next"), ns, disabled = FALSE, id_suffix = NULL,
                                    pagination = TRUE)
{
  goto <- match.arg(goto)
  input_id <- if (goto == "prev")
    "page_prev"
  else "page_next"
  input_id <- paste0(input_id, id_suffix)
  input_label <- if (goto == "prev")
    "Anterior"
  else "Siguiente"
  tags$li(class = paste(if (pagination)
    "page-item", if (disabled)
      "disabled"), actionLink(ns(input_id), input_label, class = "page-link"))
}
