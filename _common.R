library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(fontawesome)
library(glue)
library(markdown)
library(xml2)
library(rvest)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

# ------------------------------------------------------------
# Google Scholar information
# ------------------------------------------------------------

scholar_user <- "1WwDhp0AAAAJ"

gscholar_stats <- function(url = "https://scholar.google.com/citations?user=1WwDhp0AAAAJ&hl=en") {
  cites <- get_cites(url)
  return(glue(
    "Citations: {cites$citations} | h-index: {cites$hindex} | i10-index: {cites$i10index}"
  ))
}

get_cites <- function(url) {
  html <- xml2::read_html(url)
  node <- rvest::html_nodes(html, xpath = '//*[@id="gsc_rsb_st"]')
  cites_df <- rvest::html_table(node)[[1]]
  cites <- data.frame(t(as.data.frame(cites_df)[, 2]))
  names(cites) <- c("citations", "hindex", "i10index")
  return(cites)
}

# ------------------------------------------------------------
# Publications and research outputs
# ------------------------------------------------------------
# Option 1:
# If you want to manage publications from a CSV, create:
# content/publications.csv
#
# Suggested columns:
# category, author, year, title, journal, number, doi,
# url_pub, url_pdf, url_repo, url_other, other_label, id_scholar, summary
#
# Option 2:
# If the CSV does not exist, the function below uses the small built-in list.

get_pubs <- function(path = "content/publications.csv") {
  
  if (file.exists(path)) {
    pubs <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    pubs <- tibble::tribble(
      ~category, ~author, ~year, ~title, ~journal, ~number, ~doi, ~url_pub, ~url_pdf, ~url_repo, ~url_other, ~other_label, ~id_scholar, ~summary,
      
      "peer_reviewed",
      "Muriqi, D., Bayham, J., Goemans, C., Manning, D. T., and Suter, J. F.",
      2025,
      "Inland flooding in the United States: a review of research related to risk management, economic behavior, and data resources",
      "Environmental Research Letters",
      "20(6), 063003",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      FALSE,
      
      "working_paper",
      "Chakraborty, J., Bayham, J., Goemans, C., Manning, D., Muriqi, D., and Suter, J.",
      2025,
      "The Economic Impact of Inland Flooding in the United States: A Disaggregated Analysis",
      NA_character_,
      "Working paper",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      FALSE,
      
      "thesis",
      "Muriqi, D.",
      2025,
      "The Economic Consequences of Flood Alerts: Evidence from Employment Outcomes",
      "Colorado State University",
      "Master's thesis",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      FALSE,
      
      "thesis",
      "Muriqi, D.",
      2022,
      "A Cost-Benefit Analysis of Four Leakage Reduction Methods in Peja, Kosovo: A Replicated Study",
      "Colorado College",
      "Undergraduate thesis",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      FALSE
    )
  }
  
  pubs <- ensure_pub_columns(pubs)
  pubs <- make_citations(pubs)
  pubs$summary <- ifelse(is.na(pubs$summary), FALSE, pubs$summary)
  pubs$stub <- make_stubs(pubs)
  pubs$url_summary <- file.path("research", pubs$stub, "index.html")
  
  pubs$url_scholar <- ifelse(
    is.na(pubs$id_scholar),
    NA,
    glue(
      "https://scholar.google.com/citations?view_op=view_citation&hl=en&user={scholar_user}&citation_for_view={scholar_user}:{pubs$id_scholar}"
    )
  )
  
  return(pubs)
}

ensure_pub_columns <- function(pubs) {
  needed_cols <- c(
    "category", "author", "year", "title", "journal", "number", "doi",
    "url_pub", "url_pdf", "url_repo", "url_other", "other_label",
    "id_scholar", "summary"
  )
  
  for (col in needed_cols) {
    if (!col %in% names(pubs)) {
      pubs[[col]] <- NA
    }
  }
  
  return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

make_citation <- function(pub) {
  if (!is.na(pub$journal)) {
    pub$journal <- glue("_{pub$journal}_.")
  }
  
  if (!is.na(pub$number)) {
    pub$number <- glue("{pub$number}.")
  }
  
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }
  
  pub$year <- glue("({pub$year})")
  pub$title <- glue('"{pub$title}"')
  pub[, which(is.na(pub))] <- ""
  
  return(paste(
    pub$author,
    pub$year,
    pub$title,
    pub$journal,
    pub$number,
    pub$doi
  ))
}

make_doi <- function(doi) {
  return(glue("DOI: [{doi}](https://doi.org/{doi})"))
}

make_stubs <- function(pubs) {
  title <- str_to_lower(pubs$title)
  title <- str_replace_all(title, ":", "")
  title <- str_replace_all(title, "`", "")
  title <- str_replace_all(title, "'", "")
  title <- str_replace_all(title, "\\.", "")
  title <- str_replace_all(title, "&", "")
  title <- str_replace_all(title, ",", "")
  title <- str_replace_all(title, "  ", "-")
  title <- str_replace_all(title, " ", "-")
  title <- str_sub(title, 1, 60)
  
  return(paste0(pubs$year, "-", title))
}

# ------------------------------------------------------------
# Publication list formatting
# ------------------------------------------------------------

make_pub_list <- function(pubs, category) {
  x <- pubs[which(pubs$category == category), ]
  
  if (nrow(x) == 0) {
    return(htmltools::HTML(""))
  }
  
  pub_list <- list()
  
  for (i in 1:nrow(x)) {
    pub_list[[i]] <- make_pub(x[i, ], index = i)
  }
  
  return(htmltools::HTML(paste(unlist(pub_list), collapse = "")))
}

make_pub <- function(pub, index = NULL) {
  altmetric <- make_altmetric(pub)
  
  if (is.null(index)) {
    cite <- pub$citation
    icons <- make_icons(pub)
  } else {
    cite <- glue("{index}) {pub$citation}")
    icons <- glue('<ul style="list-style: none;"><li>{make_icons(pub)}</li></ul>')
  }
  
  return(htmltools::HTML(glue(
    '<div class="pub">
      <div class="grid">
        <div class="g-col-11"> {markdown_to_html(cite)} </div>
        <div class="g-col-1"> {altmetric} </div>
      </div>
      {icons}
    </div>'
  )))
}

make_altmetric <- function(pub) {
  altmetric <- ""
  
  if (pub$category == "peer_reviewed" && !is.na(pub$doi) && pub$doi != "") {
    altmetric <- glue(
      '<div data-badge-type="donut" data-doi="{pub$doi}" data-hide-no-mentions="true" class="altmetric-embed"></div>'
    )
  }
  
  return(altmetric)
}

markdown_to_html <- function(text) {
  if (is.null(text)) {
    return(text)
  }
  
  return(HTML(markdown::renderMarkdown(text = text)))
}

make_icons <- function(pub) {
  html <- c()
  
  if (!is.na(pub$url_pub)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "View",
      url  = pub$url_pub
    )))
  }
  
  if (!is.na(pub$url_pdf)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-file-pdf",
      text = "PDF",
      url  = pub$url_pdf
    )))
  }
  
  if (!is.na(pub$url_repo)) {
    html <- c(html, as.character(icon_link(
      icon = "fab fa-github",
      text = "Code & Data",
      url  = pub$url_repo
    )))
  }
  
  if (!is.na(pub$url_other)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = pub$other_label,
      url  = pub$url_other
    )))
  }
  
  if (!is.na(pub$url_scholar)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-google-scholar",
      text = "Scholar",
      url  = pub$url_scholar
    )))
  }
  
  return(paste(html, collapse = ""))
}

# ------------------------------------------------------------
# Icon helpers
# ------------------------------------------------------------

icon_link <- function(
    icon = NULL,
    text = NULL,
    url = NULL,
    class = "icon-link",
    target = "_blank"
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  
  return(htmltools::a(
    href = url,
    text,
    class = class,
    target = target,
    rel = "noopener"
  ))
}

make_icon_text <- function(icon, text) {
  return(HTML(paste0(make_icon(icon), " ", text)))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

# ------------------------------------------------------------
# Utility functions
# ------------------------------------------------------------

last_updated <- function() {
  return(span(
    paste0(
      "Last updated on ",
      format(Sys.Date(), format = "%B %d, %Y")
    ),
    style = "font-size:0.8rem;"
  ))
}