library(xml2)
library(tidyverse)
library(rvest)
library(purrr)
# megadic
library(stringdist)
# clean_rows
library(httr)
library(stringr)
library(tibble)
library(readr)
library(lubridate)


# URLS creation -----------------------------------------------------------------------
# FUNCTION generate all URLS for all election years

generate_url <- function(years) {
  if (years == 2019) {
    c(
      "https://en.wikipedia.org/wiki/Opinion_polling_for_the_November_2019_Spanish_general_election",
      "https://en.wikipedia.org/wiki/Opinion_polling_for_the_April_2019_Spanish_general_election"
    )
  } else {
    paste0("https://en.wikipedia.org/wiki/Opinion_polling_for_the_", years, "_Spanish_general_election")
  }
}


years <- c(2023, 2019, 2016, 2015,
           2011, 2008, 2004, 2000, 1996, 1993, 1989, 1986, 1982, 1979)

urls <- unlist(map(years, generate_url))



# MEGADICCIONARIO -----------------------------------------------------------------------
# FUNCTION match party names to megadiccionario


# mega_diccionario <- read.csv("mega_diccionario.csv")

match_party_names <- function(wiki_names, reference_names, max_dist = 0.20) {

  # need to do case insensitive matching cause the distance if not is too great
  reference_names_lower <- tolower(reference_names)

  matched_names <- sapply(wiki_names, function(name) {

    name_lower <- tolower(name)
    distances <- stringdist::stringdist(name_lower, reference_names_lower, method = "jw")
    min_dist <- min(distances, na.rm = TRUE) # most similar match!

    if (min_dist < max_dist) {

      # good enough match found => gives me the closest party name
      # but! returns the original correct-case reference name
      reference_names[which.min(distances)]

    } else {

      warning(glue::glue("No good match found for party name: '{name}'"))
      name  # fallback to original if no good match

    }
  })
  return(matched_names)
}

# PARTY NAMES -----------------------------------------------------------------
# FUNCTION get the parties names out of tables depending if the name is a pic or if its text

conditional_extraction <- function(node) {
  has_file_span <- xml_find_all(node, ".//span[contains(@typeof, 'mw:File')]")

  if (length(has_file_span) > 0) { # imagenes
    node %>%
      xml_find_first(".//a") %>%
      xml_attr("title")
  } else { # texto
    node %>%
      xml_text() %>%
      str_trim()
  }
}

extract_party_names <- function(tables) {
  # iterate over each table
  map(tables, function(x) {
    table_node <- x$table

    nodes <- table_node %>%
      xml_find_all(".//th[
                   contains(@style, 'width:35px') or
                   contains(@style, 'width:43px') or
                   contains(@style, 'width:40px')]")

    map_chr(nodes, conditional_extraction)
  })
}

get_all_parties <- function(all_tables) {

  # get party names from each set of table nodes
  all_parties <- extract_party_names(all_tables)

  # return with names as indices since each table gets its own party names
  names(all_parties) <- seq_along(all_parties)

  return(all_parties)

}



# TABLES -----------------------------------------------------------------------
# FUNCTION get all survey TABLES present in the wiki of one electoral year

get_tables_after_voting_estimates <- function(url) { # we need to find all tables after the h4 <h4 id="Voting_intention_estimates"> but before any other h4 or higher

  siblings <- url %>%
    read_html() %>%
    xml_find_first(".//div[h4[@id='Voting_intention_estimates']]") %>% # starting node: has to be the parents => the div that contains <h4 id='Voting_intention_estimates'>
    xml_find_all("following-sibling::*") # get all * following sibling tables after the one we just mentioned (div of h4) (https://www.roborabbit.com/blog/mastering-xpath-using-the-following-sibling-and-preceding-sibling-axes/)

  # manual trimming baby
  boundary <- which(xml_name(siblings)
                    == "div"
                    & str_detect(xml_attr(siblings, "class"), "mw-heading[234]")
  )

  if (length(boundary) > 0) {
    siblings <- siblings[1:(boundary[1] - 1)] # will go until the one before (-1!)
  }


  # GET THE YEARS FOR EACH TABLE OUT OF H5 id + THEIR CORRESPONDING TABLES
  tables_with_years <- list()
  last_h5_id <- NULL
  current_year <- NULL

  for (i in seq_along(siblings)) {
    node <- siblings[[i]]

    # checking if h5 header
    if (xml_name(node) == "div" &&
        str_detect(xml_attr(node, "class"), "mw-heading5")) {

      # extract the year
      h5_id <- node %>%
        xml_find_first(".//h5") %>%
        xml_attr("id")

      last_h5_id <- h5_id  # store for later

      current_year <- case_when(
        str_detect(h5_id, "^\\d{4}$") ~ as.numeric(h5_id), # 4 digits
        str_detect(h5_id, "^\\d{4}_") ~ as.numeric(str_extract(h5_id, "^\\d{4}")), # paranthesis
        str_detect(h5_id, "^\\d{4}–\\d{4}$") ~ as.numeric(str_extract(h5_id, "^\\d{4}")), # year range
        TRUE ~ as.numeric(str_extract(h5_id, "\\d{4}")) # anything else
      )
    }

    # checking table => associate it with current year
    if (xml_name(node) == "table") {

      # if no h5 header was found => extract year from polling_firm
      if (is.null(current_year) || is.na(current_year)) {
        table_data <- tryCatch({
          html_table(node)
        }, error = function(e) NULL)

        if (!is.null(table_data) && nrow(table_data) > 1 && ncol(table_data) > 0) {

          # check first column (polling_firm) in all rows to find a year
          for (row_i in 1:nrow(table_data)) {
            polling_firm_year <- table_data[[row_i, 1]] |>
              str_extract("[0-9]{4}") |>
              as.numeric()

            if (!is.na(polling_firm_year)) {
              current_year <- polling_firm_year
              cat(sprintf("No h5 header found, using year from polling_firm (row %d): %s\n", row_i, current_year))
              break  # found a year, stop looking
            }
          }
        }
      }

      # only add table if we have a valid year
      if (!is.null(current_year) && !is.na(current_year)) {

        tables_with_years[[length(tables_with_years) + 1]] <- list(
          table = node,
          year = current_year,
          id = last_h5_id  # keep full h5 id as label
        )

        if (!is.null(last_h5_id)) {
          cat(sprintf("Found h5 with ID: '%s' -> Year: %s\n", last_h5_id, current_year))
          cat(sprintf("\t Found table under h5\n"))
          last_h5_id <- NULL  # reset so we dont double print
        }

      }
    }
  }

  # named by year
  names(tables_with_years) <- map_chr(tables_with_years, ~ as.character(.x$year))

  return(tables_with_years)
}

# LINKS ----------------------------------------------------------
# FUNCTION get all survey LINKS present in the wiki of one electoral year

get_links_after_voting_estimates <- function(url) {

  siblings <- url %>%
    read_html() %>%
    xml_find_first(".//div[h4[@id='Voting_intention_estimates']]") %>%
    xml_find_all("following-sibling::*")

  # manual trimming baby
  boundary <- which(xml_name(siblings) == "div" &
                      str_detect(xml_attr(siblings, "class"), "mw-heading[234]"))

  if (length(boundary) > 0) {
    siblings <- siblings[1:(boundary[1] - 1)]
  }

  links_with_years <- list()
  current_year <- NULL
  last_h5_id <- NULL

  for (i in seq_along(siblings)) {
    node <- siblings[[i]]

    # checking if h5 header
    if (xml_name(node) == "div" &&
        str_detect(xml_attr(node, "class"), "mw-heading5")) {

      # extract the year
      h5_id <- node %>%
        xml_find_first(".//h5") %>%
        xml_attr("id")

      current_year <- case_when(
        str_detect(h5_id, "^\\d{4}$") ~ as.numeric(h5_id), # 4 digits
        str_detect(h5_id, "^\\d{4}_") ~ as.numeric(str_extract(h5_id, "^\\d{4}")), # paranthesis
        str_detect(h5_id, "^\\d{4}–\\d{4}$") ~ as.numeric(str_extract(h5_id, "^\\d{4}")), # year range
        TRUE ~ as.numeric(str_extract(h5_id, "\\d{4}")) # anything else
      )

      last_h5_id <- h5_id

    }

    # checking hatnote links after (!) heading
    if (!is.null(current_year) && !is.na(current_year) &&
        xml_name(node) == "div" &&
        str_detect(xml_attr(node, "class"), "hatnote")) {

      link_node <- node %>%
        xml_find_first(".//a[@href]")

      if (!is.na(link_node)) {
        full_url <- paste0("https://en.wikipedia.org",
                           xml_attr(link_node, "href"))

        links_with_years[[length(links_with_years) + 1]] <- list(
          url = full_url,
          year = current_year,
          id = last_h5_id
        )

      }
    }
  }

  names(links_with_years) <- map_chr(links_with_years, function(x) {
    if (!is.null(x$id)) x$id else as.character(x$year)
  })

  return(links_with_years)
}


# FUNCTION get tables from the LINKS found in the og urls

get_tables_from_links <- function(url) {

  sub_links <- get_links_after_voting_estimates(url)

  if (length(sub_links) == 0) return(list())

  all_tables_from_links <- list()

  for (i in seq_along(sub_links)) {
    link_info <- sub_links[[i]]
    link_url <- link_info$url
    link_year <- link_info$year

    cat(sprintf("\n🔗🔗🔗 Processing link for subpage %s\n", link_year))

    # get all siblings after the first h2 or just get all tables
    tryCatch({
      siblings <- link_url %>%
        read_html() %>%
        xml_find_all("//div[h3] | //table[contains(@class, 'wikitable')]")

      last_h3_id <- NULL
      current_year <- link_year  # fallback

      for (j in seq_along(siblings)) {
        node <- siblings[[j]]

        # if we find an h3 header, update current year
        if (xml_name(node) == "div" &&
            str_detect(xml_attr(node, "class"), "mw-heading3")) {

          h3_id <- node %>%
            xml_find_first(".//h3") %>%
            xml_attr("id")

          # try to extract year from h3_id, fallback to link_year
          extracted_year <- case_when(
            str_detect(h3_id, "^\\d{4}$") ~ as.numeric(h3_id),
            str_detect(h3_id, "^\\d{4}_") ~ as.numeric(str_extract(h3_id, "^\\d{4}")),
            str_detect(h3_id, "^\\d{4}–\\d{4}$") ~ as.numeric(str_extract(h3_id, "^\\d{4}")),
            TRUE ~ as.numeric(str_extract(h3_id, "\\d{4}"))
          )

          current_year <- if (!is.na(extracted_year)) extracted_year else link_year
          last_h3_id <- h3_id

        }

        # if we find a wikitable
        if (xml_name(node) == "table" &&
            str_detect(xml_attr(node, "class"), "wikitable")) {

          # test if table can be parsed
          table_data <- tryCatch(html_table(node), error = function(e) NULL)

          if (!is.null(table_data) && nrow(table_data) > 1 && !is.null(current_year)) {

            all_tables_from_links[[length(all_tables_from_links) + 1]] <- list(
              table = node,
              year = current_year,
              id = last_h3_id
            )

            if (!is.null(last_h3_id)) {
              cat(sprintf("\t️Found h3 with ID: '%s' -> Year: %s\n", last_h3_id, current_year))
            }

            cat(sprintf("\tFound table in link with ID: '%s' -> Year: %s\n", h3_id, current_year))

          }
        }
      }
    })

  }

  # named by year
  names(all_tables_from_links) <- map_chr(all_tables_from_links, ~ as.character(.x$year))

  return(all_tables_from_links)
}


# CLEAN ROWS -----------------------------------------------------------------------
# FUNCTION clean wiki pages

clean_rows <- function(df, electoral_year = NULL) {

  last_col <- ncol(df)

  # election type patterns to filter out! => only found these in the wikis!
  election_patterns <- c("general election", "local election", "EP election")

  df <- df %>%
    # empty rows
    filter(!if_all(5:last_col, ~ is.na(.) | . == "")) %>%
    # variables saved as first row
    slice(-1) %>%
    # rows that contain election type strings in the first column (polling firm!!)
    filter(!str_detect(tolower(polling_firm), paste(election_patterns, collapse = "|"))) %>%

    mutate(across(
      everything(),
      ~ case_when(
        .x %in% c("–", "", "?", "—") ~ NA,
        str_starts(.x, fixed("?")) ~ NA,
        TRUE ~ .x
      )
    )) %>%

    mutate(across(5:last_col, ~ str_extract(as.character(.x), "\\d+\\.\\d+|\\d+")))

  df <- df |>
    mutate(
      polling_firm = str_remove_all(polling_firm, "\\[.*?\\]"),
      polling_firm = str_trim(polling_firm),
      media = str_extract(polling_firm, "(?<=/)\\s*[^/]+$"),
      polling_firm = str_extract(polling_firm, "^[^/]+")
    ) |>

    relocate(media, .after = polling_firm)

  df <- df |>
    mutate(sample_size = str_replace_all(sample_size, ",", ""))


  has_year <- any(str_detect(df$fieldwork_date, "[0-9]{4}"))

  if (has_year) {
    df <- df %>%
      mutate(
        start_day = str_extract(fieldwork_date, "^[0-9]{1,2}") |> as.numeric(),
        end_str = str_extract(fieldwork_date, "(?<=–)[0-9]{1,2}\\s*[A-Za-z]{3}\\s*[0-9]{4}") |>
          coalesce(str_extract(fieldwork_date, "[0-9]{1,2}\\s*[A-Za-z]{3}\\s*[0-9]{4}")),
        end_day = str_extract(end_str, "^[0-9]{1,2}") |> as.numeric(),
        end_day = if_else(is.na(end_day), start_day, end_day),
        month = str_extract(end_str, "[A-Za-z]{3}"),
        year = str_extract(end_str, "[0-9]{4}") |> as.numeric(),

        # new vars
        fieldwork_start = sprintf("%02d.%02d.%d", start_day, match(month, month.abb), year),
        fieldwork_end   = sprintf("%02d.%02d.%d", end_day,   match(month, month.abb), year)
      ) %>%
      select(-c(turnout, fieldwork_date, start_day, end_str, end_day, month, year))
  }
  else {

    # fallback: using the passed electoral_year parameter from h5 header!!
    if (is.null(electoral_year) || is.na(electoral_year)) {

      extracted_year <- df$polling_firm[1] |>
        str_extract("[0-9]{4}") |>
        as.numeric()

      if (!is.na(extracted_year)) {
        electoral_year <- extracted_year
        cat(sprintf("Using year from polling_firm: %d\n", electoral_year))
      } else {
        stop("No electoral_year provided, no year found in dates, and no year found in polling_firm")
      }

    }

    df <- df |>
      mutate(

        # full range two months, ex.: 29 Jun–17 Jul
        full_str = str_extract(fieldwork_date, "([0-9]{1,2})\\s*([A-Za-z]{3})\\s*–\\s*([0-9]{1,2})\\s*([A-Za-z]{3})"),
        # fallback one month two dates, ex.: for 29–31 Jul (no month on start)
        end_str = coalesce(full_str, fieldwork_date), # str_to_lower  # stringr::str_squish

        # extract start and end parts
        start_day = as.numeric(str_extract(end_str, "^[0-9]{1,2}")),
        # this does not work for start_month: str_extract(end_str, "[A-Za-z]{3}$"),
        start_month = str_extract(end_str, "^[0-9]{1,2}\\s*([A-Za-z]{3})") %>% str_extract("[A-Za-z]{3}"),
        end_day = as.numeric(str_extract(end_str, "(?<=–)\\s*[0-9]{1,2}") %>% str_trim()),
        end_month = str_extract(end_str, "[A-Za-z]{3}$"),

        # fallbacks
        start_month = if_else(is.na(start_month), end_month, start_month),
        end_day = if_else(is.na(end_day), start_day, end_day),
        end_month = if_else(is.na(end_month), start_month, end_month),

        # new vars
        fieldwork_start = sprintf("%02d.%02d.%d", start_day, match(start_month, month.abb), electoral_year),
        fieldwork_end = sprintf("%02d.%02d.%d", end_day, match(end_month, month.abb), electoral_year)

      ) %>%
      select(-c(turnout, fieldwork_date, full_str, end_str, start_day, end_day, start_month, end_month))


  }

  df <- df %>%
    relocate(c(fieldwork_start, fieldwork_end), .after = media) |>

    mutate(
      fieldwork_start = as.Date(fieldwork_start, format = "%d.%m.%Y"),
      fieldwork_end = as.Date(fieldwork_end, format = "%d.%m.%Y")
    ) %>%
    mutate(
      # if fieldwork_start is after fieldwork_end =>  minus 1 year
      fieldwork_start = if_else(
        fieldwork_start > fieldwork_end,
        fieldwork_start - years(1),
        fieldwork_start
      )
    ) %>%

    mutate(
      n_field_days = as.integer(fieldwork_end - fieldwork_start + 1)
    ) %>%
    relocate(n_field_days, .after = fieldwork_end) %>%

    mutate(across(
      -c(polling_firm, media, fieldwork_start, fieldwork_end),
      ~ as.numeric(.)
    ))

  # trunkating one decimal after the point without rounding! logic: we make the number bigger than smaller again!
  # not sample_size!

  numeric_cols <- names(df)[sapply(df, is.numeric)]
  cols_to_truncate <- setdiff(numeric_cols, "sample_size")

  for (col in cols_to_truncate) {
    df[[col]] <- trunc(df[[col]] * 10) / 10
  }

  return(df)

}




# FINAL FUNCTION -----------------------------------------------------------------------
# FUNCTION extract polling data + add names + clean tables

extract_polling_data <- function(url, election_year, election_type = NULL) {

  cat(sprintf("🚀 Starting extraction for %s (url: %s)\n\n", election_year, url))

  # tables from original urls and from links inside urls
  tables_main <- get_tables_after_voting_estimates(url)
  tables_links <- get_tables_from_links(url)

  all_tables_by_year <- c(tables_main, tables_links) %>% split(., names(.))
  all_tables <- flatten(all_tables_by_year)
  # str(all_tables)

  cat(sprintf("\nFound %d tables total (%d from main page, %d from links)\n",
              length(all_tables), length(tables_main), length(tables_links)))

  # party names for each individual table (2019 case is automatically handled!)
  all_parties <- get_all_parties(all_tables)

  # convert xml tables to data frames
  tables_list <- map(all_tables, function(x) html_table(x$table))
  table_years <- map_dbl(all_tables, function(x) {as.numeric(x$year)})
  table_ids <- map_chr(all_tables, ~ .x$id %||% NA_character_)

  # clean each table individually
  clean_data <- map2(tables_list, seq_along(tables_list), function(data, table_index) {

    original_id <- table_ids[[table_index]]
    year <- table_years[[table_index]]
    cat(sprintf("\t Processing table %d: %s (year: %s)\n", table_index, original_id, year))

    # party names for this specific table
    raw_party_names <- all_parties[[table_index]]

    # fuzzy party_names matching mega_diccionario$siglas
    # party_names <- match_party_names(raw_party_names, mega_diccionario$siglas)
    party_names <- raw_party_names

    colnames(data) <- c("polling_firm", "fieldwork_date", "sample_size", "turnout", party_names, "Lead")

    # passing specific year for this table
    # clean_rows(data, electoral_year = table_years[table_index])
    return(data)
  })

  cat(sprintf("\nCompleted extraction for %s\n", url))

  # to keep the ids as names! but #TODO: actually then you loose some names for some years, eg 2016: <unknown>
  names(clean_data) <- table_ids

  return(clean_data)

}


# DATA -----------------------------------------------------------------------

# 2023
wiki2023 <- extract_polling_data(urls[[1]], 2023)
View(wiki2023) # YES: 7 tables

# walk2(wiki2023, names(wiki2023), ~ write.csv(.x, file = paste0("table_2023_", .y, ".csv"), row.names = FALSE))


# 2019 November
wiki2019nov <- extract_polling_data(urls[[2]], 2019, "November")
View(wiki2019nov) # YES: 1 table


# 2019 April
wiki2019april <- extract_polling_data(urls[[3]], 2019, "April")
View(wiki2019april) # YES: 4 tables

# 2016
wiki2016 <- extract_polling_data(urls[[4]], 2016)
View(wiki2016) # YES: 1 table

# 2015 
wiki2015 <- extract_polling_data(urls[[5]], 2015)
wiki2015 # YES: 5 tables

# 2011
wiki2011 <- extract_polling_data(urls[[6]], 2011)
wiki2011 # YES: 1 table

# 2008
wiki2008 <- extract_polling_data(urls[[7]], 2008)
wiki2008 # YES: 1 table

# 2004
wiki2004 <- extract_polling_data(urls[[8]], 2004)
wiki2004 # YES: 1 table

# 2000
wiki2000 <- extract_polling_data(urls[[9]], 2000)
wiki2000 # YES: 1 table

# 1996
wiki1996 <- extract_polling_data(urls[[10]], 1996)
wiki1996 # YES: 1 table

# 1993
wiki1993 <- extract_polling_data(urls[[11]], 1993)
wiki1993 # YES: 1 table

# 1989
wiki1989 <- extract_polling_data(urls[[12]], 1989)
wiki1989 # YES: 1 table

# 1986
wiki1986 <- extract_polling_data(urls[[13]], 1986)
wiki1986 # YES: 1 table

# 1982
wiki1982 <- extract_polling_data(urls[[14]], 1982)
wiki1982 # YES: 1 table

# 1979
wiki1979 <- extract_polling_data(urls[[15]], 1979)
wiki1979 # YES: 1 table

# -----------------------------------------------------------------------



