library(tidyverse)
library(here)
library(glue)
library(lubridate)
library(googlesheets4)
gs4_deauth()
publications <- read_sheet("https://docs.google.com/spreadsheets/d/1jF1pjT_GcL-y_geOz8C-4FBeTNBRi_1zpvhDL3PhPIg/edit?usp=sharing",
           sheet = "publications",
           col_types = "c") %>% 
  mutate(anchor = ifelse(is.na(short_title), 
                         janitor::make_clean_names(title),
                         janitor::make_clean_names(short_title)))


# create the table data
publications_table <- publications %>%
  arrange(desc(date), type) %>%
  mutate(year=ifelse(sapply(publications$date,function(x) !is.na(as_date(x))),
                     year(as_date(publications$date)),
                     publications$date)) %>%
  mutate(url = ifelse(!is.na(doi) & is.na(url),str_c('https://doi.org/',doi),url)) %>%
    mutate(title = ifelse(!is.na(url), str_c("<a href='", url, "'>", title, "</a>"),title)) %>% #turn title into link
  mutate(award = case_when(
    award == "Best Paper"         ~ str_c("<i class='fa fa-trophy'></i><em> ", award, "</em> ·"),
    # award == "Honourable Mention" ~ str_c("<img src='/assets/images/ribbon_xs.png' style='height: 1em;'><em> ", award, "</em> ·"),
    TRUE                          ~       ""
  ),
  venue = ifelse(is.na(venue),"",str_c("<i>",venue,"</i>")),
  venue = ifelse(is.na(volume_page),venue,str_c(venue," ",volume_page)),
  pdf = ifelse(is.na(pdf), "", str_c("<span class='publication-extra'><a href='", pdf, "'>", 'pdf', "</a></span>")),
  materials = ifelse(is.na(materials),  "", str_c(" · <span class='publication-extra'><a href='", materials, "'>", 'materials', "</a></span>")),
  ebook = ifelse(is.na(ebook), "", str_c(" · <span class='publication-extra'><a href='", ebook, "'>", 'ebook', "</a></span>")),
  blog = ifelse(is.na(blog),  "", str_c(" · <span class='publication-extra'><a href='", blog, "'>", 'blog', "</a></span>")),
  full_talk = ifelse(is.na(full_talk), "", str_c(" · <span class='publication-extra'><a href='", full_talk, "'>", 'video of full talk', "</a></span>")),
  bibtex = ifelse(is.na(bibtex),  "", str_c(" · <span class='publication-extra'><a href='", bibtex, "'>", 'bibtex', "</a></span>")),
  authors_full = ifelse(grepl("\\*",authors_full),str_c(authors_full,"<br><small>* Equal contributions</small>"),authors_full),
  featured = ifelse(is.na(featured),"",str_c("<span class='publication-featured'>",featured,"</span>")),
  press = sapply(press,function(str) {
                   ifelse(!is.na(str),
                  str_c("<span class='publication-featured'><br><b>Press: </b>",
                                       str %>% str_split('\n') %>% sapply(function(x) str_c(
                                         '<a href="',str_extract(x,'(?<=\\().+(?=\\))'),'">',str_extract(x,'(?<=\\[).+(?=\\])'),'</a>')
                                        ) %>% 
                                         paste(collapse=" · ")
                                       ,"</span>"),
                  "")
                   })
  ) %>%
  # featured = sapply(ifelse(is.na(featured),"",featured),
  #                   function(x) markdown::markdownToHTML(text=x,fragment.only = TRUE) %>%
  #                     str_extract('^(?<=<p>)+*<(?=</p>)$'))) %>% 
  mutate(citation = str_c("<a class='anchor' id='", anchor, "'></a>",
                          "<span class='pub-title'>", title, "</span><br>"),
         citation = str_c(citation,
                          authors_full, "<br>",
                          venue, "<br>", 
                          award, pdf, materials, ebook, blog, full_talk, bibtex,
                          featured, press,
                          sep = " ")
           ) %>% 
  mutate(citation = str_replace(citation, "Hsieh PH", "<b>Hsieh PH</b>")) %>%  # make my name bold
  mutate(teaser_video_embed = case_when(
      str_detect(teaser_video_embed, "youtube") ~ glue("<div class='embed-responsive embed-responsive-16by9'><iframe class='embed-responsive-item' src='{teaser_video_embed}' allowfullscreen></iframe></div>"),
      str_detect(teaser_video_embed, "vimeo") ~ glue("<div class='embed-responsive embed-responsive-16by9'><iframe class='embed-responsive-item' src='{teaser_video_embed}' allow='fullscreen'></iframe></div>"),
      is.na(teaser_video_embed) ~ ""
    )) %>%
  mutate(altmetric_badge = ifelse(is.na(doi),"",
                                  str_c("<div data-badge-popover='right' data-badge-type='donut' data-doi='",
                                  doi,"' data-hide-no-mentions='true' class='altmetric-embed'></div>")))





library(scholar)
id <- 'CvMzX6AAAAAJ'
gs <- get_profile(id)

pub.types <- sort(table(publications_table$type),decreasing = T)
pub.types <- pub.types[pub.types>1]
pub.types <- paste(pub.types,' ',names(pub.types),'s',sep="") 

# other.stats <- c(citations=gs$total_cites,
#                  `h-index`=gs$h_index)
# other.stats <-paste(other.stats,' ',names(other.stats),sep="") 

cites.link <- str_c('[',gs$total_cites,
      '](https://scholar.google.com/citations?user=CvMzX6AAAAAJ&hl=en>)')
h.link <- str_c('[',gs$h_index,
                '](https://scholar.google.com/citations?user=CvMzX6AAAAAJ&hl=en>)')

other.stats <- c(str_c(cites.link,' citations'),
                 str_c('h-index: ',h.link))
# other.stats <-paste(other.stats,' ',names(other.stats),sep="") 

stats.string <- c(pub.types,other.stats) %>% paste(collapse=" · ")
# including contributions to Science, PNAS, Global Change Biology, and Current Biology

# draw a table with heading for each year
outfile <- c('publications.md')
header <- 
  '---
layout: single
permalink: /publications/
author_profile: true
title: "Publications"
classes: wide
header:
  image: /assets/images/101516_hybrids_opener_free.JamesCarey.v2.png
---
<script type="text/javascript" src="https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js"></script>
'
write_lines(header,outfile)
write_lines(str_c(stats.string),outfile,append = T)
write_lines(str_c('<br><small>Last updated ',format(as_date(Sys.time()),format="%d %b %Y"),'</small>'),outfile,append = T)


years <- unique(publications_table$year)
for (cur_year in years){
  publications_table %>% 
    filter(year == cur_year) %>% 
    select(altmetric_badge,citation) %>%
    knitr::kable(caption = cur_year, format = "html",
                 table.attr='class="publication-table"', escape = FALSE) %>%
    write_lines(outfile,append=TRUE)
} 


