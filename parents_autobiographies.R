# Load in libraries
library(highcharter)
library(gutenbergr)
library(DT)
library(tidytext)
library(tidyverse)
library(scales)

# Locate autobiographies
gutenberg_works()  %>% 
  filter(str_detect(str_to_lower(title), 'autobiography')) -> autobiographies

# Download them and count allusions to father/mother
pmap_dfr(autobiographies, function (gutenberg_id, title, author, ...)
{
  book <- gutenberg_download(gutenberg_id)
  book %>% select(text) %>% paste(collapse=" ") %>% str_to_lower()-> text
  str_count(text, 'my mother') -> mother
  str_count(text, 'my father') -> father
  tibble(gutenberg_id, title, author, mother, father)
}) -> data

# Datatable to show the dataset
datatable(data, 
          rownames = FALSE,
          options = list(
          searching = FALSE,
          pageLength = 10)) %>% 
  htmlwidgets::saveWidget("table_autobiographies.html")

# Histogram of father-mother
ggplot(data, aes(x=father-mother)) + 
  geom_histogram(aes(y =..count../sum(..count..)),
                 bins=80,
                 fill="yellow",
                 col="black") + 
  scale_y_continuous(expand=c(0,0), labels = percent_format(accuracy = 2)) +
  labs(title="Allusions to fathers and mothers in autobiographies", 
       subtitle = "Difference between the amount of allusions to father and mother",
       x="# allusions to father - # allusions to mother", 
       y="Works (%)",
       caption = "Source: Project Gutenberg") +
    theme_minimal()

# Interactive scatter plot
highchart() %>% 
  hc_xAxis(title = list(text = "Number of allusions to the father")) %>%  
  hc_yAxis(title = list(text = "Number of allusions to the mother")) %>%
  hc_title(text    = "Allusions to the father and mother in autobiographies") %>%
  hc_subtitle(text = "Total number of allusions")%>%
  hc_credits(enabled = TRUE, text = "Books from Project Gutenberg. Done with R and Highcharts by @aschinchon") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_chart(zoomType = "xy") %>%
  hc_add_series(data = purrr::map(0:max(data$father), function(x) list(x, x)), 
                type = "line", 
                dashStyle = "Dash",
                color = "gray",
                enableMouseTracking = FALSE) %>% 
  hc_add_series(data, 
                type = "scatter", 
                hcaes(x = father,
                      y = mother)) %>% 
  hc_plotOptions(scatter = list(marker = list(symbol = "circle",  
                                              fillColor='#ffffff',
                                              lineWidth=2,
                                              lineColor=NULL)),
                 line    = list(marker = list(enabled = FALSE))) %>% 
  hc_legend(enabled = FALSE) %>% 
  hc_tooltip(formatter = JS("function(){
                            return ('    <b> Title: </b>' + this.point.title + 
                            '<br><b>Author: </b>' + this.point.author + 
                            '<br><b>Father: </b>' + this.point.father +
                            '<br><b>Mother: </b>' + this.point.mother)}")) %>% htmlwidgets::saveWidget("highchart_autobiographies.html")

# When do authors was born?
autobiographies %>% 
  inner_join(gutenberg_authors, by="gutenberg_author_id") %>% 
  group_by(birthdate) %>% 
  summarise(amount = n()) -> birth_dates

