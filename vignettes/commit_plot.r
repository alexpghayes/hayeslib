library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

# this for public repos
# for private repos use a personal access token
#
# myapp <- oauth_app("github",
#                    key = "b6b2f0531b584bc9893a",
#                    secret = "91cf30b39adcc31ddc6140323e86c970126abb8c")
# github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
# gtoken <- config(token = github_token)

req_url <- "https://api.github.com/repos/alexpghayes/comp540/commits"
pac <- "793cce65738d7c8b56aa6b205851ff632242dea0"

# ?page=2&per_page=100'
req <- GET(req_url, query = list(access_token = pac))
stop_for_status(req)

df <- fromJSON(content(req, type = "text"))

author <- ifelse(df$commit$author$name == "Seth Davis", "me", "partner")
time <- df$commit$committer$date

data_frame(author, time) %>%
  mutate(time = ymd_hms(time),
         flag = 1) %>%
  arrange(time) %>%
  group_by(author) %>%
  mutate(count = cumsum(flag)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = time, y = count, color = author)) +
  ggtitle("Why I Hate Group Projects") +
  labs(x = "Time", y = "Commits To Date")
