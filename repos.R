library(dplyr)
library(readr)
library(githubinstall)
library(stringr)
library(tidyjson)
library(httpuv)
library(httr)
myapp <- oauth_app(appname = "Github for socinfo",
                   key = "7d46b74499a2c1075b45",
                   secret = "a9568ff92b8d6b05a6099b47f0d6a65d8c2a2e24")

r_urls = gh_list_packages()
r_urls = r_urls %>% 
  mutate(url = paste("https://api.github.com/repos/", username, "/", package_name, sep = ""))
r_urls = r_urls %>% 
  mutate(url_normal = paste("https://github.com/", username, "/", package_name, sep = ""))
url_proj = paste0("'", r_urls$url, "'", collapse = ", ", sep = "")

r_50 <- read_csv("~/emc_github_project/r_50.csv")
#r_50 <- read_csv("~/emc_github_project/all_packages.csv")
library(stringr)
r_50$Name = enc2utf8(r_50$Name)
r_50$Name = sapply(str_split(r_50$Name," "),'[',2)
r_50 = r_50 %>% arrange(-Total)# %>% top_n(10)

r_urls2 = r_urls %>% filter(package_name %in% r_50$Name) #%>% subset(!(package_name %in% repo_r$name))
#left = r_50 %>% subset(!(Name %in% repo_r$name))

unique(r_urls2$package_name) %>% length()

#jeroenooms/stringi ---> gagolews/stringi добавила отдельно 
#stringi not found

#u = "https://api.github.com/repos/gagolews/stringi" #r_urls2$url[1]
#repo_r = data.frame()
for (u in r_urls2$url[27:nrow(r_urls2)]) {
  repos = GET(u, github_token)
  repos2 = content(repos, "text")
  repos3 = repos2 %>% spread_all
  
  repo_r = plyr::rbind.fill(repo_r, repos3)
  Sys.sleep(1.2)
}

library(readr)
write_csv(repo_r, "repo_r.csv")

r_url = r_url$full_name %>% as.character()
#commits = data.frame()
#y = 8
#i = 1
for (y in r_url[42:56]) {
  for (i in 1:150) {
    link = paste("https://api.github.com/repos/", y, "/commits?page=", i, sep = "")
    temp <- GET(link, github_token)
    js = content(temp)
    if (length(js)>0) {
      commits_temp = jsonlite::fromJSON(jsonlite::toJSON(js, recursive = TRUE), flatten = TRUE)
      commits_temp2 = data.frame(
        sha = commits_temp$sha %>% unlist(),
        url = commits_temp$url %>% unlist(),
        date = commits_temp$commit.author.date %>% unlist()
      ) 
      if (nrow(commits_temp2)>1) {
      commits3 = plyr::rbind.fill(commits3, commits_temp)}
      Sys.sleep(1.5)
    } else {
      break
    }}
print(y)}

write_csv(commits, "commits_simplified.csv")

#data.table проверить

commits = unlist(commits3[1])
commits = data.frame(
  sha = commits3$sha %>% unlist(),
  url = commits3$url %>% unlist(),
  date = commits3$commit.author.date %>% unlist()
) 
commits2 = commits$commit$author


