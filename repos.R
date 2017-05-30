library(dplyr)
library(readr)
library(githubinstall)
library(stringr)
library(tidyjson)
library(httpuv)
library(httr)
oauth_endpoints("github")
myapp <- oauth_app(appname = "socinfo",
                   key = "7d46b74499a2c1075b45",
                   secret = "a9568ff92b8d6b05a6099b47f0d6a65d8c2a2e24")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp, cache = F)
gtoken <- config(token = github_token)

r_urls = gh_list_packages()
r_urls = r_urls %>% 
  mutate(url = paste("https://api.github.com/repos/", username, "/", package_name, sep = ""))
r_urls = r_urls %>% 
  mutate(url_normal = paste("https://github.com/", username, "/", package_name, sep = ""))
url_proj = paste0("'", r_urls$url, "'", collapse = ", ", sep = "")

r_50 <- read_csv("~/emc_github_project/all_packages.csv")
#r_50$Name = sapply(str_split(r_50$Name," "),'[',2)

r_urls2 = r_urls %>% filter(package_name %in% r_50$Name) #%>% subset(!(package_name %in% repo_r2$name))
#left = r_50 %>% subset(!(Name %in% repo_r$name))

unique(r_urls2$package_name) %>% length()

#jeroenooms/stringi ---> gagolews/stringi добавила отдельно 
#stringi not found

#u = "https://api.github.com/repos/gagolews/stringi" #r_urls2$url[1]
#repo_r2 = data.frame()
for (u in r_urls2$url[1:nrow(r_urls2)]) {
  repos = GET(u, github_token)
  repos2 = content(repos, "text")
  repos3 = repos2 %>% spread_all
  if (ncol(repos3)>5) {
    repo_r2 = plyr::rbind.fill(repo_r2, repos3)
    print(repos3$full_name)}
  Sys.sleep(1)
}

#write_csv(repo_r2, "repo_r.csv")
repo_r = read_csv("repo_r.csv")

r_url = repo_r$full_name %>% as.character()

#commits_new = data.frame()
#y = r_url[1]
#i = 1
urlss = strsplit(commits3$url %>% unlist() %>% as.character(), "/", perl=TRUE) 
urlss = paste(lapply(urlss, `[[`, 5) %>% unlist(), "/", lapply(urlss, `[[`, 6) %>% unlist(), sep = "") 
urlss = unique(urlss)

r_url2 = subset(r_url, !(r_url %in% urlss)) 
grep('AnnePetersen1/PCADSC', r_url2)

for (y in r_url2[42:4506]) {
  for (i in 1:150) {
    link = paste("https://api.github.com/repos/", y, "/commits?page=", i, sep = "")
    temp <- GET(link, github_token)
    js = content(temp)
    if (length(js)>2) {
      commits_temp = jsonlite::fromJSON(jsonlite::toJSON(js, recursive = TRUE), flatten = TRUE)
      parents = commits_temp %>% select(parents) %>% as.character() 
      commits_temp2 = commits_temp %>% select(-parents) 
      commits_temp2 = sapply(commits_temp2, unlist)
      commits_temp2 = cbind(commits_temp2, parents) %>% as.data.frame() 
      commits_new = plyr::rbind.fill(commits_new, commits_temp)
      Sys.sleep(1)
    } else {
      next
    }
  }
  print(y)}
#AMolinaro/partDSA - This repository is empty.  

write_csv(commits, "commits_simplified.csv")

#data.table проверить


