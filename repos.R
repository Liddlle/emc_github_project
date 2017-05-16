library(githubinstall)
r_urls = gh_list_packages()
r_urls = r_urls %>% 
  mutate(url = paste("https://api.github.com/repos/", username, "/", package_name, sep = ""))
r_urls = r_urls %>% 
  mutate(url_normal = paste("https://github.com/", username, "/", package_name, sep = ""))
url_proj = paste0("'", r_urls$url, "'", collapse = ", ", sep = "")

r_50 <- read_csv("~/emc_github_project/r_50.csv")
library(stringr)
r_50$Name = enc2utf8(r_50$Name)
r_50$Name = sapply(str_split(r_50$Name,"�"),'[',2)
r_50 = r_50 %>% arrange(-Total)# %>% top_n(10)

r_urls1 = r_urls %>% filter(package_name %in% r_50$Name) %>% filter(username != "tidyverse")
r_urls2 = r_urls %>% filter(package_name %in% r_50$Name) %>% subset(!(package_name %in% repo_r$name))
u = r_urls$url[10]
#stringi not found
#repo_r = data.frame()
for (u in r_urls2$url[27:nrow(r_urls2)]) {
  repos = GET(u, github_token)
  repos = content(repos)
  repos = jsonlite::fromJSON(jsonlite::toJSON(repos, recursive = TRUE), flatten = TRUE)
  #repos2 =  tryCatch((plyr::ldply(repos[c(1:3,5:63, 65:69, 72:73)], cbind)), error = function() next)
  repos2 = subset(repos, !(names(repos) %in% c("owner","parent","source",   "homepage", "mirror_url", "permissions", "organization", "description")))
  repos2 = plyr::ldply(repos2, cbind)
  
  if (repos$parent  %>% is.null() == FALSE) {
    repos_parent = data.frame(.id = "parent", `1` = repos$parent$full_name)
    names(repos_parent)[2] = 1
    }
  
  
  repos_desc = plyr::ldply(repos$description, cbind) 
  if (repos_desc$`1`  %>% is.null() == FALSE) {
    if (nrow(repos_desc)==1) {
      repos_desc$.id = "description"
    #} else { repos_desc$.id = paste("desc_", repos_desc$.id, sep = "")
    }}
  
  repos_owner = plyr::ldply(repos$owner, cbind) 
  repos_owner$.id = paste("owner_", repos_owner$.id, sep = "")
  
  repos_permission = plyr::ldply(repos$permissions, cbind)
  repos_permission$.id = paste("permissions_", repos_permission$.id, sep = "")
  
  repos_org =  plyr::ldply(repos$organization, cbind) 
  if (repos_org$.id  %>% is.null() == FALSE) {
    repos_org$.id = paste("org_", repos_org$.id, sep = "")
  }
  
  repos_final = plyr::rbind.fill(repos2, repos_owner, repos_permission, repos_org, repos_desc, repos_parent)
  repos_final = t(repos_final)
  colnames(repos_final) = repos_final[1,] 
  repos_final = as.data.frame(repos_final)
  repos_final = repos_final[-1,] %>% as.data.frame()
  
  repo_r = plyr::rbind.fill(repo_r, repos_final)
  Sys.sleep(1)
}


#commits = data.frame()
commits2 = data.frame()
r_url = repo_r %>% filter(full_name != "Rcpp")
r_url = r_url$full_name %>% as.character()
for (y in r_url) {
for (i in 1:150) {
  link = paste("https://api.github.com/repos/", y, "/commits?page=", i, sep = "")
  temp <- GET(link, github_token)
  js = content(temp)
  if (length(js)>0) {
    commits_temp = jsonlite::fromJSON(jsonlite::toJSON(js, recursive = TRUE), flatten = TRUE)
    parents = commits_temp %>% select(parents) %>% as.character()
    commits_temp2 = commits_temp %>% select(-parents)
    commits_temp2 = sapply(commits_temp2, unlist)
    commits_temp2 = cbind(commits_temp2, parents) %>% as.data.frame()
    commits2 = rbind(commits2, commits_temp)
    Sys.sleep(1.5)
  } else {
    next
  }
}}

commits = unlist(commits2[1])
commits2 = commits$commit$author

