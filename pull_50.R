library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

top_50 =  r_urls %>% filter(package_name %in% r_50$Name)
url_proj = paste0(top_50$username, "/", top_50$package_name)

#pulls_new = data.frame()
#grep(y, url_proj)
for (y in url_proj[1:62]) {
  for (i in 1:150) {
    link = paste("https://api.github.com/repos/", y, "/pulls?state=all&page=", i, sep = "")
    temp <- GET(link, github_token)
    js = content(temp)
    
    if (length(js)>2) {
      for (element in seq_along(js)) {
        pulls_temp = js[[element]] %>% jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>% as.character() %>% spread_all()
        pulls_new = plyr::rbind.fill(pulls_new, pulls_temp)
      }
      Sys.sleep(0.2)
    } else {
      break
    }
  }
  print(y)} 
  
#write_csv(pulls_new, "~/emc_github_project/pulls50.csv")
pull_table = pulls_new %>% 
  select(user.login, head.repo.name, head.repo.full_name, number,
         created_at:merge_commit_sha, comments_url, contains("merge"))


actors = pull_table  %>% group_by(user.login) %>% tally() %>% arrange(-n)
actors$type = ifelse(actors$user.login %in% repo_r$owner.login, "core" , actors$n)
actors$type = ifelse(actors$n > 9 & actors$type != "core", "middle" , actors$type)
actors$type = ifelse(actors$n < 10, "new" , actors$type) 

pull_table$closed_at=ymd_hms(pull_table$closed_at)
pull_table$merged_at=ymd_hms(pull_table$merged_at)
pull_table$created_at=ymd_hms(pull_table$created_at)
#pull_table[5:7] = lapply(pull_table[5:8], ymd_hms) %>% as.data.frame()

pull_table = pull_table %>% mutate(diff = (merged_at - created_at)/3600)
pull_table$diff2 = round(as.numeric( as.character(pull_table$diff)),2)

pull_table2 = left_join(pull_table, actors)

ggplot(data = pull_table2, aes(x = factor(type), y = diff)) + geom_boxplot() + ylim(0,50)
ggplot(data = pull_table2, aes(x = factor(type), y = log(diff2))) + geom_boxplot()


wilcox.test(pull_table2$diff2[pull_table2$type=="new"],pull_table2$diff2[pull_table2$type=="core"])


middle = pull_table2 %>% filter(type == "middle") %>% arrange(user.login, created_at)
middle2 = middle %>% group_by(user.login) %>% dplyr::mutate(Count = row_number())
ggplot(data = middle2, aes(x = Count, y = log(diff2), color = user.login)) + geom_point()

ggplot(data = middle2, aes(x = created_at, y = log(diff2), color = user.login)) + geom_point() +
  geom_smooth(se = FALSE, method = "lm")

middle3 = left_join(middle2, pulls_new %>% select(merge_commit_sha, head.repo.name))
ggplot(data = middle3, aes(x = created_at, y = log(diff2), color = base_repo)) + geom_point() +
  geom_smooth(se = FALSE, method = "lm")
ggplot(data = middle3, aes(x = created_at, y = log(diff2), color = head.repo.name)) + geom_point() +
  geom_smooth(se = FALSE, method = "lm")

