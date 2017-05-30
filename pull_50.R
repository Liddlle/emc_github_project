library(dplyr)
library(stringr)
library(ggplot2)

library(lubridate)
#ищу номера pull request из подписей к коммитам 
req = commits3 %>% filter(str_detect(commit.message,"#[0-9]+"))  %>% 
  filter(str_detect(url,"RcppCore/Rcpp")==FALSE)
req = req %>% mutate(pull = str_extract(req$commit.message,"#[0-9]+") %>% str_replace("#", ""),
                     url = req$url %>% unlist()) %>% select(pull, url)
req2 = strsplit(req$url, "/(?=[^/]+$)", perl=TRUE) 
req2= lapply(req2, `[[`, 1) %>% unlist()
req$url = req2
req = req %>% unique()
req$url = str_replace(req$url, "commits", "")

y = 2772
events_total = data.frame()
#поиск информации о реквестах 
for (y in 7484:nrow(req)) {
  link = paste(req$url[y], "pulls/", req$pull[y], sep = "")
  temp <- GET(link, github_token)
  js = content(temp)
  if (length(js)>3) {
    commits_temp = jsonlite::fromJSON(jsonlite::toJSON(js, recursive = TRUE), flatten = TRUE) 
    commits_temp2 = 
      subset(commits_temp, !(names(commits_temp) %in% 
                               c("user","assignee", "assignees", "requested_reviewers",   "milestone", 
                                 "head", "base", "_links", "merged_by", "mergeable", "rebaseable")))
    
    commits_temp3 =  data.frame(base_repo = commits_temp$base$repo$full_name)
    if (length(commits_temp$user$login) != 0) {
      commits_temp3$pull_user =  commits_temp$user$login
      commits_temp3$pull_user_id = commits_temp$id
      
    }
    
    if (length(commits_temp$merged_by$login) != 0) {
      commits_temp3$merged_by =  commits_temp$merged_by$login
    }
    
        if (length(commits_temp$merged_by$login) != 0) {
      commits_temp3$merged_by =  commits_temp$merged_by$login
    }
    
    if (length(commits_temp$head$repo$full_name) != 0) {
      commits_temp3$head_repo =  commits_temp$head$repo$full_name
    }
    
    if (length(commits_temp$assignee) != 0) {
      commits_temp3$assignee =  commits_temp$assignee$login
    }
    if (length(commits_temp$assignees) != 0) {
      commits_temp3$assignees =  commits_temp$assignees$login
    }
    if (length(commits_temp$requested_rewiewers) != 0) {
      commits_temp3$requested_rewiewers =  commits_temp$requested_rewiewers
    }
    
    if (nrow(commits_temp3)>0) {
      commits_temp2 = do.call("cbind", commits_temp2)
      commits_temp3 = cbind(commits_temp2, commits_temp3)
      events_total = plyr::rbind.fill(events_total, commits_temp3)}
    Sys.sleep(1.5)
  } else {
    next
  }}

  
pull_table = events_total %>% 
  select(pull_user, head_repo, merged_by, number,
         created_at:merge_commit_sha, merged, comments, review_comments, commits:changed_files)


actors = pull_table  %>% group_by(pull_user) %>% tally() %>% arrange(-n)
actors$type = ifelse(actors$pull_user %in% pull_table$merged_by, "core" , actors$n)
actors$type = ifelse(actors$n > 9 & actors$type != "core", "middle" , actors$type)
actors$type = ifelse(actors$n < 10, "new" , actors$type) 

ymd_hms(pull_table$closed_at)
pull_table[5:8] = lapply(pull_table[5:8], ymd_hms) %>% as.data.frame()

pull_table = pull_table %>% mutate(diff = (merged_at - created_at)/3600)
pull_table$diff2 = round(as.numeric( as.character(pull_table$diff)),2)

pull_table2 = left_join(pull_table, actors)

ggplot(data = pull_table2, aes(x = factor(type), y = diff)) + geom_boxplot() + ylim(0,50)
ggplot(data = pull_table2, aes(x = factor(type), y = log(diff2))) + geom_boxplot()

wilcox.test(pull_table2$diff2[pull_table2$type=="new"],pull_table2$diff2[pull_table2$type=="core"])

ggplot(data = pull_table2, aes(x = factor(type), y = log(diff2))) + geom_boxplot()


middle = pull_table2 %>% filter(type == "middle") %>% arrange(pull_user, created_at)
middle2 = middle %>% group_by(pull_user) %>% dplyr::mutate(Count = row_number())
ggplot(data = middle2, aes(x = Count, y = log(diff2), color = pull_user)) + geom_point()

ggplot(data = middle2, aes(x = created_at, y = log(diff2), color = pull_user)) + geom_point() +
  geom_smooth(se = FALSE, method = "lm")

middle3 = left_join(middle2, events_total %>% select(merge_commit_sha, base_repo))
ggplot(data = middle3, aes(x = created_at, y = log(diff2), color = base_repo)) + geom_point() +
  geom_smooth(se = FALSE, method = "lm")
ggplot(data = middle3, aes(x = created_at, y = log(diff2), color = base_repo)) + geom_point() +
  geom_smooth(se = FALSE, method = "lm")

