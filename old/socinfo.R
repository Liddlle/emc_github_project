```{r}
#ssh -i ~/.ssh/id_rsa -L 33306:web.ghtorrent.org:3306 ghtorrent@web.ghtorrent.org
my_db = src_mysql("ghtorrent", host = "127.0.0.1", port = 33306, user = "ght", password = "")

source("~/emc_github_project/start.R")
```


https://github.com/RcppCore/Rcpp
https://github.com/tidyverse/ggplot2
https://github.com/tidyverse/readr
https://github.com/tidyverse/dplyr
https://github.com/sjmgarnier/viridisLite
https://github.com/jeroen/curl
https://github.com/hadley/devtools
https://github.com/hadley/lubridate
https://github.com/topepo/caret
https://github.com/tidyverse/readxl

```{r}
top_10 = c("RcppCore/Rcpp", "tidyverse/ggplot2", "tidyverse/readr", "tidyverse/dplyr", "sjmgarnier/viridisLite", "jeroen/curl", "hadley/devtools", "hadley/lubridate", "topepo/caret", "tidyverse/readxl")
url_proj = str_c("https://api.github.com/repos/", top_10)
#paste("'https://api.github.com/repos/", top_10, "'", collapse = ", ", sep = "")



```



```{r}
#строчка снизу не работает)))))))00
projects = data.frame()
for (i in url_proj) {
  tmp = dbGetQuery(ghtor, paste0("select * from projects where url = '", i, "'"))
  projects = rbind(projects, tmp)
}

#r_id_project = projects$id
r_ids = paste0("'", projects$id, "'", collapse = ", ", sep = "")  


```

skip
```{r}

forked = data.frame() #curl - 0
for (i in projects$id) {
  tmp2 = dbGetQuery(ghtor, paste0("select * from projects where forked_from = '", i, "'"))
  forked = rbind(forked, tmp2)
  Sys.sleep(5)
}


forked2 = forked %>% group_by(name) %>% tally() %>% filter(n > 1)
```



```{r}

pull_requests = data.frame() 
for (i in projects$id) {
  tmp3 = my_db %>% tbl("pull_requests") %>% 
    filter(base_repo_id == i) %>% 
    as.data.frame()
  pull_requests = rbind(pull_requests, tmp3)
  Sys.sleep(1)
}

pull_id = paste(c(pull_requests$id), collapse = "','")
test = paste0("select * from pull_requests where base_repo_id in (", r_ids, ") and id not in ('", pull_id, "')", collapse = "")
pull_requests2 = dbGetQuery(ghtor, test)


pull_requests_head = data.frame() 
for (i in projects$id) {
  tmp3 = my_db %>% tbl("pull_requests") %>% 
    filter(head_repo_id == i) %>% 
    as.data.frame() #dbGetQuery(ghtor, paste0("select * from pull_requests where head_repo_id = '", i, "'"))
  pull_requests_head = rbind(pull_requests_head, tmp3)
  Sys.sleep(5)
}

#pull_requests = dbGetQuery(ghtor, paste0(c('select * from pull_requests where base_repo_id in ', "(", r_ids, ")"), collapse = "")) 
#pull_requests_head = dbGetQuery(ghtor, paste0(c('select * from pull_requests where head_repo_id in ', "(", r_ids, ")"), collapse = ""))

commits =  my_db %>% tbl("project_commits") %>% 
  filter(project_id == "6209178") %>% 
  as.data.frame()

commits2 = data.frame() 
for (l in projects$id) {
  tmp3 = my_db %>% tbl("project_commits") %>% 
    filter(project_id == l) %>% 
    as.data.frame()
  commits2 = plyr::rbind.fill(commits2, tmp3)
}
commits2 %>% group_by(project_id) %>% tally()

#commits = dbGetQuery(ghtor, 'select author_id from commits where project_id = "6209178" group by author_id')
x = "1382617"#dbGetQuery(ghtor, 'select count(id) from commits where project_id = "49642088"')


```

```{r}
pull_rec = my_db %>% tbl("pull_request_commits") %>% 
  filter(commit_id %in% commits2$commit_id) %>% 
  as.data.frame()

#pull_id = paste(c(pull_requests$id), collapse = "','")
#dbGetQuery(ghtor, paste0(c('select * from pull_request_history where pull_request_id in ', "('", pull_id, "')"), collapse = "")) 

pull_history = my_db %>% tbl("pull_request_history") %>% 
  filter(pull_request_id %in% pull_rec$pull_request_id) %>% 
  as.data.frame()
pull_history$created_at = ymd_hms(pull_history$created_at)

library(reshape2)
pull_history_actors = pull_history %>% filter(action == "opened") %>% select(pull_request_id, actor_opened = actor_id) 
pull_history2 = pull_history %>% dplyr::select(-actor_id, -id) %>% filter(action == "opened" | action == "merged") %>% filter(pull_request_id != 19190795 & pull_request_id != 19669531) 
pull_history2 = pull_history2 %>% filter(pull_request_id %in% pull_history2$pull_request_id[pull_history2$action=="merged"])
#del = pull_history2 %>% group_by(pull_request_id) %>% tally() %>% filter(n>3)
#pull_history2 = subset(pull_history2,  !(pull_request_id %in% del$pull_request_id))
pull_history3 =  dcast(data = pull_history2, pull_request_id~action, value.var="created_at") #, fun.aggregate = min
pull_history3$merged = pull_history2$created_at[pull_history2$action=="merged"]
pull_history3$opened = pull_history2$created_at[pull_history2$action=="opened"]

del = left_join(pull_history3, pull_history_actors)  %>% group_by(actor_opened) %>% tally() %>% arrange(-n)
del$type = ifelse(del$n == 1, "new" , del$n) #del$n
del$type = ifelse(del$n > 1 & del$n < 10, "middle" , del$type) #
del$type = ifelse(del$n > 9, "core" , del$type)
#del$type = ifelse(del$n > 21, "core" , del$type)
#del$type = ifelse(del$actor_opened %in% pull_history$actor_id[pull_history$action!="opened"], "core" , del$type) 

pull_history_actors = left_join(del %>% select(-n), pull_history_actors) 
pull_history4 = inner_join(pull_history3, pull_history_actors)

pull_history4 = pull_history4 %>% mutate(diff = (ymd_hms(merged) - ymd_hms(opened))) #/3600
pull_history4$diff2 = round(as.numeric( as.character(pull_history4$diff/3600)),2)

ggplot(data = pull_history4, aes(x = factor(type), y = diff)) + geom_boxplot() + ylim(0,5000)
ggplot(data = pull_history4, aes(x = factor(type), y = log(as.numeric(diff)))) + geom_boxplot()

pull_history4 = pull_history4 %>% mutate(diff3 =log(as.numeric(diff))) #/3600

pull_history4 = pull_history4 %>% arrange(actor_opened, opened) 
pull_history4 = pull_history4 %>% dplyr::group_by(actor_opened) %>% dplyr::mutate(id = seq_along(actor_opened))
```

Заметной разницы нет, но что если мы ключим фактор времени в это говно 

```{r}

wilcox.test(pull_history4$diff2[pull_history4$type=="new"], pull_history4$diff2[pull_history4$type=="core"])

wilcox.test(pull_history4$diff2[pull_history4$type=="middle"], pull_history4$diff2[pull_history4$type=="new"])

pull_history4 %>% group_by(type) %>% summarise(n = n(), mean = mean(diff2), median = median(diff2))

temp0 = pull_history %>% filter(action == "merged")
temp = subset(pull_history, !(actor_id %in% temp0$actor_id))
temp= temp %>% filter(action == "opened") %>% group_by(actor_id) %>% tally() %>% filter(n>4)
temp = pull_history %>% filter((actor_id %in% temp$actor_id)) # & action == "merged")
temp = pull_history %>% filter(pull_request_id %in% temp$pull_request_id)

ggplot(data = pull_history4, aes(x = id, y = diff3, colour = pull_history4$type)) + geom_point()
lm(as.numeric(diff)~id, data = pull_history4) %>% summary()
```

