library(dply)
library(tidyr)
install.packages("babynames")
library(babynames)
install.packages("ggvis")
class(babynames)
babynames
babynames %>% select(-prop)
babynames %>% select(year:n)
babynames %>% filter(name == "Anna")
babynames %>% filter(name == "Mary" , year == "1880")
a <- babynames %>% 
  mutate(first = 1, second = 2 )
babynames %>% summarise(n = sum(prop))
babynames %>% arrange(name)
babynames %>% group_by(name) %>% summarise(sum(n))
babynames %>% group_by(year,sex) %>% mutate (rank = min_rank(desc(n))) %>% head(20) 
babynames %>% group_by(name) %>% summarise(smry = sum(n)) %>% arrange (desc(smry))
babynames %>% filter(name == "Hadley") %>% group_by(year,sex) %>% spread(sex, n) %>% View()
babynames %>% group_by(year,sex) %>%  mutate(rank = min_rank(desc(n))) %>% filter(rank == 1)







install.packages("cocaine")
