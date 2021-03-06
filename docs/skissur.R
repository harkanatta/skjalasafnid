tafla <- read.csv("docs/tedersoo2021/tafla.csv")
library(plyr)
tafla %>% 
  ddply(.(author_familiarity,data_availability_from_corresponding_author),summarise,N=table(data_availability_from_corresponding_author))

tafla[tafla$data_availability_final=='none',] %>% 
  ddply(.(data_availability_initial, discipline,data_availability_final, reason_for_decline),summarise,N=table(reason_for_decline))

reason <- tafla[tafla$data_availability_final!='full',] %>% 
  ddply(.(discipline,reason_for_decline),summarise,N=table(reason_for_decline))


reason <- tafla[tafla$data_availability_final=='none',] %>% 
  ddply(.(discipline, reason_for_decline),summarise,N=table(reason_for_decline))

reason <- tafla %>% 
  ddply(.(discipline, data_availability_initial, reason_for_decline),summarise,N=table(reason_for_decline))

sum(reason$N[grep("agreement|privacy",reason$reason_for_decline)])
sum(reason$N[grep("time",reason$reason_for_decline)])
sum(reason$N[grep("lost",reason$reason_for_decline)])



library(networkD3)
library(dplyr)
value <- c(371, 41, 26, 33.2, 60, 3, 0.2, 8.2, 0.34, 1, 0.002) #Uppspretta lægra mat
losun <- c(164, 5.7,  0, 15.2, 3.2, 0.3, 0, 8.2, 0.34, 0, 0.002) #losun lægra mat
#value <- c(379,586,38,233,21,48,3,11,8,32,0.3,3)
heiti <- c('Bifreiðahjólbarðar',
           'Vegmerkingar',
           'Flugvélahjólbarðar',
           'Húsamálning',
           'Skipamálning',
           'Gervigras',
           'Leikvellir',
           'Þvottur',
           'Snyrtivörur',
           'Haglaskot',
           'Sigvatn')
land <- value-losun
df <- data.frame(heiti,land,losun)
df <- df[order(df$heiti),]
library(networkD3)
nodes <- c(levels(df$heiti), 'Land','Haf')
nodes <- as.data.frame(nodes)
names(nodes) <- "name"
a <- list()
for (i in 1:nrow(df)) {
  a[i] <- list(rbind(matrix(c(df[i,c(2,3)]))))
}
b <- do.call(c,a)
value <- unlist(b)
links <- 
  data.frame(
    source=c(0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10),
    target=c(rep(c(11,12),11)),
    value=value)
Sank <- list(nodes, links)
names(Sank) <- c('nodes', 'links')
sn <- sankeyNetwork(
  Links = Sank$links,
  Nodes = Sank$nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  units = "tonn",
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
  fontSize = 20,
  nodeWidth = 30, width = "770px", height = "500px", fontFamily = "Courier"
)
if (knitr::is_html_output()) {
  htmlwidgets::onRender(sn, 'document.getElementsByTagName("svg")[0].setAttribute("viewBox", "")') #Svo að þetta sé ekki ofursmátt í Firefox
} else{
  B <- paste(nodes$name,c(df$land+df$losun,sum(df$land),sum(df$losun)), sep="- ")
  nodes <- paste(B,"t")
  nodes <- as.data.frame(nodes)
  names(nodes) <- "name"
  
  Sank <- list(nodes,links)
  names(Sank) <- c('nodes','links')
  
  sn <- sankeyNetwork(
    Links = Sank$links,
    Nodes = Sank$nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    units = "tonn",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
    fontSize = 12,
    nodeWidth = 30)
  
  sn
}
```







discipline data_availability_initial  N
1 Psychology                      full 35
2 Psychology                      none 56
3 Psychology                   partial  3

discipline data_availability_initial data_availability_from_corresponding_author  N
1 Psychology                      full                                             35
2 Psychology                      none                                    declined 16
3 Psychology                      none                                     ignored 19
4 Psychology                      none                                    obtained 21
5 Psychology                   partial                                    obtained  3

discipline data_availability_initial data_availability_from_corresponding_author data_availability_final  N
1 Psychology                      full                                                                full 35
2 Psychology                      none                                    declined                    none 14
3 Psychology                      none                                    declined                 partial  2
4 Psychology                      none                                     ignored                    none 19
5 Psychology                      none                                    obtained                    full 20
6 Psychology                      none                                    obtained                 partial  1
7 Psychology                   partial                                    obtained                    full  3

discipline data_availability_initial data_availability_from_corresponding_author data_availability_final           reason_for_decline  N
1  Psychology                      full                                                                full                              35
2  Psychology                      none                                    declined                    none                    data_lost  2
3  Psychology                      none                                    declined                    none data_protected_by_agreements  2
4  Psychology                      none                                    declined                    none            no_time_to_search  3
5  Psychology                      none                                    declined                    none                not_specified  1
6  Psychology                      none                                    declined                    none                 other:_moved  2
7  Psychology                      none                                    declined                    none                      privacy  2
8  Psychology                      none                                    declined                    none       privacy,big_data_files  2
9  Psychology                      none                                    declined                 partial                      no_time  2
10 Psychology                      none                                     ignored                    none                              19
11 Psychology                      none                                    obtained                    full                              20
12 Psychology                      none                                    obtained                 partial                               1
13 Psychology                   partial                                    obtained                    full                               3










rass <- tafla %>% 
  select(discipline, 
         data_availability_initial,
         data_availability_from_corresponding_author,
         data_availability_final,
         period_of_publication,
         reason_for_decline)

tafla$period_of_publication



rass <- tafla %>% 
  ddply(.(discipline, 
          data_availability_initial,
          data_availability_from_corresponding_author,
          data_availability_final,
          period_of_publication,
          reason_for_decline
  ),
  summarise,N=table(discipline))

library(ggalluvial)
ggplot(rass,
       aes(axis1 = discipline, axis2 = data_availability_final)) +
  geom_alluvium(aes(fill = data_availability_final), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("discipline", "data_availability_final"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("gagnaheimt")




discipline data_availability_initial data_availability_from_corresponding_author data_availability_final                  reason_for_decline  N
1 Biomaterials_and_biotechnology                      full                                                                n.a.                                     65
2 Biomaterials_and_biotechnology                      none                                                                n.a.                                     14
3 Biomaterials_and_biotechnology                   partial                                                                n.a.                                     21
4                        Ecology                      full                                                                full                                     65
5                        Ecology                      none                                    declined                    none no_time_to_search,purpose_not_given  1
6                        Ecology                      none                                    declined                    none                       not_specified  1


ggplot(rass,
       aes(y = N, axis1 = data_availability_initial, axis2 = discipline)) +
  geom_alluvium(aes(fill = data_availability_final), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")






















library(ggsankey)
library(dplyr)
library(ggplot2)

df <- mtcars %>%
  make_long(cyl, vs, am, gear, carb)

rass <- tafla %>% 
  select(discipline, 
         data_availability_initial,
         data_availability_from_corresponding_author,
         data_availability_final,
         period_of_publication,
         reason_for_decline) %>% 
    ddply(.(discipline,data_availability_initial),summarize,N=table(discipline)) #%>% 
  #pivot_wider(names_from = data_availability_initial,values_from = N)


ggplot(rass,
       aes(y=N, axis1 = discipline, axis2 = data_availability_initial)) +
  geom_alluvium(aes(fill = data_availability_initial), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("discipline", "data_availability_final"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("") +
  theme_void()

###########################

rass <- tafla %>% 
  select(discipline, 
         data_availability_initial,
         data_availability_from_corresponding_author,
         data_availability_final,
         period_of_publication,
         reason_for_decline) %>% 
  ddply(.(discipline,data_availability_final),summarize,N=table(discipline)) #%>% 
#pivot_wider(names_from = data_availability_initial,values_from = N)
rass$data_availability_final <- stringr::str_replace(rass$data_availability_final, 'n.a.','full')

ggplot(rass,
       aes(y=N, axis1 = discipline, axis2 = data_availability_final)) +
  geom_alluvium(aes(fill = data_availability_final), width = 1/12,show.legend = F, knot.pos = .4, alpha =.7) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Discipline", "Data\n availability"), expand = c(.05, .05)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(title = "ástæður", subtitle = "ástæður fyrir neitun", caption = "Tedersoo 2021") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_line(size = .7),
        axis.text.y = element_text(face = 'bold'),
        axis.text.x = element_text(size = 14, colour = 'black', face = 'bold', margin = margin(t=0,r=0,b=5,l=0)),
        axis.line.y = element_line(colour = 'black', size = .7),
        plot.title = element_text(size = 16, face = 'bold'))

#######################
tafla <- read.csv("docs/tedersoo2021/tafla.csv")
library(plyr)
library(tidyverse)
rass <- tafla 
rass$data_availability_final <- stringr::str_replace(rass$data_availability_final, 'n.a.','full')
rass <- rass%>% 
  filter(reason_for_decline!="",
         data_availability_final!='full') %>% 
  select(discipline, 
         data_availability_initial,
         data_availability_from_corresponding_author,
         data_availability_final,
         period_of_publication,
         reason_for_decline) %>% 
  ddply(.(discipline,data_availability_final,reason_for_decline),summarize,N=table(discipline))


rass$nyr <- rass$N
rass$nyr[grep("agreement|privacy",rass$reason_for_decline)] <- "privacy"
rass$nyr[grep("time",rass$reason_for_decline)] <- "no time"
rass$nyr[grep("lost",rass$reason_for_decline)] <- "lost"
rass$nyr[grep("[:digit:]",rass$nyr, invert = T)] <- "other"
rass$discipline[grep("Materials_for_Energy_and_Catalysis",rass$discipline)] <- "MEC"
rass$discipline[grep("Social_sciences",rass$discipline)] <- "Social\nsciences"


library(harrypotter)
pal <- hp(n = 4, house = "NewtScamander")

library(ggalluvial)
ggplot(rass,
       aes(y=N, axis1 = discipline, axis2 = nyr)) +
  geom_alluvium(aes(fill = nyr), width = 1/12,show.legend = F, knot.pos = .4, alpha =.7, size = 16) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Fög", "Ástæður"), expand = c(.05, .05)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(title = "Ástæður", subtitle = "fyrir neitun á afhendingu gagna", caption = "Tedersoo 2021") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_line(size = .7),
        axis.text.y = element_text(face = 'bold'),
        axis.text.x = element_text(size = 14, colour = 'black', face = 'bold', margin = margin(t=0,r=0,b=5,l=0)),
        axis.line.y = element_line(colour = 'black', size = .7),
        plot.title = element_text(size = 16, face = 'bold'))

ggsave("myndir/reason.png",height = 7,width=12)



https://corybrunson.github.io/ggalluvial/reference/alluvial-data.html




















library(tidyr)
library(dplyr)
library(networkD3)

tafla <- read.csv("docs/tedersoo2021/tafla.csv")
library(plyr)
library(tidyverse)
rass <- tafla 
rass$data_availability_final <- stringr::str_replace(rass$data_availability_final, 'n.a.','full')
rass <- rass%>% 
  #filter(reason_for_decline!="",
  #       data_availability_final!='full') %>% 
  select(discipline, 
         data_availability_initial,
         data_availability_from_corresponding_author,
         data_availability_final,
         period_of_publication,
         reason_for_decline) %>% 
  ddply(.(discipline,data_availability_final,reason_for_decline),summarize,N=table(discipline))


rass$nyr <- rass$N
rass$nyr[grep("agreement|privacy",rass$reason_for_decline)] <- "privacy"
rass$nyr[grep("time",rass$reason_for_decline)] <- "no time"
rass$nyr[grep("lost",rass$reason_for_decline)] <- "lost"
rass$nyr[grep("[:digit:]",rass$nyr, invert = T)] <- "other"
rass$discipline[grep("Materials_for_Energy_and_Catalysis",rass$discipline)] <- "MEC"
rass$discipline[grep("Social_sciences",rass$discipline)] <- "Social\nsciences"

rass$nyr <-  as.character(rass$nyr)

links <-
  rass[,-c(3,4)] %>% 
  mutate(row = row_number()) %>%  # add a row id
  pivot_longer(-row, names_to = "col", values_to = "source") %>%  # gather all columns
  mutate(col = match(col, names(df))) %>%  # convert col names to col ids
  #mutate(source = paste0(source, '_', col)) %>%  # add col id to node names
  group_by(row) %>%
  mutate(target = lead(source, order_by = col)) %>%  # get target from following node in row
  ungroup() %>% 
  filter(!is.na(target)) %>%  # remove links from last column in original data
  group_by(source, target) %>% 
  summarise(value = n(), .groups = "drop")  # aggregate and count similar links

# create nodes data frame from unque nodes found in links data frame
nodes <- data.frame(id = unique(c(links$source, links$target)),
                    stringsAsFactors = FALSE)
# remove column id from node names
nodes$name <- sub('_[0-9]*$', '', nodes$id)

# create node ids in links data to the 0-based index of the nodes in the nodes data frame
links$source_id <- match(links$source, nodes$id) - 1
links$target_id <- match(links$target, nodes$id) - 1

sankeyNetwork(Links = links, Nodes = nodes, Source = 'source_id',
              Target = 'target_id', Value = 'value', NodeID = 'name')
