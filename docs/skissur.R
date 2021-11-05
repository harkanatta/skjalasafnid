tafla <- read.csv("docs/tedersoo2021/tafla.csv")

tafla %>% 
  ddply(.(author_familiarity,data_availability_from_corresponding_author),summarise,N=table(data_availability_from_corresponding_author))

tafla %>% 
  ddply(.(tafla$data_availability_final,data_availability_from_corresponding_author),summarise,N=table(data_availability_from_corresponding_author))


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