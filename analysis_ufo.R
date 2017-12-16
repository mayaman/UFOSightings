# Call in libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

# load map data
states <- map_data("county")
usa <- map_data("usa")

# read in data, take out unnecessary rows
pop = read.csv("state_pops.csv")[,1:3]
data = read.csv("new_ufo_dat.csv")
data = data[-43775,]

# prepare data to create pop density plot
data.bystate = filter(data, Country == "us") %>% group_by(State)
data.bystate = filter(data.bystate, State != "hi")
data.bystate = filter(data.bystate, State != "ak")
state.count = data.bystate %>% count(State)
merged = merge(pop, state.count, by = "State")
merged$HundredThPpl = merged$pop/100000
merged$Density = merged$n/merged$HundredThPpl
all = merge(states, merged, by.x = "region", by.y = "StateFull")

# plot the density by state
p <- ggplot()
p <- p + geom_polygon(data=all, aes(x=long,y=lat,group=group,fill=all$Density), colour = "white") +
  scale_fill_continuous(low = "thistle2", high = "darkred", guide = "colorbar")
p

# plot the location of sightings
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  coord_fixed(1.3) +  geom_point(data = data.bystate, aes(x = Longitude, y = Latitude), color = "red", size = 0.1)


# kmeans clustering
fit.km <- kmeans(data[,10:11], 100, 1000)
data$cluster <- fit.km$cluster

# plot world points
world <- map_data("world")
ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) +  geom_point(data = data, aes(x = Longitude, y = Latitude), color = "red", size = 0.1)


# Filter to get lower 48 
US <- filter(data, State != "")
US <- filter(US, State != "ak")
US <- filter(US, State != "hi")
US <- filter(US, Latitude < 50)
US <- filter(US, Latitude > 20)
US <- filter(US, Longitude > -130)
US <- filter(US, Longitude < -65)

# plot density
p <- ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "black") + coord_fixed(1.3)
overlay <- geom_density2d(data = US, aes(x = Longitude, y = Latitude))
p + overlay + stat_density2d(data = US, aes( x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0.00, 0.30), guide = FALSE) + ggtitle("Density of Sightings Across the United States")

# Combine shapes to add up their frequencies
unknown <- filter(data, Shape %in% c("", "other", "unknown"))
change <- filter(data, Shape %in% c("changed", "changing"))
triangle <- filter(data, Shape %in% c("chevron", "delta", "triangle", "cone", "pyramid"))
cylinder <- filter(data, Shape %in% c("cigar", "cylinder", "rectangle"))
circle <- filter(data, Shape %in% c("circle", "disk", "dome", "egg", "hexagon", "oval", "round", "sphere", "teardrop"))
light <- filter(data, Shape %in% c("flare", "flash", "light"))
fireball <- filter(data, Shape %in% c("fireball"))
formation <- filter(data, Shape %in% c("formation"))


# new mexico sightings
nm = filter(data, State == "nm")
nm = filter(nm, Latitude > 30)

nm.region <- subset(states, region %in% c("new mexico"))
ggplot(data = nm.region) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3) + 
  geom_point(data = nm, aes(x=Longitude,y=Latitude)) +
  geom_point(aes(x=-105.243,y=33.968), col="red", size=7) +
  ggtitle("New Mexico Sightings (Roswell in Red)")

# southern california region sightings
ca.region <- subset(states, region %in% c("california"))
ca.region.southern <- subset(ca.region, subregion %in% c("san luis obispo", "san bernardino", "santa barbara", "los angeles", "orange", "riverside","imperial","san diego","kern", "ventura"))

california <- filter(US, Latitude < 36)
california <- filter(california, Latitude > 32)
california <- filter(california, Longitude < -114)
california <- filter(california, Longitude > -122)

ggplot(data = ca.region.southern) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color="black") +
  coord_fixed(1.3) +
  geom_point(data = california, aes(x=Longitude,y=Latitude)) +
  geom_point(aes(x=-117.7118,y=34.0977),col = "blue", size = 8) +
  geom_point(aes(x=-117.7118,y=34.0977),col = "orange", size = 7) +
  ggtitle("Sightings in Southern California (Claremont in orange)")

# area51
nv = filter(data, State == "nv")
nv = filter(nv, Longitude < -100)

nv.region <- subset(states, region %in% c("nevada"))
ggplot(data = nv.region) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3) + 
  geom_point(data = nv, aes(x=Longitude,y=Latitude)) +
  geom_point(aes(x=-115.811111,y=37.235), col="red", size=3) +
  ggtitle("Nevada Sightings (Area 51 in Red)")

# plant 42
ca.region <- subset(states, region %in% c("california"))
ca.region.southern <- subset(ca.region, subregion %in% c("san luis obispo", "san bernardino", "santa barbara", "los angeles", "orange", "riverside","imperial","san diego","kern", "ventura"))

plant42 = filter(data, State == "ca")
plant42 <- filter(plant42, Latitude < 36)
plant42 <- filter(plant42, Latitude > 32)
plant42 <- filter(plant42, Longitude < -114)
plant42 <- filter(plant42, Longitude > -122)

ggplot(data = ca.region.southern) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color="black") +
  coord_fixed(1.3) +
  geom_point(data = plant42, aes(x=Longitude,y=Latitude)) +
  geom_point(aes(x=-117.7118,y=34.0977),col = "blue", size = 3) +
  geom_point(aes(x=-117.7118,y=34.0977),col = "orange", size = 2) +
  ggtitle("Sightings near Plant42 (Plant42 in orange)")


# plot by cluster
g <- ggplot(data, aes(y=Latitude,x=Longitude,color=as.factor(cluster))) + geom_point()

g <- g + geom_point(aes(y=Latitude,x=Longitude))
g

# Prepare data for Maya, redo kmeans, use lots of itr to converge to decent soln
data.time <- filter(data, Year < 2018)

fit.km <- kmeans(data.time[,10:11], centers = 500, iter.max = 100000)

totals <- c()
for(i in 1:500) {
  totals[i] = table(fit.km$cluster)[i]
}
maya.dat <- as.data.frame(fit.km$centers)
maya.dat$Magnitude <- totals
# write.csv maya.dat for her use

# Topic Modeling
library(tidytext)
library(tm)
library("tmap")
library("tmaptools")
library("sf")
library("leaflet")
library("scales")
library(topicmodels)
library(NLP)
library(ggstance)
library(ggthemes)
#library(broom)
text_corpus <- Corpus(VectorSource(as.vector(data$Report)))
text_corpus <- tm_map(text_corpus, content_transformer(removePunctuation))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, content_transformer(stripWhitespace))
text_corpus <-  tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, content_transformer(stemDocument), language="english")

DTM <- DocumentTermMatrix(text_corpus, control= list(wordLengths = c(2, Inf)))
DTM <- removeSparseTerms(DTM, .990)

rowTotals <- apply(DTM, 1, sum)
DTM <- DTM[rowTotals > 0, ]

(freq.terms <- findFreqTerms(DTM, lowfreq=15))
term.freq <- rowSums(as.matrix(DTM))
term.freq <- subset(term.freq, term.freq >=5)
df2 <- data.frame(term = names(term.freq), freq = term.freq)

numTopics <- 5


#Run Latent Dirichlet Allocation (LDA) using Gibbs Sampling
#set burn in
burnin <-4
#set iterations
iter<-20
#thin the spaces between samples
thin <- 5
#set random starts at 5
nstart <-4
#use random integers as seed 
seed <- list(22,12,48843,48314)
# return the highest probability as the result
best <-TRUE
#set number of topics 
k <-5
#run the LDA model
ldaOut <- LDA(DTM,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

gen_topics <- as.matrix(topics(ldaOut))
gen_terms <- as.matrix(terms(ldaOut, 10))

tidy_lda <- tidy(ldaOut)
tidy_lda

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

lda_gamma <- tidy(ldaOut, matrix = "gamma")
lda_gamma

ggplot(top_terms, aes(beta, term, fill = as.factor(topic))) +
  geom_barh(stat = "identity", show.legend = FALSE, alpha = 0.8) +
  labs(title = "Top 10 Terms in Each LDA Topic",
       y = NULL, x = "beta") +
  facet_wrap(~topic, ncol = 2, scales = "free") +
  theme_tufte(base_size = 13, ticks = FALSE) +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(plot.caption=element_text(size=9))

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~topic, ncol = 3) +
  scale_y_log10() +
  labs(title = "Distribution of Probability for Each Topic",
       y = NULL, x = "gamma") +
  theme_minimal(base_size = 13) +
  theme(strip.text=element_text(hjust=0)) +
  theme(plot.caption=element_text(size=9))
