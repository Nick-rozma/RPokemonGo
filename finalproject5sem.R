library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(corrplot)
library(car)
library(reshape2)
library(pheatmap)
library(reshape2)
library(nnet)
library(foreign)
PoGo <- read.csv('./300k.csv')
head(PoGo)
dim(PoGo)
sum(is.na(PoGo))

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$appearedMinute)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$appearedHour)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$appearedDay)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$terrainType)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$temperature)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$windSpeed)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$windBearing)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$population_density)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(windshp$gymDistanceKm)

windshp <- PoGo%>%
  slice_sample(n = 4999)
shapiro.test(as.numeric(windshp$pokestopDistanceKm))

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = PoGo, aes(x = longitude, y = latitude)) +
  
  
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Pokemon Encounters")+
  
  borders("world", colour="black", fill="#cfb53b")+
  theme(panel.background = element_rect(fill ="#2f2c0a"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  
  geom_point(color="#f30b7b", alpha = .2)

unique(PoGo$appearedYear)

unique(PoGo$appearedMonth)

unique(PoGo$appearedDay)

ggplot(data = PoGo, aes(x = weatherIcon, y = pokemonId))+
  geom_point()

g1 <- PoGo %>%
  group_by(weatherIcon)%>%
  summarise(n = n())
g1

weather_and_poke <- PoGo%>%
  select(pokemonId, weatherIcon)
head(weather_and_poke)

dim(weather_and_poke)

all_poke <- read.csv("./All_Pokemon.csv")%>%
  rename(pokemonId = Number)%>%
  select(pokemonId, Type.1, Type.2, Name)%>%
  filter(pokemonId <= 151)%>%
  filter(!grepl("Mega ", Name))%>%
  filter(!grepl("Alolan", Name))%>%
  filter(!grepl("Galarian", Name))
head(all_poke)

dim(all_poke)

weather_type <- merge(x = weather_and_poke, y = all_poke, by="pokemonId", all.x=TRUE)
head(weather_type)

dim(weather_type)

sum(is.null(weather_type$Type.2)) 

sum(is.na(weather_type$Type.2))

sapply(X = weather_type, FUN = function(x) sum(is.na(x)))

map(weather_type, ~sum(is.na(.)))

weather_type[103103, 4]

coalesc <- weather_type%>%
  mutate(Type.2 = coalesce(Type.2, Type.1))

head(coalesc)

long_w_t <- weather_type %>%
  pivot_longer(cols = Type.1:Type.2, names_to = "nTypes", values_to = "Types")%>%
  filter(Types != '')
head(long_w_t)


dim(long_w_t)
unique(long_w_t$Types)
tabl <- table(long_w_t$weatherIcon, long_w_t$Types)
tabl

vectabl <- as.vector(tabl)
vectabl
mattabl <- matrix(data = vectabl, byrow = FALSE, nrow = 8)
mattabl
colnms <- c("Bug", "Dragon", "Electric", "Fairy", "Fighting",  "Fire", "Flying", "Ghost","Grass", "Ground",   "Ice", "Normal", "Poison", "Psychic",  "Rock", "Steel", "Water")
rownms <- c("clear-day", "clear-night", "cloudy", "fog", "partly-cloudy-day", "partly-cloudy-night", "rain", "wind")
rownames(mattabl) <- rownms
colnames(mattabl) <- colnms
mattabl

dftabl <- as.data.frame(mattabl) 
dftabl
as.numeric(as.vector(dftabl[1,]))
ks.test(as.numeric(as.vector(dftabl[1,])), as.numeric(as.vector(dftabl[2,])))


exitmatrix <- matrix(, nrow = 8, ncol = 8)
rownames(exitmatrix) <- rownms
colnames(exitmatrix) <- rownms

for(i in 1:8){
  for(j in 1:8){
    exitmatrix[i, j] <- ks.test(as.numeric(as.vector(dftabl[i,])), as.numeric(as.vector(dftabl[j,])),exact=FALSE)$p.value
  }
}

exitmatrix
as.numeric(exitmatrix)
adj_pvals <- p.adjust(as.numeric(exitmatrix), method = 'holm')
adj_pvals
adjmat <- matrix(data = adj_pvals, byrow = FALSE, nrow = 8)
rownames(adjmat) <- rownms
colnames(adjmat) <- rownms
adjmat
adjexitdf <- as.data.frame(adjmat)
adjexitdf
bk1 <- c(seq(0, 0.049, by = 0.001))
bk2 <- c(seq(0.05, 1, by = 0.01))
bk <- c(bk1, bk2)


my_palette <- c(colorRampPalette(colors = c("#ff204e", "#fe1aa4"))(n = length(bk1)-1),
                c(colorRampPalette(colors = c("#bcb227","#2f2c0a" ))(n = length(bk2)-1)))



pheatmap(adjmat, cluster_cols = TRUE, cluster_rows = TRUE, color = my_palette, breaks = bk)
g2 <- PoGo%>%
  select(pokemonId, starts_with("cooc"))
head(g2)
gg2 <- g2%>%
  mutate(across(where(is.character), as.logical))%>%
  mutate(across(where(is.logical), as.numeric))

head(gg2)
correl <- cor(gg2)
png("Plot3.png", width = 4, height = 4, units = 'in', res = 900)
corrplot(correl, method = 'color', number.cex=0.75, tl.cex=0.2)
dev.off()
corrplot(correl, method = 'color', number.cex=0.75, tl.cex=0.2)
corr <- cor(gg2)
corr[lower.tri(corr,diag=TRUE)] <- NA 

corr[corr == 1] <- NA  

corr <- as.data.frame(as.table(corr))

corr <- na.omit(corr)   


corr <- subset(corr, abs(Freq) > 0.5) 

corr <- corr[order(-abs(corr$Freq)),] 

corr 
model <- lm(pokemonId ~., data = gg2)
summary(model)
plot(model, 1)
plot(model, 2)
plot(model, 3)



pres <- PoGo%>%
  select(pokemonId, pressure)
head(pres)

press_type <- merge(x = pres, y = all_poke, by="pokemonId", all.x=TRUE)
head(weather_type)
dim(weather_type)

long_p_t <- press_type %>%
  pivot_longer(cols = Type.1:Type.2, names_to = "nTypes", values_to = "Types")%>%
  filter(Types != '')
head(long_p_t)

dim(long_p_t)

grass <- long_p_t%>%
  filter(Types=='Grass')%>%
  slice_sample(n = 4999)
shapiro.test(grass$pressure)
mean(long_p_t$pressure)

long_p_t$prog <- relevel(factor(long_p_t$Types), ref = 'Grass')
head(long_p_t)
test <- multinom(prog~pressure, data = long_p_t)
sum <- summary(test)
sum
z <- sum$coefficients/sum$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p