install.packages("tidyverse")
library(tidyverse)
library(scales)
library("ggplot2")
library(scales)

load("filmy.RData")

#Zadania:
#1. Przedstawić rozkład zmiennej avg_vote (średnia ocena filmu przez użytkowników)
#pakiet graphics
barplot(height = table(filmy$avg_vote),
        main = "Średnia ocena filmu przez użytkowników",
        xlab = "Ocena", ylab = "Liczba ocen",
        space = 0.1,
        col = "gold") 

#pakiet ggplot2
ggplot(filmy, aes(avg_vote)) +
  geom_histogram(bins = 90, fill = "gold", color = "black")+
  labs(
    title = "Rozkład ocen filmów",
    x = "Ocena",
    y = "Liczba ocen"
  )
 

#.........................................................................
#2. Przedstawić rozkład zmiennej genre (gatunek filmu)

genre_count <- table(filmy$genre)
genre_count <- sort(genre_count, decreasing = T)

#pakiet graphics
barplot(sort(genre_count),
        main = "Rozkład gatunków filmów",
        xlab = "Liczba filmów",
        names.arg = names(genre_count),
        las = 1, 
        horiz = T, 
        col = rainbow(nrow(genre_count), start = 1, end = 0.4))

#pakiet ggplot2
genre_count <- as.data.frame(table(filmy$genre))
colnames(genre_count) <- c("genre", "count")

ggplot(genre_count, aes(x = count, y = genre)) +
  geom_col(col = "black",  fill = rainbow(nrow(genre_count), start = 1, end = 0.4)) +
  labs(
    title = "Rozkład gatunków filmów",
    x = "Gatunek",
    y = "Liczba filmów"
  )

#..................................................................................
#3. Przedstawić zależność między zmiennymi votes (liczba ocen użytkowników) i avg_vote (średnia ocena)
#pakiet graphics
  plot(x = filmy$votes, 
       y = filmy$avg_vote,
       col="red",
       main = "Zależność między liczbą a średnią ocen",
       xlab = "Liczba ocen",
       ylab = "Średnia ocen")
  

#pakiet ggplot2
  ggplot(data = filmy, mapping = aes(votes, avg_vote)) +
  geom_point(color = "red") +
   labs(title = "Zależność między liczbą a średnią ocen ",
       x = "Liczba ocen",
       y = "Średnia ocen")+
   theme(legend.position = "bottom")
  
  
#..................................................................................
#4. Przedstawić łączny rozkład zmiennych genre (gatunek) i country (kraj produkcji)
#pakiet graphics
  plot(x = table(filmy$country, filmy$genre),
       las = 2,
       main = "Rozkład gatunków filmów w zależności od kraju produkcji",
       xlab = "Kraj",
       ylab = "Gatunek")
  

#pakiet ggplot2
  ggplot(filmy, aes(genre, country)) + 
    geom_count()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#..................................................................................
  
#5. Przedstawić rozkład zmiennej avg_votes (średnia ocena) w zależności od zmiennej genre (gatunek)
#pakiet graphics
  boxplot(avg_vote ~ genre,
          data = filmy,
          col = "gold",
          main = "Rozkład ocen w zależności od gatunku filmu",
          xlab = "Gatunek",
          ylab = "Średnia ocena",
          las = 2)
  
#pakiet ggplot2
  ggplot(filmy, aes(x = genre, y = avg_vote )) +
    geom_boxplot(fill = "gold", color = "black") +
    labs(
      title = "Rozkład ocen w zależności od gatunku filmu",
      x = "Gatunek",
      y = "Średnia ocena"
    )+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  