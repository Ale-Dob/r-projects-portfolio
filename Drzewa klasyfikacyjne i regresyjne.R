install.packages("rpart")
install.packages("rpart.plot")


library(rpart)
library(rpart.plot)
data("ptitanic")

drzewo <- rpart(
  formula = survived ~ pclass + sex,#zmienna
  data = ptitanic #argument
) # funkcja do tworzenia drzewa

drzewo2 <- rpart(
  formula = survived ~ ., #kropka oznacza wszystkie zmienne
  data = ptitanic 
)
rpart.plot(drzewo2)

#funkcja ma 'limit' zeby model nie uczył sie w nieskończonosc ->przeuczenie
#potrzeba zatrzymac

drzewo3 <- rpart(
  formula = survived ~ ., 
  data = ptitanic ,
  control = rpart.control(cp = 0 ) #słuzy do zatrzymania drzewa
  #cp mowi o ile zmniejszy sie błąd o część błedy wejsciowego...
  #cp = 0 -> dziel podzbiory jesli zysk jest dowolnie mały
  #wyjsciowo cp = 0,02
  #cp może byc od 0 do 1
  #zeby sie nie przeuczył - zeby byl przydatky predykcyjnie
  
)
rpart.plot(drzewo3)

#wymyslamy osobe:
osoba <- data.frame(
  pclass = "2nd",
  sex = "female",
  age = 37,
  sibsp = 1,
  parch = 0
)

#predykcja dla obiektu osoba w narzym drzewie:
predict(object = drzewo, newdata = osoba)

class(factor(ptitanic$pclass, order=T)) # nie zmienia klasy zmiennej ale ustala, ze jest znacząca (?), drzewo dzielone tez wg niej
