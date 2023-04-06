#OPTYMALIZACJA NIELINIOWA
#PROJEKT 1
#ZLOTEK SEBASTIAN
#https://www.kaggle.com/datasets/ravisane1/market-price-of-onion-2020

#Wybranie katalogu glownego
getwd()
setwd("C:/Users/Sebastian/Desktop/studia/III rok 1 semestr/Optymalizacja_nieliniowa_PROJEKT")

#Usuniecie notacji wykladniczej
options(scipen = 999)

#Wczytanie danych
onion <- read.csv("onion2020.csv", sep = ',')

#Sortowanie danych wedlug stanu oraz rodzaju cebuli
onionsorted <- onion[which(onion$state == 'Gujarat' & onion$variety == 'White'),]

#Usuniecie duplikatow dat, dla uzyskania lepszego wykresu
onionsorted <- onionsorted[!duplicated(onionsorted[6]),]

#Utworzenie listy z cenami
onion_modal <- onionsorted$modal_price
View(onion_modal)

#Wykres cen
plot( x = c(1:131), y=onion_modal, col = "black", type = "l", xlim = c(0,90))

#Utworzenie ramki danych
onion_data <- data.frame( x = c(1:131), y = onion_modal )

#Utworzenie linii regresji 10 - potega przy x
onion_reg10 <- lm( y~poly( x, 10, raw = TRUE ), data = onion_data )

#Sekwencja danych
onionseq <- seq( 1, 131, length = 131 )

#Wyświetlenie linii regresji na wykresie (kolor czerwony)
lines( onionseq, predict( onion_reg10, data.frame(x = onionseq) ), col = 'red' )

#Obliczenie procentu podobienstwa regresji do danych
summary(onion_reg10)$adj.r.squared

#Opis regresji
summary(onion_reg10) 

#Funkcja utworzona z summary regresji
defonion <- function(x){ (0.00000000000002937) * (x^10) + (-0.00000000002191333) * (x^9) + (0.00000000690730167)*(x^8)+
    (-0.00000120000276833) * (x^7) + (0.00012566485574807) * (x^6) + (-0.00814851318300418) * (x^5) + (0.32364959235614527) *(x^4) + (-7.52946661137093365) * (x^3) + (94.61588510468364177) * (x^2) + ((-590.76471308716702424) * x) + (3520.66787771067311041)
}

#Wyswietlam funkcje
plot( x = c(1:131), y=onion_modal, col = "black", type = "l", xlim = c(0,90))

lines( onionseq, defonion(x = onionseq), col = "blue")

#algorytm golden podstawowy - minimum
golden <- function(f,lower, upper, tol){
  czas1 <- Sys.time()
  alpha <- (sqrt(5)-1)/2
  x1 <- alpha * lower + (1-alpha) * upper
  f.x1 <- f(x1)
  x2 <- (1-alpha) * lower + alpha * upper
  f.x2 <- f(x2)
  
  i<-0
  cat("iteracja:",i,"a=",lower,"b=",upper,"b-a=",upper-lower,"\n")
  
  while(abs(upper - lower) > 2 * tol){
    i<-i+1
    if (f.x1 < f.x2){
      upper <- x2
      x2 <- x1
      f.x2 <- f.x1
      x1 <- alpha * lower + (1-alpha) * upper
      f.x1 <- f(x1)
    } else {
      lower <- x1
      x1 <- x2
      f.x1 <- f.x2
      x2 <- (1-alpha) * lower + alpha * upper
      f.x2 <- f(x2)
    }
    cat("iteracja:",i,"a=",lower,"b=",upper,"b-a=",upper-lower,"\n")
  }
  czas2 <- Sys.time()
  cat("Czas działania: ",czas2-czas1,"\n")
  return((lower + upper) / 2)
}

#Wartość minimalna golden
golden(defonion,22,38,0.000001)

#Algorytm golden podstawowy - maximum
golden_max <- function(f,lower, upper, tol){
  czas1 <- Sys.time()
  alpha <- (sqrt(5)-1)/2
  x1 <- alpha * lower + (1-alpha) * upper
  f.x1 <- f(x1)
  x2 <- (1-alpha) * lower + alpha * upper
  f.x2 <- f(x2)
  
  i<-0
  cat("iteracja:",i,"a=",lower,"b=",upper,"b-a=",upper-lower,"\n")
  
  while(abs(upper - lower) > 2 * tol){
    i<-i+1
    if (f.x1 > f.x2){
      upper <- x2
      x2 <- x1
      f.x2 <- f.x1
      x1 <- alpha * lower + (1-alpha) * upper
      f.x1 <- f(x1)
    } else {
      lower <- x1
      x1 <- x2
      f.x1 <- f.x2
      x2 <- (1-alpha) * lower + alpha * upper
      f.x2 <- f(x2)
    }
    cat("iteracja:",i,"a=",lower,"b=",upper,"b-a=",upper-lower,"\n")
  }
  czas2 <- Sys.time()
  cat("Czas działania: ",czas2-czas1,"\n")
  return((lower + upper) / 2)
}

#Wartosc maksymalna golden
golden_max(defonion,44,60,0.000001)

#Algorytm siecznych - minimum
secant <- function(df, x1, x2, tol) {
  i <- 0
  cat("Iteracja: ",i,"x=",x1,"\n")
  df.x2 <- df(x2)
  repeat {
    i<-i+1
    df.x1 <- df(x1)
    new.x <- x1 - df.x1 * (x1 - x2) / (df.x1 - df.x2)
    
    cat("Iteracja: ",i,"x=",x1,"krok=",- df.x1 * (x1 - x2) / (df.x1 - df.x2),"\n")
    
    if (abs(new.x - x1) < tol) {
      return(new.x)
    }
    x2 <- x1
    df.x2 <- df.x1
    x1 <- new.x
  }
}

dfdefonion <- function(x){ (0.0000000000002937) * (x^9) + (-0.00000000019722) * (x^8) + (0.0000000552584)*(x^7)+
    (-0.00000840001937831) * (x^6) + (0.00075398913448842) * (x^5) + (-0.0407425659150209) * (x^4) + (1.2945983694245811) *(x^3) + (-22.5883998341128010) * (x^2) + (189.2317702093672835) * (x) - 590.76471308716702424
}

#Wartosc minimalna z uzyciem algorytmu siecznych
secant(dfdefonion,28,38,0.000001)

























