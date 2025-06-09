## Ćwiczenie 5.1: Klasa S3 "track" dla tras rowerowych

# Wymaga pakietów:
#install.packages(c("trackeR","leaflet"))
library(trackeR) # Pakiet trackeR służy m.in. do wczytywania śladów GPS z plików TCX.
library(leaflet) # do tworzenia interaktywnych map internetowych
library(htmlwidgets) # framework (szkielet) do tworzenia interaktywnych wizualizacji internetowych (tzw. widgetów HTML) z pakietów R

# Oblicza odległość między dwoma punktami na kuli ziemskiej (w metrach) według wzoru haversine:

#Wzór Haversine'a:
#    1. Konwertowanie stopni na radiany: Szerokości i długości geograficzne (lat1, lon1, lat2, lon2) należy przekonwertować z stopni na radiany.
#    2. Obliczanie różnic: Oblicza się różnice między szerokościami i długościami geograficznymi dwóch punktów (dLat, dLon).
#    3. Obliczanie połowy kąta: Wykorzystuje się wzór haversine dla połowy kąta między dwoma punktami (a).
#    4. Obliczanie kąta środkowego: Oblicza się kąt środkowy (c) między dwoma punktami.
#    5. Obliczanie odległości:Odległość (d) między dwoma punktami oblicza się mnożąc kąt środkowy (c) przez promień Ziemi (R).

haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6371000  # promień Ziemi w metrach
  #Promień Ziemi nie jest wszędzie jednakowy, ponieważ Ziemia nie jest idealną kulą, a spłaszczoną elipsoidą. Najczęściej podaje się średni promień Ziemi, który wynosi około 6 371 000 metrów.
  to_rad <- pi / 180
  phi1 <- lat1 * to_rad
  phi2 <- lat2 * to_rad
  delta_phi <- (lat2 - lat1) * to_rad
  delta_lambda <- (lon2 - lon1) * to_rad
  a <- sin(delta_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# Funkcja pomocnicza do nadawania tytułu na podstawie czasu rozpoczęcia
getTitle <- function(ride) {
  start_time <- ride$time[1]
  hour <- as.numeric(format(start_time, "%H"))
  if(hour >= 5 && hour < 12) {
    "Morning ride"
  } else if(hour >= 12 && hour < 18) {
    "Afternoon ride"
  } else if(hour >= 18 && hour < 23) {
    "Evening ride"
  } else {
    "Night ride"
  }
}

# Konstruktor klasy "track"
track <- function(file, title = NULL) { 
  ride <- readTCX(file)                 # Wczytanie ścieżki z pliku TCX ( wczytuje dane GPS (czas, długość/szerokość geogr., wysokość, prędkość itp.). )
  ride <- ride[!duplicated(ride$time), ] # Usunięcie zduplikowanych wierszy względem czasu
 
  if(missing(title) || is.null(title)) {  # Ustalenie tytułu
    title <- getTitle(ride)
  }
  # Zachowaj również ścieżkę pliku, by wiedzieć, gdzie zapisywać wykresy
  filepath <- normalizePath(file)
  obj <- list(ride = ride, title = title, filepath = filepath)
  class(obj) <- "track"                 # Nadaj klasę S3
  obj
}

# Metody generyczne dla klasy "track"
# Wyświetla krótki opis:
print.track <- function(x, ...) {
  cat("Track: ", x$title, "\n", sep = "")
  cat("Liczba punktów: ", nrow(x$ride), "\n")
  invisible(x)
}
# Zwracają pierwsze/ostatnie wiersze ramki danych ride.
head.track <- function(x, n = 6, ...) {
  head(x$ride, n)
}

tail.track <- function(x, n = 6, ...) {
  tail(x$ride, n)
}

# Drukuje podsumowanie:
# zakres czasu (start/koniec)
# łączną odległość (sumując kolejne odcinki via haversine)
# min/max wysokość
summary.track <- function(object, ...) {
  df <- object$ride
  cat("Summary for track: ", object$title, "\n")
  # Zakres czasu
  start_time <- format(min(df$time), "%Y-%m-%d %H:%M:%S")
  end_time   <- format(max(df$time),   "%Y-%m-%d %H:%M:%S")
  cat(sprintf("Time range: %s – %s\n", start_time, end_time))
  # Obliczenie dystansu
  coords <- df[, c("longitude", "latitude")]
  if (nrow(coords) > 1) {
    lon <- coords[,1]; lat <- coords[,2]
    dists <- haversine(lon[-length(lon)], lat[-length(lat)], lon[-1], lat[-1])
    total_km <- sum(dists) / 1000
  } else {
    total_km <- 0
  }
  cat(sprintf("Distance (approx.): %.2f km\n", total_km))
  # Wysokość
  cat(sprintf("Elevation (min/max): %.1f/%.1f m\n",
              min(df$altitude, na.rm = TRUE), max(df$altitude, na.rm = TRUE)))
  invisible(object)
}

# Akcesory: ride() i trackTitle()
ride <- function(x) {
  if(!inherits(x, "track")) stop("Nie jest obiektem.") # zwracanie błędu gdy nie jest obiektem.
  x$ride # zwraca całą ramkę danych z GPS (pole ride).
}

trackTitle <- function(x) {
  if(!inherits(x, "track")) stop("Nie jest obiektem.")
  x$title # zwraca tytuł trasy.
}

# Setter dla tytułu (alternatywna nazwa):
trackTitle<- function(x, value) {
  if(!inherits(x, "track")) stop("Nie jest obiektem.")
  x$title <- value # pozwala zmienić tytuł:
  x
}

# Generyczna prędkość
velocity <- function(x, ...) {
  UseMethod("velocity")
}

# Generyk dla mapy
map <- function(x, ...) {
  UseMethod("map")
}

velocity.track <- function(x, ...) {
  df <- x$ride
  # obliczamy dystanse i różnice czasów pomiędzy kolejnymi punktami
  coords <- df[, c("longitude", "latitude")]
  lon1 <- coords[-nrow(coords), 1]
  lat1 <- coords[-nrow(coords), 2]
  lon2 <- coords[-1, 1]
  lat2 <- coords[-1, 2]
  distances <- haversine(lon1, lat1, lon2, lat2)
  times <- as.numeric(difftime(df$time[-1], df$time[-nrow(df)], units = "secs"))
  n <- min(length(distances), length(times))
  v <- rep(NA, nrow(df))
  v[2:(n+1)] <- distances[1:n] / times[1:n] # w m/s
  v
} # Zwraca wektor prędkości (m/s), z wartością NA w pierwszym punkcie (brak poprzednika).

# Metoda plot dla klasy track (Pozwala na wygenerowanie wykresu)
# Automatyczny zapis do pliku (PNG/JPEG/PDF) w katalogu źródłowym:
# Budowanie nazwy pliku na podstawie tytułu i znacznika czasu.
# Otwarcie odpowiedniego urządzenia graficznego (png(), jpeg(), pdf()).
# Przygotowanie wektora osi X (czas lub skumulowany dystans).
# Przygotowanie wektora osi Y (wysokość lub prędkość w km/h).
# Wywołanie plot(...) i zamknięcie urządzenia dev.off().
plot.track <- function(x,
                       type = c("elevation", "speed"),
                       xaxis = c("time", "distance"),
                       file = NULL,
                       width = 800, height = 600, res = 96,
                       ...)  {
  type <- match.arg(type)
  xaxis <- match.arg(xaxis)
  
  # Ustal katalog bazowy jako katalog pliku wejściowego
  base_dir <- dirname(x$filepath)
  
  # Generowanie nazwy pliku: jeśli nie podano, użyj tytułu i znacznika czasu
  if (is.null(file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    safe_title <- gsub("[^A-Za-z0-9]", "_", x$title)
    filename <- paste0(safe_title, "_", timestamp, ".png")
  } else {
    filename <- file
  }
  full_path <- file.path(base_dir, filename)
  
  # Otwórz urządzenie graficzne
  ext <- tools::file_ext(full_path)
  if (tolower(ext) == "png") {
    png(filename = full_path, width = width, height = height, res = res)
  } else if (tolower(ext) %in% c("jpeg", "jpg")) {
    jpeg(filename = full_path, width = width, height = height, res = res)
  } else if (tolower(ext) == "pdf") {
    pdf(file = full_path, width = width/72, height = height/72)
  } else {
    stop("Unsupported file format: ", ext)
  }
  
  # Przygotowanie danych do wykresu
  df <- x$ride
  if (xaxis == "time") {
    xs <- df$time; xlab <- "Czas"
  } else {
    coords <- df[, c("longitude", "latitude")]
    lon <- coords[,1]; lat <- coords[,2]
    dists <- c(0, cumsum(haversine(lon[-length(lon)], lat[-length(lat)], lon[-1], lat[-1])) / 1000)
    xs <- dists; xlab <- "Dystans (km)"
  }
  if (type == "elevation") {
    ys <- df$altitude; ylab <- "Wysokość (m)"
  } else {
    vs <- velocity(x); ys <- vs * 3.6; ylab <- "Prędkość (km/h)" # Wektor vs zwraca prędkość w m/s, natomiast etykieta osi Y ma być w km/h.
  }
  plot(xs, ys, type = "l", xlab = xlab, ylab = ylab, main = x$title, ...)
  
  dev.off()
}

# Metoda dla obiektów klasy 'track'
map.track <- function(x,
                      popup_time = TRUE,
                      start_end_markers = TRUE,
                      ...) {
  df <- x$ride
  m <- leaflet(df) %>% addTiles()
  # ścieżka
  m <- m %>% addPolylines(
    lng     = ~longitude,
    lat     = ~latitude,
    weight  = 3,
    opacity = 0.8,
    popup   = if (popup_time) ~as.character(time) else NULL,
    ...
  )
  # markery start/meta
  if (start_end_markers && nrow(df) >= 2) {
    m <- m %>%
      addMarkers(
        lng   = df$longitude[1],
        lat   = df$latitude[1],
        label = "Start"
      ) %>%
      addMarkers(
        lng   = df$longitude[nrow(df)],
        lat   = df$latitude[nrow(df)],
        label = "Meta"
      )
  }
  m
}


# Przykład użycia:
tr <- track("C:/Users/abc/Documents/r_source/1/bike.zip") # Utworzenie obiektu track
#tr <- track("C:/Users/abc/Documents/r_source/1/bike.zip", "testowa nazwa") # Utworzenie obiektu track z nazwą
print(tr) # Podstawowe informacje
head(tr); tail(tr) # Pierwsze/ostatnie punkty
summary(tr) # Podsumowanie trasy 
plot(tr, type = "speed", xaxis = "distance") # Zapis wykresu prędkości vs dystans


m <- map(tr, popup_time = TRUE, start_end_markers = TRUE) # Wygeneruj mapę
# Wyznacz katalog i nazwę pliku .html obok bike.zip
base_dir    <- dirname(tr$filepath)
html_name   <- sub("\\.zip$", ".html", basename(tr$filepath))
output_file <- file.path(base_dir, html_name)
saveWidget(m, output_file, selfcontained = TRUE) # Zapisz do pliku HTML
browseURL(output_file) # (opcjonalnie) otwórz w przeglądarce