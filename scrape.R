library(data.table)

print(Sys.Date())
print(Sys.time())

EDszam <- rvest::read_html("https://elvira.mav-start.hu/")
EDszam <- rvest::html_text(rvest::html_nodes(EDszam, "script"))
EDszam <- sapply(EDszam, function(s)
  sub("ed:'([0-9A-F]+)'", "\\1",
      regmatches(s, regexpr("ed:'[0-9A-F]+'", s))))
EDszam <- EDszam[[which(sapply(EDszam, length) > 0)]]

datum <- format(Sys.Date(), "%y.%m.%d")

elviraurl <- "https://elvira.mav-start.hu/elvira.dll/x/vt?v="

maxv <- 1000
for(d in c(1000, 100, 10, 1)) {
  repeat {
    tab <- rvest::html_table(rvest::read_html(paste0(
      elviraurl, maxv, "&d=", datum, "&ed=", EDszam)))
    if(length(tab) == 0) break
    maxv <- maxv + d
  }
  maxv <- maxv - d
}

print(maxv)
print(EDszam)
print(datum)

pb <- progress::progress_bar$new(
  format = paste0("  downloading [:bar] :current/:total ",
                  "(:percent) in :elapsedfull eta: :eta"),
  total = maxv, force = TRUE, clear = FALSE)
pb$tick(0)

res <- lapply(1:maxv, function(v) {
  pb$tick()
  pg <- purrr::insistently(function() rvest::read_html(paste0(
    elviraurl, v, "&d=", datum, "&ed=", EDszam)),
    rate = purrr::rate_delay(pause = 2, max_times = 10))()
  tab <- rvest::html_table(pg, header = FALSE)[[1]]
  tab <- if(tab$X1[3] == "Km") tab[-c(1, 3),] else tab[-2,]
  cbind(setNames(tab[-1,], make.names(as.character(tab[1,]),
                                      unique = TRUE)),
        Datum = as.Date(datum, format = "%y.%m.%d"), Vonat = v,
        VonatSzam = rvest::html_text(rvest::html_nodes(
          pg, xpath = "//div[@id='tul']/h2")))
})

saveRDS(res, paste0(
  "raw/raw", format(as.Date(datum, format = "%y.%m.%d"),
                    "%Y%m%d"), ".rds"))

res <- rbindlist(res, fill = TRUE)
res <- res[, apply(res, 2, function(x) sum(!is.na(x))) > 0,
           with = FALSE]

res[Menetrend.szerint == ""]$Menetrend.szerint <- NA
res[Menetrend.szerint.1 == ""]$Menetrend.szerint.1 <- NA
res[Tényleges == ""]$Tényleges <- NA
res[Tényleges.1 == ""]$Tényleges.1 <- NA
res[Várható == ""]$Várható <- NA
res[Várható.1 == ""]$Várható.1 <- NA
res[Km == ""]$Km <- NA

res <- res[!is.na(Km)]
res <- res[!Vonat %in% res[, .N, .(Vonat)][N == 1]$Vonat]

res$Km <- as.numeric(res$Km)

tdiff <- function(x, y) {
  temp <- as.numeric(lubridate::hm(y) - lubridate::hm(x))/60
  dplyr::if_else(temp < -720, temp + 1440, temp)
}

res[tdiff(Tényleges, Tényleges.1) < 0,
    c("Tényleges", "Tényleges.1") := list(NA, NA)]

res2 <- res[, rbind(
  .SD[1, .(KmIndulo = Km, KmErkezo = Km, Indulo = Állomás,
           Erkezo = Állomás, Nominalis = 0, KumNominalis = 0,
           Tenyleges = tdiff(Menetrend.szerint.1, Tényleges.1),
           KumTenyleges = tdiff(Menetrend.szerint.1, Tényleges.1),
           Tipus = "InduloAllomas", Order = 1)],
  .SD[, .(KmIndulo = Km[-length(Km)], KmErkezo = Km[-1],
          Indulo = Állomás[-length(Állomás)],
          Erkezo = Állomás[-1],
          Nominalis = tdiff(Menetrend.szerint.1[
            -length(Menetrend.szerint.1)], Menetrend.szerint[-1]),
          KumNominalis = tdiff(Menetrend.szerint.1[1],
                               Menetrend.szerint[-1]),
          Tenyleges = tdiff(Tényleges.1[-length(Tényleges.1)],
                            Tényleges[-1]),
          KumTenyleges = tdiff(Menetrend.szerint.1[1],
                               Tényleges[-1]),
          Tipus = c(rep("Szakasz", .N - 2), "ZaroSzakasz"),
          Order = seq(2, by = 2, length.out = .N - 1))],
  .SD[, .(KmIndulo = Km[-c(1, .N)], KmErkezo = Km[-c(1, .N)],
          Indulo = Állomás[-c(1, .N)],
          Erkezo = Állomás[-c(1, .N)],
          Nominalis = tdiff(Menetrend.szerint[-c(1, .N)],
                            Menetrend.szerint.1[-c(1, .N)]),
          KumNominalis = tdiff(Menetrend.szerint.1[1],
                               Menetrend.szerint.1[-c(1, .N)]),
          Tenyleges = tdiff(Tényleges[-c(1, .N)],
                            Tényleges.1[-c(1, .N)]),
          KumTenyleges = tdiff(Menetrend.szerint.1[1],
                               Tényleges.1[-c(1, .N)]),
          Tipus = "KozbensoAllomas",
          Order = seq(3, by = 2, length.out = .N - 2))])[
            order(Order)], .(Datum, Vonat, VonatSzam)]

res2$Keses <- res2$Tenyleges - res2$Nominalis
res2$KumKeses <- res2$KumTenyleges - res2$KumNominalis

qgrepl <- function(x) grepl(x, res2$VonatSzam, ignore.case = TRUE)

res2$VonatJelleg <- dplyr::case_when(
  qgrepl("személyvonat") ~ "Személyvonat",
  qgrepl("InterCity") ~ "InterCity",
  qgrepl("InterRégió") ~ "InterRégió",
  qgrepl("vonatpótló autóbusz") ~ "Vonatpótló autóbusz",
  qgrepl("railjet xpress") ~ "Railjet xpress",
  qgrepl("railjet") ~ "Railjet",
  qgrepl("gyorsvonat") ~ "Gyorsvonat",
  qgrepl("TramTrain") ~ "TramTrain",
  qgrepl("Expresszvonat") ~ "Expresszvonat",
  qgrepl("sebesvonat") ~ "Sebesvonat",
  qgrepl("EuroCity") ~ "EuroCity",
  qgrepl("EuRegio") ~ "EuRegio",
  qgrepl("EuroNight") ~ "EuroNight",
  qgrepl("Night Jet") ~ "Night Jet",
  qgrepl("Interregional") ~ "Interregional",
  qgrepl("International") ~ "International",
  .default = "Egyéb"
)

saveRDS(res2, paste0(
  "proc/proc", format(as.Date(datum, format = "%y.%m.%d"),
                      "%Y%m%d"), ".rds"))
