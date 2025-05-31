library(data.table)

EDszam <- rvest::read_html("https://elvira.mav-start.hu/")
EDszam <- rvest::html_text(rvest::html_nodes(EDszam, "script"))
EDszam <- sapply(EDszam, function(s)
  sub("ed:'([0-9A-F]+)'", "\\1",
      regmatches(s, regexpr("ed:'[0-9A-F]+'", s))))
EDszam <- EDszam[[which(sapply(EDszam, length) > 0)]]

datum <- format(Sys.Date(), "%y.%m.%d")

elviraurl <- "https://elvira.mav-start.hu/elvira.dll/x/vt?v="

i <- 1000
repeat {
  tab <- rvest::html_table(rvest::read_html(paste0(
    elviraurl, i, "&d=", datum, "&ed=", EDszam)))
  if(length(tab) == 0) break
  i <- i + 1000
}
i <- i - 1000
repeat {
  tab <- rvest::html_table(rvest::read_html(paste0(
    elviraurl, i, "&d=", datum, "&ed=", EDszam)))
  if(length(tab) == 0) break
  i <- i + 100
}
i <- i - 100
repeat {
  tab <- rvest::html_table(rvest::read_html(paste0(
    elviraurl, i, "&d=", datum, "&ed=", EDszam)))
  if(length(tab) == 0) break
  i <- i + 10
}
i <- i - 10
repeat {
  tab <- rvest::html_table(rvest::read_html(paste0(
    elviraurl, i, "&d=", datum, "&ed=", EDszam)))
  if(length(tab) == 0) break
  i <- i + 1
}

maxv <- i - 1

print(maxv)
print(EDszam)
print(datum)

pb <- progress::progress_bar$new(
  format = "  downloading [:bar] :current/:total (:percent) in :elapsedfull eta: :eta",
  total = maxv, clear = FALSE)
pb$tick(0)

# cl <- parallel::makeCluster(2)
# cl <- parallel::makeCluster(parallel::detectCores() - 1)
# parallel::clusterExport(cl, c("datum", "EDszam"))

res <- lapply(1:maxv, function(v) {
  pb$tick()
  pg <- purrr::insistently(function() rvest::read_html(paste0(
    elviraurl, v, "&d=", datum, "&ed=", EDszam)),
    rate = purrr::rate_delay(pause = 2, max_times = 10))()
  tab <- rvest::html_table(pg, header = FALSE)[[1]]
  tab <- if(tab$X1[3] == "Km") tab[-c(1, 3),] else tab[-2,]
  cbind(setNames(tab[-1,], make.names(as.character(tab[1,]),
                                      unique = TRUE)),
        Vonat = v,
        VonatSzam = rvest::html_text(rvest::html_nodes(
          pg, xpath = "//div[@id='tul']/h2")))
})

# parallel::stopCluster(cl)

res <- rbindlist(res, fill = TRUE)

save(res, "res.RDS")
