#1日粘度規格から1週間粘度規格を決定する。
specest.dw <- function(pcode, db, dt) {
  mean.est <- rratio(pcode, ix = 11, plot = F, verbose = F)$mean.est[1]
  si.dw <- si(pcode, verbose = F)[4]
  hw <- (dt - db) * si.dw / 2
  c(mean.est - hw, mean.est + hw)
}
