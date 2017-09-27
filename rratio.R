#回帰の寄与をデータ点の個数を減らしながら調べる。粘度値としてdhを使用するよう決め打ちしていることに注意
#解析対象列を選択できるようにし、現行規格値をプロットするようにしたバージョン
#デフォルトで1日粘度の解析。1週間はix=11,specix=5とする
#オプションrm.outでboxplotの外れ値を除去した解析を指定する
#戻り値:回帰の寄与の小さいベスト3のデータフレーム{r:回帰の寄与率, lot, mean.est:サンプルの平均, sd.res:残差標準偏差
rratio <- function (pcode, ix = 9, specix = ix - 6, plot = T, rm.out = F, verbose = T) 
{
  if (plot) {
    if (!is.null(dev.list())) dev.off()
    windows()
    split.screen(c(2, 2))
  }
  x <- t[t$pcode == pcode, ]
  out <- boxplot(x[,ix], plot=F)$out
  if (rm.out) x <- ss(x, !(x[,ix] %in% out))
  x <- x[order(x$day), ]
  n <- nrow(x)
  r <- vector()
  sd.res <- vector()
  mean.est <- vector()
  rmin <- 1
  nmin <- 1
  coefmin <- vector()
  for (i in 1:(n - 1)) {
    aov <- aov(x[i:n,ix] ~ x$day[i:n])
    coef <- coef(aov)
    mean.est <- c(mean.est, coef[2] * as.integer(x$day[n]) + 
                    coef[1])
    s <- summary(aov)[[1]]
    ss <- s$"Sum Sq"
    sd.res <- c(sd.res, sqrt(s$"Mean Sq"[2]))
    rho <- ss[1]/sum(ss)
    if (is.nan(rho)) rho = 1 #最新2ロットの粘度が等しい時、rho = 0/0 = NaNになってしまう
    r <- c(r, rho)
    if (rho < rmin) {
      rmin <- rho
      nmin <- i
      coefmin <- coef
    }
  }
  d <- data.frame(r, x$lot[1:(n - 1)], mean.est, sd.res)
  names(d)[2] <- "lot"
  nspan <- round(n/5)
  ma <- filter(x[,ix], rep(1/nspan, nspan), sides = 1)
  ma <- t(t(ma))
  pc <- pcode
  specv <- ss(spec, pcode == pc)
  if (verbose) {
    print(out)
    print(d[order(d$r), ][1:3, ]) #回帰の寄与の小さいベスト3
    print(d[c(1, order(d$r)[1] + 1), ]) #全ロットによる結果と、回帰最小のロットの一つ後のロット
    print(specv)
  }
  #1週間粘度規格が無い場合、1日粘度規格を用いる
  if (is.na(specv[,6])) specv[,6]<-specv[,4]
  if (is.na(specv[,7])) specv[,7]<-specv[,5]
  if (plot) {                                                     
    screen(2)
    par(mar = c(2, 2, 3, 0))
    plot(x$day[1:(n - 1)], r, ty = "l", main = "回帰の寄与率(ρ)")
    abline(v = x$day[nmin])
    screen(4)
    par(mar = c(2, 2, 3, 0))
    plot(x$day[1:(n - 1)], sd.res, ty = "l", main = "残差標準偏差(σ)")
    abline(v = x$day[nmin])
    screen(3)
    par(mar = c(2, 2, 3, 0))
    plot(x$day[1:n], ma[1:n], ty = "l", ylim = c(min(x[,ix], na.rm = T), 
                                                 max(x[,ix], na.rm = T)), main = "移動平均")
    abline(v = x$day[nmin])
    screen(1)
    par(mar = c(2, 2, 3, 0))
    plot(x$day, x[,ix], main = "粘度")
    abline(h = mean.est[nmin], lwd = 2)
    abline(h = mean.est[nmin] + sd.res[nmin] * 2)
    abline(h = mean.est[nmin] - sd.res[nmin] * 2)
    abline(v = x$day[nmin])
    #	abline(h = specv[1,specix], col="red")
    #	abline(h = specv[1,specix+1], col="red")
  }
  d[order(d$r), ][1:3, ]
}