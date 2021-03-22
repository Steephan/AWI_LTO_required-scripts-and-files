###############################################################################
#
#   plotting functions
#
#   by: stephan.lange@awi.de
#   last modified: 2016-04-25
#
##
####
####
####
####
####
####
####
###
##
#
########################################################################################################
##### plot NA distribution #####
plotNAdistribution <- function(d, title, output = "screen") {
  tmp <- reshape(d,
                 direction = "long",
                 idvar = "UTC",
                 varying = 2:ncol(d),
                 v.names = "value",
                 timevar = "sensor",
                 times = names(d)[2:ncol(d)])
  tmp$gap <- is.na(tmp$value)
  tmp <- addDateDetails(tmp)
  tmp.day <- aggregate(gap ~ monthday + month + year, data = tmp, sum)
  tmp.day$UTC <- as.POSIXct(paste(tmp.day$year, tmp.day$month,
                                  tmp.day$monthday, sep = "-"), format = "%Y-%m-%d")
  tmp.hour <- aggregate(gap ~ hour + monthday + month + year, data = tmp, sum)
  tmp.hour$UTC <- as.POSIXct(paste(tmp.hour$year, tmp.hour$month,
                                   tmp.hour$monthday, tmp.hour$hour, sep = "-"), format = "%Y-%m-%d-%H")
  if (output != "screen") {
    png(output, width = 600, height = 300)
  }
  plotdata <- tmp.hour
  # ToDo? Add hours to x-axis
  plot(data.frame(Date = plotdata$UTC, NAs = plotdata$gap), t = "l",
       xaxt = "n", yaxt = "n", main = title, ylim = c(0, max(plotdata$gap)))
  axis.POSIXct(1, at = plotdata$UTC, lwd = 0.5)
  axis(2, at = seq(0, max(plotdata$gap), signif(max(plotdata$gap)/20, 1)))
  if (output != "screen") {
    dev.off()
  }
}



# function for plotting maintenance stays (Kerstin Binder)
plot_maintenance <- function(jahr) {
  i <- grep(jahr, p.1maint$start)
  if (length(i) >= 1) {
    for (l in 1:length(i)) {
      rect(xleft = as.numeric(strptime(p.1maint$start[i[l]], format = "%d.%m.%Y")),
           xright = as.numeric(strptime(p.1maint$end[i[l]], format = "%d.%m.%Y")),
           ybottom = (-1500), ytop = 1500, col = color, border = "transparent")
    }
  }
}

# function to determine y plotting bounderies (Kerstin Binder)
plot_bounderies <- function(up, down) {
  if (min(up, na.rm = T) < min(down, na.rm = T) ) {
    y_min <- min(up, na.rm = T)
  } else {
    y_min <- min(down, na.rm = T)
  }
  if (max(up, na.rm = T) > max(down, na.rm = T) ) {
    y_max <- max(up, na.rm = T)
  } else {
    y_max <- max(down, na.rm = T)
  }
  y_limit <- c(y_min, y_max)

  return(y_limit)
}

wind.rose <- function(frec, fnum = 4, fint = 5, flab = 2, ang = 3 * pi / 16, text.cex = 1,
                     col = rainbow(10, .5, .92, start = .33, end = .2), margen = c(0, 0, 0, 0),#margen=c(0, 0, 4, 0),
                     key = TRUE, uni = "m/s", ...) {
  ## copied from climatol-packages
  ## wind.freq()-function is necessary
  ## see: db_helper.R
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (is.matrix(frec)) frec <- as.data.frame(frec)

  if (is.vector(frec)) {
    ndir <- length(frec)
    nr <- 1
  } else {
    ndir <- length(frec[1, ])
    nr <- nrow(frec)
  }
  fmax <- fnum * fint

  tot <- sum(frec)
  fr <- 100 * frec / tot
  key <- (nr > 1) && key

  if (key) mlf <- 3 else mlf <- 1
  par(mar = margen, new = FALSE)

  fx <- cos(pi / 2 - (2 * pi/ndir * 0:(ndir - 1)))
  fy <- sin(pi / 2 - (2 * pi/ndir * 0:(ndir - 1)))

  plot(fx, fy, xlim = c(-fmax - mlf * fint, fmax + fint), ylim = c(-fmax - fint, fmax + fint),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", asp = 1, type = "n", ...)
  if (nr == 1) {
    cx <- fx * fr
    cy <- fy * fr
  } else {
    f <- apply(fr, 2, sum)
    cx <- fx * f
    cy <- fy * f
    for (i in nr:2) {
      f <- f - fr[i, ]
      cx <- c(cx, NA, fx * f)
      cy <- c(cy, NA, fy * f)
    }
  }
  polygon(cx, cy, col = col[nr:1])
  symbols(c(0 * 1:fnum), c(0 * 1:fnum), circles = c(fint * 1:fnum), inches = FALSE, add = TRUE)
  segments(0 * 1:ndir, 0 * 1:ndir, fmax * fx, fmax * fy)
  fmaxi <- fmax + (fint / 2)
  text(0, fmaxi, "N", cex = text.cex)
  text(0, -fmaxi, "S", cex = text.cex)
  text(fmaxi, 0, "E", cex = text.cex)
  text(-fmaxi, 0, "W", cex = text.cex)
  if (flab == 2)
    for (i in 1:fnum) text(i * fint * cos(ang), i * fint * sin(ang), paste(i * fint, "%"))
  else if (flab == 1)
    text(fmax * cos(ang), fmax * sin(ang), paste(fmax, "%"))
  if (key) {
    legend(-fmaxi - 2.3 * fint, fmaxi + 2, fill = col, legend = attr(frec, "row.names"))
    text(-fmaxi - 1.4 * fint, fmaxi + .9 * fint, uni)
  }
  invisible()

}


wind.rose.2 <- function(frec, fnum = 4, fint = 5, ang = 3 * pi / 16, lwd.boost = 1,
                        zoom = 1.1, ypos = 8, xpos = 920000000, text.cex = 0.5, hurz = 1, koloss = 700000, gigant = 0.012,
                        colz = rainbow(10, .5, .92, start = .33, end = .2), margen = c(0, 0, 0, 0), line.col = "gray44",#margen=c(0, 0, 4, 0),
                        uni = "m/s", ...) {
  #frec=wind.freq(db.wind$windv,db.wind$windd);fnum=6; fint=4; ang=3*pi/16;ypos=8;xpos=960000000;mlf <- 1
  #colz=rainbow(10,.5,.92,start=.33,end=.2)
  ## copied from climatol-packages
  ## wind.freq()-function is necessary
  ## see: db_helper.R
#   old.par <- par(no.readonly = TRUE)
#   on.exit(par(old.par))
  if (is.matrix(frec)) frec <- as.data.frame(frec)
  ndir <- length(frec[1, ])
  nr <- nrow(frec)
  fmax <- fnum * fint
  mlf <- 1
  tot <- sum(frec)
  fr <- 100 * frec / tot


  fx <- cos(pi/2 - (2 * pi / ndir * 0:(ndir - 1)))
  fy <- sin(pi/2 - (2 * pi / ndir * 0:(ndir - 1)))
 #par(new = TRUE)
  #plot(fx,fy,xlim=c(-fmax-mlf*fint,fmax+fint),ylim=c(-fmax-fint,fmax+fint),
   #    xaxt="n",yaxt="n",xlab="",ylab="",bty="n",asp=1,type="n")
  # zoom=2 # 1.1
   # koloss=700000  # 400000
   # gigant=0.012   # 0.01
  if (nr == 1) {
    cx <- fx * fr * zoom * koloss + xpos
    cy <- fy * fr * zoom * gigant + ypos
  } else {
    f <- apply(fr, 2, sum)
    cx <- fx * f * zoom * koloss + xpos
    cy <- fy * f * zoom * gigant + ypos
    for (i in nr:2) {
      f <- f - fr[i, ]
      cx <- c(cx, NA, fx * f * zoom * koloss + xpos)
      cy <- c(cy, NA, fy * f * zoom * gigant + ypos)
    }
  }
  polygon(cx, cy, col = colz[nr:1], border = "transparent")
  symbols((c(0 * 1:fnum) + xpos), (c(0 * 1:fnum) + ypos),
          circles = (c(fint * 1:fnum) * zoom * koloss),
          inches = FALSE, add = TRUE, lwd = lwd.boost, fg = line.col)
  segments((0 * 1:ndir) + xpos, (0 * 1:ndir) + ypos, fmax * fx * zoom * koloss + xpos,
           fmax * fy * zoom * gigant + ypos, lwd = lwd.boost, col = line.col)
  fmaxi <- fmax + fint / 4
  if (hurz == 1) {
    text(xpos, ypos + (fmaxi + 2) * zoom * gigant, "N", cex = text.cex, col = line.col)
    text(xpos, ypos - (fmaxi + 2) * zoom * gigant, "S", cex = text.cex, col = line.col)
    text(xpos + (fmax - 33) * zoom * koloss, ypos, "W", cex = text.cex, col = line.col)
    text(xpos - (fmax - 33) * zoom * koloss, ypos, "E", cex = text.cex, col = line.col)
  }
  #}
  invisible()
}

wind.rose.3 <- function(frec, fnum = 4, fint = 5, flab = 2, ang = 3 * pi / 16, text.cex = 1,
                      col = rainbow(10, .5, .92, start = .33, end = .2), margen = c(0, 0, 0, 0),#margen=c(0, 0, 4, 0),
                      key = TRUE, uni = "m/s", ...) {
  ## copied from climatol-packages
  ## wind.freq()-function is necessary
  ## see: db_helper.R
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par, new = T))
  if (is.matrix(frec)) frec <- as.data.frame(frec)

  if (is.vector(frec)) {
    ndir <- length(frec)
    nr <- 1
  } else {
    ndir <- length(frec[1, ])
    nr <- nrow(frec)
  }
  fmax <- fnum * fint

  tot <- sum(frec)
  fr <- 100 * frec / tot
  key <- (nr > 1) && key

  if (key) mlf <- 3 else mlf <- 1
  par(mar = margen, new = FALSE)

  fx <- cos(pi / 2 - (2 * pi / ndir * 0:(ndir - 1)))
  fy <- sin(pi / 2 - (2 * pi / ndir * 0:(ndir - 1)))

  plot(fx, fy, xlim = c(-fmax - mlf * fint, fmax + fint),
       ylim = c(-fmax - fint, fmax + fint),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", asp = 1, type = "n", ...)
  if (nr == 1) {
    cx <- fx * fr
    cy <- fy * fr
  } else {
    f <- apply(fr, 2, sum)
    cx <- fx * f
    cy <- fy * f
    for (i in nr:2) {
      f <- f - fr[i, ]
      cx <- c(cx, NA, fx * f)
      cy <- c(cy, NA, fy * f)
    }
  }
  polygon(cx, cy, col = col[nr:1])
  symbols(c(0 * 1:fnum), c(0 * 1:fnum), circles = c(fint * 1:fnum), inches = FALSE, add = TRUE)
  segments(0 * 1:ndir, 0 * 1:ndir, fmax * fx, fmax * fy)
  fmaxi <- fmax + (fint / 2)
  text(0, fmaxi, "N", cex = text.cex)
  text(0, -fmaxi, "S", cex = text.cex)
  text(fmaxi, 0, "E", cex = text.cex)
  text(-fmaxi, 0, "W", cex = text.cex)
  if (flab == 2)
    for (i in 1:fnum) text(i * fint * cos(ang), i * fint * sin(ang), paste(i * fint, "%"))
  else if (flab == 1)
    text(fmax * cos(ang), fmax * sin(ang), paste(fmax, "%"))
  if (key) {
    legend(-fmaxi - 2.3 * fint, fmaxi + 2, fill = col, legend = attr(frec, "row.names"))
    text(-fmaxi - 1.4 * fint, fmaxi + .9 * fint, uni)
  }
  invisible()

}

