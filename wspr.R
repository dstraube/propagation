######################################################################
## Copyright (C) 2016, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

# This program visualizes radio wave propagation seasonality using Weak Signal Reporter Network data.

setwd("~/R/WSPR")
rm(list=ls())

library(stringi)
library(dplyr)
library(ggplot2)

# Function to get spots for a given month.  Downloads zip file from WSPR web site and saves it as local
# RData if necessary, otherwise reads local RData file.  Performs minor cleanup on downloaded file.

GetSpots <- function(year, month) {
    
    if ( ! dir.exists("./spots") ) {
        dir.create("./spots")
    }
    month <- sprintf("%02d", month)
    rdata <- stri_join("./spots/wsprspots-", year, "-", month, ".RData")
    if ( file.exists(rdata) ) {
        message(stri_join("Loading: ", rdata))
        load(rdata)
    } else {
        url <- stri_join("http://wsprnet.org/archive/wsprspots-", year, "-", month, ".csv.zip")
        zip <- stri_join("./spots/wsprspots-", year, "-", month, ".csv.zip")
        csv <- stri_join("./spots/wsprspots-", year, "-", month, ".csv")
        if ( ! file.exists(csv) ) {
            message(message(stri_join("Downloading: ", zip)))
            download.file(url, zip, method = "curl")
            unzip(zip, exdir = "./spots")
            file.remove(zip)
        }
        message(stri_join("Reading: ", csv))
        spots <- read.csv(csv,
                          header = FALSE,
                          stringsAsFactors = FALSE,
                          col.names = c("id", "ts", "dst.call", "dst.grid", "snr", "freq", "src.call",
                                        "src.grid", "pwr", "drift", "km", "az", "band", "ver", "code"),
                          colClasses = c("integer", "integer", "character", "character", "integer", "numeric",
                                         "character", "character", "integer", "integer", "integer", "integer",
                                         "integer", "character", "integer"))
        # Generate POSIXlt from decimal.
        spots$ts <- as.POSIXlt(spots$ts, origin = "1970-01-01", tz = "GMT")
        # UCASE call signs and grids.
        spots$src.call <- toupper(spots$src.call)
        spots$src.grid <- toupper(spots$src.grid)
        spots$dst.call <- toupper(spots$dst.call)
        spots$dst.grid <- toupper(spots$dst.grid)
        # Dump zero-distance spots.
        spots <- spots[spots$km > 0,]
        # Save as RData for future quick load.
        save(spots, file = rdata)
        file.remove(csv)
    }
    spots
}

bandname <- list()
bandname[[28]] <- "10M"
bandname[[14]] <- "20M"
bandname[[7]]  <- "40M"

rdata <- "./spotinfo.RData"
if ( file.exists(rdata) ) {
    message(stri_join("Loading: ", rdata))
    load(rdata)
} else {
    spotinfo <- NULL
    year <- 2014
    # Get data for July and January.
    for ( month in c(7, 1) ) {
        spots <- GetSpots(year, month)
        
        # We want to compare winter against summer which is hemisphere dependent.
        # Reduce to spots in northern hemisphere only by src/dst grid.  See Maidenhead
        # grid chart at https://en.wikipedia.org/wiki/Maidenhead_Locator_System.
        spots <- spots[grepl("^[A-R][J-R]*", spots$src.grid, ignore.case = TRUE),]
        spots <- spots[grepl("^[A-R][J-R]*", spots$dst.grid, ignore.case = TRUE),]
        
        # Analyze 28 MHz, 14 MHz, and 7 MHz frequencies, aka 10, 20, and 40 meter bands.
        for ( band in c(28, 14, 7) ) {
            
            # Trim to band of interest and remove ground wave (~100 miles/160km) spots.
            tmp <- spots[spots$band == band & spots$km > 160, c("ts", "src.call", "km")]
            
            # We will soon rely on the fact that all spots are transmitted on the top of the
            # minute - i.e. at YYYY-MM-MM HH:MM:00. (Note 00 seconds.)  Verify.
            stopifnot(0 == sum(spots$ts$sec != 0))
            
            # Convert timestamp to character as dplyr can't sort on POSIXlt.
            tmp$tsc <- as.character(tmp$ts)
            tmp$ts <- NULL
            
            # Group by src.call and timestamp.  I.e. group all receptions of a single transmission.
            g <- group_by(tmp, src.call, tsc)
            
            # Keep only the most distant reception for each transmission.
            s <- summarise(g, km.max = max(km, na.rm = TRUE))
            
            # Build data frame such that we can use ggplot2 facets.
            spotinfo <- rbind(spotinfo, data.frame(Month = month.name[month],
                                                   Band = bandname[[band]],
                                                   vals = s$km.max))
        }
    }
    save(spotinfo, file = rdata)
}

g <- ggplot(spotinfo, aes(x = vals)) +
    geom_density(data = spotinfo,
                 alpha = 0.3,
                 aes(x = vals, group = Month, colour = Month)) +
    facet_grid(Band ~ .) + 
    scale_color_brewer(palette="Set1") +
    ggtitle("Density Distribution of WSPR Spots\nJanuary 2014 vs July 2014\n(using only most distant reception per spot)") +
    xlab("Kilometers") +
    ylab("Density") +
    theme(axis.title.x = element_text(size = rel(1.25))) +
    theme(axis.title.y = element_text(size = rel(1.25))) +
    theme(strip.text.y = element_text(angle = 0, face = "bold", size = rel(1.5)))
g

svg("./propagation.svg", width = 7, height = 6)
g
dev.off()


