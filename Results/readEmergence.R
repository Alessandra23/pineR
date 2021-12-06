listFiles <- list.files("Data/Sites", pattern = "*.xlsx", full.names = TRUE)
readFiles <- lapply(listFiles, dataFrameData)
emergence <- lapply(readFiles, readEmergence)
names(emergence) <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(listFiles))
emergence
