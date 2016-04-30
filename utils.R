sourceall <- function(directory){
  # Got this nifty piece of code from http://stackoverflow.com/questions/10291520/reading-all-scripts-and-data-files-from-multiple-folders
  if (dir.exists(directory)){
    file.sources = list.files(path=directory, pattern="*.R$", full.names = T)
    data.sources = list.files(path=directory, pattern="*.rda$", full.names = T)
    sapply(data.sources,load,.GlobalEnv)
    sapply(file.sources,source,.GlobalEnv)
  }
}