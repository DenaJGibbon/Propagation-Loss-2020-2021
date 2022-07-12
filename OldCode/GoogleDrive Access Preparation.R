library("googledrive")
googledrive::drive_auth()

Folder.list.tibble <- drive_find(type = "folder")

Folder.list.tibble[1,]$id

Temp.tibble <- drive_ls(as_id(Folder.list.tibble[1,]$id) , recursive = FALSE)

Temp.tibble[1,]

Temp.tibble2 <- drive_ls(as_id(Temp.tibble[1,]$id) , recursive = FALSE)

Rungan.playbacks.list.files <- drive_ls(as_id(Temp.tibble2[1,]$id) , pattern='.txt', recursive = FALSE)
Rungan.playbacks.wav.files <- drive_ls(as_id(Temp.tibble2[1,]$id) , pattern='.wav', recursive = FALSE)


drive_download(as_id(Rungan.playbacks.wav.files$id[2]))

write.csv(as.data.frame(Rungan.playbacks.wav.files[,1:2]),'Rungan.playbacks.wav.files.csv',row.names =F)
