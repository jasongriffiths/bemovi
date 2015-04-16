require(tools)

merge_data <- function (to.data, particle.data.folder, trajectory.data.folder, video.description.folder, video.description.file, merged.data.folder) 
{
    meta_file <- paste(to.data,video.description.folder, video.description.file, sep = "")
   
    if(file_ext(meta_file)=="txt"){
	    file.sample.info <- as.data.table(read.table(paste(to.data,video.description.folder, video.description.file, sep = ""), sep = "\t", header = TRUE))
    }
    if(file_ext(meta_file)=="csv"){
	    file.sample.info <- as.data.table(read.table(paste0(to.data,video.description.folder, video.description.file), sep = ",", header = TRUE))
    }
    
    # some tests of the video desciption information format 
    if(file_ext(meta_file)!="txt" & file_ext(meta_file)!="csv"){ warning("please save video desciption information as a .csv or a .txt file")}	
    if(any(is.na(file.sample.info[1,]))==TRUE ){ stop("video description file is mis-specified and when read in it is generating NA values")}

    # set all letters to lower case
    file.sample.info$file <- tolower(file.sample.info$file)
    
    # load the morphology data and the movement data
    load(paste0(to.data, particle.data.folder, "particle.RData"))
    load(paste0(to.data, trajectory.data.folder, "trajectory.RData"))
    trajectory.data <- as.data.table(trajectory.data)
    morphology.data <- as.data.table(morphology.data)

    # re-arrange x and y coordinates and add one to frame numbers
    trajectory.data$Y1 <- -trajectory.data$X
    trajectory.data$X1 <- trajectory.data$Y
    trajectory.data$X <- trajectory.data$X1
    trajectory.data$Y <- trajectory.data$Y1
    trajectory.data$frame <- trajectory.data$frame + 1
    trajectory.data$X1 <- NULL
    trajectory.data$Y1 <- NULL
    
    morphology.data$frame <- morphology.data$Slice
    morphology.data$Slice <- NULL
    morphology.data$file <- sub(".cxd | .avi", "", morphology.data$file)
    
    # check if anything tracked and merge morphology and trajectory data 
    if(length(morphology.data$file) ==0) {warning("no particles located or tracked ")}
    merged1 <- merge(morphology.data, trajectory.data, by = c("X", 
        "Y", "frame", "file"), all = T)
    merged1 <- merged1[, `:=`(file, tolower(file))]
    setkey(merged1, file)
    setkey(file.sample.info, file)
    
    #merge meta info with behav and morph output
    merged2 <- merge(merged1, file.sample.info, all = F)
    
    # make a folder to store the merged data
    dir.create(paste0(to.data, merged.data.folder), showWarnings = F)
    trajectory.data <- merged2[!is.na(merged2$id), ]
    setkey(trajectory.data, file, id, frame)
    save(trajectory.data, file = paste0(to.data, merged.data.folder,"Master.RData"))
}