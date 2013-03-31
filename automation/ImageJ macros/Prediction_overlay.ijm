setBatchMode(true);

avi_input = "C:/Users/Frank/Documents/PhD/Programming/franco/data/1 - raw/";
overlay_input = "C:/Users/Frank/Documents/PhD/Programming/franco/data/3 - overlay plots/";
overlay_output = "C:/Users/Frank/Documents/PhD/Programming/franco/data/4 - overlays/";
lag = 25
list = getFileList(avi_input);
list2 = getFileList(overlay_input);

for (i=0; i<lengthOf(list2); i++) {

// Open stack with trajectories from folder (incrementing positions through time)
run("Image Sequence...", "open=["+overlay_input+replace(list2[i],"/","")+"] sort");
getDimensions(width, height, channels, slices, frames);
run("RGB Color");
vid1 = getTitle();

// Open stack with picture sequence from which trajectories were extracted (original gray scale images)
run("AVI...", "select=["+avi_input+replace(list2[i],"/",".avi")+"] first=1 last="+slices+"");
run("RGB Color");
run("Invert", "stack");
vid2 = getTitle();

// merge both stacks into
imageCalculator("AND create stack", vid2, vid1);
run("Invert", "stack");
run("AVI... ", "compression=JPEG frame=26 save=["+overlay_output+replace(list2[i],"/",".avi")+"]");
close();
close();
close();



if (endsWith(list[i],"cxd")){
// Open stack with trajectories from folder (incrementing positions through time)
run("Image Sequence...", "open=["+overlay_input+replace(list2[i],"/","")+"] sort");
getDimensions(width, height, channels, slices, frames);
run("RGB Color");
vid1 = getTitle();

// Open stack with picture sequence from which trajectories were extracted (original gray scale images)
run("Bio-Formats", "open=["+avi_input+replace(list2[i],"/",".cxd")+"] autoscale color_mode=Default view=[Standard ImageJ] stack_order=Default");
vidtemp = getTitle();
run("Make Substack...", "  slices=1-"+slices+"");
vid2 = getTitle();
selectWindow(vidtemp);
close();
selectWindow(vid2);
run("RGB Color");
run("Invert", "stack");

// merge both stacks into
imageCalculator("AND create stack", vid2, vid1);
run("Invert", "stack");
run("AVI... ", "compression=JPEG frame=26 save=["+overlay_output+replace(list2[i],"/",".avi")+"]");
close();
close();
close();
}




}
run("Quit");