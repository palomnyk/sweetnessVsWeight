
# Author: Aaron Yerke (aaronyerke@gmail.com)
# Template of forest plot for Kelly to use while making figures.
# Instructions:
# In the section called "#### Set up variables to manipulate for template ####", 
# There are three variables for you to adjust: xaxis, yaxis, and y_subset. These
# will allow you to select the x axis columns (MD, or SMD), or the y axis with 
# the subset. For more info, see the comments on each var in the section


#### Loading libraries and data ####
#read in libraries
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE))  BiocManager::install("ggplot2")
library("ggplot2")
print("Libraries are loaded.")

# Read in data
df <- read.table("md_smd_results_KH_62323.txt", sep="\t",
                 header = T, check.names = F,
                 encoding = "latin1", na.strings = c("NA"))
print("Data has been read in.")

#### Data pre-processing and cleaning ####
# replace "-" with NA for cells
df[df == "-"] <- NA
# The "-" symbols were problematic for numeric columns, therefore reclassifying columns.
df[] <- lapply(df, type.convert, as.is = TRUE)
# Fix some inconsistencies
df[df == "no LCS"] <- "No LCS"
df[df == "lower LCS"] <- "Lower LCS"
df[df == "LCSB only"] <- "LCSB"
df[df == "LCS (continuous, per serving)"] <- "LCS (continuous)"

# Need to delete bad columns and change names of good ones
#columns of interest: c("Study_Design_Clean","Outcome___Clean", "Intervention/Exposure___Clean","Comparator___Clean_2")
df <- subset(df, select = -c(`Study Design`, Reference, `Outcome - Description`, `Intervention/Exposure`))
colnames(df)[colnames(df) == "Study Design-Clean"] <- "Study Design"
colnames(df)[colnames(df) == "Outcome - Clean"] <- "Outcome"
colnames(df)[colnames(df) == "Intervention/Exposure - Clean"] <- "Intervention/Exposure"
colnames(df)[colnames(df) == "Comparator - Clean 2"] <- "Comparator"
colnames(df)[colnames(df) == "Reference 2"] <- "Reference"

# Drop rows that are selected by `IncludeinPlot`
df <- df[df$IncludeinPlot == 1, ]
print("Data pre-processing is complete.")

#### Set up variables to manipulate for template ####
xaxis <- "MD"#Can be "MD" or "SMD"
yaxis <- "Study Design" #can be any column name (for column names run colnames(df) in R console)
y_subset <- "RCT"#find options with unique(df[,xaxis]), if entire column is desired: "*"
print(paste("Variables selected:", xaxis, yaxis, y_subset))

#### Final preprocessing to accomidate manipulated varibles ####
new_df <- df[!is.na(df[xaxis]),]#drop rows with NA in yaxis column
# Need unique vars to use for yaxis, so that no forest plots overlap
my_uniques <- paste(new_df$Reference,new_df$`Number of Studies`)
new_df$uniques <- my_uniques
#subset yaxis if needed
if (y_subset != "*"){
  stopifnot(y_subset %in% new_df[,yaxis])
  print(paste("Subsetting", yaxis, "by", y_subset))
  new_df <- new_df[new_df[,yaxis] == y_subset,]
}

#rename columns in plot to get around ggplot2's depreciation of aes_string()
colnames(new_df)[colnames(new_df) == paste("Lower 95%", xaxis)] <- "xmin"
colnames(new_df)[colnames(new_df) == paste("Upper 95%", xaxis)] <- "xmax"
colnames(new_df)[colnames(new_df) == xaxis] <- "my_x"

#### Generate plot ####
# set up pdf
f_path <- file.path("output", paste(xaxis, yaxis, y_subset,"forest_plots.pdf", sep = "_"))
pdf(file = file.path(f_path), width = 24, height = 12)
# Plot
g <- ggplot2::ggplot(data = new_df, aes(y=uniques, x=my_x,
                                               xmin=xmin,
                                               xmax=xmax))+ 
  geom_pointrange()+# Makes range for ggplot values based on the data and AES specified in first line
  scale_x_continuous(limits = c(-5, 5))+
  geom_hline(yintercept=0, lty=2, size =1)+ # add a dotted line at x=0 after flip
  geom_errorbar( width=0.3, cex=1)+ # Makes whiskers on the range (more aesthetically pleasing)
  geom_point(shape = 15)+ # specifies the size and shape of the geompoint
  ggtitle(paste(xaxis, yaxis, y_subset))+ # Blank Title for the Graph
  ylab("References")+ # Label on the Y axis (flipped specification do to coord_flip)
  xlab(paste(yaxis,"95% CI"))+ # Label on the X axis (flipped specification do to coord_flip)
  # scale_y_continuous(limits = c(-.75,.75), breaks = c(-.75,-.38,0,.38,.75)) # limits and tic marks on X axis (flipped specification do to coord_flip)
  # scale_y_continuous(limits = c(-.75,.75), breaks = c(-.75,-.38,0,.38,.75))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
  theme(line = element_line(colour = "black", linewidth = 1), # My personal theme for GGplots
        legend.position ="none",
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.spacing = unit(2, "lines") # added to theme to add space in between facet_wrap plots
  )
print(g)
  
dev.off()

