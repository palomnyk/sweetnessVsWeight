
# Author: Aaron Yerke (aaronyerke@gmail.com)
# Best review tutorial for forest plots:
# https://ianasilver.com/making-a-forest-plot-with-ggplot2/
# From Kelly:
  # Variable = Reference 
  # Y=MD or SMD
yaxes <- c("MD","SMD")
y_CIs <- list(c("Lower_95prcnt_MD", "Upper_95prcnt_MD"), c("Lower_95prcnt_MD", "Upper_95prcnt_MD"))
facets <- c("Study_Design_Clean","Outcome___Clean", "Intervention/Exposure___Clean","Comparator___Clean_2")
  # Block results by Outcome [BMI, BW, Body Fat %, Body Fat (kg), HR/RR Ab OB, HR/RR OB, WC] – Column L
  # Block results by MD and SMD – Columns O-T
  # Intervention – Column H and I
  # Comparator – Column J
  # Forest plots - rows highlighted in yellow are highest priority, followed by the orange rows
  # ·        RCT or CT, BMI, all intervention/comparator, MD
  # ·        RCT or CT, BMI, all intervention/comparator, SMD
  # o   Include Laviada-Molina, 2020
  # ·        RCT or CT, BW, all intervention/comparator, MD
  # ·        RCT or CT, BW, all intervention/comparator, SMD
  # o   Include Laviada-Molina, 2020
  # ·        RCT or CT, BF %, all intervention/comparator, MD
  # ·        RCT or CT, BF %, all intervention/comparator, SMD
  # ·        RCT or CT, BF (kg), all intervention/comparator, MD
  # ·        RCT or CT, WC, all intervention/comparator, MD
  # ·        RCT or CT, WC, all intervention/comparator, SMD
  # ·        RCT or CT, BW, all intervention, Compare= Sucrose/sugar, MD
  # ·        RCT or CT, BW, all intervention, Compare= Sucrose/sugar, SMD
  # ·        RCT or CT, BMI, all intervention, Compare= Sucrose/sugar, MD
  # ·        RCT or CT, BW, all intervention, Compare=no LCS OR Water/nothing/placebo, MD
  # ·        RCT or CT, BW, all intervention, Compare=no LCS OR Water/nothing/placebo, SMD
  # ·        RCT or CT, BMI, all intervention, Compare=no LCS OR Water/nothing/placebo, MD
  # ·        RCT or CT, BMI, all intervention, Compare=no LCS OR Water/nothing/placebo, SMD
  # ·        RCT or CT, BW, Int=LCSB, Compare=SSB, MD
  # ·        RCT or CT, BMI, Int=LCSB, Compare=SSB, MD
  # ·        RCT or CT, BW, Int=LCSB, Compare= no LCS OR Water/nothing/placebo, MD
  # ·        RCT or CT, BMI, Int=LCSB, Compare= no LCS OR Water/nothing/placebo, MD
  # ·        Cohort, BMI, all intervention/comparator, MD
  # ·        Cohort, BMI, all intervention/comparator, SMD
  # ·        Cohort, BW, all intervention/comparator, MD
  # ·        Cohort, BW, all intervention/comparator, SMD
  # ·        Cohort, Body fat (kg), all intervention/comparator, MD
  # ·        Cohort, HR/RR AB OB, all intervention/comparator, MD
  # ·        Cohort, HR/RR OB, all intervention/comparator, MD
  # ·        Cohort, HR/RR OB, all intervention/comparator, SMD
  # ·        Cohort, BW, all intervention, Compare = Lower/lowest LCS, MD
  # ·        Cohort, BW, all intervention, Compare = Lower/lowest LCS, SMD
  # ·        Cohort, BMI, all intervention, Compare = Lower/lowest LCS, MD
  # ·        Cohort, BMI, all intervention, Compare = Lower/lowest LCS, SMD
  # ·        Cohort, HR/RR OB, all intervention, Compare = Lower/lowest LCS, MD
  # ·        Cohort, BW, Int=LCSB, Compare= Sucrose/sugar, MD
  # ·        Cohort, BW, Int=LCSB, Compare= Sucrose/sugar, SMD
  # ·        Cohort, BW, Int=LCSB, Compare= Lower LCS or Lowest LCS, MD
  # ·        Cohort, BW, Int=LCSB, Compare= Lower LCS or Lowest LCS, SMD
  # ·        Cohort, HR/RR OB, Int=LCSB, Compare= Lower LCS or Lowest LCS, MD
  
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("ggplot2", quietly = TRUE))  BiocManager::install("ggplot2")
library("ggplot2")

df <- read.table("md_smd_results.txt", sep="\t",
                 header = T, check.names = F,
                 encoding = "latin1", na.strings = c("NA"))

orig_names <- colnames(df)
# fix the column names that have spaces (replace with "_")
names(df) <- gsub(" ", "_", names(df))
# replace "%" with "prcnt"
names(df) <- gsub("%", "prcnt", names(df))
names(df) <- gsub("-", "_", names(df))
# replace "-" with NA for cells
df[df == "-"] <- NA
# The "-" symbols were problematic for numeric columns, therefore reclassifying columns.
df[] <- lapply(df, type.convert, as.is = TRUE)
# Fix some inconsistancies
df[df$S == "-"] <- NA

# Need unique variables to use for y-axis
my_uniques <- paste(df$Reference,df$Number_of_Studies)
# make sure that we have unique ids
length(my_uniques) == nrow(df)
# Should be "TRUE"
df$uniques <- my_uniques


new_df <- df[!is.na(df$MD),]#drop rows with NA in MD column

pdf(file = file.path("output", "study_design_md.pdf"),
    width = 9, height = 11)

g <- ggplot2::ggplot(data = new_df, aes(x=uniques, y=MD,
                                        ymin=Lower_95prcnt_MD, 
                                        ymax=Upper_95prcnt_MD)) +
  geom_pointrange()+# Makes range for ggplot values based on the data and AES specified in first line
  geom_hline(yintercept=0, lty=2, size =1)+ # add a dotted line at x=0 after flip
  geom_errorbar( width=0.3, cex=1)+ # Makes whiskers on the range (more aesthetically pleasing)
  facet_wrap(~Study_Design_Clean)+ # Makes DV header (Can handle multiple DVs)
  coord_flip()+  # flip coordinates (puts labels on y axis)
  geom_point(shape = 15, size = 2)+ # specifies the 5size and shape of the geompoint
  ggtitle("Study Design")+ # Blank Title for the Graph
  xlab("Independent Variables")+ # Label on the Y axis (flipped specification do to coord_flip)
  ylab("95% CI")+ # Label on the X axis (flipped specification do to coord_flip)
  # scale_y_continuous(limits = c(-.75,.75), breaks = c(-.75,-.38,0,.38,.75)) # limits and tic marks on X axis (flipped specification do to coord_flip)
  # scale_y_continuous(limits = c(-.75,.75), breaks = c(-.75,-.38,0,.38,.75))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
  theme(line = element_line(colour = "black", size = 1), # My personal theme for GGplots
        # strip.background = element_rect(fill="gray90"),
        legend.position ="none",
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        # panel.border= element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        # panel.background = element_blank(),
        panel.spacing = unit(2, "lines"), # added to theme to add space in between facet_wrap plots
        # axis.ticks = element_blank(),
        # axis.title.x = element_text(family="Times",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l =0)),
        # axis.title.y = element_text(family="Times",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        # plot.title = element_text(family="Times", colour = "Black", margin = margin(t = 0, r = 0, b = 20, l = 0)),
        # axis.text=element_text(family="Times",size=24, color = "Black"),
        # text=element_text(family="Times",size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm")
        )
g
dev.off()