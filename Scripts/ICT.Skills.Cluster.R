# Author: Molo  Muli
# Commencement Date: 7th July 2022


# Libraries and Data set ####

## Libraries ####
library(devtools)
library(easypackages)
libraries("inspectdf", "tidyverse","dlookr","tufte","formattable","clustMixType","klaR","clusteval")

## Dataset ####
ICTSkills <- read.csv("Data/ICTSkills.csv")
ICTSkills <- ICTSkills %>%
  dplyr::select(-c(ISO3, Source, Performed.at.least.one.out.of.nine.activities))

# Rename 
ICTSkills <- ICTSkills %>%
  rename(`Copied or moved a file or folder` = Copied.or.moved.a.file.or.folder,
         `Used a copy and paste tool to duplicate or move information within a document`= Used.a.copy.and.paste.tool.to.duplicate.or.move.information.within.a.document, 
         `Sent e-mail with attached file, such as a document, picture or video` = Sent.e.mail.with.attached.file..such.as.a.document..picture.or.video, 
         `Used a basic arithmetic formula in a spreadsheet` = Used.a.basic.arithmetic.formula.in.a.spreadsheet, 
         `Connected and installed a new device e.g Modem` = Connected.and.installed.a.new.device..such.as.a.modem..camera.or.printer, 
         `Found, downloaded, installed and configured software` = Found..downloaded..installed.and.configured.software, 
         `Created an electronic presentation with presentation software` = Created.an.electronic.presentation.with.presentation.software..including.text..images..sound..video.or.charts,
         `Transferred a file between a computer and other device` = Transferred.a.file.between.a.computer.and.other.device, 
         `Wrote a computer program in any programming language` = Wrote.a.computer.program.in.any.programming.language, 
         # `Performed at least one out of nine activities` = Performed.at.least.one.out.of.nine.activities, 
         `UNICEF region` = UNICEF.Region, 
         `Wealth quantile` = Wealth.Quantile)

ICTSkills$`Wealth quantile` <- as.character(ICTSkills$`Wealth quantile`)
NumericDescriptives <- diagnose_numeric(ICTSkills)
NumericDescriptives <- NumericDescriptives %>%
  select(-c(min, minus)) %>%
  arrange(desc(outlier))

# Descritpive Analytics ####
ND <- NumericDescriptives %>%
  mutate_if(is.numeric, round, digits=3)

FormattableND <- formattable(ND, 
            # align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(variables = formatter("span", style = ~ style(color = "#47B5FF", font.weight = "bold")), 
                 outlier = color_bar("#F5DF99"))
            )
# FormattableND

NumericalDistribution <- ICTSkills %>%
  gather(variable, value, -c(1:4,14)) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill="lightblue2", color='black') + 
  facet_wrap(~variable, scales='free_x') + 
  labs(title = 'Distribution of numeric variables',x='values', y='Frequency') + 
  theme_minimal()

# Clustering ####
# Convert chars to factors ####
ICTSkills <- ICTSkills %>%
  mutate_if(is.character, as.factor)

# seed
set.seed(123)
k.max <- 5
wss <- sapply(1:k.max, 
              function(k){kproto(ICTSkills, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# clustering k-modes 
set.seed(367)
ict_clusters <- kproto(ICTSkills, k=3, lambda = NULL, iter.max = 100, nstart = 1, na.rm = TRUE, verbose = T)
ict_clusters
