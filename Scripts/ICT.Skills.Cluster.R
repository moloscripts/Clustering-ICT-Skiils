# Author: Molo  Muli
# Commencement Date: 7th July 2022


# Libraries and Data set ####

## Libraries ####
library(easypackages)
libraries("inspectdf", "tidyverse","dlookr","tufte","formattable")

## Dataset ####
ICTSkills <- read.csv("Data/ICTSkills.csv")
ICTSkills <- ICTSkills %>%
  select(-c(ISO3, Source, Performed.at.least.one.out.of.nine.activities))

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

NumericDescriptives <- diagnose_numeric(ICTSkills)
NumericDescriptives <- NumericDescriptives %>%
  select(-c(min, minus)) %>%
  arrange(desc(outlier))

ND <- NumericDescriptives %>%
  mutate_if(is.numeric, round, digits=3) 
# %>%
# formattable(ND,
#             list(variables = formatter("span", style = ~ style(color="#513252", font.weight="bold")),
#                  mean=percent, 
#                  outlier=color_bar("#EB4747")))

FormattableND <- formattable(ND, 
            # align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(variables = formatter("span", style = ~ style(color = "#47B5FF", font.weight = "bold")), 
                 outlier = color_bar("#F5DF99"))
            )
FormattableND



