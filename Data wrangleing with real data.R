
#cell phenotype 
cell_pheno <- read.csv("https://raw.githubusercontent.com/How-to-Learn-to-Code/Rclass-DataScience/main/data/wrangling-files/cellPhenotypes.csv", 
                       sep=",", header = TRUE)
head(cell_pheno)

# Load the data. The sample IDs were stored as the first row, so lets make those the row.names
cell_prop <- read.table("https://raw.githubusercontent.com/How-to-Learn-to-Code/Rclass-DataScience/main/data/wrangling-files/cellProportions.csv",
                         row.names = 1, header = TRUE, sep = ",")
head(cell_prop)

cell_bind <- cbind(cell_pheno, cell_prop)
cell_combined <-merge(cell_pheno, cell_prop, by.x = "id", by.y="row.names")
head(cell_combined)  
  
  #table
  table(cell_combined$genotype, useNA = "ifany")
  
  #summarize
  
  cell_combined |> group_by(genotype, treatment) |> summarise(n())
  
  #
  library(tidyverse)
  
 cell_long <-  pivot_longer(cell_combined,
               col= colnames(cell_prop),
               names_to ="cell.type", values_to = "proportion")
  
  cell_long <- cell_long |> dplyr::filter(type =="whole_tissue") |> dplyr::mutate(id=as.character(id))
    ggplot(cell_long, aes(x=id, y=proportion, fill = cell.type)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle=45, hjust =1 , vjust =1.5, size=8)) +
      labs(title= "cell proportions by ids", x="group", y="proptions")

  cell_long |> dplyr::filter(type == "whole_tissue") |> dplyr::mutate(id=as.character(id)) |>
    ggplot(aes(x=id, y=proportion, fill = cell.type)) +
    geom_bar(stat="identity") +
    facet_grid(.~type,scale="free") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45))
    
  #boxplot
  cell_long |> dplyr::filter(type == "whole_tissue") |> dplyr::mutate(id=as.character(id)) |>
    ggplot(aes(x=cell.type, y=proportion,)) +
    geom_bar(stat="summary", fun="mean") +
    facet_grid(genotype~treatment, scale="free") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, vjust =0.5)) +
    labs(y="cell type proportion")
  