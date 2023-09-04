library(gt)
setwd('/media/steppe/hdd/Barneby_Lives-dev/manuscript')

footnote = 'Counties and States are merged into the same dataset while setting up the package. The value for "County" includes State.'

local_data <- data.frame(
  Variable = c(
    'County', 'State', 'Ownership', 
    'TRS', 'Place Names', 'Mountains',
    'Elevation', 'Slope', 'Aspect',
    'Geomorphons', 'Surficial Geology', 'Taxonomic Spellings', 
    'Author Abbreviations'
  ),
  Usage = c(
    'Political', '', '',
    '', 'Site Name', 'Site Name', 
    'Site Characteristics', '', '', 
    '', '', 'Spell Checks', 
    ''
  ),
  Source = c(
    'US Census Bureau', '', 'US Geological Survey',
    '', '', 'EarthEnv', 
    'Open Topography', '', '',
    '', 'US Geological Survey', 'World Flora Online',
    'IPNI'
  ),
  Name = c(
    'Counties', 'States', 'Protected Areas Database', 
    'Public Land Survey System', 'Geographic Names Information System', 'GMBA Mountain Inventory v2',
    'Geomorpho90m - Elevation', 'Geomorpho90 - Slope', 'Geomorpho90m - Aspect',
    'Geomorpho90m - Geomorphons', 'State Geologic Map Compilation', 'World Flora Online',
    'International Plant Names Index'
  ),
  Data_Model = c(
    'Vector', '', '',
    '', '', '',
    'Raster', '', '', 
    '', 'Vector', 'Text',
    ''
  ),
  Subset_Size = c(
    '0.073', '0.0*', '0.435',
    '0.816', '0.081', '0.004',
    '4.2', '4.6', '4.1',
    '0.455', '0.708', '0.002',
    '0.001'
  )
)

data_sources <- local_data |> 
  gt() |>  
  fmt_markdown(columns = everything())|> 
  tab_header(
    title = 'Data Sources for Package')|> 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |> 
  cols_align(align = "center" ) |> 
  cols_label(
    Data_Model = "Data Model",
    Subset_Size = "Size (GiB)") |>  
  tab_footnote(footnote) |> 
  
  tab_options(heading.background.color = "#94524A",
              heading.border.lr.color='#2F0A28', 
              table.background.color='#eff4ec',
              row.striping.background_color = 'black',
              footnotes.background.color = '#A27E6F',
              heading.padding = 0)


gtsave(data_sources, "../graphics/tables/rab_tab_examples.png")




sessioninfo::session_info()
