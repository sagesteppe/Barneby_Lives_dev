library(tidyverse)
library(DiagrammeR)
setwd('/media/steppe/hdd/Barneby_Lives-dev/manuscript')

grViz("

digraph boxes_and_circle {
  
  node [shape = oval, fontsize = 28, fontname = arial, style = filled, fontcolor = White]

      subgraph cluster_import{color= 'white'
        node [color = '#2F0A28']
        'Raw Spreadsheet'
      }
      
        # Geospatial Operations
      subgraph cluster_geospatial { style=dashed; color= 'white'; 
        node [color = '#ffa600']
  
          'Raw Spreadsheet' -> 'Convert Coordinates\nfrom DMS to DD' 
              [label = '     dms2dd', fontsize = 26, penwidth = 6]
          'Convert Coordinates\nfrom DMS to DD' -> 'Clean Coordinates' 
              [label = '     autofill_checker', fontsize = 26, penwidth = 6]
          'Clean Coordinates' -> 'Geospatial Data' 
              [label = '     coords2sf', fontsize = 26, penwidth = 6]
          'Geospatial Data' -> 'Political\nBoundaries' 
              [label = '     political_grabber', fontsize = 26, penwidth = 6]
          'Political\nBoundaries' -> 'Site Characteristics' 
              [label = '     physical_grabber', fontsize = 26, penwidth = 6]
          'Site Characteristics' -> 'Site Name' 
              [label = '     site_writer', fontsize = 26, penwidth = 6]
              
              
          'Raw Spreadsheet' -> 'Seperate\nname'
              [label = '     split_binomial', fontsize = 26, penwidth = 6, style = dashed]
          'Raw Spreadsheet' -> 'Check for\nautofill values' 
              [label = '     autofill_checker', fontsize = 26, penwidth = 6, style = dashed]
          'Seperate\nname' -> 'Convert Coordinates\nfrom DMS to DD' 
              [penwidth = 6, style = dashed]
          'Check for\nautofill values' -> 'Convert Coordinates\nfrom DMS to DD' 
              [penwidth = 6, style = dashed]
              
          'Geospatial Data' -> 'Directions'
              [label = '     directions_grabber', fontsize = 26, penwidth =6, style=dashed]
          'Directions' -> 'Site Name'  [penwidth = 6, style=dashed]
      }
              
              
          # Taxonomic Operations
      subgraph cluster_taxonomy { style=dashed; color= 'white'; 
        node [color = '#176B87']
  
          'Site Name' -> 'Spell Check (Binomial)' 
              [label = '     spell_check', fontsize = 26, penwidth = 6]
          'Spell Check (Binomial)' -> 'Spell Check (Family)' 
              [label = '     spell_check_family', fontsize = 26, penwidth = 6]
          'Spell Check (Family)' -> 'Spell Check\n(Authorities)' 
              [label = '     author_check', fontsize = 26, penwidth = 6]
          'Spell Check\n(Authorities)' -> 'Spell Check\n(Associated Species)' 
              [label = '    associates_check', fontsize = 26, penwidth = 6]
              
          'Spell Check (Binomial)' -> 'Synonym\nCheck' 
              [label = '    powo_searcher', fontsize = 26, penwidth = 6, style=dashed]
           'Synonym\nCheck' -> 'Spell Check\n(Associated Species)'  [penwidth = 6, style=dashed]
      }
      
           # Label Quality
      subgraph cluster_miscellaneous {
        style=dashed; color='white'; 
        node [color = '#94524A']
          'Spell Check\n(Associated Species)' -> 'Remove collection\nfrom Associated Species' 
              [label = '    associate_dropper', fontsize = 26, penwidth = 6]
          'Remove collection\nfrom Associated Species' -> 'Check Field Lengths'
              [label = '     field_lengths', fontsize = 26, penwidth = 6]
          'Check Field Lengths' -> 'Date Formatting' 
              [label = '     date_parser', fontsize = 26, penwidth = 6]

      }
      
      node [color = '#C94277']
          'Date Formatting' -> 'Processed\nSpreadsheet' 
              [penwidth = 6]
          'Processed\nSpreadsheet' -> 'Manual Review' 
              [penwidth = 6]
                            
          'Date Formatting' -> 'Geodata' 
              [label = '     geodata_writer', fontsize = 26, penwidth = 6, style=dashed]
          'Geodata' -> 'Manual Review'
              [penwidth = 6, style = dashed]
      
          # Printed Materials
      subgraph cluster_printing {style=dashed; color='white'; 
        node [color = '#A27E6F']
          'Manual Review' -> 'Label\nMaker' 
              [label = '    labels\nskeleton', fontsize = 26, penwidth = 6]
          'Manual Review' -> 'Shipping\nManifest' 
              [label = '    manifest\nskeleton', fontsize = 26, penwidth = 6]
              
           'Label\nMaker' -> 'Label\nAssembly' 
              [label = '    creating\nlabels', fontsize = 26, penwidth = 6]
      }  
}
") %>%  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/flowchart.png")

