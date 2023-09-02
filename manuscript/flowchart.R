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
  
          'Raw Spreadsheet' -> 'Clean Coordinates' 
              [label = '     dms2dd', fontsize = 26, penwidth = 6]
          'Clean Coordinates' -> 'Geospatial Data' 
              [label = '     coords2sf', fontsize = 26, penwidth = 6]
          'Geospatial Data' -> 'Political\nBoundaries' 
              [label = '     political_grabber', fontsize = 26, penwidth = 6]
          'Political\nBoundaries' -> 'Site\nCharacteristics'
              [label = '     physical_grabber', fontsize = 26, penwidth = 6]
          'Site\nCharacteristics' -> 'Site Name' 
              [label = '     site_writer', fontsize = 26, penwidth = 6]
              
          'Geospatial Data' -> 'Directions'
              [label = '     directions_grabber', fontsize = 26, penwidth =6, style=dashed]
          'Directions' -> 'Site Name'  [penwidth = 6, style=dashed]
      }
              
              
          # Taxonomic Operations
      subgraph cluster_taxonomy { style=dashed; color= 'white'; 
        node [color = '#176B87']
  
          'Site Name' -> 'Spell Check (Collection)' 
              [label = '     spell_check', fontsize = 26, penwidth = 6]
          'Spell Check (Collection)' -> 'Spell Check\n(Authorities)' 
              [label = '     author_check', fontsize = 26, penwidth = 6]
          'Spell Check\n(Authorities)' -> 'Spell Check\n(Associated Species)' 
              [label = '    associates_check', fontsize = 26, penwidth = 6]
              
          'Spell Check (Collection)' -> 'Synonym\nCheck' 
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
          'Date Formatting' -> 'Processed Spreadsheet' 
              [penwidth = 6]
              
          'Processed Spreadsheet' -> 'Manual Review' 
              [penwidth = 6]
      
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

