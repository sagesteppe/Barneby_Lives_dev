library(tidyverse)
library(DiagrammeR)
library(ggpubr)
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
              [label = '     dms2dd', fontsize = 40, penwidth = 6]
          'Convert Coordinates\nfrom DMS to DD' -> 'Clean Coordinates' 
              [label = '     autofill_checker', fontsize = 40, penwidth = 6]
          'Clean Coordinates' -> 'Geospatial Data' 
              [label = '     coords2sf', fontsize = 40, penwidth = 6]
          'Geospatial Data' -> 'Political\nBoundaries' 
              [label = '     political_grabber', fontsize = 40, penwidth = 6]
          'Political\nBoundaries' -> 'Site Characteristics' 
              [label = '     physical_grabber', fontsize = 40, penwidth = 6]
          'Site Characteristics' -> 'Site Name' 
              [label = '     site_writer', fontsize = 40, penwidth = 6]
              
              
          'Raw Spreadsheet' -> 'Seperate\nname'
              [label = '     split_binomial', fontsize = 40, penwidth = 6, style = dashed]
          'Raw Spreadsheet' -> 'Check for\nautofill values' 
              [label = '     autofill_checker', fontsize = 40, penwidth = 6, style = dashed]
          'Seperate\nname' -> 'Convert Coordinates\nfrom DMS to DD' 
              [penwidth = 6, style = dashed]
          'Check for\nautofill values' -> 'Convert Coordinates\nfrom DMS to DD' 
              [penwidth = 6, style = dashed]
              
          'Geospatial Data' -> 'Directions'
              [label = '     directions_grabber', fontsize = 40, penwidth =6, style=dashed]
          'Directions' -> 'Site Name'  [penwidth = 6, style=dashed]
      }
              
              
          # Taxonomic Operations
      subgraph cluster_taxonomy { style=dashed; color= 'white'; 
        node [color = '#176B87']
  
          'Site Name' -> 'Spell Check (Binomial)' 
              [label = '     spell_check', fontsize = 40, penwidth = 6]
          'Spell Check (Binomial)' -> 'Spell Check (Family)' 
              [label = '     spell_check_family', fontsize = 40, penwidth = 6]
          'Spell Check (Family)' -> 'Spell Check\n(Authorities)' 
              [label = '     author_check', fontsize = 40, penwidth = 6]
          'Spell Check\n(Authorities)' -> 'Spell Check\n(Associated Species)' 
              [label = '    associates_check', fontsize = 40, penwidth = 6]
              
          'Spell Check (Binomial)' -> 'Synonym\nCheck' 
              [label = '    powo_searcher', fontsize = 40, penwidth = 6, style=dashed]
           'Synonym\nCheck' -> 'Spell Check\n(Associated Species)'  [penwidth = 6, style=dashed]
      }
      
           # Label Quality
      subgraph cluster_miscellaneous {
        style=dashed; color='white'; 
        node [color = '#94524A']
          'Spell Check\n(Associated Species)' -> 'Remove collection\nfrom Associated Species' 
              [label = '    associate_dropper', fontsize = 40, penwidth = 6]
          'Remove collection\nfrom Associated Species' -> 'Check Field Lengths'
              [label = '     field_lengths', fontsize = 40, penwidth = 6]
          'Check Field Lengths' -> 'Date Formatting' 
              [label = '     date_parser', fontsize = 40, penwidth = 6]

      }
      
      node [color = '#C94277']
          'Date Formatting' -> 'Processed\nSpreadsheet' 
              [penwidth = 6]
          'Processed\nSpreadsheet' -> 'Manual Review' 
              [penwidth = 6]
                            
          'Date Formatting' -> 'Geodata' 
              [label = '     geodata_writer', fontsize = 40, penwidth = 6, style=dashed]
          'Geodata' -> 'Manual Review'
              [penwidth = 6, style = dashed]
      
          # Printed Materials
      subgraph cluster_printing {style=dashed; color='white'; 
        node [color = '#A27E6F']
          'Manual Review' -> 'Label\nMaker' 
              [label = '    labels\nskeleton', fontsize = 40, penwidth = 6]
          'Manual Review' -> 'Shipping\nManifest' 
              [label = '    manifest\nskeleton', fontsize = 40, penwidth = 6]
              
           'Label\nMaker' -> 'Label\nAssembly' 
              [label = '    creating\nlabels', fontsize = 40, penwidth = 6]
      }  
}
") %>%  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/flowchart.png")






grViz("

digraph boxes_and_circle {
  graph[rankdir = LR, ratio = 0.1]
  
  node [shape = circle, fontsize = 36, fontname = arial, style = filled, fontcolor = White]

      subgraph cluster_import{color= 'white'
        node [color = '#2F0A28']
        'Raw\nSpreadsheet'
      }
      
        # Geospatial Operations
      subgraph cluster_geospatial { style=dashed; color= 'white'; 
        node [color = '#ffa600']
  
          'Raw\nSpreadsheet' -> 'Convert\nCoordinates\nfrom\nDMS to DD' 
              [label = '     dms2dd', fontsize = 32, penwidth = 6]
          'Convert\nCoordinates\nfrom\nDMS to DD' -> 'Clean\nCoordinates' 
              [label = '     autofill_checker', fontsize = 32, penwidth = 6]
          'Clean\nCoordinates' -> 'Geospatial\nData' 
              [label = '     coords2sf', fontsize = 32, penwidth = 6]
          'Geospatial\nData' -> 'Political\nBoundaries' 
              [label = '     political_grabber', fontsize = 32, penwidth = 6]
          'Political\nBoundaries' -> 'Site\nCharacteristics' 
              [label = '     physical_grabber', fontsize = 32, penwidth = 6]
          'Site\nCharacteristics' -> 'Site\nName' 
              [label = '     site_writer', fontsize = 32, penwidth = 6]
              
          'Raw\nSpreadsheet' -> 'Check for\nautofill\nvalues' 
              [label = '     autofill_checker', fontsize = 32, penwidth = 6, style = dashed]
          'Check for\nautofill\nvalues' -> 'Convert\nCoordinates\nfrom\nDMS to DD' 
              [penwidth = 6, style = dashed]
          'Geospatial\nData' -> 'Directions'
              [label = '     directions_grabber', fontsize = 32, penwidth =6, style=dashed]
          'Directions' -> 'Site\nName'  [penwidth = 6, style=dashed]
      }
      }") %>%  
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Spatmod.png")



grViz("

digraph boxes_and_circle {
  graph[rankdir = LR, ratio = 0.1]
  
  node [shape = circle, fontsize = 36, fontname = arial, style = filled, fontcolor = White]
      
      subgraph cluster_import{color= 'white'
        node [color = '#ffa600']
        '  Spatial\nData  '
      }
      
          # Taxonomic Operations
      subgraph cluster_taxonomy { style=dashed; color= 'white'; 
        node [color = '#176B87']
  
          '  Spatial\nData  ' -> 'Spell\nCheck\n(Binomial)' 
              [label = '     spell_check', fontsize = 40, penwidth = 6]
          'Spell\nCheck\n(Binomial)' -> 'Spell\nCheck\n(Family)' 
              [label = '     spell_check_family', fontsize = 40, penwidth = 6]
          'Spell\nCheck\n(Family)' -> 'Spell\nCheck\n(Authorities)' 
              [label = '     author_check', fontsize = 40, penwidth = 6]
          'Spell\nCheck\n(Authorities)' -> 'Spell\nCheck\n(Associated\nSpecies)' 
              [label = '    associates_check', fontsize = 40, penwidth = 6]
              
          'Spell\nCheck\n(Binomial)' -> 'Synonym\nCheck' 
              [label = '    powo_searcher', fontsize = 40, penwidth = 6, style=dashed]
           'Synonym\nCheck' -> 'Spell\nCheck\n(Associated\nSpecies)'  [penwidth = 6, style=dashed]
      }
      }") %>%  
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Taxmod.png")



grViz("

digraph boxes_and_circle {
  graph[rankdir = LR, ratio = 0.1]
  
  # Formatting
  node [shape = circle, fontsize = 36, fontname = arial, style = filled, fontcolor = White]
  
    subgraph cluster_import{color= 'white'
        node [color = '#176B87']
        'Taxonomic\nData'
    }
      
      
    subgraph cluster_miscellaneous {
        style=dashed; color='white'; 
        node [color = '#94524A']
          'Taxonomic\nData' -> 'Remove\ncollection\nfrom\nAssociated\nSpecies' 
              [label = '    associate_dropper', fontsize = 40, penwidth = 6]
          'Remove\ncollection\nfrom\nAssociated\nSpecies' -> 'Check\nField\nLengths'
              [label = '     field_lengths', fontsize = 40, penwidth = 6]
          'Check\nField\nLengths' -> 'Date\nFormatting' 
              [label = '     date_parser', fontsize = 40, penwidth = 6]

      }
      }
      }") %>%  
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Formatmod.png")




grViz("

digraph boxes_and_circle {
  
  graph[rankdir = LR, ratio = 0.1]
  node [shape = circle, fontsize = 36, fontname = arial, style = filled, fontcolor = White]
  
    subgraph cluster_import{color= 'white'
        node [color = '#94524A']
        'Formatted\nData'
    }

      node [color = '#C94277']
          'Formatted\nData' -> 'Processed\nSpreadsheet' 
              [penwidth = 6]
                            
          'Formatted\nData' -> 'Geodata' 
              [label = '     geodata_writer', fontsize = 40, penwidth = 6, style=dashed]
          'Geodata' -> 'Processed\nSpreadsheet'
              [penwidth = 6, style = dashed]
              
          'Processed\nSpreadsheet' -> 'Manual\nReview' 
              [penwidth = 6]
              
      }  
}") %>% 
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Reviewmod.png")



grViz("

digraph boxes_and_circle {
  
  graph[rankdir = LR, ratio = 0.1]
  
    
  node [shape = circle, fontsize = 36, fontname = arial, style = filled, fontcolor = White]

  subgraph cluster_import{color= 'white'
        node [color = '#C94277']
        'Clean\nData'
    }
          # Final Materials
      subgraph cluster_printing {style=dashed; color='white'; 
        node [color = '#A27E6F']
        
          'Clean\nData' -> 'Label\nMaker' 
              [label = '    labels\nskeleton', fontsize = 40, penwidth = 6]
          'Label\nMaker' -> 'Label\nAssembly' 
              [label = '    render_labels.sh', fontsize = 40, penwidth = 6]
          'Label\nAssembly' -> 'Database\nImport' 
              [label = '    format_database_import', fontsize = 40, penwidth = 6]
          'Database\nImport' -> 'Shipping\nManifest' 
              [label = '    manifest\nskeleton', fontsize = 40, penwidth = 6] 
              }  }") %>% 
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Resultsmod.png")


library(ggpubr)
text <- paste("Sample workflow for using BarnebyLives on data which has been manually entered into a spreadsheet.",
              "The top two rows indicate the main data cleaning functionality and are best run in the order outline above",
              "although taxonomic steps may be ran before spatial steps.",
              'The third row can be interspersed with the above two, but indicates
              a step where most input data are formatted for downstream applications.',
              'The final row include creation of labels, which allows for detection of formatting or other issues which 
              were not captured by the pipeline or in earlier manual review.',
              '',
              sep = "\n")

# Create a text grob
tgrob <- text_grob(text, face = "italic", color = "steelblue")
# Draw the text
as_ggplot(tgrob)

ggsave('../graphics/plots/caption.png')

