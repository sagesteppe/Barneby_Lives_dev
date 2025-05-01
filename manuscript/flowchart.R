library(tidyverse)
library(DiagrammeR)
library(ggpubr)
#setwd('/media/steppe/hdd/Barneby_Lives-dev/manuscript')
setwd('/home/sagesteppe/Documents/assoRted/Barneby_Lives_dev/manuscript')

grViz("

digraph boxes_and_circle {
  graph[rankdir = LR, ratio = 0.2, nodesep = 0.01, ranksep = 0.01]
  
  node [shape = circle, fontsize = 72, fontname = arial, style = filled, fontcolor = White]
      
        # Geospatial Operations
      subgraph cluster_geospatial {style=dashed; color= 'white'; 
        node [color = '#94524A']

          'Convert\nfrom\nDMS to DD' -> 'Check for\nautofill\nvalues' 
              [label = '     autofill_checker', fontsize = 72, penwidth = 6, style = dashed]
          'Check for\nautofill\nvalues' -> 'Geospatial\nData' 
              [label = '     coords2sf', fontsize = 72, penwidth = 6]
          'Raw\nSpreadsheet' -> 'Check for\nautofill\nvalues' 
              [label = '     autofill_checker', fontsize = 72, penwidth = 6]
          'Geospatial\nData' -> 'Political\nBoundaries' 
              [label = '     political_grabber', fontsize = 72, penwidth = 6]
          'Political\nBoundaries' -> 'Site\nCharacteristics' 
              [label = '     physical_grabber', fontsize = 72, penwidth = 6]
          'Site\nCharacteristics' -> 'Site\nName' 
              [label = '     site_writer', fontsize = 72, penwidth = 6]
              
          'Site\nCharacteristics' -> 'Directions'
              [label = '     directions_grabber', fontsize = 72, penwidth =6, style=dashed]
          'Directions' -> 'Site\nName'  
              [label = '     site_writer', fontsize = 72, penwidth =6, style=dashed]
      }
      }") %>%  
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Spatmod.png")



grViz("

digraph boxes_and_circle {
  graph[rankdir = LR, ratio = 0.1, nodesep = 0.01, ranksep = 0.01]
  
  node [shape = circle, fontsize = 72, fontname = arial, style = filled, fontcolor = White]
      
      subgraph cluster_import{color= 'white'
        node [color = '#94524A', shape = egg]
        '  Spatial\nData  '
      }
      
          # Taxonomic Operations
      subgraph cluster_taxonomy { style=dashed; color= 'white'; 
        node [color = '#176B87']
  
          '  Spatial\nData  ' -> 'Spell\nCheck\n(Binomial)' 
              [label = '     spell_check', fontsize = 72, penwidth = 6]
          'Spell\nCheck\n(Binomial)' -> 'Spell\nCheck\n(Family)' 
              [label = '     spell_check_family', fontsize = 72, penwidth = 6]
          'Spell\nCheck\n(Family)' -> 'Spell\nCheck\n(Authorities)' 
              [label = '     author_check', fontsize = 72, penwidth = 6]
          'Spell\nCheck\n(Authorities)' -> 'Spell\nCheck\n(Associated\nSpecies)' 
              [label = '    associates_check', fontsize = 72, penwidth = 6]
              
          'Spell\nCheck\n(Binomial)' -> 'Synonym\nCheck' 
              [label = '    powo_searcher', fontsize = 72, penwidth = 6, style=dashed]
           'Synonym\nCheck' -> 'Spell\nCheck\n(Associated\nSpecies)'  [penwidth = 6, style=dashed]
      }
      }") %>%  
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Taxmod.png")



grViz("

digraph boxes_and_circle {
  graph[rankdir = LR, ratio = 0.1, nodesep = 0.01, ranksep = 0.01]
  
  # Formatting
  node [shape = circle, fontsize = 72, fontname = arial, style = filled, fontcolor = White]
  
    subgraph cluster_import{color= 'white'
        node [color = '#176B87', shape = egg]
        'Taxonomic\nData'
    }
      
      
    subgraph cluster_miscellaneous {
        style=dashed; color='white'; 
        node [color = '#A27E6F']
          'Taxonomic\nData' -> 'Remove\ncollection\nfrom\nAssociated\nSpecies' 
              [label = '    associate_dropper', fontsize = 72, penwidth = 6]
          'Remove\ncollection\nfrom\nAssociated\nSpecies' -> 'Check\nField\nLengths'
              [label = '     field_lengths', fontsize = 72, penwidth = 6]
          'Check\nField\nLengths' -> 'Date\nFormatting' 
              [label = '     date_parser', fontsize = 72, penwidth = 6]

      }
      }
      }") %>%  
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Formatmod.png")




grViz("

digraph boxes_and_circle {
  
  graph[rankdir = LR, ratio = 0.1, nodesep = 0.01, ranksep = 0.01]
  node [shape = circle, fontsize = 72, fontname = arial, style = filled, fontcolor = White]
  
    subgraph cluster_import{color= 'white'
        node [color = '#A27E6F', shape = egg]
        'Formatted\nData'
    }

      node [color = '#2F0A28']
          'Formatted\nData' -> 'Processed\nSpreadsheet' 
              [penwidth = 6]
                            
          'Formatted\nData' -> 'Geodata' 
              [label = '     geodata_writer', fontsize = 72, penwidth = 6, style=dashed]
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
  
  graph[rankdir = LR, ratio = 0.1, nodesep = 0.01, ranksep = 0.01]
  
  node [shape = circle, fontsize = 72, fontname = arial, style = filled, fontcolor = White]

  subgraph cluster_import{color= 'white'
        node [color = '#2F0A28', shape = egg]
        'Clean\nData'
    }
          # Final Materials
      subgraph cluster_printing {style=dashed; color='white'; 
        node [color = '#8DAB7F']
        
          'Clean\nData' -> 'Label\nMaker' 
              [label = '    labels\nskeleton', fontsize = 72, penwidth = 6]
          'Label\nMaker' -> 'Label\nAssembly' 
              [label = '    render_labels.sh', fontsize = 72, penwidth = 6]
          'Label\nAssembly' -> 'Database\nImport' 
              [label = '    format_database_import', fontsize = 72, penwidth = 6]
          'Database\nImport' -> 'Shipping\nManifest' 
              [label = '    manifest\nskeleton', fontsize = 72, penwidth = 6] 
              }  }") %>% 
  DiagrammeRsvg::export_svg() %>%
  charToRaw %>% 
  rsvg::rsvg_png("../graphics/plots/Resultmod.png")


text <- paste(
  "Sample workflow for using BarnebyLives on data which has been manually entered into a spreadsheet.",
  "The top two rows indicate the main data cleaning functionality and are best run in the order outlined",
  "above although taxonomic steps may be ran before spatial steps. The third row can be interspersed",
  "with the above two, includes creation of labels, which allows for detection of formatting or other",
  "issues which were not captured by the pipeline or in earlier manual review. Further support is",
  "offered to export data in a format which allows mass upload at the receiving institution, ", 
  "and to create a shipping manifest and transfer notice.",
  sep = "\n")

# Create a text grob
tgrob <- text_grob(text, face = "italic", size = 18)
# Draw the text
as_ggplot(tgrob)

ggsave('../graphics/plots/caption.png', width = 14, units = 'in')

