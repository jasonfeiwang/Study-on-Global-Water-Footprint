

# Source all global variables, packages, functions, data and tables here
source('global.R')

## UI ####
ui <- navbarPage("",
                 navbarMenu("Global Virtual Water Flow",
                            tabPanel("Top and bottom countries",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("watermetric", "Select Metric:", c("Virtual water import"="import", "Virtual water export"="export",
                                                                                         "Net virtual water import" = "net")),
                                         radioButtons("countries", "Order of Countries:", c("Descending"="top", "Ascending"="bottom")),
                                         checkboxGroupInput("producttype", "Related Products:", choices = c("Crop"="crop", "Animal"="animal", "Industrial"="industrial"),
                                                            selected = c("Crop"="crop", "Animal"="animal", "Industrial"="industrial")),
                                         checkboxGroupInput("watertype", "Water Types:", choices = c("Green"="green", "Blue"="blue", "Grey"="grey"),
                                                            selected = c("Green"="green", "Blue"="blue", "Grey"="grey"))
                                       ),
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Top and bottom countries"),
                                                         style = "success",
                                                         ""),
                                         tabsetPanel(
                                           tabPanel("Table", dataTableOutput("topbottomTable")),
                                           tabPanel("Plot", br(), br(), plotlyOutput("topbottomPlot", height = "600px", width = "1000px"))
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Global distribution",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("watermetric_distr", "Select Metric:",
                                                      c("Virtual water import"="import", "Virtual water export"="export",
                                                        "Net virtual water import" = "net")),
                                         radioButtons("watertype_dist", "Water Types:", choices = c("Green"="green", "Blue"="blue", "Grey"="grey", "All" = "red"))
                                       ),
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Global distribution"),
                                                         style = "success",
                                                         ""),
                                         plotlyOutput("distributionPlot", height = "600px", width = "1100px")
                                       )
                                     )
                            ),
                            tabPanel("Ranking Per Country",
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("country_us", "Enter a country name:", value = "USA")
                                       ),
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Ranking Per Country"),
                                                         style = "success",
                                                         ""),
                                         dataTableOutput("rankingPercentageTable")
                                       )
                                     )
                            )
                 ),
                 navbarMenu("Per Capita Analysis",
                            tabPanel("Global Distribution",
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("year", "Select a year:", min = 2005, max = 2015, value = 2005, step = 1),
                                         radioButtons("countries_percapita", "Order of Countries:", c("Descending"="top", "Ascending"="bottom")),
                                         checkboxGroupInput("producttype_percapita", "Related Products:", choices = c("Crop"="crop", "Animal"="animal", "Industrial"="industrial"),
                                                            selected = c("Crop"="crop", "Animal"="animal", "Industrial"="industrial")),
                                         checkboxGroupInput("watertype_percapita", "Water Types:", choices = c("Green"="green", "Blue"="blue", "Grey"="grey"),
                                                            selected = c("Green"="green", "Blue"="blue", "Grey"="grey"))
                                       ),
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Export: top and bottom countries"),
                                                         style = "success",
                                                         ""),
                                         tabsetPanel(
                                           tabPanel("Table", dataTableOutput("topbottomTable_percapita")),
                                           tabPanel("Map", br(), br(), plotlyOutput("percapitaMap", height = "600px", width = "1100px"),
                                                    p("Note: Per Capita in Millions of m3 is plused by one and then transformed on log Scale (base 10).", align = 'center'))
                                         )
                                       )
                                     )
                                     
                            ),
                            tabPanel("Ranking Per Country",
                                     sidebarLayout(
                                       sidebarPanel(
                                         # sliderInput("year", "Select a year:", min = 2005, max = 2015, value = 2005, step = 1),
                                         radioButtons("countries_percapita_change", "Order of Countries:", c("Descending"="top", "Ascending"="bottom"))
                                       ),
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Export: per capita change"),
                                                         style = "success",
                                                         ""),
                                         tabsetPanel(
                                           tabPanel("Table", dataTableOutput("topbottomTable_percapita_change")),
                                           tabPanel("Plot", br(), br(), plotlyOutput("topbottomTable_percapita_change_plot", height = "600px", width = "1000px"))
                                         )
                                         
                                       )
                                     )
                                     
                            )
                 ),
                 tags$style(type = 'text/css',
                            '.navbar { font-family: Arial; font-size: 19px; }',
                            '.navbar-dropdown { font-family: Arial; font-size: 29px; }')
)




## Server ####
server <- function(input, output, session) {
  output$summary <- renderPrint({
    summary(cars)
  })
  
  VW_name_match <- data.frame(
    short = c("import", "export", "net"),
    name = c("Virtual water import", "Virtual water export", "Net virtual water import")
  )
  
  color_name_match <- data.frame(
    short = c("blue", "green", "grey", "red"),
    name = c("Blue only", "Green only", "Grey only", "All water types")
  )
  
  topbottom <- reactive({
    if(input$countries == "top"){
      df = (dfWaterFlow %>%
              filter(action %in% input$watermetric) %>%
              filter(product_type %in% input$producttype) %>%
              filter(water_type %in% input$watertype) %>%
              group_by(country) %>%
              summarize(water.footPrint = sum(amount)) %>%
              select(country, water.footPrint) %>%
              arrange(desc(water.footPrint)) %>%
              mutate(percentage = water.footPrint/sum(water.footPrint)) %>%
              mutate(water.footPrint = format(as.numeric(water.footPrint), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = format(round(percentage*100, 2), nsmall = 2, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = paste0(percentage, "%"))) %>%
        data.frame(stringsAsFactors = FALSE)
    } else {
      df = (dfWaterFlow %>%
              filter(action %in% input$watermetric) %>%
              filter(product_type %in% input$producttype) %>%
              filter(water_type %in% input$watertype) %>%
              group_by(country) %>%
              summarize(water.footPrint = sum(amount)) %>%
              select(country, water.footPrint) %>%
              arrange(water.footPrint) %>%
              mutate(percentage = water.footPrint/sum(water.footPrint)) %>%
              mutate(water.footPrint = format(as.numeric(water.footPrint), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = format(round(percentage*100, 2), nsmall = 2, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = paste0(percentage, "%"))) %>%
        data.frame(stringsAsFactors = FALSE)
    }
    names(df) = c("Country", "Virtual Water (Millions of m3)", "Percentage of World Total")
    return(df)
  })
  
  output$topbottomTable <- renderDataTable({
    if(input$watermetric != "net"){
      datatable(topbottom(), options = list(#columnDefs = list(list(className = 'dt-center', targets = 5)),
        pageLength = 5,
        lengthMenu = c(5, 10, 20)))
    } else {
      # topbottom()[, 3] = "NA"
      datatable(topbottom()[, 1:2], options = list(#columnDefs = list(list(className = 'dt-center', targets = 5)),
        pageLength = 5,
        lengthMenu = c(5, 10, 20)))
    }
    
  })
  
  output$topbottomPlot <- renderPlotly({
    plotdata = topbottom()[1:100, ]
    plotdata$Country <- factor(plotdata$Country, levels = topbottom()[1:100, ]$Country)
    plotdata = plotdata %>%
      mutate(`Virtual Water (Millions of m3)` = as.numeric(gsub(",", "", `Virtual Water (Millions of m3)`))
      )
    plot_ly(plotdata, x = ~Country, y = ~`Virtual Water (Millions of m3)`, type = 'bar',
            # name = 'USA',
            hoverinfo = 'text',
            text = ~paste('Water:', prettyNum(`Virtual Water (Millions of m3)`,big.mark=",",scientific=FALSE), '<br>Country:', Country)
    ) %>%
      layout(yaxis = list(title = 'Virtual Water (Millions of m³)', range = c(0, max(plotdata[,2]))),
             margin= list(b = 150, r = 80, l = 100),
             title = paste0(VW_name_match[VW_name_match$short==input$watermetric,]$name, ', ', as.character(input$countries), ' 100 countries'),
             xaxis = list(title = 'Country', tickangle = 45)
      )
  })
  
  distributionTable <- reactive({
    if(input$watermetric_distr == "net"){
      if(input$watertype_dist != "red"){
        df =  dfWaterFlow %>%
          filter(action %in% c('net')) %>%
          filter(water_type %in% input$watertype_dist) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }else{
        df =  dfWaterFlow %>%
          filter(action %in% c('net')) %>%
          filter(water_type %in% c('green', 'blue', 'grey')) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }
      
    } else {
      if(input$watertype_dist != "red"){
        df =  dfWaterFlow %>%
          filter(action == input$watermetric_distr) %>%
          filter(water_type %in% input$watertype_dist) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }else{
        df =  dfWaterFlow %>%
          filter(action == input$watermetric_distr) %>%
          filter(water_type %in% c('green', 'blue', 'grey')) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }
      
      
    }
    df = left_join(df, country_code, by = "country")
    df[df$country == "USA",]$code = "USA"
    return(df)
  })
  
  output$distributionPlot <- renderPlotly({
    plotdata = distributionTable()
    l <- list(color = toRGB("grey"), width = 0.5)
    names(plotdata) = c("Country", "Water", "Code")
    # plotdata = plotdata %>%
    #             mutate(Water = prettyNum(Water,big.mark=",",scientific=FALSE))
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(plotdata) %>%
      add_trace(z = ~Water, color = ~Water,
                colors = paste0(tools::toTitleCase(input$watertype_dist), 's'),
                locations = ~Code, marker = list(line = l),
                hovertext = 'text',
                text = ~paste('Water:', prettyNum(Water,big.mark=",",digit=0,scientific=FALSE), '<br>Country:', Country)
      ) %>%
      colorbar(title = 'in Millions of m3') %>%
      layout(title = paste0(VW_name_match[VW_name_match$short==input$watermetric_distr,]$name, ', ',
                            color_name_match[color_name_match$short==input$watertype_dist,]$name),
             geo = g)
  })
  
  rankingPercentage <- reactive({
    df =  dfWaterFlow %>%
      filter(action %in% c("import", "export")) %>%
      unite(temp1, action, product_type) %>%
      unite(temp, temp1, water_type) %>%
      spread(temp, amount) %>%
      mutate(total_import_green = import_crop_green + import_animal_green)  %>%
      mutate(total_import_blue = import_crop_blue + import_animal_blue + import_industrial_blue)  %>%
      mutate(total_import_grey = import_crop_grey + import_animal_grey + import_industrial_grey)  %>%
      mutate(total_export_green = export_crop_green + export_animal_green)  %>%
      mutate(total_export_blue = export_crop_blue + export_animal_blue + export_industrial_blue)  %>%
      mutate(total_export_grey = export_crop_grey + export_animal_grey + export_industrial_grey)  %>%
      mutate(net_green = total_import_green - total_export_green)  %>%
      mutate(net_blue = total_import_blue - total_export_blue)  %>%
      mutate(net_grey = total_import_grey - total_export_grey)  %>%
      mutate(percent_export_animal_blue = export_animal_blue/sum(export_animal_blue))  %>%
      mutate(percent_export_animal_green = export_animal_green/sum(export_animal_green))  %>%
      mutate(percent_export_animal_grey = export_animal_grey/sum(export_animal_grey))  %>%
      mutate(percent_export_crop_blue = export_crop_blue/sum(export_crop_blue))  %>%
      mutate(percent_export_crop_green = export_crop_green/sum(export_crop_green))  %>%
      mutate(percent_export_crop_grey = export_crop_grey/sum(export_crop_grey))  %>%
      mutate(percent_export_industrial_blue = export_industrial_blue/sum(export_industrial_blue))  %>%
      mutate(percent_export_industrial_grey = export_industrial_grey/sum(export_industrial_grey))  %>%
      mutate(percent_import_animal_blue = import_animal_blue/sum(import_animal_blue))  %>%
      mutate(percent_import_animal_green = import_animal_green/sum(import_animal_green))  %>%
      mutate(percent_import_animal_grey = import_animal_grey/sum(import_animal_grey))  %>%
      mutate(percent_import_crop_blue = import_crop_blue/sum(import_crop_blue))  %>%
      mutate(percent_import_crop_green = import_crop_green/sum(import_crop_green))  %>%
      mutate(percent_import_crop_grey = import_crop_grey/sum(import_crop_grey))  %>%
      mutate(percent_import_industrial_blue = import_industrial_blue/sum(import_industrial_blue))  %>%
      mutate(percent_import_industrial_grey = import_industrial_grey/sum(import_industrial_grey))  %>%
      mutate(percent_total_import_green = total_import_green/sum(total_import_green))  %>%
      mutate(percent_total_import_blue = total_import_blue/sum(total_import_blue))  %>%
      mutate(percent_total_import_grey = total_import_grey/sum(total_import_grey))  %>%
      mutate(percent_total_export_green = total_export_green/sum(total_export_green))  %>%
      mutate(percent_total_export_blue = total_export_blue/sum(total_export_blue))  %>%
      mutate(percent_total_export_grey = total_export_grey/sum(total_export_grey))  %>%
      mutate(percent_net_green = net_green/sum(net_green))  %>%
      mutate(percent_net_blue = net_blue/sum(net_blue))  %>%
      mutate(percent_net_grey = net_grey/sum(net_grey))  %>%
      arrange(desc(export_animal_blue))  %>%
      mutate(position_export_animal_blue = row_number())  %>%
      arrange(desc(export_animal_green))  %>%
      mutate(position_export_animal_green = row_number())  %>%
      arrange(desc(export_animal_grey))  %>%
      mutate(position_export_animal_grey = row_number())  %>%
      arrange(desc(export_crop_blue))  %>%
      mutate(position_export_crop_blue = row_number())  %>%
      arrange(desc(export_crop_green))  %>%
      mutate(position_export_crop_green = row_number())  %>%
      arrange(desc(export_crop_grey))  %>%
      mutate(position_export_crop_grey = row_number())  %>%
      arrange(desc(export_industrial_blue))  %>%
      mutate(position_export_industrial_blue = row_number())  %>%
      arrange(desc(export_industrial_grey))  %>%
      mutate(position_export_industrial_grey = row_number())  %>%
      arrange(desc(import_animal_blue))  %>%
      mutate(position_import_animal_blue = row_number())  %>%
      arrange(desc(import_animal_green))  %>%
      mutate(position_import_animal_green = row_number())  %>%
      arrange(desc(import_animal_grey))  %>%
      mutate(position_import_animal_grey = row_number())  %>%
      arrange(desc(import_crop_blue))  %>%
      mutate(position_import_crop_blue = row_number())  %>%
      arrange(desc(import_crop_green))  %>%
      mutate(position_import_crop_green = row_number())  %>%
      arrange(desc(import_crop_grey))  %>%
      mutate(position_import_crop_grey = row_number())  %>%
      arrange(desc(import_industrial_blue))  %>%
      mutate(position_import_industrial_blue = row_number())  %>%
      arrange(desc(import_industrial_grey))  %>%
      mutate(position_import_industrial_grey = row_number())  %>%
      arrange(desc(total_import_green))  %>%
      mutate(position_total_import_green = row_number())  %>%
      arrange(desc(total_import_blue))  %>%
      mutate(position_total_import_blue = row_number())  %>%
      arrange(desc(total_import_grey))  %>%
      mutate(position_total_import_grey = row_number())  %>%
      arrange(desc(total_export_green))  %>%
      mutate(position_total_export_green = row_number())  %>%
      arrange(desc(total_export_blue))  %>%
      mutate(position_total_export_blue = row_number())  %>%
      arrange(desc(total_export_grey))  %>%
      mutate(position_total_export_grey = row_number())  %>%
      arrange(desc(net_green))  %>%
      mutate(position_net_green = row_number())  %>%
      arrange(desc(net_blue))  %>%
      mutate(position_net_blue = row_number())  %>%
      arrange(desc(net_grey))  %>%
      mutate(position_net_grey = row_number()) %>%
      filter(country %in% input$country_us)
    
    final_df = data.frame(Metric = c("Export Animal Blue", "Export Animal Green", "Export Animal Grey", "Export Crop Blue", "Export Crop Green",
                                     "Export Crop Grey",  "Export Industrial Blue", "Export Industrial Grey", "Import Animal Blue",
                                     "Import Animal Green", "Import Animal Grey", "Import Crop Blue", "Import Crop Green", "Import Crop Grey",
                                     "Import Industrial Blue", "Import Industrial Grey", "Total Import Green", "Total Import Blue", "Total Import Grey",
                                     "Total Export Green", "Total Export Blue", "Total Export Grey", "Net Green", "Net Blue", "Net Grey"),
                          Amount = as.numeric(df[1, 2:26]),
                          Percentage= c(as.numeric(df[1, 27:48]), NA, NA, NA),
                          Ranking = as.numeric(df[1, 52:76]))
    final_df = final_df %>%
      mutate(Percentage = format(round(Percentage*100, 2), big.mark=",", scientific = FALSE)) %>%
      mutate(Amount = format(as.numeric(Amount), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
      mutate(Percentage = paste0(Percentage, "%"))
    final_df[final_df$Metric %in% c("Net Green", "Net Blue", "Net Grey"), ]$Percentage = "NA"
    names(final_df) = c("Metric", "Virtual Water (Millions of m3)", "Percentage of World Total", "World Ranking")
    
    return(final_df)
  })
  
  output$rankingPercentageTable <- renderDataTable({
    datatable(rankingPercentage(), options = list(
      pageLength = 25,
      lengthMenu = c(25, 50)))
  })
  
  
  topbottom_percapita <- reactive({
    if(input$countries_percapita == "top"){
      df = (df_population %>%
              filter(year == input$year) %>%
              filter(product_type %in% input$producttype_percapita) %>%
              filter(water_type %in% input$watertype_percapita) %>%
              group_by(country, pop) %>%
              summarize(water.footPrint = sum(exp)) %>%
              select(country, water.footPrint, pop) %>%
              mutate(per_capita = water.footPrint/pop) %>%
              arrange(desc(per_capita)) %>%
              # head(5)
              mutate(water.footPrint = format(as.numeric(water.footPrint), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(pop = format(as.numeric(pop*1000000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(per_capita = format(as.numeric(per_capita), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)))
    } else {
      df = (df_population %>%
              filter(year == input$year) %>%
              filter(product_type %in% input$producttype_percapita) %>%
              filter(water_type %in% input$watertype_percapita) %>%
              group_by(country, pop) %>%
              summarize(water.footPrint = sum(exp)) %>%
              select(country, water.footPrint, pop) %>%
              mutate(per_capita = water.footPrint/pop) %>%
              arrange(per_capita) %>%
              # head(5) %>%
              mutate(water.footPrint = format(as.numeric(water.footPrint), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(pop = format(as.numeric(pop*1000000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(per_capita = format(as.numeric(per_capita), nsmall=0, digits = 2, big.mark=",", scientific = FALSE)))
    }
    df = left_join(df, country_code, by = "country")
    df[df$country == "USA",]$code = "USA"
    return(df)
    
  })
  
  output$topbottomTable_percapita <- renderDataTable({
    df = topbottom_percapita()[, 1:4]
    names(df) = c("Country", "Virtual Water Exported (Millions of m3)", "Population", "Virtual Water Exported per capita (m3)")
    datatable(df, options = list(#columnDefs = list(list(className = 'dt-center', targets = 5)),
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)))
  })
  
  output$percapitaMap <- renderPlotly({
    plotdata = topbottom_percapita() %>%
      mutate(per_capita = as.numeric(gsub(",", "", per_capita))) %>%
      mutate(per_capita = log10(per_capita + 1))
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(plotdata) %>%
      add_trace(z = ~per_capita, color = ~per_capita,
                colors = 'Reds',
                text = ~`country`,
                locations = ~code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Log Transformed Per Capita') %>%
      layout(title = paste0('Global Distribution of Virtual Water Exported Per Capita, ', input$year), geo = g)
  })
  
  topbottom_percapita_change <- reactive({
    if(input$countries_percapita_change == "top"){
      df_pop_totalwater = (df_population %>%
                             filter(year %in% c(2005, 2015)) %>%
                             group_by(country, pop, year) %>%
                             summarize(water.footPrint = sum(exp)) %>%
                             mutate(per_capita = water.footPrint/pop) %>%
                             data.frame())
      
      per_df = (df_pop_totalwater %>%
                  select(country, year, per_capita) %>%
                  spread(year, per_capita))
      names(per_df) = c("Country", "PerExp2005", "PerExp2015")
      per_df = (per_df %>% mutate(Increase_2015 = (PerExp2015-PerExp2005)/PerExp2005))
      names(per_df) = c("Country", "PerExp2005", "PerExp2015", "Increase_2015")
      
      pop_df = (df_pop_totalwater %>%
                  select(country, year, pop) %>%
                  spread(year, pop))
      names(pop_df) = c("Country", "Pop2005", "Pop2015")
      
      total_df = (df_pop_totalwater %>%
                    select(country, year, water.footPrint) %>%
                    spread(year, water.footPrint))
      names(total_df) = c("Country", "Total2005", "Total2015")
      
      final_df = merge(per_df, pop_df, on="Country")
      final_df = merge(final_df, total_df, on="Country")
      final_df = (final_df %>%
                    arrange(desc(Increase_2015)) %>%
                    # head(5) %>%
                    mutate(PerExp2005 = format(as.numeric(PerExp2005), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(PerExp2015 = format(as.numeric(PerExp2015), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    # mutate(Increase_2015 = format(as.numeric(Increase_2015*100), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Pop2005 = format(as.numeric(Pop2005*1000000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Pop2015 = format(as.numeric(Pop2015*1000000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Total2005 = format(as.numeric(Total2005), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Total2015 = format(as.numeric(Total2015), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Increase_2015 = format(round(Increase_2015*100, 2), nsmall = 2, big.mark=",", scientific = FALSE)) %>%
                    mutate(Increase_2015 = paste0(Increase_2015, "%")) %>%
                    data.frame())
      
      
      
    } else {
      df_pop_totalwater = (df_population %>%
                             filter(year %in% c(2005, 2015)) %>%
                             # filter(product_type %in% c('crop', 'animal', 'industrial')) %>%
                             # filter(water_type %in% c('green', 'blue', 'grey')) %>%
                             # filter(product_type %in% input$producttype_percapita_change) %>%
                             # filter(water_type %in% input$watertype_percapita_change) %>%
                             group_by(country, pop, year) %>%
                             summarize(water.footPrint = sum(exp)) %>%
                             mutate(per_capita = water.footPrint/pop) %>%
                             data.frame())
      
      per_df = (df_pop_totalwater %>%
                  select(country, year, per_capita) %>%
                  spread(year, per_capita))
      names(per_df) = c("Country", "PerExp2005", "PerExp2015")
      per_df = (per_df %>% mutate(Increase_2015 = (PerExp2015-PerExp2005)/PerExp2005))
      # names(per_df) = c("Country", "PerExp2005", "PerExp2015", "Increase_2015")
      
      pop_df = (df_pop_totalwater %>%
                  select(country, year, pop) %>%
                  spread(year, pop))
      names(pop_df) = c("Country", "Pop2005", "Pop2015")
      
      total_df = (df_pop_totalwater %>%
                    select(country, year, water.footPrint) %>%
                    spread(year, water.footPrint))
      names(total_df) = c("Country", "Total2005", "Total2015")
      
      final_df = merge(per_df, pop_df, on="Country")
      final_df = merge(final_df, total_df, on="Country")
      final_df = (final_df %>%
                    arrange(Increase_2015) %>%
                    # head(5) %>%
                    mutate(PerExp2005 = format(as.numeric(PerExp2005), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(PerExp2015 = format(as.numeric(PerExp2015), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    # mutate(Increase_2015 = format(as.numeric(Increase_2015*100), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Pop2005 = format(as.numeric(Pop2005*1000000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Pop2015 = format(as.numeric(Pop2015*1000000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Total2005 = format(as.numeric(Total2005), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Total2015 = format(as.numeric(Total2015), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
                    mutate(Increase_2015 = format(round(Increase_2015*100, 2), nsmall = 2, big.mark=",", scientific = FALSE)) %>%
                    mutate(Increase_2015 = paste0(Increase_2015, "%")) %>%
                    data.frame())
      # df = ()
    }
    names(final_df) = c("Country", "Per Capita 2005 (m3)", "Per Capita 2015 (m3)", "Per Capita Increase Percentage",
                        "Population 2005", "Population 2015", "Export 2005 (Millions of m3)", "Export 2015 (Millions of m3)")
    return(final_df)
    
  })
  
  output$topbottomTable_percapita_change <- renderDataTable({
    datatable(topbottom_percapita_change(), options = list(#columnDefs = list(list(className = 'dt-center', targets = 5)),
      pageLength = 10,
      lengthMenu = c(10, 15, 20)))
  })
  
  output$topbottomTable_percapita_change_plot <- renderPlotly({
    plotdata = topbottom_percapita_change()[1:10,] %>%
      mutate(`Per Capita 2005 (m3)` = as.numeric(gsub(",", "", `Per Capita 2005 (m3)`))) %>%
      mutate(`Per Capita 2015 (m3)` = as.numeric(gsub(",", "", `Per Capita 2015 (m3)`))) %>%
      mutate(`Per Capita Increase Percentage` = as.numeric(gsub("%", "", `Per Capita Increase Percentage`))/100)
    plotdata$Country <- factor(plotdata$Country, levels = topbottom_percapita_change()[1:10,]$Country)
    
    if(input$countries_percapita_change == "top"){
      range2 = c(0, 1.1*max(plotdata[,4]))
    } else {
      range2 = c(1.1*min(plotdata[,4]), 1.1*max(plotdata[,4]))
    }
    
    plot_ly(plotdata, x = ~factor(Country)) %>%
      add_trace(y = ~`Per Capita 2005 (m3)`,
                type = 'bar',
                name = 'Virtual Water Exported 2005',
                # ,marker = list(color = Steel, opacity=0),
                hoverinfo = 'text',
                text = ~paste('Water:', prettyNum(`Per Capita 2005 (m3)`, big.mark=",",scientific=FALSE),
                              '<br>Country:', Country,
                              '<br>Year:', '2005')
      ) %>%
      add_trace(y = ~`Per Capita 2015 (m3)`,
                type = 'bar',
                name = 'Virtual Water Exported 2015',
                # ,marker = list(color = Steel, opacity=0),
                hoverinfo = 'text',
                text = ~paste('Water:', prettyNum(`Per Capita 2015 (m3)`, big.mark=",",scientific=FALSE),
                              '<br>Country:', Country,
                              '<br>Year:', '2015')
      ) %>%
      add_trace(y = ~`Per Capita Increase Percentage`,
                type = 'scatter', mode='lines+markers', yaxis = 'y2',
                name = 'Per Capita Increase Percentage',
                # ,line = list(color = Bright.Blue),
                hoverinfo = 'text',
                text = ~paste('Increase:', paste0(`Per Capita Increase Percentage`*100, '%'),
                              '<br>Country:', Country)
      ) %>%
      layout(margin= list(b = 200, r = 90, l = 100),
             title = paste0('Virtual Water Exported Per Capita Increase, ', input$countries_percapita_change , ' 10 countries'),
             legend = list(orientation = 'h', x = 0.05, y = -0.25),
             #  showlegend = FALSE,
             barmode = 'group',
             #  yaxis = list(title = '', ))
             yaxis = list(side = 'left',
                          title = 'Virtual Water (Millions of m³)',
                          range = c(0, 1.1*max(plotdata[,2], plotdata[,3])),
                          # range = c(0, y.lim.maxn),
                          showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right',
                           tickformat = "%",
                           title = 'Per Capita Increase Percentage',
                           range = range2,
                           showgrid = FALSE, zeroline = FALSE,
                           overlaying = "y"),
             xaxis = list(title = 'Country'))
  })
  
}

## Run the application ####
shinyApp(ui = ui, server = server)