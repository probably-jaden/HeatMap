#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#
#
#inst_pckgs <- c("shiny", "leaflet", "sp", "zipcodeR", "rnaturalearth", "rmapshaper", "tidyverse", "tigris", "sf", "rsconnect", "DT")

library(shiny)
library(zipcodeR) #
library(dplyr)
library(lubridate)
library(shinyjs)

wpSales <- as_tibble(read.table("/Users/jse/Downloads/heatmap2.csv", header = TRUE, sep = ","))

format_number <- function(x) {
  if (is.na(x)) return(NA)
  if (x < 1000) return(as.character(round(x)))
  if (x < 1000000) return(paste0(round(x/1000, 1), "k"))
  if (x < 1000000000) return(paste0(round(x/1000000, 1), "m"))
  return(paste0(round(x/1000000000, 1), "b"))
}

county_pop <- tidycensus::get_estimates(
  geography = "county",
  product = "population",
  year = 2018,  # Use the most recent year available
  state = NULL  # This will get data for all states
) %>%
  filter(variable == "POP")

# Step 1: Get county data and simplify geometries (do this once, offline)
counties <- tigris::counties(cb = TRUE, resolution = "20m") %>%
  sf::st_transform(4326) %>%
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE) # Adjust keep value as needed

ct_populations <- c("Naugatuck Valley" = 454083,
                    "Lower Connecticut River Valley" = 176622,
                    "Western Connecticut" = 623690,
                    "Northeastern Connecticut" = 96196,
                    "South Central Connecticut" = 573244,
                    "Capitol" = 981447,
                    "Southeastern Connecticut" = 280403,
                    "Greater Bridgeport" = 327286,
                    "Northwest Hills" = 113234)

counties <- counties %>%
  left_join(county_pop, by = c("GEOID" = "GEOID")) %>%
  mutate(value = case_when(
    STATE_NAME == "Connecticut" ~ ct_populations[NAME.x],
    TRUE ~ value
  ))



# colnames(counties)
#
# counties %>%
#   select(c("NAME.x", "STATE_NAME", "value")) %>%
#   arrange(desc(value))


county_clean_df <- function(df){
  cleaned_df <- df %>%
    select(c(`amazon-order-id`, `quantity`, `ship-postal-code`, `purchase-date`)) %>%
    mutate(zipcode = substr(`ship-postal-code`, 1, 5)) %>%
    group_by(zipcode) %>%
    summarise(count = n())

  zipcodes_df <- zipcodeR::geocode_zip(cleaned_df$zipcode)

  cleaned_df <- cleaned_df %>%
    left_join(zipcodes_df, by = c("zipcode" = "zipcode"))

  coords_df <- cleaned_df %>%
    select(c(lng, lat, count)) %>%
    filter(!is.na(lat), !is.na(lng))

  return(coords_df)
}

county_pop_boundary_data <- function(df){
  point_data <- county_clean_df(df)
  point_data_sf <- sf::st_as_sf(point_data, coords = c("lng", "lat"), crs = 4326)

  aggregated_data <- sf::st_join(counties, point_data_sf) %>%
    group_by(GEOID) %>%
    summarize(
      NAME = first(NAME.x),
      STATE_NAME = first(STATE_NAME),
      total_count = sum(count, na.rm = TRUE),
      total_population = mean(value, na.rm = TRUE)
    ) %>%
    mutate(noSales = ifelse(total_count == 0, 0, 1)) %>%
    filter(total_population > 0)

  agg_data_noSales = aggregated_data %>%
    filter(noSales == 0) %>%
    mutate(count_per_capita = total_population,
           log_count_per_capita = log(total_population),
           norm_count_per_capita = (log_count_per_capita - mean(log_count_per_capita)) / sd(log_count_per_capita))

  agg_data_sales = aggregated_data %>%
    filter(noSales == 1) %>%
    mutate(count_per_capita = total_population/total_count,
           log_count_per_capita = log(total_count/total_population),
           norm_count_per_capita = (log_count_per_capita - mean(log_count_per_capita))/ sd(log_count_per_capita))

  county_data_final <- rbind(agg_data_sales, agg_data_noSales)

  return(county_data_final)
}

#wpSales <- read.csv("/Users/jse/Downloads/heatmap2.csv")
#df<- wpSales
#county_pop_boundary_data(wpSales)

generateHeatmap_county <- function(df){
  county_data_final <- county_pop_boundary_data(df)

  pal <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain = county_data_final$total_count
  )



  # Create the leaflet map
  this_map <- leaflet::leaflet(county_data_final) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~pal(total_count),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = leaflet::highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = ~lapply(paste0(NAME, ", ", STATE_NAME,
                             "<br><b>Total Sales:</b> ", total_count),
                      HTML),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))

  return(this_map)
}


generateHeatmap_county_per_capita <- function(df){
  county_data_final <- county_pop_boundary_data(df)

  # Create custom color palettes
  red_white_palette <- colorRampPalette(c("mistyrose", "indianred3"))
  white_green_palette <- colorRampPalette(c("honeydew", "darkgreen"))

  # Custom color function
  custom_color <- function(x, noSales) {
    if (noSales == 0) {
      red_white_palette(100)[pmin(pmax(round((x + 1) * 50), 1), 100)]
    } else {
      white_green_palette(100)[pmin(pmax(round((x + 1) * 50), 1), 100)]
    }
  }

  # Create the leaflet map
  this_map <- leaflet::leaflet(county_data_final) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~sapply(seq_along(norm_count_per_capita),
                          function(i) custom_color(norm_count_per_capita[i], noSales[i])),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = leaflet::highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = ~lapply(paste0(NAME, ", ", STATE_NAME,
                             "<br><b>Total Sales:</b> ", total_count,
                             "<br><b>", noSales, "</b> in every <b>", lapply(count_per_capita, format_number), "</b>"),
                      HTML),
     labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))

  return(this_map)
}

#wpSales <- read.table("/Users/jse/Downloads/heatmap3.txt", header = TRUE, sep = "\t")

#county_per_capita_map(wpSales)



###############################################################################
###############################################################################
###############################################################################


#rsconnect::deployApp()

state_pop_df <- structure(list(
  code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
           "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
           "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
           "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
           "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
            "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida",
            "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
            "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
            "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
            "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
            "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  pop_2014 = c(4849377, 736732, 6731484, 2966369, 38802500,
               5355866, 3596677, 935614, 658893, 19893297,
               10097343, 1419561, 1634464, 12880580, 6596855,
               3107126, 2904021, 4413457, 4649676, 1330089,
               5976407, 6745408, 9909877, 5457173, 2994079,
               6063589, 1023579, 1881503, 2839099, 1326813,
               8938175, 2085572, 19746227, 9943964, 739482,
               11594163, 3878051, 3970239, 12787209, 1055173,
               4832482, 853175, 6549352, 26956958,2942902,
               626562, 8326289, 7061530, 1850326, 5757564, 584153)
), class = "data.frame", row.names = c(NA, -51L))



options(tigris_use_cache = TRUE)
states_tigris <- tigris::states(cb = TRUE, resolution = "20m")

dspl_tbl_clean <- function(df){
  cleaned_data <- df %>%
    select(c(`sku`, `asin`, `quantity`, `ship-city`, `ship-state`,
             `purchase-date`, `order-status`, `purchase-date`)) %>%
    mutate(sku = substr(sku, 1, 15))
  return(cleaned_data)
}



#lonlat_to_state <- function(pointsDF,
#                            states = spData::us_states,
#                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
#  pts <- sf::st_as_sf(pointsDF, coords = 1:2, crs = 4326)

  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
#  states <- sf::st_transform(states, crs = 3857)
#  pts <- sf::st_transform(pts, crs = 3857)

  ## Find names of state (if any) intersected by each point
#  state_names <- states[[name_col]]
#  ii <- as.integer(sf::st_intersects(pts, states))
#  return(state_names[ii])
#}


lonlat_to_state <- function(pointsDF, name_col = "NAME") {
  # Load all states including Alaska and Hawaii
  #states <- states(cb = TRUE, resolution = "20m")

  # Convert points data.frame to an sf POINTS object
  pts <- sf::st_as_sf(pointsDF, coords = 1:2, crs = 4326)

  # Ensure both datasets are in the same CRS
  states_t <- sf::st_transform(states_tigris, crs = 4326)

  # Find names of state (if any) intersected by each point
  state_names <- states_t[[name_col]]
  ii <- as.integer(sf::st_intersects(pts, states_t))

  return(state_names[ii])
}

#df <- wpSales

all_orders_clean <- function(df){
  cleaned_df <- df %>%
    select(c(`amazon-order-id`, `quantity`, `ship-postal-code`, `purchase-date`)) %>%
    mutate(zipcode = substr(`ship-postal-code`, 1, 5)) %>%
    group_by(zipcode) %>%
    summarise(count = n())

  #cleaned_df <- as_tibble(cleaned_df)

  zipcodes_df <- zipcodeR::geocode_zip(cleaned_df$zipcode)

  cleaned_df <- cleaned_df %>%
    left_join(zipcodes_df, by = c("zipcode" = "zipcode"))

  coords_df <- cleaned_df %>%
    select(c(lng, lat)) %>%
    filter(!is.na(lat), !is.na(lng))

  states_tib <- as_tibble(lonlat_to_state(coords_df)) %>%
    rename("states" = value)

  final_df <- states_tib %>%
    group_by(states) %>%
    summarise(total_count = n())

  return(final_df)
}

#test_df <- all_orders_clean(wpSales)

#generateHeatmap_state(test_df)

generateHeatmap_state_per_capita <- function(df){
  #clean_df <- all_orders_clean(df)

  agg_df <- df %>%
    right_join(state_pop_df, by = c("states" = "state"))# %>%
    #filter(!is.na(states)) %>%
    #filter(!is.na(total_count)) %>%


  #states_data  <- left_join(states_tigris, agg_df, by = c("NAME" = "states")) %>%
  #  mutate(noSales = ifelse(is.na(total_count), 0, 1),
  #         oneInPop = ifelse(noSales == 0, pop_2014, pop_2014/(total_count)),
  #         oneInPop_formatted = ifelse(noSales == 0, sapply(pop_2014, format_number), sapply(oneInPop, format_number)))
      #noSales = ifelse(is.na(total_count), 0, 1),
           #total_count = ifelse(noSales == 0, 1, total_count),


           #log_per_capita = log(total_count/pop_2014),
           #std_log_per_capita = (log_per_capita - mean(log_per_capita))/sd(log_per_capita))

  states_data <- left_join(states_tigris, agg_df, by = c("NAME" = "states")) %>%
    mutate(noSales = ifelse(is.na(total_count), 0, 1),
           totalSales = ifelse(noSales == 0, 0, total_count),
           oneInPop = ifelse(noSales == 0, pop_2014, pop_2014/(total_count)),
           revOneInPop = 1/oneInPop,
           oneInPop_formatted = ifelse(noSales == 0, sapply(pop_2014, format_number), sapply(oneInPop, format_number)))

  pal <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain = states_data$revOneInPop
    #domain = states_data$std_log_per_capita
  )

  map <- leaflet::leaflet(states_data) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~pal(revOneInPop),#std_log_per_capita),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label =~lapply(paste0("<b>", NAME, ":</b> ", totalSales, "<br><b>", noSales, "</b> in <b>", oneInPop_formatted, "</b> people purchase"), HTML),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    )# %>%
    #leaflet::addLegend(
    #  pal = pal,
    #  values = ~revOneInPop,#std_log_per_capita,
    #  opacity = 0.7,
    #  title = "Score",
    #  position = "bottomright"
    #)

  return(map)
}


#df <- all_orders_clean(wpSales)
#wpSales

#generateHeatmap_state_per_capita(df)

#generateHeatmap_state_per_capita(test_df)

generateHeatmap_state <- function(df){
  #clean_df <- all_orders_clean(df)

  states_data <- left_join(states_tigris, df, by = c("NAME" = "states"))%>%
    mutate(total_count = ifelse(is.na(total_count), 0, total_count))

  pal <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain = states_data$std_log_per_capita
  )

  map <- leaflet::leaflet(states_data) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~pal(total_count),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste(NAME, ": ", total_count),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = ~total_count,
      opacity = 0.7,
      title = "Count",
      position = "bottomright"
    )

  return(map)
}

# Function to generate monthly client password
get_monthly_password <- function(date) {
  set.seed(year(date) * 100 + month(date))
  words <- c("jungle", "fish", "scout", "prime", "wish", "box", "cart",
             "magic", "vine", "safari", "amazon", "up", "fire", "sale",
             "boost", "star", "echo", "fresh", "deal", "map", "cloud",
             "shopping", "smart", "joy", "river", "forest", "cloud", "rain",
             "star", "sun", "moon", "run", "jump", "ship" )
  paste0(sample(words, 2, replace = FALSE), collapse = "")
}


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Amazon Shipping Heatmap"),
  tags$head(tags$script(src = "https://kit.fontawesome.com/your-kit-code.js", crossorigin = "anonymous")),

  br(),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      fluidRow(

        column(7, textInput("password", label = NULL, placeholder = "Enter password")),
        #column(8, textInput("password", "Enter password:")),
        column(3, actionButton("enter", "Enter", class = "btn-primary")),
        column(2, uiOutput("password_icon"))
        #column(4, )
      ),
      textOutput("client_password"),
      textOutput("status"),
      br(),

      # Input: Select a file ----
      fileInput("file1", "Upload 'All Orders' Report",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))

    ),
    # Main panel for displaying outputs ----
    mainPanel(
      DT::DTOutput("contents")
    )
  ),
  br(),

  fluidRow(
    tags$head(
      tags$style(HTML("
      .nav-tabs {
        padding-left: 30px; /* Adjust this value to move tabs more or less */
      }
    "))
    ),
    tabsetPanel(
      tabPanel("State",
               leaflet::leafletOutput("heatmap", height = "70vh")
      ),
      tabPanel("State per Capita",
               leaflet::leafletOutput("heatmap_per_capita", height = "70vh")
      ),
      tabPanel("County",
               leaflet::leafletOutput("heatmap_county", height = "70vh")
      ),
      tabPanel("County per Capita",
               leaflet::leafletOutput("heatmap_county_per_capita", height = "70vh")
      )
    )
  ),
  br()
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # userData <- reactive({
  #   req(input$file1)
  #   tryCatch(
  #     {
  #       #read.csv(input$file1$datapath,
  #       #         header = TRUE,
  #       #         sep = '\t',
  #       #         quote = '"')
  #
  #       as_tibble(data.table::rbindlist(lapply(input$file1$datapath, data.table::fread),
  #                           use.names = TRUE, fill = TRUE))
  #       #browser()
  #     },
  #     error = function(e) {
  #       stop(safeError(e))
  #     }
  #   )
  # })
  #

  userData <- reactive({
    # First, check if the password is valid
    req(valid_client_password())

    # Then, proceed with file processing
    req(input$file1)
    tryCatch(
      {
        as_tibble(data.table::rbindlist(lapply(input$file1$datapath, data.table::fread),
                                        use.names = TRUE, fill = TRUE))
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })

  master_password <- "Baconandeggs!"

  button_pressed <- reactiveVal(FALSE)
  valid_client_password <- reactiveVal(FALSE)

  observeEvent(input$enter, {
    button_pressed(TRUE)
    input_password <- input$password
    current_date <- Sys.Date()
    current_client_password <- get_monthly_password(current_date)

    if (input_password == master_password) {
      output$status <- renderText(paste("This month's client password:", current_client_password))
      #output$status <- renderText("Master password entered")
      valid_client_password(TRUE)
    } else if (input_password == current_client_password) {
      output$status <- renderText("")
      #output$status <- renderText("Valid client password entered")
      valid_client_password(TRUE)
    } else {
      #output$client_password <- renderText("")
      output$status <- renderText("Invalid password")
      valid_client_password(FALSE)
    }
  })

  output$password_icon <- renderUI({
    if (button_pressed()) {
      if (valid_client_password()) {
        icon("check", class = "fa-2x", style = "color: green;")
      } else {
        icon("times", class = "fa-2x", style = "color: red;")
      }
    }
  })

  userStates <- reactive({
    req(input$file1)
    tryCatch(
      {
        return(all_orders_clean(userData()))
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })

  output$contents <- DT::renderDT(dspl_tbl_clean(userData()), selection = "none",
                                  options = list(pageLength = 3), server = FALSE, editable = TRUE)

  # Render the leaflet plot
  output$heatmap <- leaflet::renderLeaflet({
    req(input$file1)
    print("Rendering state heatmap")
    tryCatch(
      {

        plot <- generateHeatmap_state(userStates())%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
        print("Heatmap created successfully")
        plot
      },
      error = function(e) {
        print("Waiting for user data:")
        print(e)
        # Return a basic leaflet map if there's an error
        leaflet::leaflet() %>% leaflet::addTiles()%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
      }
    )
  })

  output$heatmap_per_capita <- leaflet::renderLeaflet({
    req(input$file1)
    print("Rendering state per capita heatmap")
    tryCatch(
      {
        plot <- generateHeatmap_state_per_capita(userStates())%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
        print("Heatmap created successfully")
        plot
      },
      error = function(e) {
        print("Waiting for user data:")
        print(e)
        # Return a basic leaflet map if there's an error
        leaflet::leaflet() %>% leaflet::addTiles()%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
      }
    )
  })

  output$heatmap_county <- leaflet::renderLeaflet({
    req(input$file1)
    print("Rendering county heatmap")
    tryCatch(
      {
        #browser()
        plot <- generateHeatmap_county(userData())%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
        print("Heatmap created successfully")
        plot
      },
      error = function(e) {
        print("Waiting for user data:")
        print(e)
        # Return a basic leaflet map if there's an error
        leaflet::leaflet() %>% leaflet::addTiles()%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
      }
    )
  })

  output$heatmap_county_per_capita <- leaflet::renderLeaflet({
    req(input$file1)
    print("Rendering county per capita heatmap")
    tryCatch(
      {
        plot <- generateHeatmap_county_per_capita(userData())%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
        print("Heatmap created successfully")
        plot
      },
      error = function(e) {
        print("Waiting for user data:")
        print(e)
        # Return a basic leaflet map if there's an error
        leaflet::leaflet() %>% leaflet::addTiles()%>%
          leaflet::setView(lng = -98.583, lat = 39.833, zoom = 4)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
