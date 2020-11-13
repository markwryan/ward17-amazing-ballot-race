library("dplyr")
library("leaflet")

# GeoJSON via https://github.com/skorasaurus/cleboundaries
voting_precincts <- geojsonio::geojson_read("/Users/Mark/Desktop/2020-cleveland-precincts.geojson", what = "sp")
# via Cuyahoga County BoE
absentee_labels = read.csv("/Users/Mark/Desktop/AbsenteeLabels-2020-11-12.csv", header = 1)
# Remove the space between CLEVELAND and the '-' in precinct
absentee_labels$PRECINCT <- gsub('\\s+', '', absentee_labels$PRECINCT)
# Cobble together an address from the fields
absentee_labels$address <- sprintf("%s %s %s %s %s,%s %s", absentee_labels$HOUSENUMBER, absentee_labels$PREDIR, absentee_labels$STREET, absentee_labels$TYPE, absentee_labels$CITY, absentee_labels$STATE, absentee_labels$ZIP)


# Subset County Data, to just Ward 17
ward17 <- subset(voting_precincts, WARD == "17")
# Move to separate DF to add in count columns
ward17withcount <- ward17

# Reformat DATEISSUED and DATERETURNED into Dates
absentee_labels$issued <- as.Date(absentee_labels$DATEISSUED, format="%m/%d/%Y")
absentee_labels$returned <- as.Date(absentee_labels$DATERETURNED, format="%m/%d/%Y")
# Pull out returned ballots based on dates
week0 <- absentee_labels[absentee_labels$returned < '2020-10-11',] %>% na.omit()
week1 <- absentee_labels[absentee_labels$returned > '2020-10-11' & absentee_labels$returned < '2020-10-18',] %>% na.omit()
week2 <- absentee_labels[absentee_labels$returned > '2020-10-18' & absentee_labels$returned < '2020-10-25',] %>% na.omit()
week3 <- absentee_labels[absentee_labels$returned > '2020-10-25',] %>% na.omit()
# Group and count the entries into totals by precint
week0_returns <- week0 %>% group_by(week0$PRECINCT) %>% summarise(n = n())
# Rename columns to JOIN to DF
names(week0_returns)[names(week0_returns) =="week0$PRECINCT"] <- "NAME"
names(week0_returns)[names(week0_returns) =="n"] <- "week0_returned"
# Repeat for other weeks
week1_returns <- week1 %>% group_by(week1$PRECINCT) %>% summarise(n = n())
names(week1_returns)[names(week1_returns) =="week1$PRECINCT"] <- "NAME"
names(week1_returns)[names(week1_returns) =="n"] <- "week1_returned"
week2_returns <- week2 %>% group_by(week2$PRECINCT) %>% summarise(n = n())
names(week2_returns)[names(week2_returns) =="week2$PRECINCT"] <- "NAME"
names(week2_returns)[names(week2_returns) =="n"] <- "week2_returned"
week3_returns <- week3 %>% group_by(week3$PRECINCT) %>% summarise(n = n())
names(week3_returns)[names(week3_returns) =="week3$PRECINCT"] <- "NAME"
names(week3_returns)[names(week3_returns) =="n"] <- "week3_returned"

# Join weekly counts to the spacial DF
ward17withcount@data <- left_join(ward17withcount@data, week0_returns, by=c("NAME"))
ward17withcount@data <- left_join(ward17withcount@data, week1_returns, by=c("NAME"))
ward17withcount@data <- left_join(ward17withcount@data, week2_returns, by=c("NAME"))
ward17withcount@data <- left_join(ward17withcount@data, week3_returns, by=c("NAME"))
# Calculate Columns with Rolling Totals after each week
ward17withcount@data$total_returned <- rowSums(ward17withcount@data[,c("week0_returned", "week1_returned", "week2_returned", "week3_returned")], na.rm=TRUE)
ward17withcount@data$through_oct17 <- rowSums(ward17withcount@data[,c("week0_returned", "week1_returned")], na.rm=TRUE)
ward17withcount@data$through_oct24 <- rowSums(ward17withcount@data[,c("week0_returned", "week1_returned", "week2_returned")], na.rm=TRUE)

# Separate color palette into 9 separate bins
bins <- c(0, 25, 75, 100, 200, 300, 400, 450, Inf)
# Create the pallete using Blues, passing in the 9 bins
pal <- colorBin("Blues", domain = ward17withcount$total_returned, bins = bins)

# Create the map. Add polygons in groups for each separate week
m <- leaflet(ward17withcount) %>% 
    addTiles(group = "base") %>%
    addPolygons(
        group ="Total on 10/10 (Pre-Amazing Ballot Race)",
        fillColor = ~pal(week0_returned),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
        label = sprintf("%s: %s ballots returned", ward17withcount$NAME, ward17withcount$week0_returned),
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto")) %>%
    addPolygons(
        group ="Total on 10/17 (After Amazing Ballot Race Weekend 1)",
        fillColor = ~pal(through_oct17),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
        label = sprintf("%s: %s ballots returned", ward17withcount$NAME, ward17withcount$through_oct17),
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto")) %>%
    addPolygons(
        group ="Total on 10/24 (After Amazing Ballot Race Weekend 2)",
        fillColor = ~pal(through_oct24),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
        label = sprintf("%s: %s ballots returned", ward17withcount$NAME, ward17withcount$through_oct24),
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto")) %>%
        addPolygons(
        group ="Total Returned Absentee Ballots",
        fillColor = ~pal(total_returned),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
        label = sprintf("%s: %s ballots returned", ward17withcount$NAME, ward17withcount$total_returned),
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto")) %>%
    addLayersControl(
        baseGroups = c("Total on 10/10 (Pre-Amazing Ballot Race)", "Total on 10/17 (After Amazing Ballot Race Weekend 1)", "Total on 10/24 (After Amazing Ballot Race Weekend 2)", "Total Returned Absentee Ballots"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(pal = pal, values = ~total_returned, opacity = 0.7, title = "Ward 17 Amazing Ballot Race", position = "bottomright")





