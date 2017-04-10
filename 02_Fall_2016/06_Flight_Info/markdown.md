Tutorial
========

Airlines tend to offer a broad range of prices in any given market, and
it is unlikely that the average fares from each travel & booking website
will be the same as any particular fare offered. Nevertheless,
information about average fares charged, including fares charged by
dominant carriers and lower-cost competing carriers where available, can
provide useful consumer information. The fare comparisons between
markets allow consumers to evaluate prices further and make the better
purchase decision.

App Benefits:
-------------

The “Flight Information” app allows users to select the year, quarter,
departure city and arrival city, to monitor the seasonal flight price
fluctuation along with the fare information. Our goal is to assist users
in making average fare comparisons from each selected quarter within a
given year or across different years and to help users to plan out their
travel schedules ahead of time.

The app offers five categories of flight information analysis, including
the “Departure Analysis,” “Departure Visualization,” “Arrival Analysis,”
“Arrival Visualization” and “Fare Analysis.” Each category provides the
useful flight information with graphs and tables that our users might be
concerned about. By comparing the seasonal price changes, the average
passengers for each selected route and the baggage fare information,
users could effortlessly acquire the information they need from our
shiny app when making the purchase decision.

In this app, we provide the general analysis & presentation of US
domestic airfare and market share of dominant airline companies based on
arrival and departure cities from 2011 to 2015. This “Flight
Information” app can be used for both academic and market research
purposes by the airline corporate strategic planning department,
aviation researcher, urban planning researcher and practitioners as well
as national and district tourism bureaus. Our goal is to assist users in
making average fare comparisons and to facilitate researchers and
industry practitioners with urban design decision-making, strategic
pricing planning, and competitive market benchmark.

Data Source:
------------

Each year, the Department of Transportation releases a report provided
information about average prices being paid by consumers in the 1,000
largest domestic city-pair markets within the 48 contiguous states.
These markets account for approximately 75 percent of all 48 contiguous
state passengers and 70 percent of total domestic passengers. This shiny
app extracts the “table 6” from this report that lists all city-pair
markets that average the passengers each day.

We have processed the raw data in several steps. First, we have used the
“gsub” function to delete the additional brackets and to find the
different departure city and arrival city. Second, we have used the
“Geocode” package to search the longitude and latitude for each unique
city. Finally, we have added the new column to list all the longitudes
and latitudes for each departure and arrival city.

Departure Analysis:
-------------------

In the section of departure analysis, the app generates four graphs that
provide the statistical analysis on the average flight fare from
departure city to different arrival destinations and the average
passenger numbers from departure city to various arrival destinations.
The graph descriptions are as the following:

1.  "Average Fare": The graph presents the average fare from departure
    city to other arrival cities with the fare listed on X-axis and each
    destination cities listed on Y-axis.

2.  "Passengers Per day": The graph shows the average passenger numbers
    from departure city to other arrival cities with the average
    passenger numbers listed on X-axis and each destination cities
    listed on Y-axis.

3.  "Correlation": We have conducted linear regression analysis on the
    relationship between travel miles and flight fare as well as the
    relationship between passenger numbers and flight fare respectively
    on each departure city. The correlation coefficient is labeled on
    both "The Relationship Between Overall Avarage Fare and City
    Distance" graph and "The Relationship Between Overall Avarage Fare
    and Passengers Per Day" graph.

4.  "Carrier Market Share": The graph indicates the total numbers of
    time when each flight company being the market share leader and the
    lowest fare provider for selected flight route. Regarding each
    flight carrier abbreviate, 9K represents the Cape Air, AA represents
    the American Airlines, AS represents the Alaska Airlines, B6
    represents the JetBlue Airways, Co represents the Continental
    Airline, DL represents the Delta Airlines, F9 represents Frontier
    Airlines, FL represents the AirTran Airways, G4 represents the
    Allegiant Airline, NK represents the Spirit Airlines, SY represents
    the Sun Country Airlines, U5 presents the USA 3000 Airlines, UA
    represents the United Airlines, US represents the US Airways, VX
    represents the Virgin America and WN represents the South
    West Airlines.

Departure Visualization:
------------------------

In the section of departure visualization, the app generates a map graph
that lay out the route(s) with a various price ranges in a different
color(s) from selected departure city to arrival cities. For each given
departure city as the starting point, the arrow in this graph represents
the route from selected departure city to arrival cities and each
distinctive color matches particular average airfare range.

1.  Under the "Complete Visualization", users could see the original map
    graph that labels all the flight routes from selected departure city
    to various arrival cities and identify the defined price range as we
    labeled in different colors.

2.  Under the "Fare-Filtered Visualization", based on the first graph
    output, users could explicitly select one price range they are
    particularly interested in. The map would filter the user-selected
    price range from departure city to some arrival cities that may fall
    into that particular price range.

Arrival Analysis:
-----------------

In the section of arrival analysis, the app generates four graphs that
provide the statistical analysis on the average flight fare of arrival
city from different departure cities and the average passenger numbers
of arrival city from various departure cities. The graph descriptions
are as the following:

1.  "Average Fare": the graph presents the average fare of arrival city
    from other departure cities with the fare listed on X-axis and each
    departure cities listed on Y-axis.

2.  "Passengers Per day": the graph shows the average passenger numbers
    of arrival city from other departure cities with the average
    passenger numbers listed on X-axis and each departure cities listed
    on Y-axis.

3.  "Correlation": We have conducted linear regression analysis on the
    relationship between travel miles and flight fare as well as the
    relationship between passenger numbers and flight fare respectively
    on each selected arrival city. The correlation coefficient is
    labeled on both "The Relationship Between Overall Avarage Fare and
    City Distance" graph and "The Relationship Between Overall Avarage
    Fare and Passengers Per Day" graph.

4.  "Carrier Market Share": the graph indicates the total numbers of the
    time for each flight company being the market share leader and the
    lowest fare provider. Regarding each flight carrier abbreviate, 9K
    represents the Cape Air, AA represents the American Airlines, AS
    represents the Alaska Airlines, B6 represents the JetBlue Airways,
    Co represents the Continental Airline, DL represents the Delta
    Airlines, F9 represents Frontier Airlines, FL represents the AirTran
    Airways, G4 represents the Allegiant Airline, NK represents the
    Spirit Airlines, SY represents the Sun Country Airlines, U5 presents
    the USA 3000 Airlines, UA represents the United Airlines, US
    represents the US Airways, VX represents the Virgin America and WN
    represents the South West Airlines.

Arrival Visualization:
----------------------

In the section of arrival visualization, the app generates the map graph
that lay out the route(s) with the various price ranges in different
color(s) of selected arrival city from departure cities. For each given
arrival city as the starting point, the arrow represents the route of
selected arrival city from departure cities and each distinctive color
matches particular average airfare range.

1.  Under the "Complete Visualization", users could see the original map
    graph that labels all the flight routes of selected arrival city
    from various departure cities and identify the defined price range
    as we labeled in different colors.

2.  Under the "Fare-Filtered Visualization", users could explicitly
    select one price range they are particularly interested in. The map
    would filter the user-selected price range of arrival city from some
    arrival cities that may fall into that particular price range.

Fare Analysis:
--------------

In this section, the app generates the graphs and tables that display
the time trend analysis versus the fare for the selected flight route,
along with the detailed baggage information. The descriptions of the
figures and tables are as the following:

1.  "Quarterly Time Trend Analysis in A Given Year": the graph specifies
    three fare types - the average fare for all carriers, the average
    fare for the largest carrier as the market share leader and the
    average fare for the as the cheapest price provider quarterly in a
    given year. Along with the graph of “All Quarters in Selected Year",
    the app generates a table corresponding to the results. The
    "Carrier" table lists all the carriers ordered by the largest as the
    market share leader to the lowest as the cheapest price provider
    quarterly in a given year. The baggage information for each unique
    carrier in the previous table is presented in a separate "Baggage
    Information" table accordingly.

2.  "Time Trend Analysis in Selected Quarter Across Years": The graph
    specifies three fare types - the average fare for all carriers, the
    average fare for the largest carrier as the market share leader and
    the average fare for the lowest carrier as the cheapest price
    provider in a selected quarter through the year 2011 to 2015. Along
    with the graph of "Selected Quarter 2011-2015", the app generates a
    table corresponding to the results. The "Carrier" table lists all
    the carriers ordered by the largest as the market share leader to
    the lowest as the cheapest price provider in a selected quarter
    through the year 2011 to 2015. The baggage information for each
    unique carrier in the previous table is presented in a separate
    "Baggage Information" table accordingly.

Further Information:
--------------------

We thank you for your time. If you have any question that could not be
answered by our “Introduction” section, feel free to contact the app
developers:
