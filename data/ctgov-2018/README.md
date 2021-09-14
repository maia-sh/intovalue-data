Emailed by NR on 2021-09-09

here is the file for the prospective registration metric that was loaded by the Shiny app to display the prospective registration metric. It can be loaded using the readRDS function. The table is structured in a way that we have one row per NCT-id – UMC pair, so NCTs can appear in multiple rows if they are associated to multiple UMCs. Also there is a city category “All trials combined” which contains each ID once. This was easier for filtering for the Shiny app.

 

The AACT dataset was from 15.03.2019, but I filtered all start dates between 2006 – 2018, so 2018 will still be included completely but not 2019.