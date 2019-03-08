# CollarFrequency

R Shiny App to find non-conflicting VHF frequencies. Helps for ordering collars and assigning new collars to populations. 

## Requirements:

### Software: 
  - R >= 3.3
  - RStudio >= 1.1
### R Packages:
  - shiny
  - shinythemes
  - igraph
  - GGally
  - dplyr
  - ggplot2
  
## Installation: 

Download this project to your home computer. It will expect to find one of 3 data sources on your computer:

  - A copy of the Bishop CDFW All Program Collars database at location:
   ```M:\AllProgramCollars.mdb```
  - A copy of the csv export of the database at:
   ```M:\DatabaseTables\AllCollarsList.txt```
  - A local copy of the csv export or equivalently formatted csv at
   ```.\AllCollarsList.txt```
   
The local copy is the preferred option if operating from a laptop in the field or at home. 
If using this option, put the copy of the csv export in the project directory of the downloaded app. 

This database export should be of the following format:
```
"160.451",,"Sage grouse",,"Bodie Hills","GPS",,"AW","Bodie Hills"
"160.451","","deer","CDB122","CD","VHF/GPS","Various","AW","CD"
"160.455",,"Elk",,"Lone Pine","GPS","ATS Globalstar","ANW","Alabama Gates"
"160.455","1/27/2018","Bobcat","BC035","Gorge","GPS","Iridium Vectronics","AW","RV"
"160.455","10/23/2018","bighorn","S459","Southern","GPS","SirTrack Pinnacle Lite","AW","Sawmill Canyon"
"160.457","","deer","CDB062","CD","VHF/GPS","Various","AW","CD"
"160.460","10/23/2018","bighorn","S516","Southern","GPS","SirTrack Pinnacle Lite","AW","Mt. Baxter"
"160.460","10/26/2017","bighorn","S278","Southern","VHF","Telonics mod-315","AW","Olancha Peak"
"160.462","20171115","desert bighorn",,"White Mountains","GPS",,"AW","White Mountains"
```

If note already installed, install the required r packages:
```
install.packages(c(
    "GGally",
    "igraph",
    "shiny",
    "shinythemes",
    "dplyr",
    "ggplot2"))
```

## Running:   

The app is an R shiny app and can be run from Shiny Server or locally from within RStudio. 
From RStudio:

  - open ```frequency.Rproj```
  - select ```app.R```
  - hit the "Run App" button
  
## Use:   

There are 4 panels to the app. The function of each panel is described here:

  - __All Collars Table__:
   This is just an interactive version of the all collars table. It is the local copy of the database the app is using. 
   It can be used to search for specific collars and their deployment status.
  - __Interference Network__:
   This section contains 2 parts. The first is a plot of all the collared populations and their interference edges. 
   The nodes of  the graph represent populations, and the edges represent the potential for VHF interference between
   the populations. The second part of this panel is a table of the attributes of each population. This can be used
   to look up the population codes for your species.
  - __Find Available Frequencies__: 
  Use this panel to find which parts of the spectrum are available for your select populations. 
  Select the populations where you want to place collars, and the plot will show in grey all frequencies that don't interfere
  with existing collars. The plot can be moused over and selected to get details on specific frequencies and the collars
  within selected frequency ranges.
  The "Interfering Populations" table shows which populations have the potential for collar conflicts.
  - __Find Populations for Collars__:
  Use this panel when you have collars with specific frequencies and you want to see where they may be placed without conflict. 
  Input your desired species and collar frequency. 
  Conflicting collars are shown and available populations for your species are displayed.
  
## Example:
A version of this app is running at:
https://snbs.shinyapps.io/frequency/
  
