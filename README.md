# CollarFrequency
R Shiny App to find non-conflicting VHF frequencies. Helps for ordering collars and assigning new collars to populations. 

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

## Running: 
The app is an R shiny app and can be run from Shiny Server or locally from within RStudio. 
From RStudio:
  - open ```frequency.Rproj```
  - select ```app.R```
  - hit the "Run App" button
  
## Use:
