# SAPPquery
## A shiny APP for querying a local SPAQRL database 

### The goal of the app is to make the SPARQL querying easy and offer a good overview of the results.

#### How to use.
##### In the Navigation bar the user can select to query using reactions(EC numbers) or the proteins(NP numbers)
##### Results are loaded into datatables which has a search option.

## Installation

You can run the Shiny app on your local computer to query the Cigene Blazegraph server.

The installation instructions below were tested by [jonovik](https://gitlab.com/jonovik) on Windows 10, R 3.3.2, with R packages updated as of 2017-02-18.

Prerequisites:

* RStudio (and R, obviously)
* Command-line `curl` (mine came with Anaconda Python)
* Working rJava (can be tricky)
* RGBOLApi (some assembly required, see https://gitlab.com/gbol and ask Jesse van Dam or Róbert Hafþórsson).
  Note: Don't `install.packages("../RGBOLApi",repos=NULL, type="source")`
  but instead create a new RStudio project in the RGBOLApi folder, then
  `Build > Build and Reload`
  (You may have to install and configure build tools in RStudio. Google it.)
* Running Blazegraph server at the URL indicated in `app.R` (search for `http://` in that file).
  (This server will normally be up and running.)

Then:

1. Clone this repo.
1. Double-click `SAPPquery/app.R` and RStudio should open the project.
1. `install.packages(c("RCurl", "shinyBS", "shinyjs", "SPARQL", "data.table", "DT", "stringr", "plyr"))`
1. **Connect to NMBU VPN if you're not already inside the firewall.** (You can check [whatismyip](https://www.whatismyip.com) to verify that your IP begins with 128.39...)
1. Click RStudio's `Run app` button (a dropdown setting lets you choose to run in an RStudio window or an external browser).

The `SAPP Query` app should open. Hopefully you should now be able to click `Submit` button, wait a while, and see a resultset of records.

**Testing just Curl and the Blazegraph server:**
```
echo SELECT ?answer WHERE { BIND (42 as ?answer) } > query.tmp
curl -X POST http://10.209.0.133:8080/blazegraph/namespace/ManualAnno/sparql --data-urlencode query@query.tmp -H Accept:text/tab-separated-values
```
should return

```
?answer
"42"^^<http://www.w3.org/2001/XMLSchema#integer>
```
