Approximately Orchestrated Routing & Transportation Analyzer (AORTA)
=====

AORTA is a traffic simulator designed especially for autonomous vehicles in urban areas.

Documentation was once hosted at aorta-traffic.org, but I've let the domain and hosting expire. You
can find some pages via Google Cache and archive.org (eg
https://web.archive.org/web/20160819160137/http://www.aorta-traffic.org/usage/). Some pieces are
replicated below.

If you want to see the git history before May 2014, it's here: https://code.google.com/p/road-rage/

Thesis at http://apps.cs.utexas.edu/tech_reports/reports/tr/TR-2157.pdf

I haven't had any time to maintain this project since graduating, but I'll try my best to answer any
questions about it. Contact dabreegster@gmail.com.

## Developer guide

### How to observe data while simulations are running

Suppose you want to process and react to events, like drivers starting a turn or intersections
deciding what lane to grant entry to next. Rather than litter calls to your code everywhere, you can
use the observer pattern to hook into events you need. Events are defined in sim/Events.Scala, and
examples of listeners can be found in analysis/Monitoring.scala and experiments/Metrics.scala. If
there's something in the simulation you can't access reasonably via events, let me know.

### Code Organization

All of the main Scala code is in utexas/aorta/.

*   analysis/ tools for analyzing simulations as they run
*   common/ misc utility code that isn't too AORTA-specific
*   common/algorithms/ reusable, well-contained general algorithms
*   contrib/ experimental or unfinished code
*   experiments/ framework for running experiments and existing experiments
*   map/ the map model
*   map/make/ conversion from OSM to AORTA's maps
*   sim/ main traffic simulation code
*   sim/drivers/ everything related to individual driver agents
*   sim/intersections/ intersection controllers
*   sim/make/ code for scenarios, which specify a simulation
*   tests/ mostly archaic, but hopefully integration tests someday
*   ui/ scary code for the Swing GUI

There's also data files:

*   osm/ Put OpenStreetMap XML files here
*   maps/ Converted maps
*   scenarios/ Generated scenarios and savestates
*   Finally, non-Scala code:

*   reports/ R scripts for turning raw data from experiments into plots
*   tools/ Main scripts go in the base directory, less important ones go here
*   tools/cloud/ for deploying AORTA to Google Compute Engine

### Style Conventions

import scala.collection.mutable and then refer to mutable.ListBuffer, mutable.HashSet, etc.
Importing directly makes it unclear that you're using a mutable collection.





- http://www.aorta-traffic.org/usage/
- http://www.aorta-traffic.org/developer-guide/
