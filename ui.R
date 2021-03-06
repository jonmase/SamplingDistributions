# Shiny Server for Robustness Model
# Authors: Jon Mason, Medical Sciences Division Learning Technologies, University of Oxford,
#		   Martin J. Hadley, Academic IT Research Support, University of Oxford
# License: GPL v3, https://www.gnu.org/licenses/gpl.html

fluidPage(
  title="Sampling Distributions",
  #h2("Sampling Distributions"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      uiOutput('resetable_input'),
      fluidRow(
        align = "center",
        actionButton("rerun", "Rerun"),
        actionButton("reset", "Reset to Defaults")
      )
    ),
    mainPanel(plotOutput("firstPlot"), style="height: 700px")
  ),
  p(
    tagList(
      "Created for the ",
      a(
        "MSc in Evidence-Based Health Care",
        href = "https://www.conted.ox.ac.uk/about/msc-in-evidence-based-health-care",
        target = "_blank"
      ),
      " at the ",
      a(
        "University of Oxford",
        href = "http://www.ox.ac.uk",
        target = "_blank"
      ),
      " by Tom Fanshawe and Jason Oke, with assistance from ",
      a(
        "MSD Learning Technologies",
        href = "http://www.medsci.ox.ac.uk/msdlt",
        target = "_blank"
      ),
      " and ",
      a(
        "Martin John Hadley",
        href = "https://orcid.org/0000-0002-3039-6849",
        target = "_blank"
      )
    )
  )
  
)