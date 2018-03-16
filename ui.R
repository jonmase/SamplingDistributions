# Shiny Server for Robustness Model
# Authors: Jon Mason, Medical Sciences Division Learning Technologies, University of Oxford,
#		   Martin J. Hadley, Academic IT Research Support, University of Oxford
# License: GPL v3, https://www.gnu.org/licenses/gpl.html

navbarPage(
  "Sampling Distributions",
  tabPanel(
    "Visualisation",
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
      )
    )
  ),
  tabPanel(
    "About",
    fluidPage(
      
      
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
      ),
      p(
        "This Shiny app is hosted on the University of Oxford's ",
        a(
          "Interactive Data Network",
          href = "http://idn.it.ox.ac.uk",
          target = "_blank"
        ),
        " shinyapps.io subscription."
      ),
      p(strong("Author: "), "John Mason"),
      p(strong("Code Deposit: "), a(
        "https://doi.org/10.6084/m9.figshare.5966248",
        href = "https://doi.org/10.6084/m9.figshare.5966248",
        target = "_blank"
      ))
      
      
    )
  ), collapsible = TRUE
)