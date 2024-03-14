# ---------------------------------------------------------------------------- #
# Data Analyst: Gabriel Okasa
# Script: Preparing titles and captions for plots in all languages
# ---------------------------------------------------------------------------- #

# define plot types
plot_types <- c("TotalAmount",
                "TotalAmountRequested",
                "AmountPerYear",
                "TotalNumberSubmitted",
                "AmountPerSubmission",
                "AverageDurationMonth",
                "SuccessRate",
                "FundingRate",
                "AverageTeamSize")

# define titles & captions for plots in three languages
plot_titles <- vector("list", length = length(plot_types))
names(plot_titles) <- plot_types

# add the titles
for (plot_idx in plot_types) {
  
  # -------------------------------------------------------------------------- #
  # add the titles accordingly
  if (plot_idx == "TotalAmount") {
    # for total amounts
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in federal funding and demand for SNSF funding",
      # de
      "Entwicklung der Bundesbeiträge und der Nachfrage nach SNF-Mitteln",
      # fr
      "Évolution des subventions fédérales et des demandes de fonds auprès du FNS"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- #
  } else if (plot_idx == "TotalAmountRequested") {
    # for total amounts requested
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in requested funding",
      # de
      "Entwicklung der Nachfrage nach Fördermitteln",
      # fr
      "Évolution des demandes de fonds"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- #
  } else if (plot_idx == "AmountPerYear") {
    # for amounts per project year
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in requested funding per application-year",
      # de
      "Entwicklung der Nachfrage pro Gesuch-Jahr",
      # fr
      "Évolution des demandes de fonds par requête et par année de financement"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- # 
  } else if (plot_idx == "TotalNumberSubmitted") {
    # for total number of submission
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in number of submitted proposals",
      # de
      "Entwicklung der Zahl der Gesuchseingänge",
      # fr
      "Évolution du nombre de requêtes soumises"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- #
  } else if (plot_idx == "AmountPerSubmission") {
    # for amounts per submission
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in requested funding per application",
      # de
      "Entwicklung der Nachfrage pro Gesuch",
      # fr
      "Évolution des demandes de fonds par requête"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- #  
  } else if (plot_idx == "AverageDurationMonth") {
    # for average duration
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in average project duration",
      # de
      "Entwicklung der durchschnittlichen Projektlaufzeit",
      # fr
      "Évolution de la durée moyenne des projets"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- #  
  } else if (plot_idx == "SuccessRate") {
    # for success rates
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in success rates",
      # de
      "Entwicklung der Erfolgsquoten",
      # fr
      "Évolution des taux de réussite"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- #
  } else if (plot_idx == "FundingRate") {
    # for funding rates
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in funding rates",
      # de
      "Entwicklung der Förderraten",
      # fr
      "Évolution du taux d’encouragement"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  # -------------------------------------------------------------------------- #
  } else if (plot_idx == "AverageTeamSize") {
    # for team size
    plot_titles[[plot_idx]] <- c(
      # en
      "Development in average team size: applicants and project partners",
      # de
      "Entwicklung der durschnittlichen Teamgrösse: Gesuchstellende und Projektpartner:innen",
      # fr
      "Évolution de la taille des équipes : requérant·es et partenaires de projet"
    )
    # and define language names
    names(plot_titles[[plot_idx]]) <- c("en", "de", "fr")
    
  }
}

# ---------------------------------------------------------------------------- #
# define legends labels of the main plot depending on the language
demand_snsf_legend <- switch(params$lang,
                             en = "Funding requests to the SNSF",
                             de = "Nachfrage nach SNF-Mitteln",
                             fr = "Demandes de fonds auprès du FNS")
# for total amount requested
federal_contrib_legend <- switch(params$lang,
                                 en = "Federal contributions",
                                 de = "Bundesbeiträge",
                                 fr = "Subventions fédérales")

# ---------------------------------------------------------------------------- #
# define labels of the main plot depending on the language
x_name <- switch(params$lang,
                 en = "Year",
                 de = "Jahr",
                 fr = "Année")
# for total amount requested
y_lab <- switch(params$lang,
                en = "CHF million",
                de = "Mio. CHF",
                fr = "mio CHF")

# ---------------------------------------------------------------------------- #
# define legend names based on language for the detailed plots
# all instruments
all_instruments <- switch(params$lang,
                          en = "All schemes",
                          de = "Alle Instrumente",
                          fr = "Tous les instruments")
# projects
projects <- switch(params$lang,
                   en = "Projects",
                   de = "Projekte",
                   fr = "Projets")
# careers
careers <- switch(params$lang,
                  en = "Careers",
                  de = "Karrieren",
                  fr = "Carrières")

# ---------------------------------------------------------------------------- #
