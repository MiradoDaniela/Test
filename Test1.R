####CREATION DE L'APPLICATION SHINY###

#Installation des packages#
install.packages("shiny")  #Pour installer le package shiny#
install.packages("vroom") #Pour installer le package vroom qui permet d'importer la base des données#
library(shiny) #Pour activer le package shiny#
library(vroom) #Pour activer le package vroom#
library(dplyr, warn.conflicts = FALSE) # Pour Activer le package dplyr et supprimer les conflits#

#Importation des données et selection des colonnes dont on souhaite utiliser# 
#pour effectuer les recherches dans shiny#
baseline <- vroom::vroom("E:/R_WWF/Daniela.txt", col_types = list(), na = "")
baseline %>% 
select(SEXE_SINGLE_FCT, Q16_FEMME_EN_SECURITE_LA_NUIT_SINGLE_FCT, Q18_PARTICIPATION_FEMMES_AUX_DEC_SINGLE_FCT, everything())


#Affichage de la date en mois et annee####
baseline$DATE <- strptime(baseline$DATE, format= "%d%m%Y") #pour faire appel aux donnees 'DATE'# 
format(baseline$DATE, format= "%Y-%m-%d")#pour activer le format POSIXct#
str(baseline) #pour verifier les changements dans les donnees#
baseline$DATE <- format(as.Date(baseline$DATE), "%m-%Y") #Affichage de la date en mois et année#



####Anonymisation de la colonne « NOM_ENQUETE_CHR »####
cols_to_mask <- c("NOM_ENQUETE_CHR") #Pour choisir la colonne à  masquer#

anonymize <- function(x, algo="crc32") {
  sapply(x, function(y) if(y == "" | is.na(y)) "" else digest(y, algo = algo))
} #pour changer en anonyme les noms des personnes enquetees#

setDT(baseline) 
baseline[, (cols_to_mask) := lapply(.SD, anonymize), .SDcols = cols_to_mask] #pour activer l'anonymat des personnes enquetes#


####Creation de l'application####

#Entrée des données et selection avec libelle des ensembles de donnees#
ui <- fluidPage(
  selectInput("sexe", "Sexe", choices = unique(baseline$SEXE_SINGLE_FCT)),
  selectInput("femmeensecurite", "Femme en sécurité la nuit", choices = NULL),
  selectInput("participation", "Participation femmes aux décisions associations", choices = NULL),
  tableOutput("data")
)

#Sortie des donnees et affichage des resultats de recherche#
server <- function(input, output, session) {  
  sexe <- reactive({                              
    filter(baseline, SEXE_SINGLE_FCT == input$sexe)
  })
  
  observeEvent(sexe(), {
    choices <- unique(sexe()$Q16_FEMME_EN_SECURITE_LA_NUIT_SINGLE_FCT)
    updateSelectInput(session, "femmeensecurite", choices = choices) 
  })
  
  femme <- reactive({
    req(input$femmeensecurite)
    filter(sexe(), Q16_FEMME_EN_SECURITE_LA_NUIT_SINGLE_FCT == input$femmeensecurite)
  })
  observeEvent(femme(), {
    choices <- unique(femme()$Q18_PARTICIPATION_FEMMES_AUX_DEC_SINGLE_FCT)
    updateSelectInput(session, "participation", choices = choices)
  })
  
  output$data <- renderTable({
    req(input$participation)
    femme() %>% 
      filter(Q18_PARTICIPATION_FEMMES_AUX_DEC_SINGLE_FCT == input$participation) 
  })
}
shinyApp(ui, server) #Pour lancer l'application#
