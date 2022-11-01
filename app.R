# clear global environment
rm(list = ls())

#import packages

pacman::p_load(shiny)
shiny::runExample("01_hello")
shiny::runExample("06_tabsets")
shiny::runExample("07_widgets")
shiny::runExample("08_html")
shiny::runExample("09_upload")
shiny::runExample("10_download")
shiny::runExample("11_timer")
paste()

shiny::runApp("App-1")
shiny::runApp("census-app", display.mode = "showcase")
shiny::runApp("stockVis", display.mode = "showcase")
