#' Please do not rename this file !
library(shiny.exe)
hostWin(
    appDir = 'D:/11_Coding_test/smartdashboard_v2',
    port = getOption('shiny.port'),
    launch.browser = TRUE,
    host = 'Public',
                  workerId = '',
                  quiet = FALSE,
                  display.mode = c('auto', 'normal', 'showcase'),
                  test.mode = getOption('shiny.testmode', FALSE))
