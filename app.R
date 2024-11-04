library(bslib)
library(mwshiny)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(deSolve)

# -----UI-----
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Métodos Numéricos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introducción", tabName = "intro", icon = icon("play")),
      menuItem("Eliminación de Cafeína", tabName = "caf", icon = icon("coffee")),
      menuItem("Eliminación de Fármaco", tabName = "farm", icon = icon("flask")),
      menuItem("Comparación de los Métodos", tabName = "comp", icon = icon("sliders")
    )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h2("Métodos numéricos para la solución de ODEs"),
              tags$h4("Método de Euler y Runge-Kutta 4to Orden"),
              tags$article("Tanto el método de Euler como el de Runge-Kutta de
                           4to Orden (RK4), son métodos númericos que son utilizados
                           para obtener la solución de una ecuación diferencial."),
              tags$article(""),
              tags$article("El método de Euler es de primer orden y se basa en la tasa
                           de cambio del punto actual en el que se encuentra la estimación
                           y su implementación es relativamente sencilla."),
              tags$article(""),
              tags$img(src = "Euler.jpeg", height = "200px", width = "200px"),
              tags$article("Por el otro lado, el método de RK4 es una <<mejora>> por así decirlo
                           del método de Euler. Esta versión del método en específico es de cuarto
                           orden y utiliza cálculos de la pendiente en múltiples pasos."),
              tags$h4("Método de Runge-Kutta-Fehlhberg (RKF45)"),
              tags$article("El Método de RKF45 es una <<mejora>> del RK4, desarrollado por Erwin
                           Fehlberg. Este método varía del RK4, ya que añade un control del paso de
                           forma adaptativa. Esto aumenta la precisión y eficiencia del método, haciendo
                           uso de una solución tanto de orden 4 como de orden 5 y hallando su diferencia
                           para obtener el error local de cada paso. Por lo tanto, en caso de que el
                           error resulta ser grande, el paso es reducido, y por el otro lado, si el error
                           es pequeño el paso incrementa."),
              tags$article(""),
              tags$article("Así pues, el método de Runge_kutta_Fehlhberg tiene varias ventajas sobre 
                           otros métodos de solución de ecuaciones diferenciales:"),
              tags$ul(
                tags$li("Precisión: Dado su uso de ambos órdenes 4 y 5, el proceso de cálculo es más certero."),
                tags$li("Capacidad de Solución: Es aplicable a ecuaciones diferenciales cuya solución via otros
                        métodos puede resultar imprecisa o errónea (esto está estrechamente relacionado con su
                        precisión pero es pertinente mecionarlo de todas formas)."),
                tags$li("Adaptabilidad: Como ya se ha mencionado anteriormente, RKF45 adapta el tamaño del paso 
                        según va realizando cálculos, basándose directamente en el error del paso anterior."),
                tags$li("Eficiencia: Dada su precisión, este método evita hacer cálculos inncesarios (puntos donde
                        no se obtiene mucha información importante), es decir, que optimiza el proceso de cálculo 
                        de pasos.")
              )
      ),
      tabItem(tabName = "caf",
              h2("Eliminación metabólica de la Cafeína"),
              fluidRow(
                column(width = 4,
                  box(
                    title = "Parámetros del Modelado", width = NULL, solidHeader = TRUE, status = "primary",
                    helpText("Variables de la Ecuación"),
                    numericInput(
                      "X0_caf", "Cantidad inicial de Cafeína (mg)", value = 105.6, step = 0.1
                    ),
                    numericInput(
                      "k_rate", "Tasa de eliminación de la cafeína (k)", value = 0.1, step = 0.01
                    ),
                    numericInput(
                      "a_input", "Consumo adicional y constante de cafeína (a)", value = 17.6, step = 0.1
                    ),
                    helpText("Variables del Método Numérico"),
                    numericInput(
                      "h_input", "Tamaño de paso (h)", value = 5, step = 0.5
                    ),
                    numericInput(
                      "tol_input","Tolerancia de error (RFK45)", value = 0.006737947 
                    ),
                  )
              ),
              column(width = 8,
                     box(
                       title = "dx/dt = -kx + a", width = NULL, solidHeader = TRUE, status = "primary",
                       plotOutput("grafico_cafeina")
                     ),
                     box(
                       title = "Selección de Métodos", width = 12, solidHeader = TRUE, status = "info",
                       fluidRow(
                         column(4,
                             checkboxGroupInput("metodos_seleccionados", "Seleccione los métodos a mostrar:",
                                                choices = c("Euler", "RK4", "RK45"),
                                                selected = c("Euler", "RK4", "RK45"))
                             ),
                         column(4,
                             checkboxGroupInput("metodos_seleccionados2", "",
                                                choices = c("Euler deSolve", "RK4 deSolve", "RK45 deSolve"),
                                                selected = c("Euler deSolve", "RK4 deSolve", "RK45 deSolve"))
                             ),
                       )
                     )
              )
            )
      ),
      tabItem(tabName = "farm",
              h2("Eliminación del Fármaco"),
              fluidRow(
                column(width = 4,
                  box(
                    title = "Parámetros del Modelado", width = NULL, solidHeader = TRUE, status = "primary",
                    helpText("Variables de la ecuación"),
                    numericInput(
                      "Xg_0", "Cantidad inicial de medicamento en el estómago", value = 400, step = 1
                    ),
                    numericInput(
                      "a_rate", "Tasa de eliminación de medicamento del estómago", value = 0.025, step = 0.025
                    ),
                    numericInput(
                      "b_rate", "Tasa de eliminación de medicamento de la sangre", value = 0.033, step = 0.033
                    ),
                    helpText("Variables del Método Numérico"),
                    numericInput(
                      "h_input2", "Tamaño de paso (h)", value = 10, step = 5
                    ),
                    numericInput(
                      "tol_input2", "Tolerancia de error (RFK45)", value = 0.06737947
                    )
                  )
                ),
                column(width = 8,
                       box(
                         title = "dx/dt = -a * Xg | dx/dt = a * Xg - b * Xb", width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("grafico_medicamento")
                       ),
                       box(
                         title = "Selección de Métodos", width = 12, solidHeader = TRUE, status = "info",
                         fluidRow(
                           column(4,
                                  checkboxGroupInput("metodos_seleccionados3", "Seleccione los métodos a mostrar:",
                                                     choices = c("Euler", "RK4", "RK45"),
                                                     selected = c("Euler", "RK4", "RK45"))
                           )
                         )
                       )
                )  
              )
      ),
      tabItem(tabName = "comp",
              h2("Comparación de los métodos numéricos (Eliminación de la Cafeína)"),
              fluidRow(
                 box(
                  title = "Euler",  width = 4, solidHeader = TRUE, status = "primary",
                  tableOutput("tabla_caf")
                 ),
                 box(
                   title = "RK4",  width = 4, solidHeader = TRUE, status = "primary",
                   tableOutput("tabla_caf2")
                 ),
                 box(
                   title = "RKF45",  width = 4, solidHeader = TRUE, status = "primary",
                   tableOutput("tabla_caf3")
                 )
              )
      )
    )
  )
)

# -----LÓGICA DEL SERVIDOR-----
server <- function(input, output) {
  
  output$grafico_cafeina <- renderPlot({
    
    h1 <- input$h_input
    tol <- input$tol_input
    
    f_caf <- function(t, X) {
      return(-input$k_rate * X + input$a_input)
    } 
    
    metodo_euler <- function(x0, y0, h) {
      x_end <- 40
      n <- (x_end - x0) / h
      
      x_val <- numeric(n + 1)
      y_val <- numeric(n + 1)
      
      x_val[1] <- x0
      y_val[1] <- y0
      
      for (i in 1:n) {
        y_val[i + 1] <- y_val[i] + h * f_caf(x_val[i], y_val[i])
        x_val[i + 1] <- x_val[i] + h
      }
      
      res <- data.frame(t = x_val, y = y_val)
      return(res)
    }
    
    metodo_rk4 <- function(x0, y0, h) {
      # Define the range and calculate n based on h
      x_end <- 40
      n <- (x_end - x0) / h
      
      # Initialize x and y vectors
      x_val <- numeric(n + 1)
      y_val <- numeric(n + 1)
      
      x_val[1] <- x0
      y_val[1] <- y0
      
      # Perform RK4 method
      for (i in 1:n) {
        k1 <- h * f_caf(x_val[i], y_val[i])
        k2 <- h * f_caf(x_val[i] + h / 2, y_val[i] + k1 / 2)
        k3 <- h * f_caf(x_val[i] + h / 2, y_val[i] + k2 / 2)
        k4 <- h * f_caf(x_val[i] + h, y_val[i] + k3)
        
        y_val[i + 1] <- y_val[i] + (k1 + 2 * k2 + 2 * k3 + k4) / 6
        x_val[i + 1] <- x_val[i] + h
      }
      
      # Return the result as a data frame
      res <- data.frame(t = x_val, y = y_val)
      return(res)
    }
    
    metodo_rkf45 <- function(x0, y0, h, tol) {
      x_end <- 40
      x_val <- c(x0)
      y_val <- c(y0)
      
      # Coefficients for RKF45
      a2 <- 1 / 4; a3 <- 3 / 8; a4 <- 12 / 13; a5 <- 1; a6 <- 1 / 2
      b21 <- 1 / 4
      b31 <- 3 / 32; b32 <- 9 / 32
      b41 <- 1932 / 2197; b42 <- -7200 / 2197; b43 <- 7296 / 2197
      b51 <- 439 / 216; b52 <- -8; b53 <- 3680 / 513; b54 <- -845 / 4104
      b61 <- -8 / 27; b62 <- 2; b63 <- -3544 / 2565; b64 <- 1859 / 4104; b65 <- -11 / 40
      
      c1 <- 16 / 135; c3 <- 6656 / 12825; c4 <- 28561 / 56430; c5 <- -9 / 50; c6 <- 2 / 55
      dc1 <- c1 - 25 / 216; dc3 <- c3 - 1408 / 2565
      dc4 <- c4 - 2197 / 4104; dc5 <- c5 + 1 / 5; dc6 <- c6
      
      while (x_val[length(x_val)] < x_end) {
        # Current x and y
        x <- x_val[length(x_val)]
        y <- y_val[length(y_val)]
        
        # Compute the six slopes
        k1 <- h * f_caf(x, y)
        k2 <- h * f_caf(x + a2 * h, y + b21 * k1)
        k3 <- h * f_caf(x + a3 * h, y + b31 * k1 + b32 * k2)
        k4 <- h * f_caf(x + a4 * h, y + b41 * k1 + b42 * k2 + b43 * k3)
        k5 <- h * f_caf(x + a5 * h, y + b51 * k1 + b52 * k2 + b53 * k3 + b54 * k4)
        k6 <- h * f_caf(x + a6 * h, y + b61 * k1 + b62 * k2 + b63 * k3 + b64 * k4 + b65 * k5)
        
        # RKF estimates for y
        y_rk4 <- y + c1 * k1 + c3 * k3 + c4 * k4 + c5 * k5 + c6 * k6
        y_rk5 <- y + (c1 - dc1) * k1 + (c3 - dc3) * k3 + (c4 - dc4) * k4 + (c5 - dc5) * k5 + (c6 - dc6) * k6
        
        # Calculate the error and adjust step size
        error <- abs(y_rk5 - y_rk4)
        if (error <= tol) {
          # Accept step
          x_val <- c(x_val, x + h)
          y_val <- c(y_val, y_rk5)
        }
        
        # Adaptive step size adjustment
        h <- h * min(max(0.84 * (tol / error)^(1/4), 0.1), 4.0)
      }
      
      # Return the results as a data frame
      res <- data.frame(t = x_val, y = y_val)
      return(res)
    }

    f_caf_deSolve <- function(t, state, parm)
    {
      with(as.list(state),
           {
             dxdt = rep(0, length(state))
             dxdt = -input$k_rate*X+input$a_input
             return(list(dxdt))
           })
    }
    
    init = c(X = input$X0_caf)
    t = seq(0, 40, by = h1)
    
    
    
    euler_cafeina <- metodo_euler(0, input$X0_caf, h1)
    rk4_cafeina <- metodo_rk4(0, input$X0_caf, h1)
    rkf45_cafeina <- metodo_rkf45(0, input$X0_caf, h1, tol)
    
    names(euler_cafeina) <- c("t", "y")
    names(rk4_cafeina) <- c("t", "y")
    names(rkf45_cafeina) <- c("t", "y")
    
    euler_cafeina$Metodo <- "Euler"
    rk4_cafeina$Metodo <- "RK4"
    rkf45_cafeina$Metodo <- "RK45"
    
    euler_cafeina_deSolve <- ode(
      y = init,
      times = t,
      func = f_caf_deSolve,
      parms = NULL,
      method = "euler"
    )
    
    rk4_cafeina_deSolve <- ode(
      y = init,
      times = t,
      func = f_caf_deSolve,
      parms = NULL,
      method = "rk4"
    )
    
    rk45_cafeina_deSolve <- ode(
      y = init,
      times = t,
      func = f_caf_deSolve,
      parms = NULL,
      method = "ode45"
    )
    
    rk45_cafeina_deSolve <- as.data.frame(rk45_cafeina_deSolve)
    names(rk45_cafeina_deSolve) <- c("t", "y")
    rk45_cafeina_deSolve$Metodo <- "RK45 deSolve"
    
    rk4_cafeina_deSolve <- as.data.frame(rk4_cafeina_deSolve)
    names(rk4_cafeina_deSolve) <- c("t", "y")
    rk4_cafeina_deSolve$Metodo <- "RK4 deSolve"
    
    euler_cafeina_deSolve <- as.data.frame(euler_cafeina_deSolve)
    names(euler_cafeina_deSolve) <- c("t", "y")
    euler_cafeina_deSolve$Metodo <- "Euler deSolve"
    
    cafeina_total <- rbind(euler_cafeina, rk4_cafeina, rkf45_cafeina, euler_cafeina_deSolve, rk4_cafeina_deSolve, rk45_cafeina_deSolve)
    cafeina_total <- subset(cafeina_total, Metodo %in% c(input$metodos_seleccionados, input$metodos_seleccionados2))

    
    ggplot(cafeina_total, aes(x = t, y = y)) +
      geom_line(aes(color = Metodo, linetype = Metodo), linewidth = 1.2) +
      ggtitle("Eliminación de la cafeína",
              subtitle = "Tras ingerir X cantidad de cafeína + variables extra") +
      labs(x = "Tiempo (horas)", y = "Cafeína (mg)") +
      
      scale_color_manual(name = "Métodos",
                         values = c("Euler" = "cornflowerblue", "RK4" = "darkorchid1", "RK45" = "violet", "Euler deSolve" = "red",
                                    "RK4 deSolve" = "mediumspringgreen", "RK45 deSolve" = "goldenrod2")) +
      scale_linetype_manual(name = "Métodos",
                            values = c("Euler" = "solid", "RK4" = "solid", "RK45" = "solid", "Euler deSolve" = "dashed",
                                       "RK4 deSolve" = "dashed", "RK45 deSolve" = "dashed")) +
      
      
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "azure3"),
        plot.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(color = "darkblue"),
        plot.subtitle = element_text(color = "blue3")
      )
  })
  
  output$grafico_medicamento <- renderPlot({
    
    h2<- input$h_input2
    tol <- input$tol_input2
    
    f_med_deSolve <- function(t, state, parms) {
      with(as.list(state),
           {
             dxdt = rep(0, length(state))
             
             dxdt[1] = -input$a_rate*Xg
             dxdt[2] = input$a_rate*Xg - input$b_rate * Xb
             
             return(list(dxdt))
           })
    }
    
    Xg <- input$Xg_0
    Xb <- 0
    a <- input$a_rate
    b <- input$b_rate
    t <- seq(0,360, by = h2)
    init <- c(Xg=Xg, Xb=Xb)
    
    euler_med_deSolve <- ode(
      y = init,
      times = t,
      func = f_med_deSolve,
      parms = NULL,
      method = "euler"
    )
    
    rk4_med_deSolve <- ode(
      y = init,
      times = t,
      func = f_med_deSolve,
      parms = NULL,
      method = "rk4"
    )
    
    rk45_med_deSolve <- ode(
      y = init,
      times = t,
      func = f_med_deSolve,
      parms = NULL,
      method = "ode45"
    )
    
    rk45_med_deSolve <- as.data.frame(rk45_med_deSolve)
    rk4_med_deSolve <- as.data.frame(rk4_med_deSolve)
    euler_med_deSolve <- as.data.frame(euler_med_deSolve)
    
    names(rk45_med_deSolve) <- c("t", "y1", "y2")
    names(rk4_med_deSolve) <- c("t", "y1", "y2")
    names(euler_med_deSolve) <- c("t", "y1", "y2")
    
    rk45_med_deSolve$Metodo <- "RK45"
    rk4_med_deSolve$Metodo <- "RK4"
    euler_med_deSolve$Metodo <- "Euler"
    
    med_total <- rbind(rk45_med_deSolve, rk4_med_deSolve, euler_med_deSolve)
    med_total <- subset(med_total, Metodo %in% input$metodos_seleccionados3)
    
    
    ggplot(med_total, aes(x = t)) +
      geom_line(aes(y = y1, color = Metodo, linetype = Metodo), linewidth = 1.2) +
      geom_line(aes(y = y2, color = Metodo, linetype = Metodo), linewidth = 1.2) +
      ggtitle("Eliminación de Fármaco",
              subtitle = "Pastilla Oral de Paracetamol (500 mg)") +
      labs(x = "Tiempo (min)", y = "Cantidad de Fármaco (mg)") +
      
      scale_color_manual(name = "Métodos",
                         values = c("Euler" = "cornflowerblue", "RK4" = "darkorchid1", "RK45" = "violet")) +
      scale_linetype_manual(name = "Métodos",
                            values = c("Euler" = "solid", "RK4" = "solid", "RK45" = "solid")) +
      theme_stata() +
      theme(
        panel.background = element_rect(fill = "azure3"),
        plot.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(color = "darkblue"),
        plot.subtitle = element_text(color = "blue3")
      )
  })
  
  output$tabla_caf <- renderTable ({
    
    init = c(X = 105.6)
    t = seq(0, 40, 5)
    
    f_caf_deSolve <- function(t, state, parm)
    {
      with(as.list(state),
           {
             dxdt = rep(0, length(state))
             dxdt = -input$k_rate*X+input$a_input
             return(list(dxdt))
           })
    }
    
    euler_cafeina_deSolve <- ode(
      y = init,
      times = t,
      func = f_caf_deSolve,
      parms = NULL,
      method = "euler"
    )
    
    rk45_cafeina_deSolve <- ode(
      y = init,
      times = t,
      func = f_caf_deSolve,
      parms = NULL,
      method = "ode45"
    )
    
    rk45_cafeina_deSolve <- as.data.frame(rk45_cafeina_deSolve)
    names(rk45_cafeina_deSolve) <- c("t", "y")
    rk45_cafeina_deSolve$Metodo <- "RK45 deSolve"

    euler_cafeina_deSolve <- as.data.frame(euler_cafeina_deSolve)
    names(euler_cafeina_deSolve) <- c("t", "y")
    
    head(euler_cafeina_deSolve)
  })
  
  output$tabla_caf2 <- renderTable ({
    
    init = c(X = 105.6)
    t = seq(0, 40, 5)
    
    f_caf_deSolve <- function(t, state, parm)
    {
      with(as.list(state),
           {
             dxdt = rep(0, length(state))
             dxdt = -input$k_rate*X+input$a_input
             return(list(dxdt))
           })
    }
    
    rk4_cafeina_deSolve <- ode(
      y = init,
      times = t,
      func = f_caf_deSolve,
      parms = NULL,
      method = "rk4"
    )
    
    rk4_cafeina_deSolve <- as.data.frame(rk4_cafeina_deSolve)
    names(rk4_cafeina_deSolve) <- c("t", "y")
    
    head(rk4_cafeina_deSolve)
  })
  
  output$tabla_caf3 <- renderTable ({
    
    init = c(X = 105.6)
    t = seq(0, 40, 5)
    
    f_caf_deSolve <- function(t, state, parm)
    {
      with(as.list(state),
           {
             dxdt = rep(0, length(state))
             dxdt = -input$k_rate*X+input$a_input
             return(list(dxdt))
           })
    }
    
    rk45_cafeina_deSolve <- ode(
      y = init,
      times = t,
      func = f_caf_deSolve,
      parms = NULL,
      method = "ode45"
    )
    
    rk45_cafeina_deSolve <- as.data.frame(rk45_cafeina_deSolve)
    names(rk45_cafeina_deSolve) <- c("t", "y")
    
    head(rk45_cafeina_deSolve)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)