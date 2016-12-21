library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

server <- function(input, output) {
  
  # Make fence
  
  grid <- data.frame(x = c(rep(1,100),1:100), y = c(1:100, rep(1,100)), z = rep(1:100,2))
  
  grid2 <- data.frame(x = c(rep(100,100),1:100), y = c(1:100, rep(100,100)), z = rep(101:200,2))
  
  grid3 <- data.frame(x = c(rep(1,100),1:100), y = c(100:1, rep(100,100)), z = rep(201:300,2))
  
  grid4 <- data.frame(x = c(rep(100,100),1:100), y = c(100:1, rep(1,100)), z = rep(301:400,2))
  
  grid <- rbind(grid,grid2,grid3,grid4)
  
  fence <- ggplot(grid, aes(x,y,group=z))+
    geom_line(alpha = .7)+
    theme_void()
  
  # letters for plotting
  
  A <- data.frame(x =  + c(rep((.5),4),1.5,1.5,rep((2.5),4)),
                  y =  + c(1:4,5,3,1:4),
                  z="a")
  
  B <- data.frame(x =  + c(rep((.5),5),rep((1.5),3),rep((2.5),2)),
                  y =  + c(1:5,5,3,1,4,2),
                  z = "b")
  
  C <- data.frame(x =  + c(rep((.5),5),rep((1.5),2),rep((2.5),2)),
                  y =  + c(1:5,5,1,5,1),
                  z = "c")
  
  D <- data.frame(x =  + c(rep((.5),5),rep((1.5),2),rep((2.5),3)),
                  y =  + c(1:5,5,1,2:4),
                  z = "d")
  
  E <- data.frame(x =  + c(rep((.5),5),.5 + rep(1:2,3)),
                  y =  + c(1:5,5,5,3,3,1,1), 
                  z= "e")
  
  `F` <- data.frame(x =  + c(rep((.5),5),.5 + rep(1:2,2)),
                    y =  +c(1:5,5,5,3,3),
                    z="f")
  
  G <- data.frame(x =  + c(rep((.5),5),rep((1.5),2),
                                rep((2.5),3),rep((3.5),4)),
                  y =  + c(1:5,1,5,3,1,5,5,1:3), 
                  z="g")
  
  H <- data.frame(x =  + c(rep((.5),5),1.5,2.5,rep((3.5),5)),
                  y =  + c(1:5,3,3,1:5), 
                  z= "h")
  
  I <- data.frame(x =  + c(rep((.5),5)),
                  y =  + c(1:5),
                  z = "i")
  
  J <- data.frame(x =  + c(rep((2.5),5),1.5,rep((.5),2)),
                  y =  + c(1:5,1,1:2),
                  z = "j")
  
  K <- data.frame(x =  + c(rep((.5),5),1.5,rep((2),2),rep((2.5),2),
                                rep((3),2),rep((3.5),2)),
                  y =  + c(1:5,3,3.5,2.5,2,4,1.5,4.5,1,5),
                  z = "k")
  
  L <- data.frame(x =  + c(rep((.5),5),.5 + 1:2),
                  y =  + c(1:5,1,1), 
                  z= "l")
  
  M <- data.frame(x =  + c(rep((.5),5),seq(1,3,.5),rep((3.5),5)),
                  y =  + c(1:5,4.5,4,3.5,4,4.5,1:5), 
                  z= "m")
  
  N <- data.frame(x =  + c(rep((.5),5),seq(1,2,.5),rep((2.5),5)),
                  y =  + c(1:5,3.5,3,2.5,1:5), 
                  z= "N")
  
  O <- data.frame(x =  + c(rep((.5),5),rep((1.5),2),rep((2.5),5)),
                  y =  + c(1:5,1,5,1:5), 
                  z= "o")
  
  P <- data.frame(x =  + c(rep((.5),5),rep((1.5),2),rep((2.5),3)),
                  y =  + c(1:5,3,5,3:5), 
                  z= "p")
  
  Q <- data.frame(x =  + c(rep((.5),5),rep((1.5),2),2,rep((2.5),4),3,3.5),
                  y =  + c(1:5,1,5,1.5,2:5,1.5,1), 
                  z= "q")
  
  R <- data.frame(x =  + c(rep((.5),5),rep((1.5),2),rep((2),2),rep((2.5),4)),
                  y =  + c(1:5,3,5,2.5,1.5,3:5,1), 
                  z= "r")
  
  S <- data.frame(x =  + c(rep((.5),4),rep((1.5),3),rep((2.5),4)),
                  y =  + c(1,3:5,1,3,5,1:3,5), 
                  z= "s")
  
  `T` <- data.frame(x =  + c(rep((1.5),5),.5,2.5),
                    y =  + c(1:5,5,5), 
                    z= "t")
  
  U <- data.frame(x =  + c(rep((.5),5),1.5,rep((2.5),5)),
                  y =  + c(1:5,1,1:5), 
                  z= "u")
  
  V <- data.frame(x =  + c(rep((.5),4),1,1.5,2,rep((2.5),4)),
                  y =  + c(2:5,1.5,1,1.5,2:5), 
                  z= "v")
  
  W <- data.frame(x =  + c(rep((.5),5),seq(1,3,.5),rep((3.5),5)),
                  y =  + c(1:5,1.5,2,2.5,2,1.5,1:5), 
                  z= "w")
  
  X <- data.frame(x =  + c(seq(.5,4.5,.5),seq(2,.5,-.5),seq(4.5,3,-.5)),
                  y =  + c(seq(1,5,.5),seq(3.5,5,.5),seq(1,2.5,.5)),
                  z = "x")
  
  Y <- data.frame(x =  + c(seq(2,.5,-.5),rep(2.5,3),seq(3,4.5,.5)),
                  y =  + c(seq(3.5,5,.5),1:3,seq(3.5,5,.5)),
                  z = "y")
  
  Z <- data.frame(x =  + c(seq(1,3.5,.5),rep(seq(.5,3.5,1),2)),
                  y =  + c(seq(1.5,4,.5),rep(5,4),rep(1,4)),
                  z = "z")
  
  One <- data.frame(x =  rep(.5,5),
                    y = c(1:5),
                    z = "1")
  
  Two <- data.frame(x = c(rep((.5),4),rep((1.5),3),rep((2.5),4)),
                    y = c(1:3,5,1,3,5,1,3:5), 
                    z = "2")
  
  Three <- data.frame(x =  c(rep((2.5),5),.5 + rep(0:1,3)),
                      y =  c(1:5,5,5,3,3,1,1), 
                      z= "3")
  
  Four <- data.frame(x =  c(rep((.5),3),1.5,rep(2.5,5)),
                     y =  c(3:5,3,1:5), 
                     z= "4")
  
  Five <- data.frame(x = c(rep((.5),4),rep((1.5),3),rep((2.5),4)),
                     y = c(1,3:5,1,3,5,1:3,5), 
                     z = "5")
  
  Six <- data.frame(x = c(rep((.5),5),rep((1.5),3),rep((2.5),4)),
                    y = c(1:5,1,3,5,1:3,5), 
                    z = "6")
  
  Seven <- data.frame(x = c(rep((2.5),5),.5,1.5),
                      y = c(1:5,5,5), 
                      z = "7")
  
  Eight <- data.frame(x = c(rep((.5),5),rep((1.5),3),rep((2.5),5)),
                      y = c(1:5,1,3,5,1:5), 
                      z = "8")
  
  Nine <- data.frame(x = c(rep((.5),3),rep((1.5),2),rep((2.5),5)),
                     y = c(3:5,3,5,1:5), 
                     z = "9")
  
  Zero <- data.frame(x = c(rep((.5),5),rep((1.5),2),rep((2.5),5)),
                     y = c(1:5,1,5,1:5), 
                     z = "0")
  
  dot <- data.frame(x = .5, y = 1, z = "dot") 
  
  exclam <- data.frame(x =  rep(.5,4),
                       y = c(1,3:5),
                       z = "exclam")
  
  space <- data.frame(x= .5,y=1000,z="space")
  
  # final letter list + reference
  
  letters <- list(A,B,C,D,E,`F`,G,H,I,J,K,L,M,N,O,P,Q,R,S,`T`,U,V,W,X,Y,Z,
                  Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine,dot,exclam,space)
  
  ref <- data.frame(lower = c(str_to_lower(LETTERS),as.character(0:9),".","!"," "),1:39)
  
  ref$lower <- as.character(ref$lower)
  
  # functions
  
  fun1 <- function(string){
    
    string <- str_to_lower(string)
    
    vec <- NULL
    
    for(i in 1:nchar(string)){
      
      vec[i] <- str_sub(string,i,i)
      
    }
    
    vec <- data.frame(lower = vec)
    
    vec <- left_join(vec,ref)
    
    vec <- vec[,2]
    
    df <- data.frame(letters[[vec[1]]], group = 1)
    
    i <- 2
    
    cups <- NULL
    
    for(i in 2:length(vec)){
      
      cups <- data.frame(letters[[vec[i]]], group = i)
      
      df <- rbind(df,cups)
      
    }
    
    df
  }
  
  fun2 <- function(x){
    
    vec <- NULL
    
    vec[1] <- 3
    
    i <- 2
    
    for(i in 2:length(x)){
      
      vec[i] <- {ifelse(x[i-1] %in% c("i","space","1","dot","exclam"), 2,
                        ifelse(x[i-1] %in% c("g","k","m","q","w","z"), 5,
                               ifelse(x[i-1] %in% c("x","y"),6,4))) + vec[i-1]}
      
    }
    
    df <- data.frame(z = x, n = vec)
    
    df
  }
  
  # plot 
  
  inputdata <- eventReactive(input$go,{
  
  inputdata <- fun1(input$upper_text)
  
  chars <- inputdata %>% 
    group_by(group) %>% 
    summarise(z = nth(z,1))
  
  inputdata <- left_join(inputdata,
                    data.frame(group = chars$group, n = fun2(chars$z)$n))
  
  })
  
  inputdata2 <- eventReactive(input$go,{
    
    inputdata2 <- fun1(input$lower_text)
    
    chars2 <- inputdata2 %>% 
      group_by(group) %>% 
      summarise(z = nth(z,1))
    
    inputdata2 <- left_join(inputdata2,
                           data.frame(group = chars2$group, n = fun2(chars2$z)$n))
    
  })
  
  output$cups <- renderPlot({
    fence +
      coord_cartesian(xlim = c(10,80), ylim = c(2,14))+
      geom_point(data = inputdata(), aes(x=x+n+6,y=y+8,group=z), size = 5, alpha = .5, 
                 color = isolate(input$color))+
      geom_point(data = inputdata2(), aes(x=x+n+6,y=y+2,group=z), size = 5, alpha = .5,
                 color = isolate(input$color))
    
  })
  
  output$num_cups <- eventReactive(input$go,{
    
    inputdata <- fun1(input$upper_text)
    
    chars <- inputdata %>% 
      group_by(group) %>% 
      summarise(z = nth(z,1))
    
    inputdata <- left_join(inputdata,
                           data.frame(group = chars$group, n = fun2(chars$z)$n))
    
    inputdata2 <- fun1(input$lower_text)
    
    chars2 <- inputdata2 %>% 
      group_by(group) %>% 
      summarise(z = nth(z,1))
    
    inputdata2 <- left_join(inputdata2,
                            data.frame(group = chars2$group, n = fun2(chars2$z)$n))
    
    str_c((as.numeric(nrow(inputdata %>% filter(z != "space"))) +
                               as.numeric(nrow(inputdata2 %>% filter(z != "space")))), 
          " cups used (",
          as.numeric(nrow(inputdata %>% filter(z != "space"))), " in the first row)")
    
  })

}





