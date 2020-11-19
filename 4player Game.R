gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" ,
                                                "Baltic Avenue" , "Income Tax" , "Reading Railroad" ,
                                                "Oriental Avenue" , "Chance" , "Vermont Avenue" ,
                                                "Connecticut Avenue" , "Jail" , "St. Charles Place" ,
                                                "Electric Company" , "States Avenue" , "Virginia Avenue" ,
                                                "Pennsylvania Railroad" , "St. James Place" , "Community Chest" ,
                                                "Tennessee Avenue" , "New York Avenue" , "Free Parking" ,
                                                "Kentucky Avenue" , "Chance" , "Indiana Avenue" ,
                                                "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" ,
                                                "Ventnor Avenue" , "Water Works" , "Marvin Gardens" ,
                                                "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" ,
                                                "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" ,
                                                "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"))
chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." ,
                                                "Advance to St. Charles Place" , "Advance token to nearest Utility" ,
                                                "Advance token to the nearest Railroad" ,
                                                "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" ,
                                                "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" ,
                                                "Get out of Jail Free" , "Make general repairs on all your property" ,
                                                "Pay poor tax of $15" , "You have been elected Chairman of the Board" ,
                                                "Your building loan matures"))
communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor. Collect $200" ,
                                                "Doctor's fees Pay $50" , "From sale of stock you get $45" ,
                                                "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" ,
                                                "Income tax refund" , "Life insurance matures. Collect $100" ,
                                                "Pay hospital fees of $100" , "Pay school tax of $150" ,
                                                "Receive for services $25" , "You are assessed for street repairs" ,
                                                "You have won second prize in a beauty contest" , "You inherit $100"))
## Begin the Game
dice <- function(verbose=FALSE){
  faces <- sample(1:6, 2, replace=TRUE)
  if(faces[1] == faces[2]) doubles = TRUE
  else doubles = FALSE
  movement = sum(faces)
  if(verbose) cat("Rolled:", faces[1], faces[2], "\n")
  return(list(faces=faces, doubles=doubles, movement=movement))
}
player <- setRefClass("player", 
                      fields = list(
                        pos = "numeric",      # position on the board
                        verbose = "logical",
                        jail = "numeric",
                        rolldouble = "numeric"
                      ), 
                      methods = list(
                        move_n = function(n) {
                          if(verbose) cat("Player at:", pos)
                          if(verbose) cat(" Player moves:", n)
                          pos <<- pos + n
                          if(pos > 40) pos <<- pos - 40
                          if(verbose) cat(" Player now at:", pos,"\n")
                        },
                        go_2_space_n = function(n){
                          if(verbose) cat("Player at:", pos,".")
                          pos <<- n
                          if(verbose) cat(" Player now at:", pos,".\n")
                        },
                        go_2_jail = function(){
                          if(verbose) cat("Player at:", pos,".")
                          pos <<- 11
                          if(verbose) cat(" Player now in jail.\n")
                          jail <<- 1
                        },
                        increase_jail = function(){
                          jail <<- jail + 1
                        },
                        reset_jail = function(){
                          jail <<- 0
                        },
                        increase_double = function(){
                          rolldouble <<- rolldouble + 1
                        },
                        reset_double = function(){
                          rolldouble <<- 0
                        }
                      )
)

#Landing on a Chance Card
drawchance <- function(player, tracking,verbose = FALSE){
  draw <- sample(1:15, 1)
  #tally at chance if you draw a card that moves you
  if (draw %in% 1:9){
    tracking$increase_count(player$pos)
    if (verbose){
      cat("Tally at", player$pos, as.character(gameboard$title[player$pos]), "\n")
    }
  }
  if (verbose){
    cat("Drew Chance card", draw, "-", as.character(chancedeck$card[draw]),'\n')
  }
  if (draw == 1){
    player$go_2_space_n(1)
    if (verbose){
      cat("Player now at position 1:", as.character(gameboard$title[1]), "\n")
    }
  }
  if (draw == 2){
    player$go_2_space_n(25)
    if (verbose){
      cat("Player now at position 25:", as.character(gameboard$title[25]), "\n")
    }
  }
  if (draw == 3){
    player$go_2_space_n(12)
    if (verbose){
      cat("Player now at position 12:", as.character(gameboard$title[12]), "\n")
    }
  }
  if (draw == 4){
    if (player$pos == 8){
      player$go_2_space_n(13)
      if (verbose){
        cat("Player now at position 13:", as.character(gameboard$title[13]), "\n")
      }
    }
    if (player$pos == 23){
      player$go_2_space_n(29)
      if (verbose){
        cat("Player now at position 29:", as.character(gameboard$title[29]), "\n")
      }
    }
    if (player$pos == 37){
      player$go_2_space_n(5)
      if (verbose){
        cat("Player now at position 5:", as.character(gameboard$title[5]), "\n")
      }
    }
  }
  if (draw == 5){
    if (player$pos == 8){
      player$go_2_space_n(16)
      if (verbose){
        cat("Player now at position 16:", as.character(gameboard$title[16]), "\n")
      }
    }
    if (player$pos == 23){
      player$go_2_space_n(26)
      if (verbose){
        cat("Player now at position 26:", as.character(gameboard$title[26]), "\n")
      }
    }
    if (player$pos == 37){
      player$go_2_space_n(6)
      if (verbose){
        cat("Player now at position 6:", as.character(gameboard$title[6]), "\n")
      }
    }
  }
  if (draw == 6){
    player$go_2_space_n(6)
    if (verbose){
      cat("Player now at position 6:", as.character(gameboard$title[6]), "\n")
    }
  }
  if (draw == 7){
    player$go_2_space_n(40)
    if (verbose){
      cat("Player now at position 40:", as.character(gameboard$title[40]), "\n")
    }
  }
  if (draw == 8){
    player$go_2_jail()
    if (verbose){
      cat("Player now in Jail.\n")
    }
  }
  if (draw == 9){
    player$move_n(-3)
    if (verbose){
      cat("Player now at position", player$pos, as.character(gameboard$title[player$pos]), "\n")
    }
  }
}

#Community Chest Cards
drawcommunity <- function(player, tracking, verbose=FALSE){
  draw <- sample(1:16, 1)
  if (verbose){
    cat("Drew Community card", draw, "-", as.character(communitydeck$card[draw]),'\n')
  }
  #tally at chance if you draw a card that moves you
  if (draw %in% 1:2){
    tracking$increase_count(player$pos)
    if (verbose){
      cat("Tally at", player$pos, as.character(gameboard$title[player$pos]), "\n")
    }
  }
  if (draw == 1){
    player$go_2_space_n(1)
    if (verbose){
      cat("Player now at position 1:", as.character(gameboard$title[1]), "\n") #Advance to 'Go'
    }
  }
  if (draw == 2){
    player$go_2_jail()
    if (verbose){
      cat("Player now in Jail.\n")#Go to Jail
    }
  }
}
# Space Tracking Reference Class ------------------------------------------
# a *basic* reference class to keep track of where people landed
tracking <- setRefClass("tracking",
                        fields = list(
                          tally = "numeric",
                          verbose = "logical"
                        ),
                        methods = list(
                          increase_count = function(n){
                            tally[n] <<- tally[n] + 1
                            if(verbose){
                              cat("Tally at", n , ":", as.character(gameboard$title[n]), "\n")
                            }
                          }
                        )
)
# Taking a turn -----------------------------------------------------------
# THIS IS THE MAIN FUNCTION!!
taketurn <- function(player, tracking, verbose=FALSE){
  roll <- dice()
  #keep track of whether or not player will roll again
  go_again <- roll$doubles
  #get out of jail if a double is rolled
  if (roll$doubles & player$jail){
    player$reset_jail()
    player$move_n(roll$movement)
    player$reset_double()
    go_again <- FALSE
  } else if (roll$doubles & !player$jail){
    player$increase_double()
    #if player rolled 3 doubles in a row, go to jail
    if (player$rolldouble == 3){
      player$go_2_jail()
      player$reset_double()
      go_again <- FALSE
    } else {
      player$move_n(roll$movement)
    }
  } else if (!roll$doubles & player$jail){
    #get out of jail if 3rd turn in jail
    if (player$jail == 3){
      player$move_n(roll$movement)
      player$reset_jail()
    } else {
      player$increase_jail()
    }
  } else if (!roll$doubles & !player$jail){
    #normal scenario
    player$move_n(roll$movement)
    player$reset_double()
  }
  #draw chance card
  if (player$pos %in% c(8,23,37)){
    drawchance(player, tracking, verbose)
    if (player$jail){
      go_again <- FALSE
    }
  }
  #draw community card
  if (player$pos %in% c(3,18,34)){
    drawcommunity(player, tracking, verbose)
    if (player$jail){
      go_again <- FALSE
    }
  }
  #if player isn't on "go to jail" spot
  if (player$pos != 31){
    #tally up
    tracking$increase_count(player$pos)
    #if player rolled a double, go again
    if (go_again){
      taketurn(player, tracking)
    }
  } else {
    #go to jail and tally up
    player$go_2_jail()
    tracking$increase_count(player$pos)
  }
}
set.seed(1)
space_tracking <- tracking$new(tally = rep(0,40), verbose = FALSE)
for(i in 1:1000){ # simulate 100 games
  #cat("#### NEW GAME",i,"##### \n")
  player1 <- player$new(pos = 1, jail = 0, rolldouble = 0, verbose = FALSE)  # create new players
  player2 <- player$new(pos = 1, jail = 0, rolldouble = 0, verbose = FALSE)
  player3 <- player$new(pos = 1, jail = 0, rolldouble = 0, verbose = FALSE)
  player4 <- player$new(pos = 1, jail = 0, rolldouble = 0, verbose = FALSE)
  for(i in 1:150){ # 150 turns for each game
    if(player1$verbose) cat("Player 1 turn\n")
    taketurn(player1, space_tracking)  
    if(player2$verbose) cat("Player 2 turn\n")
    taketurn(player2, space_tracking)
    if(player3$verbose) cat("Player 3 turn\n")
    taketurn(player3, space_tracking) 
    if(player4$verbose) cat("Player 4 turn\n")
    taketurn(player4, space_tracking) 
  }
}
cat("1000 Games Ran \n")
# the results after 100 turns. No rules have been implemented
results <- cbind(gameboard, tally = space_tracking$tally)
results <- cbind(results, rel = results$tally/sum(results$tally))
print(results)
sum(results$tally)
arrange(results, desc(rel))
