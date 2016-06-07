;--------------------------------------------------;
;                   pong.asm                       ;
;        A simple Pong game for the NES            ;
;--------------------------------------------------;


;--------------------------------------------------;
;                   iNES Headers                   ;
;--------------------------------------------------;
  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;--------------------------------------------------;
;                    Constants                     ;
;--------------------------------------------------;
TRUE          = $01
FALSE         = $00
BUTTON_A      = %10000000
BUTTON_B      = %01000000
BUTTON_SELECT = %00100000
BUTTON_START  = %00010000
BUTTON_UP     = %00001000
BUTTON_DOWN   = %00000100
BUTTON_LEFT   = %00000010
BUTTON_RIGHT  = %00000001
SPRITE_SIZE   = $08

SPRITE_LOCATION   = $0200
GRAPHICS_LOCATION = $A000  

;Game-specific constants
PADDLE1X      = $0B
PADDLE2X      = $ED
RIGHTWALL     = $F8
LEFTWALL      = $01
BOTTOMWALL    = $C8
TOPWALL       = $38
BLANK_TILE_BG = $24

;Game States
STATE_LOADTITLE     = $00
STATE_TITLE         = $01
STATE_LOADPLAYING   = $02
STATE_PLAYING       = $03
STATE_P1PAUSE       = $04
STATE_P2PAUSE       = $05
STATE_LOADGAMEOVER  = $06
STATE_GAMEOVER      = $07

;--------------------------------------------------;
;                    Variables                     ;
;--------------------------------------------------;
  .rsset $0000
ballSpeedY .rs 1
timer2 .rs 1
debug .rs 2
tempByte .rs 1
tempWord .rs 2
gamestate .rs 1
buttonsP1 .rs 2 ;first byte is the current state, 2nd byte is the previous state
buttonsP2 .rs 2
buttonsP2Prev .rs 1
myWord .rs 2
myDec .rs 5
player1ScoreDec .rs 5
player2ScoreDec .rs 5
timerDec .rs 5
timer1 .rs 1
tempCarry .rs 1
player1Score .rs 2
player2Score .rs 2
backgroundNeedsRedrawn .rs 1
scoresNeedRedrawn .rs 1
timerNeedsRedrawn .rs 1
ballX .rs 1
ballY .rs 1
ballGoingDown .rs 1
ballGoingRight .rs 1
ballGoingLeft .rs 1
ballGoingUp .rs 1
ballSpeedX .rs 1
paddle1YBot .rs 1
paddle1YTop .rs 1
paddle1Speed .rs 1
paddle1Size .rs 1
paddle2YTop .rs 1
paddle2YBot .rs 1
paddle2Speed .rs 1
paddle2Size .rs 1
ballInMotion .rs 1
ballInP1Control .rs 1
ballInP2Control .rs 1
timesUp .rs 1
counterWord .rs 2
gamePausedP1 .rs 1
gamePausedP2 .rs 1

;--------------------------------------------------;
;                     Macros                       ;
;--------------------------------------------------;

;Zero flag clear if button is currently held down
Test_Button .macro ;usage: Test_Button buttonsP1, BUTTON_RIGHT.  BEQ if not pressed.
  LDA \1
  AND #\2
  .endm

;Zero flag clear if button has just been pressed
Test_Button_Pressed .macro 
;usage: 
;Test_Button_Pressed ButtonsP1, BUTTON_RIGHT
  LDA \1+1
  AND #\2
  BNE .Test_Button_Pressed_PrevWasPressed 
  LDA \1 
  AND #\2
  JMP .Test_Button_Pressed_Done
.Test_Button_Pressed_PrevWasPressed
  LDX #$FF
  INX       ;force Zero flag to be set before returning
.Test_Button_Pressed_Done
  .endm

;Zero flag clear if button has just been released (prev = 1, curr = 0)
Test_Button_Released .macro
  LDA \1
  AND #\2
  BNE .Test_Button_Released_CurrWasReleased 
  LDA \1+1
  AND #\2
  JMP .Test_Button_Released_Done
.Test_Button_Released_CurrWasReleased
  LDX #$FF
  INX       ;force Zero flag to be set before returning
.Test_Button_Released_Done
  .endm

;--------------------------------------------------;
;                     Bank 0                       ;
;--------------------------------------------------;
  .bank 0
  .org $8000

;--------------------------------------------------;
;                     RESET                        ;
;--------------------------------------------------;
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  JSR vblankwait
clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA SPRITE_LOCATION, x
  INX
  BNE clrmem
  JSR vblankwait

  ;Initialize variables
  LDA #FALSE
  STA backgroundNeedsRedrawn

  LDA #STATE_LOADTITLE
  STA gamestate

  ;Initialize graphics
  JSR LoadPalette
  JSR LoadSprites
  JSR LoadNametable
  JSR LoadAttribute

  ;Initialize PPU Settings (enable Sprites and background)
  LDA #%10010000   ; enable NMI, Sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable Sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop
 
;--------------------------------------------------;
;                      NMI                         ;
;--------------------------------------------------;
NMI:
  ;Transfer sprite data to PPU using DMA
  LDA #low(SPRITE_LOCATION)
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #high(SPRITE_LOCATION)
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  ;Check flag to redraw background 
  LDA backgroundNeedsRedrawn
  CMP #TRUE
  BNE .continue
  JSR RedrawBackgroundTiles 
  LDA #FALSE
  STA backgroundNeedsRedrawn
.continue

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, Sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable Sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  JSR ReadControllerState

GameEngine:
  LDA gamestate         ;Load the current game state
  ASL A             ;x2 to index into the table correctly (<128 game states)
  TAY
  LDA GameEnginePointerTable, Y     ;Load the address from the table
  STA tempWord
  LDA GameEnginePointerTable+1, Y
  STA tempWord+1
  JMP [tempWord]        ;Jump to the proper engine state
GameEngineDone:

  JSR UpdateSprites
  RTI             ; return from interrupt
 
GameEnginePointerTable:
  .dw EngineLoadTitle     ;One for each game state
  .dw EngineTitle
  .dw EngineLoadPlaying
  .dw EnginePlaying
  .dw EngineP1Pause
  .dw EngineP2Pause
  .dw EngineLoadGameOver
  .dw EngineGameOver

;--------------------------------------------------;
;               Game Engine Blocks                 ;
;--------------------------------------------------;
EngineLoadTitle:
  
  JSR InitializeGameVariables

  LDA #%00000000        ;Turn the screen off
  STA $2000
  STA $2001
  JSR LoadNametable     ;Load the new Nametable, Attribute, and Palette
  JSR LoadAttribute
  JSR LoadPalette
  JSR LoadSprites
  LDA #%10001000        ;Turn the screen on
  STA $2000

  JSR HideAllSprites

  LDA #STATE_TITLE
  STA gamestate


  JMP GameEngineDone
EngineTitle:
  JSR CheckStart
  JMP GameEngineDone
EngineLoadPlaying:
  
  ;Redraw screen
  LDA #%00000000        ;Turn the screen off
  STA $2000
  STA $2001
  JSR LoadNametable     ;Load the new Nametable, Attribute, and Palette
  JSR LoadAttribute
  JSR LoadPalette
  JSR LoadSprites
  LDA #%10001000        ;Turn the screen on
  STA $2000
  LDA #STATE_PLAYING    ;Set the new gamestate
  STA gamestate

  LDA timer2
  STA myWord
  LDA #$00
  STA myWord+1
  JSR HexWordToUnsignedDecimal
  LDA myDec+3
  STA timerDec
  LDA myDec+4
  STA timerDec+1

  LDA #TRUE
  STA backgroundNeedsRedrawn
  STA timerNeedsRedrawn

  JMP GameEngineDone

EnginePlaying:
  ;reset redraw flags
  LDA #FALSE
  STA backgroundNeedsRedrawn
  STA scoresNeedRedrawn
  STA timerNeedsRedrawn
  JSR UpdateTimer
  JSR CheckPaddle1Collision
  JSR CheckPaddle2Collision
  JSR Player1ReleaseBall
  JSR Player2ReleaseBall
  JSR MoveBallUp
  JSR MoveBallDown
  JSR MoveBallLeft
  JSR MoveBallRight
  JSR MovePaddle1Up
  JSR MovePaddle2Up
  JSR MovePaddle1Down
  JSR MovePaddle2Down
  JSR CheckP1Pause
  JSR CheckP2Pause
  JSR CheckTimeUp

  JMP GameEngineDone

EngineP1Pause:
  JSR CheckP1Unpause 
  JMP GameEngineDone

EngineP2Pause:
  JSR CheckP2Unpause
  JMP GameEngineDone

EngineLoadGameOver:
  LDA #%00000000        ;Turn the screen off
  STA $2000
  STA $2001
  JSR LoadNametable     ;Load the new Nametable, Attribute, and Palette
  JSR LoadAttribute
  JSR LoadPalette
  JSR LoadSprites
  LDA #%10001000        ;Turn the screen on
  STA $2000
  JSR HideAllSprites
  LDA #TRUE
  STA backgroundNeedsRedrawn
  LDA #STATE_GAMEOVER    ;Set the new gamestate
  STA gamestate

  JMP GameEngineDone

EngineGameOver:
  JSR CheckReturnToTitle
  JMP GameEngineDone

;--------------------------------------------------;
;                  Subroutines                     ;
;--------------------------------------------------;
UpdateSprites:
  
  ;If we're in EngineLoadTitle, EngineTitle, EngineLoadGameOver, or EngineGameOver, don't update paddle/ball sprites:
  LDA gamestate
  CMP #STATE_LOADTITLE
  BEQ .UpdateGameComponentsDone
  CMP #STATE_TITLE
  BEQ .UpdateGameComponentsDone
  CMP #STATE_LOADGAMEOVER
  BEQ .UpdateGameComponentsDone
  CMP #STATE_GAMEOVER
  BEQ .UpdateGameComponentsDone

  ;Update ball coordinates
  LDA ballY
  STA SPRITE_LOCATION + (Sprite_Ball - Sprites)
  LDA ballX
  STA SPRITE_LOCATION + (Sprite_Ball - Sprites) + 3

  ;Update p1 paddle coordinates (only need to change Y currently)
  LDA paddle1YTop
  LDX paddle1Size
  LDY #$00

.loop1
  CPX #$00
  BEQ .done1 
  STA SPRITE_LOCATION + (Sprite_Paddle1 - Sprites), Y
  CLC
  ADC #$08
  INY
  INY
  INY
  INY
  DEX
  JMP .loop1
.done1 

  ;Update p2 paddle coordinates (only need to change Y currently)
  LDA paddle2YTop
  LDX paddle2Size
  LDY #$00
.loop2
  CPX #$00
  BEQ .done2
  STA SPRITE_LOCATION + (Sprite_Paddle2 - Sprites), Y
  CLC
  ADC #$08
  INY
  INY
  INY
  INY
  DEX
  JMP .loop2
.done2
.UpdateGameComponentsDone
  
  RTS

RedrawBackgroundTiles:
  ;Draw winner at GameOver, no other redraws 
  LDA gamestate
  CMP #STATE_GAMEOVER
  BNE .gameoverChecked
  LDA player1Score
  CMP player2Score
  BEQ .tie 
  BCC .p2Win 
  BCS .p1Win 
.tie 
  JSR DrawTie 
  RTS
.p1Win 
  JSR DrawP1Winner
  RTS
.p2Win 
  JSR DrawP2Winner
  RTS
.gameoverChecked

  ;Non-gameover redraws
  LDA scoresNeedRedrawn   ;Check if scores need updated
  CMP #TRUE
  BNE .scoresChecked
  JSR DrawP1Score
  JSR DrawP2Score
.scoresChecked
  LDA timerNeedsRedrawn
  CMP #TRUE
  BNE .timerChecked
  JSR DrawTimer
.timerChecked
  LDA gamestate ;check if paused 
  CMP #STATE_P1PAUSE
  BNE .p1PauseChecked
  JSR DrawP1Pause
  JMP .pauseChecked
.p1PauseChecked
  LDA gamestate
  CMP #STATE_P2PAUSE
  BNE .p2PauseChecked
  JSR DrawP2Pause
  JMP .pauseChecked
.p2PauseChecked
  ;not paused, draw blank tiles
  JSR RemovePause
.pauseChecked
  ;Additional background redraws

  RTS

InitializeGameVariables:
;Initialize variables
  LDA #$00
  STA player1Score
  STA player2Score 
  STA player1ScoreDec
  STA player1ScoreDec+1
  STA player1ScoreDec+2
  STA player1ScoreDec+3
  STA player1ScoreDec+4
  STA player2ScoreDec
  STA player2ScoreDec+1
  STA player2ScoreDec+2
  STA player2ScoreDec+3
  STA player2ScoreDec+4

  STA timerDec
  STA timerDec+1

  LDA #$01
  STA ballSpeedY
  LDA #$01
  STA ballGoingDown
  STA ballGoingRight
  LDA #$00
  STA ballGoingUp
  STA ballGoingLeft
  LDA #$03
  STA paddle1Speed
  STA paddle2Speed
  LDA #$02
  STA ballSpeedX
  LDA #$70
  STA paddle1YTop
  LDA #$70 + 8 * 4
  STA paddle1YBot
  LDA #$70
  STA paddle2YTop
  LDA #$70 + 8 * 4
  STA paddle2YBot
  LDA #$05
  STA paddle1Size
  STA paddle2Size
  LDA #FALSE
  STA ballInMotion
  LDA #TRUE
  STA ballInP1Control
  LDA paddle1YTop
  CLC
  ADC #SPRITE_SIZE * 2
  TAY
  LDA #PADDLE1X
  CLC
  ADC #SPRITE_SIZE
  TAX
  STY ballY
  STX ballX
  LDA #99
  STA timer2
  LDA #$00
  STA timer1
  RTS

CheckReturnToTitle:
  Test_Button_Pressed buttonsP1, BUTTON_START 
  BEQ .CheckReturnToTitleDone
  LDA #STATE_LOADTITLE
  STA gamestate
  LDA #$AA 
  STA debug
.CheckReturnToTitleDone
  RTS

CheckTimeUp:
  LDA timer2
  CMP #$00
  BNE .CheckWinDone
  LDA #STATE_LOADGAMEOVER
  STA gamestate
.CheckWinDone
  RTS

CheckP2Unpause:
  Test_Button_Pressed buttonsP2, BUTTON_START
  BEQ .checkP1UnpauseDone
  LDA #STATE_PLAYING
  STA gamestate
  LDA #TRUE
  STA backgroundNeedsRedrawn
.checkP1UnpauseDone
  RTS

CheckP1Unpause:
  Test_Button_Pressed buttonsP1, BUTTON_START
  BEQ .checkP1UnpauseDone
  LDA #STATE_PLAYING
  STA gamestate
  LDA #TRUE
  STA backgroundNeedsRedrawn
.checkP1UnpauseDone
  RTS

CheckP1Pause:
  Test_Button_Pressed buttonsP1, BUTTON_START
  BEQ .checkP1PauseDone
  LDA #STATE_P1PAUSE
  STA gamestate
  LDA #TRUE
  STA backgroundNeedsRedrawn
.checkP1PauseDone
  RTS

CheckP2Pause:
  Test_Button_Pressed buttonsP2, BUTTON_START
  BEQ .checkP2PauseDone
  LDA #STATE_P2PAUSE
  STA gamestate
  LDA #TRUE
  STA backgroundNeedsRedrawn
.checkP2PauseDone
  RTS

DrawP1Winner:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (GameOverWinnerTiles - Nametable_GameOver))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (GameOverWinnerTiles - Nametable_GameOver))
  STA $2006
  LDA #$19
  STA $2007
  LDA #$15
  STA $2007
  LDA #$0A
  STA $2007
  LDA #$22
  STA $2007
  LDA #$0E
  STA $2007
  LDA #$1B
  STA $2007
  LDA #$24
  STA $2007
  LDA #$01
  STA $2007
  LDA #$24
  STA $2007
  LDA #$20
  STA $2007
  LDA #$12
  STA $2007
  LDA #$17
  STA $2007
  LDA #$1C
  STA $2007
  LDA #$2B
  STA $2007
  RTS

DrawP2Winner:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (GameOverWinnerTiles - Nametable_GameOver))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (GameOverWinnerTiles - Nametable_GameOver))
  STA $2006
  LDA #$19
  STA $2007
  LDA #$15
  STA $2007
  LDA #$0A
  STA $2007
  LDA #$22
  STA $2007
  LDA #$0E
  STA $2007
  LDA #$1B
  STA $2007
  LDA #$24
  STA $2007
  LDA #$02
  STA $2007
  LDA #$24
  STA $2007
  LDA #$20
  STA $2007
  LDA #$12
  STA $2007
  LDA #$17
  STA $2007
  LDA #$1C
  STA $2007
  LDA #$2B
  STA $2007
  RTS

DrawTie:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (GameOverWinnerTiles - Nametable_GameOver))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (GameOverWinnerTiles - Nametable_GameOver))
  STA $2006
  LDA #$24
  STA $2007
  LDA #$24
  STA $2007
  LDA #$12
  STA $2007
  LDA #$1D
  STA $2007
  LDA #$FA
  STA $2007
  LDA #$1C
  STA $2007
  LDA #$24
  STA $2007
  LDA #$0A
  STA $2007
  LDA #$24
  STA $2007
  LDA #$1D
  STA $2007
  LDA #$12
  STA $2007
  LDA #$0E
  STA $2007
  LDA #$2B
  STA $2007
  LDA #$24
  STA $2007
  RTS

DrawP1Pause:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (PauseTiles - Nametable_Playing))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (PauseTiles - Nametable_Playing))
  STA $2006
  LDA #$19
  STA $2007
  LDA #$01
  STA $2007
  LDA #$24
  STA $2007
  LDA #$19
  STA $2007
  LDA #$0A
  STA $2007
  LDA #$1E
  STA $2007
  LDA #$1C
  STA $2007
  LDA #$0E
  STA $2007
  RTS

DrawP2Pause:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (PauseTiles - Nametable_Playing))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (PauseTiles - Nametable_Playing))
  STA $2006
  LDA #$19
  STA $2007
  LDA #$02
  STA $2007
  LDA #$24
  STA $2007
  LDA #$19
  STA $2007
  LDA #$0A
  STA $2007
  LDA #$1E
  STA $2007
  LDA #$1C
  STA $2007
  LDA #$0E
  STA $2007
  RTS

RemovePause:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (PauseTiles - Nametable_Playing))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (PauseTiles - Nametable_Playing))
  STA $2006
  LDA #$24
  STA $2007
  STA $2007
  STA $2007
  STA $2007
  STA $2007
  STA $2007
  STA $2007
  STA $2007
  RTS

CheckStart:
  Test_Button_Pressed buttonsP1, BUTTON_START
  BEQ .CheckStartDone
  LDA #STATE_LOADPLAYING
  STA gamestate
.CheckStartDone
  RTS

UpdateTimer:
  LDA ballInMotion
  CMP #TRUE
  BNE .UpdateTimerDone

  LDA timer1
  CLC
  ADC #$01
  STA timer1
  CMP #$3F
  BEQ .decrementTimer
  JMP .UpdateTimerDone
.decrementTimer
  LDA #TRUE
  STA timerNeedsRedrawn
  STA backgroundNeedsRedrawn
  LDA #$00
  STA timer1
  LDA timer2
  SEC
  SBC #$01
  STA timer2

  STA myWord
  LDA #$00
  STA myWord+1
  JSR HexWordToUnsignedDecimal
  LDA myDec+3
  STA timerDec
  LDA myDec+4
  STA timerDec+1

  LDA timer2
  CMP #$00
  BNE .UpdateTimerDone
.UpdateTimerDone
  RTS

CheckPaddle1Collision: ;checks for collision coming from the right only
  LDA ballX 
  CMP #PADDLE1X + SPRITE_SIZE
  BCS .CheckPaddle1CollisionDone ;if ballX > paddle1 X, there's no collision with the paddle
  LDA ballY                     ;otherwise, check if it's actually on the paddle
  CMP paddle1YTop               ;if ballY < paddle1Ytop, it's above the paddle, so no collision
  BCC .CheckPaddle1CollisionDone
  CMP paddle1YBot               ;if ballY > paddle1Ybot, it's below the paddle, so no collision
  BCS .CheckPaddle1CollisionDone
  LDA #FALSE                      ;if we get here, it was a collision, so we switch direction
  STA ballGoingLeft
  LDA #TRUE
  STA ballGoingRight
.CheckPaddle1CollisionDone
  RTS

CheckPaddle2Collision:  ;checks for collision coming from the left only
  LDA ballX
  CMP #PADDLE2X - SPRITE_SIZE
  BCC .CheckPaddle2CollisionDone ;if ballX < paddle2 X, there's no collision with the paddle
  LDA ballY
  CMP paddle2YTop
  BCC .CheckPaddle2CollisionDone ;if ballY < paddle2YTop, it's above the paddle, so no collision
  CMP paddle2YBot             
  BCS .CheckPaddle2CollisionDone ;if ballY > paddle2YBot, it's below the paddle, so no collision
  LDA #FALSE
  STA ballGoingRight
  LDA #TRUE
  STA ballGoingLeft
.CheckPaddle2CollisionDone
  RTS

Player1ReleaseBall:
  Test_Button buttonsP1, BUTTON_A 
  BEQ .Player1ReleaseBallDone
  LDA ballInMotion
  CMP #FALSE
  BNE .Player1ReleaseBallDone
  LDA ballInP1Control
  CMP #TRUE
  BNE .Player1ReleaseBallDone
  LDA #TRUE
  STA ballGoingRight
  LDA #FALSE
  STA ballGoingLeft
  LDA #TRUE
  STA ballInMotion
.Player1ReleaseBallDone
  RTS

Player2ReleaseBall:
  Test_Button buttonsP2, BUTTON_A 
  BEQ .Player2ReleaseBallDone
  LDA ballInMotion
  CMP #FALSE
  BNE .Player2ReleaseBallDone
  LDA ballInP2Control
  CMP #TRUE
  BNE .Player2ReleaseBallDone
  LDA #TRUE
  STA ballGoingLeft
  LDA #FALSE
  STA ballGoingRight
  LDA #TRUE
  STA ballInMotion
.Player2ReleaseBallDone
  RTS


;First check for normal ball movement, bounds on wall
MoveBallRight:
  LDA ballInMotion
  CMP #FALSE
  BEQ .MoveBallRightDone
  LDA ballGoingRight
  BEQ .MoveBallRightDone 
  LDA ballX ;Add to the ball's X coord
  CLC
  ADC ballSpeedX
  BCS .hitTheWall ;make sure it didn't go to the left side of the screen
  ;if it didn't, now we can check for collision with right wall
  STA ballX
  CMP #RIGHTWALL
  BCC .MoveBallRightDone ;if it's still less than the right wall, we're done 
.hitTheWall ;otherwise, we hit the wall, let's change direction

  ;Reposition ball (give to P1)
  LDA paddle1YTop
  CLC
  ADC #SPRITE_SIZE * 2
  TAY
  LDA #PADDLE1X
  CLC
  ADC #SPRITE_SIZE
  TAX
  STY ballY
  STX ballX

  ;Set movement flags
  LDA #FALSE
  STA ballInMotion
  STA ballInP2Control
  LDA #TRUE
  STA ballInP1Control

  LDX #$0A ; 1000 points for ball
  JSR AddToP1Score

  ;Convert to decimal
  LDA player1Score
  STA myWord
  LDA player1Score+1
  STA myWord+1

  JSR HexWordToUnsignedDecimal

  LDA myDec
  STA player1ScoreDec
  LDA myDec+1
  STA player1ScoreDec+1
  LDA myDec+2
  STA player1ScoreDec+2
  LDA myDec+3
  STA player1ScoreDec+3
  LDA myDec+4
  STA player1ScoreDec+4

  LDA #TRUE
  STA backgroundNeedsRedrawn
  STA scoresNeedRedrawn
.MoveBallRightDone  
  RTS

MoveBallLeft:
  LDA ballInMotion
  CMP #FALSE
  BEQ .MoveBallLeftDone
  LDA ballGoingLeft
  BEQ .MoveBallLeftDone
  LDA ballX
  SEC 
  SBC ballSpeedX
  BCC .hitTheWall
  STA ballX
  CMP #LEFTWALL
  BCS .MoveBallLeftDone
.hitTheWall

  ;Reposition ball (give to P2)
  LDA paddle2YTop
  CLC
  ADC #SPRITE_SIZE * 2
  TAY
  LDA #PADDLE2X
  SEC
  SBC #SPRITE_SIZE
  TAX
  STY ballY
  STX ballX

  ;Set movement flags
  LDA #FALSE
  STA ballInMotion
  STA ballInP1Control
  LDA #TRUE
  STA ballInP2Control

  LDX #$0A ; 1000 points for ball
  JSR AddToP2Score

  ;Convert to decimal
  LDA player2Score
  STA myWord
  LDA player2Score+1
  STA myWord+1

  JSR HexWordToUnsignedDecimal

  LDA myDec
  STA player2ScoreDec
  LDA myDec+1
  STA player2ScoreDec+1
  LDA myDec+2
  STA player2ScoreDec+2
  LDA myDec+3
  STA player2ScoreDec+3
  LDA myDec+4
  STA player2ScoreDec+4


  LDA #TRUE
  STA backgroundNeedsRedrawn
  STA scoresNeedRedrawn

  ;award p2 points
.MoveBallLeftDone
  RTS

MoveBallUp:
  LDA ballInMotion
  CMP #FALSE
  BEQ .MoveBallUpDone
  LDA ballGoingUp
  BEQ .MoveBallUpDone

  LDA ballY
  SEC 
  SBC ballSpeedY
  BCC .hitTheCeiling
  STA ballY
  CMP #TOPWALL  ;if ballY > top wall, nothing changes
  BCS .MoveBallUpDone

.hitTheCeiling
  LDA #TOPWALL
  STA ballY
  LDA #TRUE    ;otherwise, reverse vertical direction
  STA ballGoingDown
  LDA #FALSE
  STA ballGoingUp
.MoveBallUpDone
  RTS

MoveBallDown:
  LDA ballInMotion
  CMP #FALSE
  BEQ .MoveBallDownDone
  LDA ballGoingDown
  BEQ .MoveBallDownDone

  LDA ballY
  CLC
  ADC ballSpeedY
  BCS .hitTheFloor

  STA ballY
  CMP #BOTTOMWALL ;if bally < bottom wall, nothing changes
  BCC .MoveBallDownDone

.hitTheFloor
  LDA #BOTTOMWALL
  STA ballY
  LDA #TRUE ;otherwise, reverse vertical direction
  STA ballGoingUp
  LDA #FALSE
  STA ballGoingDown
.MoveBallDownDone
  RTS

;Handle moving paddles
MovePaddle1Up:
  Test_Button buttonsP1, BUTTON_UP ;sets Z flag accordingly
  BEQ .MovePaddle1UpDone ;if button not pressed, skip section

  LDA paddle1YTop
  CMP #TOPWALL ;if paddle top <= topwall, can't move it up
  BEQ .moveBall
  BCC .readjust

  ;subtract from the paddle's y coord (top and bottom) to move it up
  LDA paddle1YTop
  SEC
  SBC paddle1Speed
  STA paddle1YTop
  LDA paddle1YBot
  SEC
  SBC paddle1Speed
  STA paddle1YBot
  LDA paddle1YTop
  CMP #TOPWALL
  BEQ .moveBall
  BCC .readjust
  JMP .moveBall
.readjust ;paddletop was < topwall, so move it back, first find the offset (topwall - paddletop)
  LDA #TOPWALL
  SEC 
  SBC paddle1YTop
  STA tempByte ;tempByte is now the offset, now add it to paddle1 top and bottom
  LDA paddle1YTop
  CLC
  ADC tempByte
  STA paddle1YTop
  LDA paddle1YBot
  CLC
  ADC tempByte
  STA paddle1YBot

  ;move the ball sprite if it's held by paddle 1
.moveBall
  LDA ballInMotion
  CMP #FALSE
  BNE .MovePaddle1UpDone
  LDA ballInP1Control
  CMP #TRUE
  BNE .MovePaddle1UpDone
  LDA paddle1YTop
  CLC
  ADC #SPRITE_SIZE * 2
  TAY
  LDA #PADDLE1X
  CLC
  ADC #SPRITE_SIZE
  TAX
  STY ballY
  STX ballX
.MovePaddle1UpDone
  RTS

MovePaddle1Down:
  Test_Button buttonsP1, BUTTON_DOWN
  BEQ .MovePaddle1DownDone ;if button wasn't pressed, skip
  LDA paddle1YBot
  CMP #BOTTOMWALL ;if paddle bottom >= bottom wall, we can't move it down
  BCS .readjust

  ;add to paddle's y coord 
  LDA paddle1YTop
  CLC
  ADC paddle1Speed
  STA paddle1YTop
  LDA paddle1YBot
  CLC
  ADC paddle1Speed
  STA paddle1YBot
  CMP #BOTTOMWALL
  BCS .readjust
  JMP .moveBall
.readjust ;paddle bottom > bottom wall, so move it back.  But by how much?
  LDA paddle1YBot
  SEC
  SBC #BOTTOMWALL
  STA tempByte      ;tempByte = paddle1YBot - BOTTOMWALL
  LDA paddle1YBot
  SEC
  SBC tempByte
  STA paddle1YBot
  LDA paddle1YTop
  SEC
  SBC tempByte
  STA paddle1YTop

  ;move the ball sprite if it's held by paddle 1
.moveBall
  LDA ballInMotion
  CMP #FALSE
  BNE .MovePaddle1DownDone
  LDA ballInP1Control
  CMP #TRUE
  BNE .MovePaddle1DownDone
  LDA paddle1YTop
  CLC
  ADC #SPRITE_SIZE * 2
  TAY
  LDA #PADDLE1X
  CLC
  ADC #SPRITE_SIZE
  TAX
  STY ballY
  STX ballX
.MovePaddle1DownDone
  RTS

MovePaddle2Up:
  Test_Button buttonsP2, BUTTON_UP
  BEQ .MovePaddle2UpDone ;if button not pressed, skip section
  LDA paddle2YTop
  CMP #TOPWALL ;if paddle top <= topwall, we can't move it up
  BEQ .moveBall
  BCC .readjust

  ;subtract from the paddle's y coord (top and bottom) to move it up
  LDA paddle2YTop
  SEC
  SBC paddle2Speed
  STA paddle2YTop
  LDA paddle2YBot
  SEC
  SBC paddle2Speed
  STA paddle2YBot
  LDA paddle2YTop
  CMP #TOPWALL
  BEQ .moveBall
  BCC .readjust
  JMP .moveBall
.readjust ;paddletop was < topwall, so move it back, first find the offset (topwall - paddletop)
  LDA #TOPWALL
  SEC 
  SBC paddle2YTop
  STA tempByte ;tempByte is now the offset, now add it to paddle1 top and bottom
  LDA paddle2YTop
  CLC
  ADC tempByte
  STA paddle2YTop
  LDA paddle2YBot
  CLC
  ADC tempByte
  STA paddle2YBot

;move the ball sprite if it's held by paddle 2
.moveBall
  LDA ballInMotion
  CMP #FALSE
  BNE .MovePaddle2UpDone
  LDA ballInP2Control
  CMP #TRUE
  BNE .MovePaddle2UpDone
  LDA paddle2YTop
  CLC
  ADC #SPRITE_SIZE * 2
  TAY
  LDA #PADDLE2X
  SEC
  SBC #SPRITE_SIZE
  TAX
  STY ballY
  STX ballX
.MovePaddle2UpDone
  RTS

MovePaddle2Down:
  Test_Button buttonsP2, BUTTON_DOWN
  BEQ .MovePaddle2DownDone ;if button wasn't pressed, skip
  LDA paddle2YBot
  CMP #BOTTOMWALL ;if paddle bottom >= bottom wall, we can't move it down
  BCS .readjust

  ;add to paddle's y coord 
  LDA paddle2YTop
  CLC
  ADC paddle2Speed
  STA paddle2YTop
  LDA paddle2YBot
  CLC
  ADC paddle2Speed
  STA paddle2YBot
  CMP #BOTTOMWALL
  BCS .readjust
  JMP .moveBall
.readjust ;paddle bottom > bottom wall, so move it back.  But by how much?
  LDA paddle2YBot
  SEC
  SBC #BOTTOMWALL
  STA tempByte      ;tempByte = paddle1YBot - BOTTOMWALL
  LDA paddle2YBot
  SEC
  SBC tempByte
  STA paddle2YBot
  LDA paddle2YTop
  SEC
  SBC tempByte
  STA paddle2YTop

;move the ball sprite if it's held by paddle 2
.moveBall
  LDA ballInMotion
  CMP #FALSE
  BNE .MovePaddle2DownDone
  LDA ballInP2Control
  CMP #TRUE
  BNE .MovePaddle2DownDone
  LDA paddle2YTop
  CLC
  ADC #SPRITE_SIZE * 2
  TAY
  LDA #PADDLE2X
  SEC
  SBC #SPRITE_SIZE
  TAX
  STY ballY
  STX ballX
.MovePaddle2DownDone
  RTS


;Add the byte in X register to p1Score
AddToP1Score:
  STX tempByte
  LDA player1Score
  CLC
  ADC tempByte
  STA player1Score
  BCC .done
  LDA player1Score+1
  CLC
  ADC #$01
  STA player1Score+1
.done
  LDA player1Score+1
  CMP #$27
  BCS .checkLower
  JMP .notMax
.checkLower
  LDA player1Score
  CMP #$0F
  BCS .setMax
  JMP .notMax
.setMax
  LDA #$0F
  STA player1Score
  LDA #$27
  STA player1Score+1
.notMax
  RTS

;Add the byte in X register to p2Score
AddToP2Score:
  STX tempByte
  LDA player2Score
  CLC
  ADC tempByte
  STA player2Score
  BCC .done
  LDA player2Score+1
  CLC
  ADC #$01
  STA player2Score+1
.done
  LDA player2Score+1
  CMP #$27
  BCS .checkLower
  JMP .notMax
.checkLower
  LDA player2Score
  CMP #$0F
  BCS .setMax
  JMP .notMax
.setMax
  LDA #$0F
  STA player2Score
  LDA #$27
  STA player2Score+1
.notMax
  RTS

DrawTimer:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (TimerTiles - Nametable_Playing))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (TimerTiles - Nametable_Playing)) 
  STA $2006

  LDA timerDec
  STA $2007
  LDA timerDec+1
  STA $2007


DrawP1Score:
  ;Draw player1Score
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (Player1ScoreTiles - Nametable_Playing))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (Player1ScoreTiles - Nametable_Playing))
  STA $2006

  LDA player1ScoreDec
  STA myDec
  LDA player1ScoreDec+1
  STA myDec+1
  LDA player1ScoreDec+2
  STA myDec+2
  LDA player1ScoreDec+3
  STA myDec+3
  LDA player1ScoreDec+4
  STA myDec+4

;replace leading 0's with blank spaces
  LDX #$00
.loop
  LDA myDec, X
  CMP #$00
  BNE .loopDone
  LDA #BLANK_TILE_BG
  STA myDec, X
  INX
  CPX #$05
  BNE .loop
.loopDone

  LDA myDec+1
  STA $2007
  LDA myDec+2
  STA $2007
  LDA myDec+3
  STA $2007
  LDA player1ScoreDec+4 ;always taking the last to get 3 0s
  STA $2007

  RTS

DrawP2Score:
  LDA $2002
  LDA #high(GRAPHICS_LOCATION + (Player2ScoreTiles - Nametable_Playing))
  STA $2006
  LDA #low(GRAPHICS_LOCATION + (Player2ScoreTiles - Nametable_Playing))
  STA $2006

  LDA player2ScoreDec
  STA myDec
  LDA player2ScoreDec+1
  STA myDec+1
  LDA player2ScoreDec+2
  STA myDec+2
  LDA player2ScoreDec+3
  STA myDec+3
  LDA player2ScoreDec+4
  STA myDec+4

;replace leading 0's with blank spaces
  LDX #$00
.loop
  LDA myDec, X
  CMP #$00
  BNE .loopDone
  LDA #BLANK_TILE_BG
  STA myDec, X
  INX
  CPX #$05
  BNE .loop
.loopDone

  LDA myDec+1
  STA $2007
  LDA myDec+2
  STA $2007
  LDA myDec+3
  STA $2007
  LDA player2ScoreDec+4 ;taking the last for three 0's
  STA $2007

  RTS


;--------------------------------------------------;
;                Common Routines                   ;
;--------------------------------------------------;
vblankwait:
  BIT $2002
  BPL vblankwait
  RTS

;Load the 0th palette according to the palettePtr
LoadPalette:
  LDA $2002               ;read PPU status to reset the high/low latch 
  LDA #$3F 
  STA $2006             
  LDA #$00 
  STA $2006             

;Set the palette
  LDA gamestate           
  ASL A                   ;multiply by 2 because the address is 2 bytes
  TAY
  LDA PalettePointerTable, Y    
  STA tempWord
  LDA PalettePointerTable+1, Y  
  STA tempWord+1  
  
;Load the palette
  LDY #$00         
.loadPaletteLoop: 
  LDA [tempWord], Y    
  STA $2007               ;write to PPU 
  INY
  CPY #$20               
  BNE .loadPaletteLoop 
  RTS

LoadSprites:
  LDX #$00              ; start at 0
.loop
  LDA Sprites, x        ; load data from address (Sprites +  x)
  STA SPRITE_LOCATION, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #(Sprites_end - Sprites)              ; load all of the Sprites
  BNE .loop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
  RTS

HideAllSprites: ;move their vertical attribute to FE?
  LDA #(Sprites_end - Sprites)
  LSR A
  LSR A ;divide by 4
  STA tempByte ;tempByte = number of sprites 

  LDX #$00
  LDY #$00
  LDA #$FE
.loop
  CPX tempByte
  BEQ .done
  STA SPRITE_LOCATION, Y
  INY
  INY
  INY
  INY
  INX
  JMP .loop
.done


  RTS


;Load the 0th Nametable according to the nametablePtr
LoadNametable:
  LDA $2002     ;read PPU status to reset the high/low latch
  LDA #$20
  STA $2006
  LDA #$00
  STA $2006

;Set the nametable
  LDA gamestate
  ASL A
  TAY
  LDA NametablePointerTable, Y
  STA tempWord
  LDA NametablePointerTable+1, Y
  STA tempWord+1

  LDX #$04
  LDY #$00

;Load the nametable (change this, attribute is being set unnecessarily)
.loadNametableLoop:
  LDA [tempWord], Y              ;load nametable
  STA $2007                      ;draw tile
  INY                            
  BNE .loadNametableLoop
  INC tempWord+1
  DEX
  BNE .loadNametableLoop
  RTS

;Load the Attribute according to attributePtr
LoadAttribute:
  LDA $2002     ;read PPU status to reset the high/low latch
  LDA #$23 
  STA $2006
  LDA #$C0 
  STA $2006   ;put nametable (960 bytes) at 2000 in loadbackground
          ;put 64 bytes of attribute table at 2000 + 3C0 = 23C0

  LDA gamestate
  ASL A
  TAY
  LDA AttributePointerTable, Y
  STA tempWord
  LDA AttributePointerTable+1, Y
  STA tempWord+1

  LDY #$00 
.loadAttributeLoop:  ;
  LDA [tempWord], y
  STA $2007 
  INY
  CPY #$40 
  BNE .loadAttributeLoop
  RTS


LatchControllers:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons
  RTS

ReadControllerState:
  JSR LatchControllers

  LDA buttonsP1
  STA buttonsP1+1
  ;Read Controller 1
  LDX #$08
.loop1
  LDA $4016
  LSR A
  ROL buttonsP1
  DEX
  BNE .loop1

  LDA buttonsP2
  STA buttonsP2+1
  ;Read Controller 2
  LDX #$08
.loop2
  LDA $4017
  LSR A
  ROL buttonsP2
  DEX
  BNE .loop2
  RTS


;converts the myWord variable into decimal, stores into myDec variable
;myWord is 2 bytes, little endian
;myDec has 5 bytes, one for each digit, big endian
HexWordToUnsignedDecimal: 
  LDX #FALSE      ;init carry to false
  STX tempCarry
  LDA myWord+1    ;Take the highest order hex digit (i.e. A in #$ABCD)
  AND #$F0        ;Mask the lower nybble and shift right 4 times
  LSR A
  LSR A
  LSR A
  LSR A
  STA tempByte    ;Store the value in tempByte - this is used as the table index

;Find the correct index into the FortyNinetySixDigit table
  LDA #$00        
  LDX #$00        
  CPX tempByte    ;If the hex digit is 0, we're done.  Otherwise iterate.
  BEQ .findFortyNinetySixIndexDone
.findFortyNinetySixIndex
  CLC
  ADC #$05
  INX
  CPX tempByte
  BNE .findFortyNinetySixIndex
.findFortyNinetySixIndexDone
  TAY             ;Now Y contains the index into the table.

;Initialize return value digits with FortyNinetySixDigit table values
  LDA FortyNinetySixDigit, Y  ;ones digit from table
  STA myDec+4                 ;store in the rightmost byte
  INY
  LDA FortyNinetySixDigit, Y  ;tens digit
  STA myDec+3
  INY
  LDA FortyNinetySixDigit, Y  ;hundreds digit
  STA myDec+2
  INY
  LDA FortyNinetySixDigit, Y  ;thousands digit
  STA myDec+1
  INY
  LDA FortyNinetySixDigit, Y  ;ten thousands digit
  STA myDec

  LDA myWord+1        ;Take the 2nd-highest order hex digit (i.e. B in #$ABCD)
  AND #$0F            ;Mask the upper nybble
  STA tempByte

;Find the correct index into the TwoFiftySixDigit table
  LDX #$00
  LDA #$00
  CPX tempByte
  BEQ .findTwoFiftySixIndexDone
.findTwoFiftySixIndex
  CLC
  ADC #$05
  INX
  CPX tempByte
  BNE .findTwoFiftySixIndex
.findTwoFiftySixIndexDone
  TAY

;Recalculate.  Add these digits to the ones previously calculated.
;Recalculate ones digit
  LDA TwoFiftySixDigit, Y   ;Load the ones digit from the 256 table
  CLC
  ADC tempCarry             ;Add carry (this could be removed)
  ADC myDec+4               ;Add the previous decimal digit value
  CMP #$0A                  ;If it's < 10, we don't need to carry
  BCC .onesNoCarry256
  LDX #TRUE                 ;Otherwise set carry to true
  STX tempCarry
  SEC
  SBC #$0A                  ;And subtract 10 from this digit
  JMP .onesDone256 
.onesNoCarry256
  LDX #FALSE
  STX tempCarry
.onesDone256
  STA myDec+4

  INY
;Recalculate tens digit
  LDA TwoFiftySixDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+3
  CMP #$0A
  BCC .tensNoCarry256
  LDX #TRUE
  STX tempCarry
  SEC
  SBC #$0A
  JMP .tensDone256
.tensNoCarry256
  LDX #FALSE
  STX tempCarry
.tensDone256
  STA myDec+3

  INY
;Recalculate hundreds digit
  LDA TwoFiftySixDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+2
  CMP #$0A
  BCC .hundredsNoCarry256
  LDX #TRUE
  STX tempCarry
  SBC #$0A
  JMP .hundredsDone256
.hundredsNoCarry256
  LDX #FALSE
  STX tempCarry
.hundredsDone256
  STA myDec+2

  INY
;Recalculate thousands
  LDA TwoFiftySixDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+1
  CMP #$0A
  BCC .thousandsNoCarry256
  LDX #TRUE
  STX tempCarry
  SBC #$0A
  JMP .thousandsDone256
.thousandsNoCarry256
  LDX #FALSE
  STX tempCarry
.thousandsDone256
  STA myDec+1

  INY
;Recalculate tenthousands
  LDA TwoFiftySixDigit, Y
  CLC
  ADC tempCarry
  ADC myDec
  CMP #$0A                      ;This should never happen, so...
  BCC .tenthousandsNoCarry256   ;The check to set carry here could be removed
  LDX #TRUE                     ;And instead we could always set it to FALSE
  STX tempCarry
  SBC #$0A
  JMP .tenthousandsDone256
.tenthousandsNoCarry256
  LDX #FALSE
  STX tempCarry
.tenthousandsDone256
  STA myDec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  LDA myWord        ;Take the 2nd-lowest order hex digit (i.e. C in #$ABCD)
  AND #$F0
  LSR A
  LSR A
  LSR A
  LSR A
  STA tempByte

;Find the correct index into the SixteensDigit table
  LDX #$00
  LDA #$00
  CPX tempByte
  BEQ .findSixteensIndexDone
.findSixteensIndex 
  CLC
  ADC #$05
  INX
  CPX tempByte
  BNE .findSixteensIndex
.findSixteensIndexDone
  TAY

;Recalculate.  Add these digits to the ones previously calculated.
;Recalculate ones digit
  LDX #FALSE
  STX tempCarry

  LDA SixteensDigit, Y
  CLC
  ADC tempCarry ; should be 0 though
  ADC myDec+4
  CMP #$0A
  BCC .onesNoCarry16
  LDX #TRUE
  STX tempCarry
  SEC
  SBC #$0A
  JMP .onesDone16
.onesNoCarry16
  LDX #FALSE
  STX tempCarry
.onesDone16
  STA myDec+4

  INY
;Recalculate tens digit
  LDA SixteensDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+3
  CMP #$0A
  BCC .tensNoCarry16
  LDX #TRUE
  STX tempCarry
  SEC
  SBC #$0A
  JMP .tensDone16
.tensNoCarry16
  LDX #FALSE
  STX tempCarry
.tensDone16
  STA myDec+3

  INY
;Recalculate hundreds digit
  LDA SixteensDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+2
  CMP #$0A
  BCC .hundredsNoCarry16
  LDX #TRUE
  STX tempCarry
  SBC #$0A
  JMP .hundredsDone16
.hundredsNoCarry16
  LDX #FALSE
  STX tempCarry
.hundredsDone16
  STA myDec+2

  INY
;Recalculate thousands
  LDA SixteensDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+1
  CMP #$0A
  BCC .thousandsNoCarry16
  LDX #TRUE
  STX tempCarry
  SBC #$0A
  JMP .thousandsDone16
.thousandsNoCarry16
  LDX #FALSE
  STX tempCarry
.thousandsDone16
  STA myDec+1

  INY
;Recalculate tenthousands
  LDA SixteensDigit, Y
  CLC
  ADC tempCarry
  ADC myDec
  CMP #$0A
  BCC .tenthousandsNoCarry16    ;Similar situation here, should never happen
  LDX #TRUE                     ;So we could always set it to FALSE
  STX tempCarry
  SBC #$0A
  JMP .tenthousandsDone16
.tenthousandsNoCarry16
  LDX #FALSE
  STX tempCarry
.tenthousandsDone16
  STA myDec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Take the lowest order hex digit (i.e. D in #$ABCD)
  LDA myWord
  AND #$0F
  STA tempByte

;Find the correct index into the OnesDigit table
  LDX #$00
  LDA #$00
  CPX tempByte
  BEQ .findOnesIndexDone
.findOnesIndex 
  CLC
  ADC #$05
  INX
  CPX tempByte
  BNE .findOnesIndex
.findOnesIndexDone
  TAY

;Recalculate.  Add these digits to the ones previously calculated.
;Recalculate ones digit
  LDX #FALSE
  STX tempCarry

  LDA OnesDigit, Y
  CLC
  ADC tempCarry ; should be 0 though
  ADC myDec+4
  CMP #$0A
  BCC .onesNoCarry1
  LDX #TRUE
  STX tempCarry
  SEC
  SBC #$0A
  JMP .onesDone1
.onesNoCarry1
  LDX #FALSE
  STX tempCarry
.onesDone1
  STA myDec+4

  INY
;Recalculate tens digit
  LDA OnesDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+3
  CMP #$0A
  BCC .tensNoCarry1
  LDX #TRUE
  STX tempCarry
  SEC
  SBC #$0A
  JMP .tensDone1
.tensNoCarry1
  LDX #FALSE
  STX tempCarry
.tensDone1
  STA myDec+3

  INY
;Recalculate hundreds digit
  LDA OnesDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+2
  CMP #$0A
  BCC .hundredsNoCarry1
  LDX #TRUE
  STX tempCarry
  SBC #$0A
  JMP .hundredsDone1
.hundredsNoCarry1
  LDX #FALSE
  STX tempCarry
.hundredsDone1
  STA myDec+2

  INY
;Recalculate thousands
  LDA OnesDigit, Y
  CLC
  ADC tempCarry
  ADC myDec+1
  CMP #$0A
  BCC .thousandsNoCarry1
  LDX #TRUE
  STX tempCarry
  SBC #$0A
  JMP .thousandsDone1
.thousandsNoCarry1
  LDX #FALSE
  STX tempCarry
.thousandsDone1
  STA myDec+1

  INY
;Recalculate tenthousands
  LDA OnesDigit, Y
  CLC
  ADC tempCarry
  ADC myDec
  CMP #$0A                    ;Could completely ignore the carry here
  BCC .tenthousandsNoCarry1   ;It won't happen and won't be used again.
  LDX #TRUE
  STX tempCarry
  SBC #$0A                      
  JMP .tenthousandsDone1
.tenthousandsNoCarry1
  LDX #FALSE
  STX tempCarry
.tenthousandsDone1
  STA myDec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  RTS

OnesDigit:
  ;ones, tens, hundreds, thousands, tenthousands
  .db $00,$00,$00,$00,$00 ;#$0000 = 0
  .db $01,$00,$00,$00,$00 ;#$0001 = 1
  .db $02,$00,$00,$00,$00 ;#$0002 = 2
  .db $03,$00,$00,$00,$00 ;#$0003 = 3
  .db $04,$00,$00,$00,$00 ;#$0004 = 4
  .db $05,$00,$00,$00,$00 ;#$0005 = 5
  .db $06,$00,$00,$00,$00 ;#$0006 = 6
  .db $07,$00,$00,$00,$00 ;#$0007 = 7
  .db $08,$00,$00,$00,$00 ;#$0008 = 8
  .db $09,$00,$00,$00,$00 ;#$0009 = 9
  .db $00,$01,$00,$00,$00 ;#$000A = 10
  .db $01,$01,$00,$00,$00 ;#$000B = 11
  .db $02,$01,$00,$00,$00 ;#$000C = 12
  .db $03,$01,$00,$00,$00 ;#$000D = 13
  .db $04,$01,$00,$00,$00 ;#$000E = 14
  .db $05,$01,$00,$00,$00 ;#$000F = 15

SixteensDigit:
  ;ones, tens, hundreds, thousands, tenthousands
  .db $00,$00,$00,$00,$00 ;#$0000 = 0
  .db $06,$01,$00,$00,$00 ;#$0010 = 16
  .db $02,$03,$00,$00,$00 ;#$0020 = 32
  .db $08,$04,$00,$00,$00 ;#$0030 = 48
  .db $04,$06,$00,$00,$00 ;#$0040 = 64
  .db $00,$08,$00,$00,$00 ;#$0050 = 80
  .db $06,$09,$00,$00,$00 ;#$0060 = 96
  .db $02,$01,$01,$00,$00 ;#$0070 = 112
  .db $08,$02,$01,$00,$00 ;#$0080 = 128
  .db $04,$04,$01,$00,$00 ;#$0090 = 144
  .db $00,$06,$01,$00,$00 ;#$00A0 = 160
  .db $06,$07,$01,$00,$00 ;#$00B0 = 176
  .db $02,$09,$01,$00,$00 ;#$00C0 = 192
  .db $08,$00,$02,$00,$00 ;#$00D0 = 208
  .db $04,$02,$02,$00,$00 ;#$00E0 = 224
  .db $00,$04,$02,$00,$00 ;#$00F0 = 240

TwoFiftySixDigit:
  ;ones, tens, hundreds, thousands, tenthousands
  .db $00,$00,$00,$00,$00 ;#$0000 = 0
  .db $06,$05,$02,$00,$00 ;#$0100 = 256
  .db $02,$01,$05,$00,$00 ;#$0200 = 512
  .db $08,$06,$07,$00,$00 ;#$0300 = 768
  .db $04,$02,$00,$01,$00 ;#$0400 = 1024
  .db $00,$08,$02,$01,$00 ;#$0500 = 1280
  .db $06,$03,$05,$01,$00 ;#$0600 = 1536
  .db $02,$09,$07,$01,$00 ;#$0700 = 1792
  .db $08,$04,$00,$02,$00 ;#$0800 = 2048
  .db $04,$00,$03,$02,$00 ;#$0900 = 2304
  .db $00,$06,$05,$02,$00 ;#$0A00 = 2560
  .db $06,$01,$08,$02,$00 ;#$0B00 = 2816
  .db $02,$07,$00,$03,$00 ;#$0C00 = 3072
  .db $08,$02,$03,$03,$00 ;#$0D00 = 3328
  .db $04,$08,$05,$03,$00 ;#$0E00 = 3584
  .db $00,$04,$08,$03,$00 ;#$0F00 = 3840

FortyNinetySixDigit:
  ;ones, tens, hundreds, thousands, tenthousands
  .db $00,$00,$00,$00,$00 ;#$0000 = 0
  .db $06,$09,$00,$04,$00 ;#$1000 = 4096
  .db $02,$09,$01,$08,$00 ;#$2000 = 8192
  .db $08,$08,$02,$02,$01 ;#$3000 = 12288
  .db $04,$08,$03,$06,$01 ;#$4000 = 16384
  .db $00,$08,$04,$00,$02 ;#$5000 = 20480
  .db $06,$07,$05,$04,$02 ;#$6000 = 24576
  .db $02,$07,$06,$08,$02 ;#$7000 = 28672
  .db $08,$06,$07,$02,$03 ;#$8000 = 32768
  .db $04,$06,$08,$06,$03 ;#$9000 = 36864
  .db $00,$06,$09,$00,$04 ;#$A000 = 40960
  .db $06,$05,$00,$05,$04 ;#$B000 = 45056
  .db $02,$05,$01,$09,$04 ;#$C000 = 49152
  .db $08,$04,$02,$03,$05 ;#$D000 = 53248
  .db $04,$04,$03,$07,$05 ;#$E000 = 57344
  .db $00,$04,$04,$01,$06 ;#$F000 = 61440


;--------------------------------------------------;
;                     Bank 1                       ;
;--------------------------------------------------;
  .bank 1
  .org GRAPHICS_LOCATION

;--------------------------------------------------;
;                   Nametables                     ;
;--------------------------------------------------;
NametablePointerTable:
  .dw Nametable_Title     ;STATE_LOADTITLE
  .dw Nametable_Title     ;STATE_TITLE
  .dw Nametable_Playing   ;STATE_LOADPLAYING
  .dw Nametable_Playing   ;STATE_PLAYING
  .dw Nametable_Playing   ;STATE_P1PAUSE
  .dw Nametable_Playing   ;STATE_P2PAUSE
  .dw Nametable_GameOver  ;STATE_LOADGAMEOVER
  .dw Nametable_GameOver  ;STATE_GAMEOVER
  ;...

Nametable_Title:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$45,$45,$45,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$45,$24,$24,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$45,$24,$24,$45,$24,$45,$45,$45,$45,$24,$24,$45,$45,$45,$24,$45,$45,$45,$24,$45,$45,$45,$45,$45,$24,$24,$24,$24
  .db $24,$24,$24,$24,$45,$24,$24,$45,$24,$45,$24,$45,$45,$24,$24,$45,$45,$45,$24,$24,$45,$24,$24,$45,$24,$24,$24,$45,$24,$24,$24,$24
  .db $24,$24,$24,$24,$45,$45,$45,$45,$24,$45,$24,$24,$45,$24,$24,$45,$24,$45,$24,$24,$45,$24,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$45,$24,$24,$24,$24,$45,$24,$24,$45,$24,$24,$45,$24,$45,$45,$24,$45,$24,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$45,$24,$24,$24,$24,$45,$24,$24,$45,$24,$24,$45,$24,$24,$45,$24,$45,$24,$45,$24,$24,$24,$45,$45,$45,$24,$24,$24
  .db $24,$24,$24,$24,$45,$24,$24,$24,$24,$45,$24,$24,$45,$24,$24,$45,$24,$24,$45,$45,$45,$24,$45,$45,$24,$24,$24,$45,$24,$24,$24,$24
  .db $24,$24,$24,$45,$45,$45,$24,$24,$24,$45,$45,$45,$45,$24,$45,$45,$45,$24,$45,$45,$45,$24,$24,$45,$45,$45,$45,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$02,$00,$01,$06,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$19,$1B,$0E,$1C,$1C,$24,$1C,$1D,$0A,$1B,$1D,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $4B,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$50
  .db $4C,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$51 
  .db $24,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$24 
  .db $24,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$24 

Nametable_Playing: ;32 wide, 30 down
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24
Player1TextTiles: .db $19,$15,$0A,$22,$0E,$1B,$24,$01
  .db $24,$24,$24,$24
TimerTextTiles: .db $1D,$12,$16,$0E
  .db $24,$24,$24,$24
Player2TextTiles: .db $19,$15,$0A,$22,$0E,$1B,$24,$02
  .db $24,$24,$24,$24
Player1ScoreTiles: .db $24,$24,$24,$00,$00,$00
  .db $24,$24,$24,$24,$24,$24,$24
TimerTiles: .db $09,$09 ;80, 81
  .db $24,$24,$24,$24,$24
Player2ScoreTiles: .db $24,$24,$24,$00,$00,$00
  .db $24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $6B,$2C,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6C,$6D,$6E,$6F
  .db $70,$2D,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$71,$72,$73,$74
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
PauseTiles: .db $24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $4B,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$50
  .db $4C,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$4F,$51 
  .db $24,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$24 
  .db $24,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$24 

Nametable_GameOver:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
GameOverTiles: .db $10,$0A,$16,$0E,$24,$24,$18,$1F,$0E,$1B
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24
GameOverWinnerTiles: .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

;--------------------------------------------------;
;                   Attributes                     ;
;--------------------------------------------------;
AttributePointerTable:
  .dw Attribute_Title     ;STATE_LOADTITLE
  .dw Attribute_Title     ;STATE_TITLE
  .dw Attribute_Playing   ;STATE_LOADPLAYING
  .dw Attribute_Playing   ;STATE_LAYING
  .dw Attribute_Playing   ;STATE_P1PAUSE
  .dw Attribute_Playing   ;STATE_P2PAUSE
  .dw Attribute_GameOver   ;STATE_LOADGAMEOVER
  .dw Attribute_GameOver   ;STATE_GAMEOVER
  ;...

Attribute_Title:
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %11_11_00_00, %11_11_00_00, %11_11_00_00, %11_11_00_00, %11_11_00_00, %11_11_00_00, %11_11_00_00, %11_11_00_00
  .db %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11
  .db %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11, %11_11_11_11
  .db %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10
  .db %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10


Attribute_Playing:
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %01_01_01_01, %01_01_01_01, %01_01_01_01, %01_01_01_01, %01_01_01_01, %01_01_01_01, %01_01_01_01, %01_01_01_01
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %10_10_00_00, %10_10_00_00, %10_10_00_00, %10_10_00_00, %10_10_00_00, %10_10_00_00, %10_10_00_00, %10_10_00_00
  .db %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10, %10_10_10_10

Attribute_GameOver:
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00
  .db %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00, %00_00_00_00

;--------------------------------------------------;
;                    Palettes                      ;
;--------------------------------------------------;
PalettePointerTable:
  .dw Palette_Title     ;STATE_LOADTITLE
  .dw Palette_Title     ;STATE_TITLE
  .dw Palette_Playing   ;STATE_LOADPLAYING
  .dw Palette_Playing   ;STATE_PLAYING
  .dw Palette_Playing   ;STATE_P1PAUSE
  .dw Palette_Playing   ;STATE_P2PAUSE
  .dw Palette_GameOver   ;STATE_LOADGAMEOVER
  .dw Palette_GameOver   ;STATE_GAMEOVER
  ;...

Palette_Title:
  .db $22,$27,$16,$0F,  $22,$27,$16,$0F,  $11,$29,$17,$0F,  $30,$30,$21,$0F   ;;background palette
  .db $22,$0F,$0F,$17,  $22,$0F,$27,$08,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette
Palette_Playing:
  .db $10,$30,$22,$0F,  $22,$27,$16,$0F,  $11,$29,$17,$0F,  $30,$0F,$21,$30   ;;background palette
  .db $22,$0F,$0F,$17,  $22,$0F,$27,$08,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette
Palette_GameOver:
  .db $22,$30,$16,$0F,  $22,$27,$16,$0F,  $11,$29,$17,$0F,  $30,$30,$21,$0F   ;;background palette
  .db $22,$0F,$0F,$17,  $22,$0F,$27,$08,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette



;--------------------------------------------------;
;                     Sprites                      ;
;--------------------------------------------------;
Sprites:
     ;vert tile attr horiz
Sprite_Paddle1:
  .db $80, $86, $00, PADDLE1X
  .db $88, $86, $00, PADDLE1X
  .db $90, $86, $00, PADDLE1X
  .db $98, $86, $00, PADDLE1X
  .db $A0, $86, $00, PADDLE1X
Sprite_Paddle2:
  .db $80, $86, $00, PADDLE2X
  .db $88, $86, $00, PADDLE2X
  .db $90, $86, $00, PADDLE2X
  .db $98, $86, $00, PADDLE2X
  .db $A0, $86, $00, PADDLE2X
Sprite_Ball:
  .db $60, $75, $01, $60
Sprites_end:


;--------------------------------------------------;
;                    Vectors                       ;
;--------------------------------------------------;
  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw $0000          ;external interrupt IRQ is not used in this tutorial
  
;--------------------------------------------------;
;                     Bank 2                       ;
;--------------------------------------------------;
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1`