	LIST OFF
; Disassembly of C:\Games\Atari\MANUFACTURER\Atari\Haunted House (1982) (Atari).bin
; Disassembled 02/09/22 04:50:23
; Using Stella 6.6
;
; ROM Name : Haunted House (1982) (Atari)
; ROM MD5  : f0a6e99f5875891246c3dbecbf2d2cea
; ROM Type : 4K* (4K) 

	processor 6502
	include "tia_constants.h"
	include "vcs_constants.h"
	

;-----------------------------------------------------------
;      COLOR and GAME Constants
;-----------------------------------------------------------

BLACK            = $00
WHITE            = $0D
ORANGE           = $30
RED              = $40
BLUE             = $80
BLUE_CYAN        = $90
CYAN             = $A0
CYAN_GREEN       = $B0
GREEN            = $C0
GREEN_YELLOW     = $D0
BEIGE            = $F0

;YELLOW           = $10
;BROWN            = $20
;MAUVE            = $50
;VIOLET           = $60
;PURPLE           = $70
;GREEN_BEIGE      = $E0

H_CREATURE       = 10
H_STAIRCASE      = 17
H_FONT           = 8
H_TORCH          = 28
H_EYES           = 3

ID_GHOST         = 0
ID_BAT           = 1
ID_SPIDER        = 2

TORCH_DURATION   = 20

XMIN             = 0
XMAX             = 151

PLAYER_INIT_HPOS = 128
PLAYER_INIT_VPOS = 134

DOOR_LEFT_XPOS   = $3F
DOORrIGHT_XPOS  = $4F

;---AUDCx CONSTANTS

THUNKSOUND      =	2
NOISY3SOUND	=	3
URNCENTERSOUND	=	4
LEADSOUND		=	5
BASSSOUND		=	6
NOISESOUND		=	8
FOOTSOUND       =	11 
TWILIGHTSOUND	=	12

	LIST ON

;-----------------------------------------------------------
;      RIOT RAM (zero-page) labels
;-----------------------------------------------------------








; Zero-page variables (clarifying comments only, no renaming)
rollingEyesTimer              = $80    ; Timer for rolling eyes animation
itemGatheredFlag              = $81    ; Flag: item has been gathered
torchesUsed                   = $82    ; Number of torches used
creaturesInRoom               = $83    ; Number of creatures present in room
colorCycTimer                 = $84    ; Timer for color cycling effects
collisionIndex                = $85    ; Index for collision detection
itemLastSeen                  = $86    ; Last item seen by player

secondsCounter                = $87    ; Counts down from 60 every 60th frame (timing)

doorwayCrossingStatus         = $88    ; Status of doorway crossing
frameCount                    = $89    ; Frame counter (increments every frame)
rollingEyesTimer              = $80    ; Timer for rolling eyes animation
itemGatheredFlag              = $81    ; Flag: item has been gathered
torchesUsed                   = $82    ; Number of torches used
creaturesInRoom               = $83    ; Number of creatures present in room
colorCycTimer                 = $84    ; Timer for color cycling effects
collisionIndex                = $85    ; Index for collision detection
itemLastSeen                  = $86    ; Last item seen by player
secondsCounter                = $87    ; Counts down from 60 every 60th frame (timing)
doorwayCrossingStatus         = $88    ; Status of doorway crossing
frameCount                    = $89    ; Frame counter (increments every frame)
torchAnimationIdx             = $8a    ; Index for torch animation
pfScrollOffsetA               = $8b    ; Playfield scroll offset A
pfScrollOffsetB               = $8c    ; Playfield scroll offset B
playerCurrentFloor            = $8d    ; Player's current floor
movementValue                 = $8e    ; Value representing player movement
audioFrequency0Value          = $8f    ; Audio frequency value (channel 0)
audioVolume1Value             = $90    ; Audio volume value (channel 1)
doorwayMovementFlag           = $91    ; Flag for doorway movement
audioWindSoundBit             = $92    ; Bit for wind sound effect
audioSoundIndex               = $93    ; Index for audio sound
creaturesPresentMask          = $94    ; Bitmask for creatures present
creatureProcessMask           = $95    ; Bitmask for creatures being processed
playerLives                   = $96    ; Player's remaining lives
playerDoorCrossing            = $97    ; Player's doorway crossing status
windSoundCounter              = $98    ; Counter for wind sound effect
gameState                     = $99    ; Current game state
itemBeingCarried              = $9a    ; Item currently being carried by player
roomStairsStatus              = $9b    ; Bitfield: status of stairs in current room
playerCurrentRoom             = $9c    ; Player's current room number
urnAssembly0                  = $9d    ; Urn assembly progress (equals 8 when complete)
urnAssembly1                  = $9e    ; Urn assembly progress (equals $ff when complete)
urnAssembly2                  = $9f    ; Urn assembly progress (equals $ff when complete)
objFloorLoc                   = $a0    ; Floor location for objects (key, scepter, urn pieces)
masterKeyFloorLocation        = $a0    ; Floor location for master key
randFloorLoc                  = $a5    ; Randomized floor locations for objects
randFloorLoc0                 = $a5    ; Random floor location 0
randFloorLoc1                 = $a6    ; Random floor location 1
playerPosX                    = $aa    ; Player horizontal position
objPosX                       = $ab    ; Horizontal position for objects (key, scepter, urn pieces)
masterKeyPosX                 = $ab    ; Horizontal position for master key
randPosX                      = $b0    ; Randomized horizontal positions for objects
randPosX1                     = $b1    ; Random horizontal position 1
playerScrollY                 = $b5    ; Player vertical scroll position
objPosY                       = $b6    ; Vertical position for objects (key, scepter, urn pieces)
masterKeyPosY                 = $b6    ; Vertical position for master key
randPosY                      = $bb    ; Randomized vertical positions for objects
randPosY1                     = $bc    ; Random vertical position 1
objectRoomLocations           = $c0    ; Room location for objects (key, scepter, urn pieces)
randomRoomLocations           = $c5    ; Randomized room locations for objects
creatureCollisionFlag          = $ca    ; Set when player collides with a creature
playerAbsPosY                 = $cb    ; Player absolute vertical position
gameSelection                 = $cc    ; Game selection (0-8 for games 1-9)
scanline                      = $cd    ; Current scanline
tmpBallHorizPosition            = $cf    ; Temporary ball horizontal position
temporaryOne                    = $d0    ; Temporary variable (multi-use)
tmpColorTableIndex              = $d0    ; Temporary color table index
tmpRandomValue                  = $d0    ; Temporary random value
colorEOR                        = $d1    ; Color EOR (also temporaryOne + 1)
colorCycleMode                  = $d2    ; Color cycle mode
lightningColorMask              = $d3    ; Lightning color mask
calculatedFineValue             = $d4    ; Calculated fine value
torchesHNumberPTRs              = $d4    ; Torch H number pointers (with $d5)
tmpYRegisterSaveLocation        = $d5    ; Temporary Y register save location
torchesLNumberPTRs              = $d6    ; Torch L number pointers (with $d7)
tmpFloorNumber                  = $d7    ; Temporary floor number
eyeRam                          = $d8    ; Eye RAM (with $d9 and $da)
livesNumberPtrs                 = $d8    ; Lives number pointers (with $d9)
tmpRoomNumber                   = $d8    ; Temporary room number
floorNumberPtrs                 = $da    ; Floor number pointers (with $db)
spriteHorizPositions            = $dc    ; Sprite horizontal positions
p0PosY                        = $dc    ; Player 0 position Y
player1HorizPosition            = $dd    ; Player 1 horizontal position
missile0HorizPosition           = $de    ; Missile 0 horizontal position
torchTimer                      = $df    ; Torch timer (Missile 1 unused)
ballHorizPosition               = $e0    ; Ball horizontal position
sprite0GraphicPtrs               = $e1    ; Sprite 0 graphic pointers (with $e2)
sprite1GraphicPtrs               = $e3    ; Sprite 1 graphic pointers (with $e4)
selectSwitchDebounce            = $e5    ; Debounce for select switch
actionButtonDebounce            = $e6    ; Debounce for action button
playerDeltaX                   = $e7    ; Player horizontal delta (left/right movement)
playerDeltaY                   = $e8    ; Player vertical delta (up/down movement)
itemActionIndex                = $e9    ; Index for item action (urn piece pickup)
numberOfCreatures              = $ea    ; Number of creatures
randomSeed                     = $eb    ; Random seed
randomSeedAlternate            = $ec    ; Alternate random seed
spriteHeight                   = $ed    ; Sprite height
playerVertSize                  = $ee    ; Player vertical size
playerPfScrollValue             = $ef    ; Player playfield scroll value
playerVertOffset                = $f0    ; Player vertical offset
colorTableRam                  = $f1    ; Color table RAM
object1Color                   = $f1    ; Creature color (alternates/flickers if two onscreen)
object2Color                   = $f2    ; Player eye/torch color (alternates as needed)
object3Color                   = $f3    ; Third object color
statusBackgroundColor          = $f4    ; Status background color
wallColor                      = $f5    ; Wall color
stack                          = $f6    ; Stack (and up to $ff)
;======================================

creatureCollisionFlag          = $ca    ; Set when player collides with a creature

playerAbsPosY                 = $cb    ; Player absolute vertical position
gameSelection                 = $cc    ; Game selection (0-8 for games 1-9)
scanline                      = $cd    ; Current scanline

; MANY OF THESE HAVE TEMPORARY USES
;*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*
tmpBallHorizPosition            = $cf    ; Temporary ball horizontal position

temporaryOne                    = $d0    ; Temporary variable (multi-use)
tmpColorTableIndex              = $d0    ; Temporary color table index
tmpRandomValue                  = $d0    ; Temporary random value
colorEOR                        = $d1    ; Color EOR (also temporaryOne + 1)

colorCycleMode                  = $d2    ; Color cycle mode
lightningColorMask              = $d3    ; Lightning color mask

calculatedFineValue             = $d4    ; Calculated fine value
torchesHNumberPTRs              = $d4    ; Torch H number pointers (with $d5)

tmpYRegisterSaveLocation        = $d5    ; Temporary Y register save location

torchesLNumberPTRs              = $d6    ; Torch L number pointers (with $d7)
tmpFloorNumber                  = $d7    ; Temporary floor number

eyeRAM                          = $d8    ; Eye RAM (with $d9 and $da)
livesNumberPTRs                 = $d8    ; Lives number pointers (with $d9)
tmpRoomNumber                   = $d8    ; Temporary room number

floorNumberPTRs                 = $da    ; Floor number pointers (with $db)
;*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*

;======================================
; RAM Table for P0,P1,M0,M1,BL
spriteHorizPositions            = $dc    ; Sprite horizontal positions
p0PosY                        = $dc    ; Player 0 position Y
player1HorizPosition            = $dd    ; Player 1 horizontal position
missile0HorizPosition           = $de    ; Missile 0 horizontal position
torchTimer                      = $df    ; Torch timer (Missile 1 unused)
ballHorizPosition               = $e0    ; Ball horizontal position
;======================================

sprite0GraphicPTRs               = $e1    ; Sprite 0 graphic pointers (with $e2)

sprite1GraphicPTRs               = $e3    ; Sprite 1 graphic pointers (with $e4)

selectSwitchDebounce            = $e5    ; Debounce for select switch
actionButtonDebounce            = $e6    ; Debounce for action button

playerDeltaX                   = $e7    ; Player horizontal delta (left/right movement)
playerDeltaY                   = $e8    ; Player vertical delta (up/down movement)

itemActionIndex                = $e9    ; Index for item action (urn piece pickup)
numberOfCreatures              = $ea    ; Number of creatures

randomSeed                     = $eb    ; Random seed
randomSeedAlternate            = $ec    ; Alternate random seed
spriteHeight                   = $ed    ; Sprite height

playerVertSize                  = $ee    ; Player vertical size
playerPFScrollValue             = $ef    ; Player playfield scroll value
playerVertOffset                = $f0    ; Player vertical offset

colorTableRAM                  = $f1    ; Color table RAM
object1Color                   = $f1    ; Creature color (alternates/flickers if two onscreen)
object2Color                   = $f2    ; Player eye/torch color (alternates as needed)
object3Color                   = $f3    ; Third object color
statusBackgroundColor          = $f4    ; Status background color
wallColor                      = $f5    ; Wall color

STACK                          = $f6    ; Stack (and up to $ff)

;-----------------------------------------------------------
;      User Defined Labels
;-----------------------------------------------------------

start           = $f000

    SEG     CODE
    ORG     $f000

start
    sei                             ; disable interrupts        
    cld                             ; clear decimal mode        
    ldx     #$ff
    txs
    stx     randomSeed
    inx
    stx     gameSelection
    txa
.clearLoop
    sta     VSYNC,x
    inx
    bpl     .clearLoop
    jsr     resetPressed
    jsr     setSelectionVariables

mainGameLoop
    lda     #START_VERT_SYNC | DUMP_PORTS        
    sta     WSYNC
    sta     VBLANK
    sta     VSYNC
    sta     WSYNC
    sta     WSYNC
    lda     #STOP_VERT_SYNC
    sta     WSYNC
    sta     VSYNC
    inc     frameCount              ; increment every frame        
    lda     #(45)
    sta     TIM64T
    jsr     readSwitches            ; read console switches        
    lda     gameState
    and     #$43
    bne     .skipSubroutines
    jsr     handlePlayerMovement
    jsr     checkForWinning
.skipSubroutines
    jsr     updateGameVisualsAndState
.waitTime1  
    lda     INTIM
    bne     .waitTime1
    sta     VBLANK
    lda     #(228)
    sta     TIM64T
    jsr     playfieldKernel
.waitTime2
    lda     INTIM 
    bne     .waitTime2
    sta     WSYNC
;---------------------------------------
    lda     #DUMP_PORTS | DISABLE_TIA
    sta     VBLANK
    lda     #(36)
    sta     TIM64T
    bit     gameState
    bvs     .waitTime3
    jsr     handlePlayerDeath 
    jsr     handleStairs
    jsr     checkP0P1Collision
    jsr     checkDoorwayPassages
.waitTime3
    lda     INTIM 
    bne     .waitTime3
    sta     VBLANK 
    beq     mainGameLoop            ; unconditional

; ******************************************************************

readSwitches SUBROUTINE
    lda     SWCHB                   ; read console switches        
    ror                             ; put reset into carry        
    bcc     resetPressed      
    jmp     resetNotPressed
    
resetPressed
    ldx     randomSeed
    lda     randomSeedAlternate
    sta     randomSeed
    stx     randomSeedAlternate	    ; swap alternate randoms

    ldx     #$80
    lda     #0
.ramInitLoop
    sta     VSYNC,x
    inx
    cpx     #<playerLives           ; clear RAM from $80 - $95        
    bne     .ramInitLoop

    ldx     #$09
.initVariablesLoop
    lda     initialGameVariables,x
    sta     playerLives,x
    dex
    bpl     .initVariablesLoop

    jsr     scatterTheItems
    ldx     #$03
    stx     randFloorLoc0	; master key "always" on floor 4?? (except game 3)
    inx

.setItemLocationsLoop
    lda     randFloorLoc,x
    sta     objFloorLoc,x
    lda     randPosX,x
    sta     objPosX,x
    lda     randPosY,x
    sta     objPosY,x
    dex
    bpl     .setItemLocationsLoop

    ldy     gameSelection
    cpy     #$02
    bne     .notOnGame3
    inx
    stx     masterKeyFloorLocation	; x = 0, put key on player's floor (1)
    ldx     #PLAYER_INIT_HPOS - 12
    stx     masterKeyPosX
    ldx     #PLAYER_INIT_VPOS - 2
    stx     masterKeyPosY		; set master key position for level 3
.notOnGame3
    lda     #MSBL_SIZE8 | PF_PRIORITY | PF_REFLECT        
    sta     CTRLPF
    jsr     initRoom
    ldx     #$04			     ; five creatures on >= level 5
    lda     gameSelection
    cmp     #$04
    bcs     .lessThanGame5
    ldx     #$02			     ; three creatures on <= level 4
.lessThanGame5
    stx     numberOfCreatures
    lda     #PLAYER_INIT_HPOS
    sta     playerPosX
    lda     #PLAYER_INIT_VPOS
    sta     playerScrollY		; set player init pos.
    lda     #$26
    sta     playerAbsPosY
    rts

; ******************************************************************
    
scatterTheItems SUBROUTINE
    jsr     nextRandom			; get a random value
    and     #$07				; make it from 0 to 7
    cmp     #$06				
    bcs     scatterTheItems		; keep value from 0 to 5
    sta     tmpRandomValue			; save in temp.
    ldx     #$04				; loop x from 4 down to 0
.randomizeLoop
    jsr     nextRandom			; get a random value
    and     #$03				; make it from 0 to 3
    sta     randFloorLoc,x	; setup objects on random floor numbers
    tay 
    txa
.valueNotOK
    clc
    adc     tmpRandomValue			; add rnd(0->5) to rnd(0->3) to get rnd(0->8)
    cmp     #$06
    bcc     .lessThanSix
    sbc     #$06				; keep it from 0 to 5
.lessThanSix
    cmp     playerCurrentRoom		; if A different from current room #
    bne     .valueIsOK			;
    cpy     playerCurrentFloor  	; and Y differnt from current floor #
    bne     .valueIsOK			; accept value
    lda     #$05
    bne     .valueNotOK			; otherwise start with 5 and do it again (unconditional)
.valueIsOK
    sta     randomRoomLocations,x 
    sta     objectRoomLocations,x	; setup objects in random rooms
    tay 
    lda     randomLocationsTableH,y
    sta     randPosX,x		; and random horiz positions
    lda     randomLocationsTableV,y
    sta     randPosY,x		; and random vert positions
    dex
    bpl     .randomizeLoop
    rts

; ******************************************************************
    
resetNotPressed SUBROUTINE
    ror
    bcc     .selectPressed
    ldx     #$01
    stx     selectSwitchDebounce
.selectNotPressed
    rts
.selectPressed
    dec     selectSwitchDebounce
    bne     .selectNotPressed
    lda     #$2d
    sta     selectSwitchDebounce
    inc     gameSelection
    lda     gameSelection
    cmp     #$09
    bne     .notGame9
    lda     #$00
.notGame9
    sta     gameSelection
setSelectionVariables
    jsr     unlightTorch
    lda     #$40
    sta     gameState
    lda     #$10
    sta     colorCycTimer
    lda     #0
    sta     collisionIndex
    sta     rollingEyesTimer
    sta     creaturesInRoom
    rts

; ******************************************************************
    
checkForWinning SUBROUTINE
    bit     roomStairsStatus
    bpl     .countDownTorch
    lda     playerCurrentFloor		; check for lowest floor
    bne     .countDownTorch
    lda     #$02
    cmp     playerCurrentRoom		; check for the exit door
    bne     .countDownTorch
    cmp     itemBeingCarried		; check for completed urn
    bne     .justBonk
    lda     urnAssembly0
    cmp     #$08				; 8 means urn complete
    bne     .justBonk 
    lda     #$01
    sta     frameCount
    sta     torchAnimationIdx
    lda     #$44 
    sta     gameState 			; if here, you have won!
    bne     .countDownTorch		; unconditional
.justBonk
    ldx     #THUNKSOUND			; sound #2
    jsr     playSound
.countDownTorch
    dec     secondsCounter
    bne     .dontResetSeconds
    lda     #60					; @ 60 fps, counts seconds for 20 sec. torch timer
    sta     secondsCounter
    dec     torchTimer
.dontResetSeconds
    bit     torchAnimationIdx
    bvc     .doTheFlickering
    lda     creaturesInRoom
    bne     unlightTorch
    lda     torchTimer 
    bne     .doTheFlickering
unlightTorch
    lda     #0
    sta     collisionIndex
    lda     torchAnimationIdx
    and     #$07
    sta     torchAnimationIdx
.doTheFlickering
    lda     torchAnimationIdx
    eor     #$80
    sta     torchAnimationIdx
    rts

; ******************************************************************
    

handlePlayerMovement SUBROUTINE
    jsr     updatePlayerDelta
    lda     #0
    sta     doorwayMovementFlag
    lda     playerScrollY
    cmp     #$26
    bcc     SetPlayerAbsPosY
    cmp     #$d7
    bcs     AdjustPlayerAbsPosY
    lda     #$26
    bne     SetPlayerAbsPosY                   ; unconditional
AdjustPlayerAbsPosY
    sbc     #$af
SetPlayerAbsPosY
    sta     playerAbsPosY
    rts

; ******************************************************************
    

updatePlayerDelta SUBROUTINE
    lda     #0
    sta     playerDeltaX
    sta     playerDeltaY
    lda     roomStairsStatus
    and     #$7f
    sta     roomStairsStatus
    bit     doorwayMovementFlag
    bvs     endPlayerDelta
    bit     movementValue 
    bvs     handleLeftMovement
    bpl     endPlayerDelta
    inc     playerDeltaX
    lda     playerPosX
    cmp     #$94
    beq     handleStairCollision
    jsr     add_8_toHPos
    jmp     afterLeftRight

handleLeftMovement
    dec     playerDeltaX
    lda     playerPosX
    cmp     #$04
    beq     handleStairCollision
    jsr     doPlayerFineCalc
afterLeftRight
    jsr     add_2_toVPos
    bne     playNoisySound                   ; unconditional?
     
    jsr     add_0_toVPos
    bne     playNoisySound                   ; unconditional?
    
    lda     playerPosX
    clc
    adc     playerDeltaX
    sta     playerPosX
    bne     endPlayerDelta                   ; unconditional?
playNoisySound
    ldx     #NOISY3SOUND	    ; sound #3
    jsr     playSound
endPlayerDelta
    bit     doorwayMovementFlag
    bmi     .noMovement
    lda     movementValue
    and     #$30
    beq     .noMovement
    cmp     #$20
    beq     handleUpMovement
    inc     playerDeltaY
    lda     playerScrollY
    cmp     #$fb
    beq     handleStairExit
    jsr     add_7_toHPos
    jsr     add_3_toVPos
    bne     playNoisySound2
    jsr     add_0_toHPos
    jsr     add_3_toVPos
    bne     playNoisySound2
    beq     applyVerticalDelta                   ; unconditional
handleUpMovement
    dec     playerDeltaY
    lda     playerScrollY
    cmp     #$01
    beq     handleStairExit
    jsr     add_7_toHPos
    jsr     adjustNegativeY
    bne     playNoisySound2
    jsr     add_0_toHPos
    jsr     adjustNegativeY
    bne     playNoisySound2
applyVerticalDelta
    lda     playerScrollY 
    clc 
    adc     playerDeltaY
    sta     playerScrollY
    rts
playNoisySound2
    ldx     #NOISY3SOUND	    ; sound #3
    jsr     playSound
.noMovement
    rts

handleStairCollision
    lda     roomStairsStatus
    ora     #$80
    sta     roomStairsStatus
    cmp     #$8f
    beq     playNoisySound
    bne     endPlayerDelta                   ; unconditional

handleStairExit
    lda     roomStairsStatus
    ora     #$80
    sta     roomStairsStatus
    cmp     #$8f
    beq     playNoisySound2
    rts

; ******************************************************************
    
add_7_toHPos
    lda     playerPosX
    clc
    adc     #$07
    bne     .doFineCalculation       ; unconditional?
add_8_toHPos
    lda     playerPosX
    clc
    adc     #$08
    bne     .doFineCalculation       ; unconditional?
add_0_toHPos
    lda     playerPosX
    bne     .doFineCalculation       ; unconditional?

doPlayerFineCalc
    ldy     playerPosX
    dey 
    tya
.doFineCalculation
    lsr
    lsr 
    lsr
    lsr
    tay
    dey
    sty     calculatedFineValue
    rts

; ******************************************************************
    
add_0_toVPos
    lda     playerScrollY
    bne     doorCheck               ; unconditional?
adjustNegativeY
    ldy     playerScrollY
    dey
    tya
    jmp     doorCheck
add_2_toVPos
    lda     playerScrollY
    clc
    adc     #$02
    bne     doorCheck               ; unconditional?
add_3_toVPos
    lda     playerScrollY
    clc
    adc     #$03

doorCheck
    lsr
    lsr
    lsr
    lsr
    tax
    ldy     calculatedFineValue
    tya
    and     #$08
    beq     doorCheckBitmask
    cpx     #$08
    rts
doorCheckBitmask
    lda     wallBitPattern,x
    and     bitmaskThing,y
    rts

; ******************************************************************
    
updateGameVisualsAndState
    lda     #MSBL_SIZE8
    sta     NUSIZ0
    lda     #BLACK
    sta     COLUBK
    sta     object1Color
    sta     ENAM0
    sta     ENABL
    sta     NUSIZ1
    lda     gameState
    cmp     #$02
    beq     skipLogic
    and     #$04
    beq     doRollingEyes
    lda     frameCount
    bne     skipLogic
    jsr     setSelectionVariables
skipLogic
    jmp     readInput
    
doRollingEyes
    lda     rollingEyesTimer
    beq     .rollingDone
    jsr     checkCollisions
    lda     frameCount
    lsr
    and     #$07
    tax
    lda     rollingEyesTable,x
    bne     resetEyeGraphics
.rollingDone
    ldx     itemGatheredFlag
    beq     skipItemDraw
    ldx     itemActionIndex
    lda     #H_FONT
    sta     spriteHeight
    lda     playerCurrentFloor
    sta     objFloorLoc,x
    jsr     drawItem
    bpl     readInput
skipItemDraw
    bit     torchAnimationIdx
    bvc     eyesCommon
    bit     torchAnimationIdx
    bmi     eyesWithTorch
    jsr     checkItemVisibility
    bpl     readInput
eyesWithTorch
    jsr     checkCollisions
    bpl     preparePlayfield
eyesCommon
    jsr     checkCollisions 
readInput
    lda     SWCHA                   ; get joystick value

resetEyeGraphics
    ldy     #%11100111              ; no pupils
    sty     eyeRAM
    sty     eyeRAM + 1
    sty     eyeRAM + 2
    ldy     #%10100101              ; center pupils
    ldx     #$01
    rol
    bcs     setRightEyes
    ldy     #%11000110              ; right pupils
setRightEyes
    rol
    bcs     setLeftEyes
    ldy     #%01100011              ; left pupils
setLeftEyes
    rol
    bcs     adjustEyeX
    dex 
adjustEyeX
    rol
    bcs     storeEyeGraphic
    inx
storeEyeGraphic
    tya
    sta     eyeRAM,x
    lda     #<eyeRAM
    sta     sprite1GraphicPTRs
    lda     #>eyeRAM
    sta     sprite1GraphicPTRs + 1
    lda     #H_EYES
    sta     playerVertSize
    ldy     #$01
    lda     playerAbsPosY
    ldx     playerPosX
    bne     setPlayerVertOffset                   ; unconditional?

preparePlayfield
    lda     tmpBallHorizPosition
    sta     ballHorizPosition
    lda     gameSelection
    beq     .noDoors
    bit     doorwayCrossingStatus
    bmi     .noDoors
    lda     #ENABLE_BM
    sta     ENABL
    sta     ENAM0
.noDoors
    lda     frameCount
    and     #$06
    lsr
    tax					    ; x = torch sprite LSB index
    lda     torchLSBValues,x
    sta     sprite1GraphicPTRs
    lda     #>torchGraphics
    sta     sprite1GraphicPTRs + 1
    lda     playerPosX
    sec
    sbc     #$0d
    bcs     ballLeftBound
    adc     #$a0
    ldx     #XMAX
    bne     setBallPos                   ; unconditional

ballLeftBound
    cmp     #$84
    bcc     setTorchHeight
    ldx     #XMIN
setBallPos
    stx     ballHorizPosition
    ldx     #ENABLE_BM
    stx     ENABL
setTorchHeight
    tax
    lda     #H_TORCH
    sta     playerVertSize
    lda     #QUAD_SIZE
    sta     NUSIZ1
    ldy     #$04
    lda     playerAbsPosY
    sec
    sbc     #$0c
setPlayerVertOffset
    sta     playerVertOffset
    stx     player1HorizPosition
    sty     object2Color
    ldy     #$01
    sty     object3Color
    dey
    sty     statusBackgroundColor   ; y = 0      
    sty     wallColor               ; make walls black        
    lda     #$08
    clc
    adc     playerCurrentFloor
    ldx     gameSelection
    bne     .statusIsColored
    sta     wallColor
    bpl     .wallsAreColored        ; unconditional

.statusIsColored
    sta     statusBackgroundColor
.wallsAreColored
    lda     rollingEyesTimer
    beq     .notRolling
    lda     #$03
    bne     checkLightning                   ; unconditional

.notRolling
    lda     creaturesInRoom
    beq     .skipRandomLightning
    bit     SWCHB
    bvs     .skipRandomLightning
    lda     #$27
checkLightning
    and     frameCount
    bne     .skipRandomLightning
    lda     randomSeed
    ror 
    bcc     .skipRandomLightning
    ldy     #$01
    sty     wallColor               ; random black for lightning flashes?
.skipRandomLightning
    ldx     #$f7
    ldy     #$00				; color palette
    sty     tmpColorTableIndex
    lda     SWCHB
    and     #BW_MASK
    bne     .colorBWSetting 
    lda     #$0c				; BW palette
    sta     tmpColorTableIndex
    ldx     #$07
.colorBWSetting
    stx     lightningColorMask 
    lda     colorCycTimer
    and     #$10
    beq     resetColorCycle
    lda     frameCount
    bne     incColorCycle
    inc     colorCycTimer
incColorCycle
    lda     colorCycTimer
    ora     #$10
    sta     colorCycTimer
    tay 
    bne     setColorMode
resetColorCycle
    ldx     #$ff
setColorMode
    sty     colorEOR
    stx     colorCycleMode
        
    ldx     #$04
.setColorsLoop
    lda     colorTableRAM,x
    clc 
    adc     tmpColorTableIndex		; add palette index
    tay 
    lda     gameColorTable,y
    eor     colorEOR
    and     colorCycleMode
    sta     colorTableRAM,x
    dex
    bpl     .setColorsLoop
    
    lda     object2Color
    sta     COLUP1
    lda     object1Color
    cmp     #CYAN + 10			; flashing color (urn pieces)
    bne     .notUrnPiece
    lda     lightningColorMask
    ora     #BW_MASK
    and     frameCount
.notUrnPiece
    sta     COLUP0
    lda     gameState
    and     #$04
    beq     positionObjects
    lda     frameCount
    and     lightningColorMask
    sta     wallColor

positionObjects
    ldx     #$04
    lda     gameState
    cmp     #$02
    bne     .positionObjectsLoop
    ldx     #$01
.positionObjectsLoop
    lda     #$02
    cpx     #$02
    bcs     .skipDiv2
    lsr
.skipDiv2
    clc
    adc     spriteHorizPositions,x 
    ldy     #$02
    sec 
.remainderLoop
    iny 
    sbc     $0f                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BUG (should be #$0f)     
    bcs     .remainderLoop
    eor     #$ff
    sbc     #$06
    asl
    asl
    asl
    asl
    sta     WSYNC
;---------------------------------------
.positionObjectsDly
    dey                             ;2        
    bpl     .positionObjectsDly     ;2/3      
    sta     RESP0,x                 ;4        
    sta     HMP0,x                  ;4        
    dex                             ;2        
    bpl     .positionObjectsLoop    ;2/3      
    sta     WSYNC                   ;3   =  19
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #$4f                    ;2        
    sta     scanline                ;3        
    sta     lightningColorMask      ;3        
    lda     #0                      ;2        
    sta     calculatedFineValue     ;3        
    bit     torchAnimationIdx       ;3        
    bvc     checkPfScroll                   ;2/3      
    bit     doorwayCrossingStatus   ;3        
    bmi     checkPfScroll                   ;2/3      
    lda     gameSelection           ;3        
    beq     checkPfScroll                   ;2/3      
    lda     pfScrollOffsetB                  ;3        
    beq     checkScrollB                   ;2/3      
    jsr     handlePlayfieldScrolling ;6        
    bmi     checkScrollB                   ;2/3      
    sta     calculatedFineValue     ;3 = 47
checkScrollB
    lda     pfScrollOffsetA                  ;3        
    beq     checkPfScroll                   ;2/3      
    tax                             ;2        
    jsr     handlePlayfieldScrolling ;6        
    cmp     #$50                    ;2        
    bcs     checkPfScroll                   ;2/3      
    sta     lightningColorMask      ;3        
    lda     #$08                    ;2        
    sta     temporaryOne            ;3        
    txa                             ;2        
    bne     calcFineScroll                   ;2/3 = 29
checkPfScroll
    lda     playerScrollY     ;3        playfield gfx scoll calculations
    cmp     #$26                    ;2        
    bcc     setFineScrollHigh                   ;2/3      
    cmp     #$d6                    ;2        
    bcs     setFineScrollLow                   ;2/3      
    adc     #$2a                    ;2        
    sta     temporaryOne            ;3 = 16
calcFineScroll
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    lsr                             ;2        
    eor     #$0f                    ;2        
    sta     temporaryOne + 1        ;3        
    asl                             ;2        
    adc     temporaryOne + 1        ;3        
    tax                             ;2        
    bpl     nextRandom              ;2/3 = 22
setFineScrollHigh
    ldx     #$1e                    ;2        
    lda     #$00                    ;2        
    beq     storeFineScroll                   ;2/3 = 6 unconditional

setFineScrollLow
    ldx     #$00                    ;2        
    lda     #$0f                    ;2 = 4
storeFineScroll
    sta     temporaryOne            ;3 = 3
nextRandom
    lda     randomSeedAlternate     ;3      random generator  
    eor     randomSeed              ;3        
    asl                             ;2        
    asl                             ;2        
    rol     randomSeed              ;5        
    rol     randomSeedAlternate     ;5        
    lda     randomSeed              ;3        
    rts                             ;6 = 29

; ******************************************************************
    
itemIDTable
    .byte   $07,$01,$06,$06,$06    ; key, scepter, urn, urn, urn

; ******************************************************************
    
handlePlayfieldScrolling SUBROUTINE
    clc                             ;2        
    adc     playerAbsPosY        ;3        
    sec                             ;2        
    sbc     playerScrollY     ;3        
    rts                             ;6 = 16

; ******************************************************************
    
setInvItemPTRs SUBROUTINE
    txa                             ;2        
    cpx     #$02                    ;2        
    bcc     .keyOrScepter           ;2/3      
    lda     roomStairsStatus,x      ;4 = 10  (x must = 2,3, or 4) for urn pieces
.keyOrScepter
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #H_FONT                 ;2        
    ldx     #>creatureGraphics      ;2        
    rts                             ;6 = 16

; ******************************************************************
    
checkItemVisibility SUBROUTINE
    ldx     itemLastSeen            ;3 = 3
Lf4f3
    dex                             ;2        
    bpl     Lf505                   ;2/3!     
    ldx     #$04                    ;2        
    bne     Lf505                   ;2/3!= 8 unconditional
Lf4fa
    cpx     itemLastSeen            ;3        
    bne     Lf4f3                   ;2/3 = 5
Lf4fe
    lda     #0                      ;2        
    sta     p0PosY    ;3        
    sta     spriteHeight            ;3        
    rts                             ;6 = 14
Lf505
    cpx     itemBeingCarried        ;3        
    beq     Lf4fa                   ;2/3!     
    cpx     #$02                    ;2        
    bcc     .keyOrScepter           ;2/3      
    lda     roomStairsStatus,x      ;4       (x must = 2,3, or 4) for urn pieces  
    bmi     Lf4fa                   ;2/3!= 15
.keyOrScepter
    lda     gameSelection           ;3        
    cmp     #$02                    ;2        
    bcs     .game3OrHigher          ;2/3      
    cpx     #$00                    ;2        
    beq     Lf4fa                   ;2/3!= 11
.game3OrHigher
    lda     objFloorLoc,x  ;4        
    cmp     playerCurrentFloor      ;3        
    bne     Lf4fa                   ;2/3!     
    jsr     torchLightUpItem        ;6        
    beq     Lf4fa                   ;2/3!= 17
drawItem
    ldy     itemGatheredFlag        ;3        
    bne     .notGathered            ;2/3      
    lda     itemIDTable,x           ;4        
    sta     object1Color            ;3 = 12
.notGathered
    stx     itemLastSeen            ;3        
    jsr     setInvItemPTRs          ;6        
    sta     sprite0GraphicPTRs      ;3        
    stx     sprite0GraphicPTRs + 1  ;3        
    lda     #H_FONT                 ;2        
    sta     spriteHeight            ;3        
    rts                             ;6 = 26

; ******************************************************************
    
torchLightUpItem SUBROUTINE
    lda     playerPosX          ;3        
    sec                             ;2        
    sbc     objPosX,x  ;4        
    bcs     .noHTwosComplement      ;2/3      
    eor     #$ff                    ;2        
    adc     #$01                    ;2 = 15
.noHTwosComplement
    sta     temporaryOne            ;3        
    lda     playerScrollY     ;3        
    sec                             ;2        
    sbc     objPosY,x   ;4        
    bcs     .noVTwosComplement      ;2/3      
    eor     #$ff                    ;2        
    adc     #$01                    ;2 = 18
.noVTwosComplement
    sta     temporaryOne + 1        ;3        
    cmp     temporaryOne            ;3        
    bcs     shiftTempOne                   ;2/3      
    lsr                             ;2        
    bpl     checkLightRange                   ;2/3 = 12 unconditional
shiftTempOne
    lsr     temporaryOne            ;5 = 5
checkLightRange
    clc                             ;2        
    adc     temporaryOne            ;3        
    cmp     #$11                    ;2   torch's effective light range      
    bcs     exitCollision                   ;2/3      
    lda     objPosX,x  ;4        
    sta     p0PosY    ;3        
    lda     objPosY,x   ;4 = 20
checkLightVis
    jsr     handlePlayfieldScrolling ;6        
    cmp     #$50                    ;2        
    bcc     setLightVis                   ;2/3      
    cmp     #$f9                    ;2        
    bcc     exitCollision                   ;2/3 = 14
setLightVis
    sta     playerPFScrollValue     ;3        
    lda     #$01                    ;2        
    rts                             ;6 = 11

; ******************************************************************
    
checkCollisions SUBROUTINE
    ldx     collisionIndex
Lf57f
    dex
    bpl     doCreatureUpdate
    ldx     numberOfCreatures
    bit     torchAnimationIdx
    bvc     doCreatureUpdate
    inx
    jsr     checkForStairs
    beq     nextCollision
    stx     collisionIndex
    rts
nextCollision
    cpx     collisionIndex
    bne     Lf57f
exitCollision
    jmp     Lf4fe
doCreatureUpdate
    jsr     updateCreaturePosition
    beq     nextCollision
    rts

; ******************************************************************
    
updateCreaturePosition SUBROUTINE
    lda     randFloorLoc,x
    cmp     playerCurrentFloor
    bne     exitCollision
    lda     gameSelection
    beq     Lf5ba
    lda     rollingEyesTimer
    bne     Lf5ba
    lda     creaturesInRoom
    beq     exitCollision
    lda     randomRoomLocations,x
    cmp     playerCurrentRoom
    beq     Lf5ba
    cmp     playerDoorCrossing
    bne     exitCollision
Lf5ba
    inx
    stx     object1Color
    dex
    lda     randPosX,x
    sta     p0PosY
    lda     randPosY,x
    jsr     checkLightVis
    beq     nextCollision
    stx     collisionIndex
    lda     #<creatureGraphics
    clc
    adc     creatureLSBOffsetTable,x
    bit     randomSeed
    bvs     .randomAnimateCreature
    adc     #H_CREATURE				; randomly does frame 1 or frame 2
.randomAnimateCreature
    sta     sprite0GraphicPTRs
    lda     #>creatureGraphics
    sta     sprite0GraphicPTRs + 1
    lda     #H_CREATURE
    sta     spriteHeight
    rts
    
creatureLSBOffsetTable
    .byte H_CREATURE * 2 * ID_GHOST
    .byte H_CREATURE * 2 * ID_BAT
    .byte H_CREATURE * 2 * ID_SPIDER
    .byte H_CREATURE * 2 * ID_SPIDER
    .byte H_CREATURE * 2 * ID_SPIDER

; ******************************************************************
    
checkForStairs SUBROUTINE
    lda     roomStairsStatus
    and     #$0f
    cmp     #$0f
    beq     .noStairs
    tay
    cpy     #$04
    bcc     calculateStairGraphic
    tya
    and     #$03
    eor     #$01
calculateStairGraphic
    sta     temporaryOne
    asl
    asl
    asl
    asl
    clc
    adc     temporaryOne
    adc     #<stairGraphics
    sta     sprite0GraphicPTRs
    lda     #0
    adc     #>stairGraphics
    sta     sprite0GraphicPTRs + 1
    tya
    and     #$03
    tay
    lda     torchClippingValues,y
    jsr     handlePlayfieldScrolling
    sta     playerPFScrollValue
    ldy     playerCurrentRoom
    lda     stairHPositionsTable,y
    sta     p0PosY
    lda     #BLACK
    sta     object1Color
    lda     #H_STAIRCASE
    sta     spriteHeight
    lda     #MSBL_SIZE8 | DOUBLE_SIZE
    sta     NUSIZ0 
.noStairs
    rts

; ******************************************************************
    
playfieldKernel
    sta     CXCLR                   ;3        
    ldy     #$00                    ;2 = 5
.blackLinesLoop
    lda     scanline                ;3        
    cmp     lightningColorMask      ;3        
    beq     playFieldLoop           ;2/3      
    sta     WSYNC                   ;3 = 11
;---------------------------------------
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    dec     scanline                ;5        
    bpl     .blackLinesLoop         ;2/3 = 7
playFieldLoop
    lda     temporaryOne            ;3        
    and     #$0f                    ;2        
    bne     .skipPFIncrement        ;2/3      
    inx                             ;2        
    inx                             ;2        x is index into playfield data (groups of 3)
    inx                             ;2 = 13
.skipPFIncrement
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    lda     wallColor               ;3        
    sta     COLUPF                  ;3        
    sty     GRP1                    ;3        
    lda     pf0Data,x               ;4        
    sta     PF0                     ;3        
    lda     pf1Data,x               ;4        
    sta     PF1                     ;3        
    lda     pf2Data,x               ;4        
    sta     PF2                     ;3        
    lda     scanline                ;3        
    sec                             ;2        
    sbc     playerPFScrollValue     ;3        
    cmp     spriteHeight            ;3        
    bcs     .turnSpriteOff0         ;2/3      
    tay                             ;2        
    lda     (sprite0GraphicPTRs),y  ;5        
    tay                             ;2 = 52
playFieldLoop2
    dec     scanline                ;5        
    lda     scanline                ;3        
    cmp     torchesHNumberPTRs      ;3        
    beq     endPlayfieldArea        ;2/3      
    dec     temporaryOne            ;5        
    sta     WSYNC                   ;3 = 21
;---------------------------------------
    sty     GRP0                    ;3        
    lda     scanline                ;3        
    sec                             ;2        
    sbc     playerVertOffset        ;3        
    cmp     playerVertSize          ;3        
    bcs     .turnSpriteOff1         ;2/3      
    tay                             ;2        
    lda     (sprite1GraphicPTRs),y  ;5        
    tay                             ;2        
    jmp     playFieldLoop           ;3 = 28
    
.turnSpriteOff1
    ldy     #$00                    ;2        
    beq     playFieldLoop           ;3 = 5 unconditional

.turnSpriteOff0
    ldy     #$00                    ;2        
    beq     playFieldLoop2          ;3 = 5 unconditional

endPlayfieldArea
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    lda     #$00                    ;2        
    sta     GRP0                    ;3        
    sta     GRP1                    ;3        
    sta     PF1                     ;3        
    sta     PF2                     ;3        
    sta     ENAM0                   ;3        
    sta     ENABL                   ;3 = 20
.finishPlayfieldBottom
    dec     scanline                ;5        
    bmi     statusKernel            ;2/3      
    sta     WSYNC                   ;3 = 10
;---------------------------------------
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    bpl     .finishPlayfieldBottom  ;2/3 = 2 unconditional

statusKernel
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    lda     statusBackgroundColor   ;3        
    sta     COLUBK                  ;3        
    ldy     #$ff                    ;2        
    sty     PF0                     ;3  status area PF edges background      
    iny                             ;2  y = 0 (black)      
    sty     COLUPF                  ;3  set left/right edges PF black       
    ldx     #$07                    ;2        
    sta     WSYNC                   ;3 = 21
;---------------------------------------
.coarsePositionStatus
    dex                             ;2        
    bpl     .coarsePositionStatus   ;2/3      
    sta     RESP0                   ;3        
    lda     #HMOVE_L4               ;2        
    sta     HMP0                    ;3        
    sta     WSYNC                   ;3 = 15
;---------------------------------------
    sta     HMOVE                   ;3        
    lda     #MSBL_SIZE8 | TWO_MED_COPIES  ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3 
       
    stx     floorNumberPTRs + 1     ;3  x = $FF  setting up MSB's for digits    
    stx     livesNumberPTRs + 1     ;3        
    stx     torchesLNumberPTRs + 1  ;3        
    stx     torchesHNumberPTRs + 1  ;3 
       
    lda     torchesUsed             ;3        
    and     #$f0                    ;2 mask high BCD digit       
    lsr                             ;2        
    sta     torchesHNumberPTRs      ;3 set LSB left digit       
    lda     torchesUsed             ;3        
    and     #$0f                    ;2 mask low BCD digit       
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     torchesLNumberPTRs      ;3 set LSB right digit     
    lda     playerLives             ;3        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     livesNumberPTRs         ;3 set 'lives' LSB digit       
    bit     gameState               ;3        
    bvc     .showFloorNumber        ;2/3      
    ldy     gameSelection           ;3        
    bpl     .notFloorNumber         ;2/3 = 69 unconditional

.showFloorNumber
    ldy     playerCurrentFloor      ;3 = 3 floor# and game# = same digit display
.notFloorNumber
    iny                             ;2        
    tya                             ;2        
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    sta     floorNumberPTRs         ;3 set 'floor#' LSB digit        
    ldx     itemBeingCarried        ;3        
    jsr     setInvItemPTRs          ;6 set PTRs for Inventory Item        
    stx     temporaryOne + 1        ;3 set MSB for item       
    bit     itemBeingCarried        ;3        
    bpl     .carryingSomething      ;2/3      
    lda     #<blank                 ;2 = 32 set 'blank' if no item being carried
.carryingSomething
    sta     temporaryOne            ;3 set LSB for item        
    lda     object3Color            ;3        
    sta     COLUP0                  ;3        
    sta     COLUP1                  ;3        
    ldy     #H_FONT-1               ;2 = 14
.flrNumLoop
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    lda     (floorNumberPTRs),y     ;5        
    sta     GRP0                    ;3        
    ldx     #$05                    ;2 = 10
.flrDelayLoop
    dex                             ;2        
    bpl     .flrDelayLoop           ;2/3      
    lda     (temporaryOne),y        ;5        
    sta     GRP0                    ;3        
    dey                             ;2        
    bpl     .flrNumLoop             ;2/3      
    iny                             ;2          y = 0  
    sty     WSYNC                   ;3 = 21
;---------------------------------------
    sty     GRP0                    ;3        
    sty     GRP1                    ;3        
    ldx     #$03                    ;2        
    sta     WSYNC                   ;3 = 11
;---------------------------------------
.txtPosLoop
    dex                             ;2        
    nop                             ;2        
    bpl     .txtPosLoop             ;2/3      
    lda     temporaryOne            ;3        
    sta     RESP0                   ;3        
    sta     RESP1                   ;3        
    lda     #MSBL_SIZE8 | TWO_WIDE_COPIES  ;2        
    sta     NUSIZ0                  ;3        
    sta     NUSIZ1                  ;3        
    ldy     #$07                    ;2 = 25
torchCountLoop
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    lda     (torchesHNumberPTRs),y  ;5        
    sta     GRP0                    ;3        
    lda     (torchesLNumberPTRs),y  ;5        
    sta     GRP1                    ;3        
    ldx     #$03                    ;2 = 18
.coarsePositionLives
    dex                             ;2        
    bpl     .coarsePositionLives    ;2/3      
    lda     scanline                ;3        
    inx                             ;2        
    stx     GRP0                    ;3        
    lda     (livesNumberPTRs),y     ;5        
    sta     GRP1                    ;3        
    dey                             ;2        
    bpl     torchCountLoop          ;2/3      
    iny                             ;2        
    sty     GRP1                    ;3     kernel just 'ends' here?  and goes into audio stuff
   
    lda     gameState               ;3        
    bne     audioCheckFreq                   ;2/3      
    lda     creaturesInRoom                  ;3        
    bne     audioWindCheck                   ;2/3      
    ldx     windSoundCounter                  ;3        
    cpx     #$7f                    ;2        
    beq     silenceAudioZero        ;2/3      
    sta     audioWindSoundBit       ;3 = 49
audioWindCheck
    lda     windSoundCounter                  ;3        
    tay                             ;2        
    cmp     #$7f                    ;2        
    beq     setWindBit                   ;2/3      
    and     #$0f                    ;2        
    bne     updateWindCounter                   ;2/3      
    tya                             ;2        
    beq     setWindBit                   ;2/3      
    bmi     setWindBit                   ;2/3      
    ldx     randomSeed              ;3        
    lda     start,x                 ;4        ; using program code as random wind values (!)
    and     #$01                    ;2 = 28
setWindBit
    sta     audioWindSoundBit
updateWindCounter
    lda     audioWindSoundBit
    bne     decWindCounter
    inc     windSoundCounter
    bne     playWindSound 
decWindCounter
    dec     windSoundCounter
playWindSound
    ldx     #NOISESOUND
    tya
    lsr
    lsr
    lsr
    ora     #$10
    tay
    eor     #$0f
    bpl     setAudioZero			; unconditional?
audioCheckFreq
    lsr
    bcc     checkEndSound
    lda     audioFrequency0Value
    bne     updateNoiseSound
    lda     randomSeed
    and     #$70
    lsr
    lsr
    clc
    adc     #$10
    sta     audioFrequency0Value
    bpl     silenceAudioZero 
updateNoiseSound
    lda     audioFrequency0Value
    eor     #$ff
    tay 
    dec     audioFrequency0Value
    ldx     #NOISESOUND
    lda     #$0c
    bpl     setAudioZero            ; unconditional

checkEndSound
    cmp     #$22
    bne     silenceAudioZero
    lda     frameCount
    lsr
    lsr
    lsr
    lsr
    and     #$03
    tax
    lda     endingMusic,x
    tay
    lda     #$09
    ldx     #TWILIGHTSOUND
    bpl     setAudioZero            ; unconditional

silenceAudioZero
    lda     #0
setAudioZero
    stx     AUDC0
    sty     AUDF0
    sta     AUDV0
    lda     gameState
    bne     silenceAudioOne
    ldy     audioSoundIndex
    lda     audioVolume1Value
    bne     checkSound2
    sta     AUDV1
    sta     audioSoundIndex
    jmp     .skipSoundEleven

; CHECK SOUND #2
    
checkSound2
    dec     audioVolume1Value
    cpy     #2
    bne     .skipSoundTwo
    ldx     #BASSSOUND
    lda     audioVolume1Value
    eor     #$03
setBassSound
    tay
    lda     #$08
    bne     setAudioOne             ; unconditional

; CHECK SOUND #3

.skipSoundTwo
    cpy     #3
    bne     .skipSoundThree
    ldx     #THUNKSOUND
checkSoundSet
    lda     audioVolume1Value
    bpl     setBassSound

; CHECK SOUND #4

.skipSoundThree
    cpy     #4
    bne     .skipSoundFour
    lda     #$06
    ldx     #NOISESOUND
    ldy     #$0f
    bne     setAudioOne             ; unconditional

; CHECK SOUND #5

.skipSoundFour
    cpy     #5
    bne     .skipSoundFive
    lda     audioVolume1Value
    eor     #$03
setBassSound2
    ldy     #$08
    ldx     #BASSSOUND
    bne     setAudioOne             ; unconditional

; CHECK SOUND #6

.skipSoundFive
    cpy     #6
    bne     .skipSoundSix
    lda     audioVolume1Value
    bpl     setBassSound2

; CHECK SOUND #7

.skipSoundSix
    cpy     #7
    bne     .skipSoundSeven
    ldx     #URNCENTERSOUND
    bne     checkSoundSet                   ; unconditional

; CHECK SOUND #8

.skipSoundSeven
    cpy     #8
    bne     .skipSoundEight
    ldx     #NOISESOUND
    bne     checkSoundSet                   ; unconditional

silenceAudioOne
    ldy     #0
setAudioOne
    sty     AUDV1
    sta     AUDF1
    stx     AUDC1
    rts
   
.skipSoundEight
    lda     audioVolume1Value
    lsr
    lsr
    lsr
    lsr
    cpy     #9
    beq     .doSoundNine
    cpy     #10
    bne     .skipSoundTen
    eor     #$03                    ; change up or down stairs sound
.doSoundNine
    tax
    lda     stairwaySound,x
    ldx     #BASSSOUND
    ldy     #$0a
    bne     setAudioOne             ; unconditional

.skipSoundTen
    cpy     #11
    bne     .skipSoundEleven
    lda     audioVolume1Value
    tay
    and     #$04
    beq     silenceAudioOne
    ldx     #BASSSOUND
    lda     #$01
    bne     setAudioOne             ; unconditional

.skipSoundEleven
    lda     SWCHA
    eor     #$ff
    beq     silenceAudioOne
    lda     frameCount
    and     #$07
    cmp     #$03
    bcs     silenceAudioOne
    ldy     #$0f
    ldx     #FOOTSOUND
    lda     #$18                    ; setup for footsteps sound
    bne     setAudioOne             ; unconditional

playSound
    cpx     audioSoundIndex
    beq     .returnFromSoundRoutine
    cpx     #$03
    bne     Lf898
    lda     audioSoundIndex
    bne     .returnFromSoundRoutine
Lf898
    stx     audioSoundIndex
    lda     audioVolume1ValueTable,x
    sta     audioVolume1Value
.returnFromSoundRoutine
    rts						; I think this returns from 'playfieldKernel' itself
    
audioVolume1ValueTable
    .byte   $ff,$ff,$15,$15,$01,$04,$04,$0a,$15,$3f,$3f,$1f

; ******************************************************************
    
handlePlayerDeath SUBROUTINE
    lda     gameState
    and     #$fd
    sta     gameState
    ror
    bcc     processCreatures
    dec     rollingEyesTimer
    bne     processCreatures
    lda     #0
    sta     gameState
    sta     audioSoundIndex
    lda     playerLives
    bne     .playerStillHasLives
    jsr     setSelectionVariables
.playerStillHasLives
    jsr     scatterTheItems
    sta     CXCLR
    lda     creatureCollisionFlag
    cmp     #$01
    bne     .goToRTS
    ldy     gameSelection
    cpy     #$06
    bcc     .goToRTS
    ldx     itemBeingCarried
    bmi     .goToRTS
    lda     randFloorLoc1		; on bat collision, level > 7, take player's item and hide it
    sta     objFloorLoc,x
    lda     randPosX1
    sta     objPosX,x
    lda     randPosY1
    sta     objPosY,x
    lda     #$ff
    sta     itemBeingCarried
.goToRTS
    rts

; ******************************************************************
    
processCreatures SUBROUTINE
    lda     frameCount
    and     #$07
    sta     scanline
    tay
    lda     #$88
    and     bitmaskThing,y
    sta     floorNumberPTRs
    lda     #0
    sta     creaturesInRoom
    ldx     numberOfCreatures
creatureLoop
    lda     randFloorLoc,x
    cmp     playerCurrentFloor
    bne     updateCreatureLogic
    lda     randomRoomLocations,x
    cmp     playerCurrentRoom
    beq     incCreatureCount
    cmp     playerDoorCrossing
    bne     updateCreatureLogic
incCreatureCount
    inc     creaturesInRoom
    lda     rollingEyesTimer
    bne     skipCreatureDraw
    txa
    bne     checkCreatureState
    lda     gameSelection
    cmp     #$07
    bcs     moveCreature
checkCreatureState
    lda     itemBeingCarried
    cmp     #$01
    beq     skipCreatureDraw
moveCreature
    jsr     lookupCreatureSpeed
    beq     nextCreature
    lda     playerPosX
    sec
    sbc     creatureStartX,x
    bcs     storeCreatureX
    lda     playerPosX
storeCreatureX
    sta     temporaryOne
    lda     playerScrollY
    sec
    sbc     creatureStartY,x
    bcs     storeCreatureY
    lda     playerScrollY
storeCreatureY
    sta     temporaryOne + 1
    jmp     adjustCreaturePos
updateCreatureLogic
    jsr     getCreatureSpeedIndex
    beq     nextCreature
    bne     checkCreatureRoom                   ; unconditional
skipCreatureDraw
    lda     floorNumberPTRs
    beq     nextCreature
checkCreatureRoom
    lda     objectRoomLocations,x
    and     #$3f
    tay
    and     #$0f
    sta     temporaryOne
    tya
    lsr
    lsr
    lsr
    lsr
    tay
    lda     randomLocationsTableOffsets,y
    clc
    adc     temporaryOne
    tay
    lda     randomLocationsTableH,y
    sta     temporaryOne
    lda     randomLocationsTableV,y
    sta     temporaryOne + 1
    cmp     randPosY,x
    bne     adjustCreaturePos
    lda     randPosX,x
    cmp     temporaryOne
    bne     adjustCreaturePos
    lda     creatureProcessMask
    ora     bitmaskThing,x
    sta     creatureProcessMask
    bne     nextCreature
adjustCreaturePos
    lda     randPosX,x
    cmp     temporaryOne
    bcc     incCreatureX
    beq     checkCreatureY
    dec     randPosX,x
    bne     checkCreatureY
incCreatureX
    inc     randPosX,x
checkCreatureY
    lda     randPosY,x
    cmp     temporaryOne + 1
    bcc     incCreatureY 
    beq     nextCreature
    dec     randPosY,x
    jmp     nextCreature
incCreatureY
    inc     randPosY,x
nextCreature
    dex 
    bmi     processCreatureMask
    jmp     creatureLoop
processCreatureMask
    ldx     #$00
    lda     creatureProcessMask
maskLoop
    ror
    bcs     moveCreatureRoom
    inx
    cpx     numberOfCreatures
    beq     maskLoop
    bcc     maskLoop
    rts

moveCreatureRoom SUBROUTINE
    lda     creatureProcessMask
    eor     bitmaskThing,x
    sta     creatureProcessMask
    lda     objectRoomLocations,x
    tay
    and     #$30
    beq     randomizeCreature
    cmp     #$10
    bne     checkRoomMove
    tya
    and     #$c0
    sta     temporaryOne
    clc
    rol
    rol
    rol
    tay
    lda     roomToRoomOffsets,y
    clc
    adc     randomRoomLocations,x
    sta     randomRoomLocations,x
    bpl     storeCreatureRoom
checkRoomMove
    cmp     #$30
    beq     incFloor
    dec     randFloorLoc,x
    bpl     setStartRoom
incFloor
    inc     randFloorLoc,x
setStartRoom
    tya
    and     #$c0
    eor     #$40
    sta     temporaryOne
storeCreatureRoom
    lda     randomRoomLocations,x
    ora     temporaryOne
    sta     objectRoomLocations,x
    rts

; ******************************************************************
    
randomizeCreature SUBROUTINE
    tya
    rol
    rol
    rol
    and     #$03
    eor     #$01
    sta     torchesHNumberPTRs
    lda     randomSeed
    and     #$03
    sta     lightningColorMask
    sta     colorCycleMode
    bpl     checkRndVal
rndWait
    inc     colorCycleMode
    lda     colorCycleMode
    and     #$03
    sta     colorCycleMode
    cmp     lightningColorMask
    beq     setRndPtr
checkRndVal
    cmp     torchesHNumberPTRs
    beq     rndWait
    jsr     checkFloorRnd
    beq     rndWait
    rts
setRndPtr
    lda     torchesHNumberPTRs
checkFloorRnd
    sta     torchesLNumberPTRs
    lda     randFloorLoc,x
    sta     tmpFloorNumber
    lda     randomRoomLocations,x
    sta     temporaryOne
    jsr     Lfdae
    bmi     checkValidRoom
    beq     setGame9Room
    lda     gameSelection
    cmp     #$08
    beq     setGame9Room
    txa
    bne     skipOnOut
setGame9Room
    lda     tmpRoomNumber
    sta     temporaryOne
    lda     #$01
    bne     storeRndRoom                   ; unconditional

checkValidRoom
    cmp     #$ff
    beq     skipOnOut
    ldy     randFloorLoc,x
    lda     randomRoomLocations,x
    jsr     Lfd4c
    bcs     skipOnOut
    bne     setRoomBits
    lda     #$02
    bne     storeRndRoom                   ; unconditional

setRoomBits
    lda     #$03
storeRndRoom
    asl     torchesLNumberPTRs
    asl     torchesLNumberPTRs
    ora     torchesLNumberPTRs
    asl
    asl
    asl
    asl
    ora     temporaryOne
    sta     objectRoomLocations,x
    rts
skipOnOut
    lda     #$00
    rts

; ******************************************************************
    
getCreatureSpeedIndex SUBROUTINE
    lda     creaturesPresentMask
    and     bitmaskThing,x
    beq     Lfa83
lookupCreatureSpeed
    lda     gameSelection
    cmp     #$07
    bcc     .skipHardCreatureSpeeds
    lda     hardCreatureSpeeds,x
    bne     .doSetCreatureSpeeds
.skipHardCreatureSpeeds
    lda     easyCreatureSpeeds,x
.doSetCreatureSpeeds
    ldy     scanline
    and     bitmaskThing,y
    rts
Lfa83
    lda     floorNumberPTRs
    rts
    
easyCreatureSpeeds
    .byte   $aa ; fast ghost
    .byte   $91 ; med bat
    .byte   $88 ; slow spider
    .byte   $88 ; slow spider
    .byte   $88 ; slow spider

hardCreatureSpeeds
    .byte   $aa ; fast ghost
    .byte   $aa ; fast bat
    .byte   $91 ; med spider
    .byte   $91 ; med spider
    .byte   $91 ; med spider

; ******************************************************************
    
checkP0P1Collision SUBROUTINE
    lda     rollingEyesTimer
    bne     skipOnOut
    bit     CXPPMM|$30
    bpl     skipOnOut
    lda     itemGatheredFlag
    bne     skipOnOut
    bit     torchAnimationIdx
    bvc     checkCreatureKill
    bpl     handlePlayerKill
    rts
checkCreatureKill
    lda     collisionIndex
    cmp     numberOfCreatures
    beq     deadPlayerHandler
    bcc     deadPlayerHandler
    rts
handlePlayerKill
    ldx     #6				; sound #6
    jsr     playSound
    lda     itemBeingCarried
    bmi     checkItemLastSeen
    cmp     itemLastSeen
    beq     checkItemLastSeen
    cmp     #$02
    bcc     checkItemLastSeen
    lda     itemLastSeen
    cmp     #$02
    bcc     checkItemLastSeen
    clc
    adc     itemBeingCarried
    tay
    ldx     #7				; sound #7
    jsr     playSound
;^^^^^^^^^^^^ section dealing with urn piece assembly
    lda     urnAssembly1
    ora     urnAssembly2
    bpl     assembleUrn
    ldx     #11				; sound #11
    jsr     playSound
    lda     #$08			; the following stuff sets "urn complete"
    sta     urnAssembly0		;
    lda     #$ff			;
    sta     urnAssembly1		;
    sta     urnAssembly2		;
    lda     #$02
    bne     finishItemCheck			; unconditional
assembleUrn
    ldx     #$02
    cpy     #$05
    bne     setUrnParts
    dex 
setUrnParts
    lda     #$ff
    sta     urnAssembly0,x
    ldx     #$00
    cpy     #$07
    bne     setUrnComplete
    inx 
setUrnComplete
    sty     urnAssembly0,x
    inx 
    inx 
    txa
    bne     finishItemCheck
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
checkItemLastSeen
    lda     itemLastSeen
finishItemCheck
    pha
    bit     itemBeingCarried
    bmi     exitCollisionCheck
    jsr     pickupItem
exitCollisionCheck
    pla
    sta     itemBeingCarried 
allDone
    rts

; ******************************************************************
    
deadPlayerHandler SUBROUTINE
    lda     gameSelection
    cmp     #7				; scepter impotent in game 8 or 9
    bcc     .checkForScepter
    lda     collisionIndex
    beq     .doPlayerDeath
.checkForScepter
    lda     itemBeingCarried
    cmp     #1				; if has scepter, we're safe
    beq     allDone
.doPlayerDeath
    ldx     #$01
    stx     gameState
    ldx     #$ff
    stx     rollingEyesTimer	; max out length of player's death, LOL
    dec     playerLives
    lda     collisionIndex			; I think this relocates the thing which killed you (?)
    sta     creatureCollisionFlag
    rts

; ******************************************************************
    
resetCreaturePositions SUBROUTINE
    lda     gameSelection
    cmp     #$05
    bcc     handleInput
    lda     roomStairsStatus
    and     #$07
    asl
    asl
    sta     temporaryOne
    asl
    asl
    asl
    asl
    ora     temporaryOne
    and     #$d0
    ora     #$20
    ora     playerCurrentRoom
    sta     temporaryOne
resetCreaturesLoop
    ldx     numberOfCreatures
resetCreaturesInner
    lda     #0
    sta     creaturesPresentMask
checkCreatureReset
    lda     randFloorLoc,x
    cmp     playerCurrentFloor
    bne     .continueLoop
    lda     randomRoomLocations,x
    cmp     playerCurrentRoom
    bne     .continueLoop
    lda     temporaryOne
    sta     objectRoomLocations,x
    lda     creaturesPresentMask
    ora     bitmaskThing,x
    sta     creaturesPresentMask
.continueLoop
    dex
    bpl     checkCreatureReset
    rts

; ******************************************************************
    
handleStairs SUBROUTINE
    lda     roomStairsStatus
    and     #$0f
    cmp     #$0f
    beq     handleInput
    bit     roomStairsStatus
    bpl     setStairStatus
    bvs     handleInput
    jsr     resetCreaturePositions
    ldx     #9				; sound #9
    lda     roomStairsStatus
    and     #$04 
    beq     decFloor
    inc     playerCurrentFloor 
    bne     playStairSound
decFloor
    dec     playerCurrentFloor
    inx					; inc to sound #10
playStairSound
    jsr     playSound
    jsr     initRoom
    ora     #$40
setStairStatus
    sta     roomStairsStatus
handleInput
    lda     SWCHA
    and     #$f0
    eor     #$f0
    sta     movementValue
    lda     creaturesInRoom
    bne     .noItemHeld
    lda     INPT4|$30
    rol
    ror     actionButtonDebounce
    lda     actionButtonDebounce
    cmp     #$7f
    bne     .noItemHeld
    bit     torchAnimationIdx
    bvs     .skipOverTorchLighting
    lda     #TORCH_DURATION     ; if here, a torch was lit
    sta     torchTimer
    sed
    lda     torchesUsed
    clc
    adc     #1                  ; BCD increase torches used
    sta     torchesUsed
    cld 
    lda     torchAnimationIdx
    ora     #$40
    sta     torchAnimationIdx
    rts
.skipOverTorchLighting
    bit     itemBeingCarried
    bmi     .noItemHeld
    ldx     #LEADSOUND           ; sound # 5
    jsr     playSound
    jsr     pickupItem
    lda     #$ff
    sta     itemBeingCarried	; set to 'no item held'
    bne     .goToRTS
.noItemHeld
    lda     itemGatheredFlag
    beq     .goToRTS
    bit     CXP0FB|$30
    bmi     dropItem
    lda     #$00
    sta     itemGatheredFlag
    beq     .goToRTS
pickupItem
    lda     #$05
    sta     itemGatheredFlag
    lda     itemBeingCarried
    sta     itemActionIndex
    lda     playerDeltaX
    tay
    ora     playerDeltaY
    beq     dropItem
    iny
    iny
    ldx     playerDeltaY
    inx
    jsr     checkItemDrop
    bcc     .goToRTS
dropItem
    dec     itemGatheredFlag
    beq     .goToRTS
    ldx     itemGatheredFlag
    dex
    ldy     itemGatheredFlag
    dey 
    jsr     checkItemDrop
    bcs     dropItem
.goToRTS
    rts

; ******************************************************************
    
checkItemDrop SUBROUTINE
    lda     playerScrollOffsets + 1,x
    ldx     playerScrollY
    cpx     #$f4
    bcc     checkDropY
    cmp     #$0a
    beq     dropFailed
checkDropY
    clc
    adc     playerScrollY
    cmp     #$f4
    bcs     dropFailed
    ldx     itemActionIndex
    sta     objPosY,x
    jsr     handlePlayfieldScrolling
    sta     playerPFScrollValue
    lda     playerScrollOffsets,y
    clc
    adc     playerPosX
    cmp     #XMAX
    bcs     dropFailed
    ldx     itemActionIndex
    sta     objPosX,x
    sta     p0PosY
    clc
    rts
dropFailed
    sec
    rts

; ******************************************************************
    
checkDoorwayPassages SUBROUTINE
    lda     #$05
    sta     temporaryOne
    ldx     #$00
doorLoop
    ldy     temporaryOne
    lda     joystickValuesIndex,y
    tay 
    lda     movementValue
    and     joystickValues,y
    beq     nextDoor
    lda     playerPosX
    cpy     #$02
    bcc     checkDoorPos
    lda     playerScrollY
checkDoorPos
    cmp     doorwayBoundaryTable,x
    beq     checkDoorExit
    cmp     doorwayBoundaryTable + 1,x
    beq     handleDoorway
nextDoor
    inx
    inx
    dec     temporaryOne
    bpl     doorLoop
jmpIntoAnRTS
    rts
    
doorwayBoundaryTable
    .byte   $54,$5c
    .byte   $a4,$ac
    .byte   $58,$4f
    .byte   $a8,$9f
    .byte   $4f,$47
    .byte   $48,$50

joystickValuesIndex
    .byte   $01,$00,$03,$03,$02,$02

joystickValues
    .byte   ~MOVE_LEFT,~MOVE_RIGHT,~MOVE_UP,~MOVE_DOWN

; ******************************************************************
    
handleDoorway SUBROUTINE
    bit     doorwayCrossingStatus
    bpl     jmpIntoAnRTS
    tya
    ora     #$80
    cmp     doorwayCrossingStatus
    beq     doorTransition
    lda     playerDoorCrossing
    sta     playerCurrentRoom
doorTransition
    jsr     changeRoom
    sty     doorwayCrossingStatus
    lda     roomToRoomOffsets,y
    clc
    adc     playerCurrentRoom
    sta     playerCurrentRoom
    lda     #$ff
    sta     playerDoorCrossing
    jsr     initRoom
    ldx     #$08
    bne     playDoorSound
checkDoorExit
    bit     doorwayCrossingStatus
    bmi     jmpIntoAnRTS
    tya
    jsr     getRoomIndex
    beq     setDoorExit
    lda     itemBeingCarried
    bne     initDoorMove
setDoorExit
    ldy     torchesLNumberPTRs
    lda     roomToRoomOffsets,y
    clc 
    adc     playerCurrentRoom
    sta     playerDoorCrossing
    tya 
    ora     #$80
    sta     doorwayCrossingStatus
    ldx     #$04
    bne     playDoorSound                   ; unconditional
initDoorMove
    lda     torchesLNumberPTRs
    lsr
    tax
    lda     bitmaskThing + 6,x 
    sta     doorwayMovementFlag
    ldx     #2					; sound #2
playDoorSound
    lda     gameSelection
    beq     jmpIntoAnRTS
    jmp     playSound

; ******************************************************************
    
changeRoom SUBROUTINE
    sty     tmpYRegisterSaveLocation
    lda     gameSelection
    cmp     #$05
    bcc     .restoreYRegisterValue
    lda     floorYOffsets,y
    ora     #$10
    sta     temporaryOne
    tya
    jsr     getRoomIndex
    pha
    lda     temporaryOne
    ora     tmpRoomNumber
    sta     temporaryOne
    pla
    beq     Lfcfc
    ldx     #0
    jsr     resetCreaturesInner
    bmi     .restoreYRegisterValue			; unconditional (surely...)
Lfcfc
    jsr     resetCreaturesLoop
.restoreYRegisterValue
    ldy     tmpYRegisterSaveLocation
    rts

; ******************************************************************
    
playerScrollOffsets
    .byte   0, 10, 0, -10

floorYOffsets
    .byte   $00,$40,$80,$c0                 ; $fd06 (*)
    
initRoom SUBROUTINE
    lda     #2
    jsr     getFloorOffset
    sta     pfScrollOffsetA
    lda     #3
    jsr     getFloorOffset
    sta     pfScrollOffsetB
    lda     #0
    jsr     getFloorOffset
    beq     .rightSideDoor
    lda     #DOOR_LEFT_XPOS + 8		; Ball & M0 positions
    ldx     #DOOR_LEFT_XPOS		; door on left
    bne     .setDoorPosition		; unconditional
.rightSideDoor
    lda     #DOORrIGHT_XPOS		; Ball & M0 positions
    ldx     #DOORrIGHT_XPOS + 8	; door on right
.setDoorPosition
    sta     tmpBallHorizPosition
    stx     missile0HorizPosition
    ldy     playerCurrentFloor
    lda     playerCurrentRoom
    jsr     Lfd4c
    bcs     setNoDoors
    beq     getDoorBits
    lda     #$04
getDoorBits
    ldx     playerCurrentRoom
    ora     doorsByRoom,x
setDoorBits
    sta     roomStairsStatus
    rts
setNoDoors
    lda     #$0f
    bne     setDoorBits                   ; unconditional
    
doorsByRoom
    .byte   3,3,1,0,2,2             ; indexed by playerCurrentRoom

; ******************************************************************
    
Lfd4c SUBROUTINE
    sta     tmpRoomNumber
    tya
    jsr     game9FloorPlanner
    sty     tmpFloorNumber
    lda     startRoomLayout,y
    ldy     tmpRoomNumber
    and     bitmaskThing,y
    beq     Lfd6a
    ldy     tmpFloorNumber
    lda     startFloorLayout,y
    ldy     tmpRoomNumber
    clc
    and     bitmaskThing,y
    rts
Lfd6a
    sec
    rts

; ******************************************************************
    
getFloorOffset SUBROUTINE
    sta     torchesLNumberPTRs
    lda     playerCurrentFloor
    sta     tmpFloorNumber
    lda     playerCurrentRoom
    jsr     calcTableIndex
    bmi     gotoRTSWithZero
    lsr
    tay
    lda     floorDataPtrs,y
    rts

; ******************************************************************
    
floorDataPtrs
    .byte   $57,$a7,$4f                     ; $fd7f (D)
roomLayouts
    .byte   $04,$ff,$00,$80,$ff,$04,$01,$81 ; $fd82 (D)
    .byte   $05,$82,$02,$00,$83,$05,$03,$01 ; $fd8a (D)
    .byte   $06,$ff,$84,$02,$ff,$06,$85,$03 ; $fd92 (D)

; ******************************************************************
    
calcTableIndex SUBROUTINE
    asl
    asl
    clc
    adc     torchesLNumberPTRs
    tay
    lda     roomLayouts,y
    sta     livesNumberPTRs
    rts

; ******************************************************************
    
getRoomIndex SUBROUTINE
    sta     torchesLNumberPTRs
    lda     playerCurrentFloor
    sta     tmpFloorNumber
    lda     playerCurrentRoom
Lfdae
    jsr     calcTableIndex
    bmi     .gotoRTS
    ldy     gameSelection
    cpy     #$02
    bcc     gotoRTSWithZero
    lda     tmpFloorNumber
    jsr     game9FloorPlanner
    lda     game9Layout,y
    ldy     tmpRoomNumber
    and     bitmaskThing,y
    rts
gotoRTSWithZero
    lda     #0
.gotoRTS
    rts

; ******************************************************************
    
game9FloorPlanner SUBROUTINE
    ldy     gameSelection
    cpy     #8
    bne     .notOnGame9
    clc
    adc     #$04
.notOnGame9
    tay
    rts

; ******************************************************************
    
game9Layout
    .byte   $29,$47,$34,$29,$2e,$1d,$2b,$0a ; $fdd5 (*)

startRoomLayout
    .byte   $21,$3b,$3f,$25                 ; $fddd (D) ; games 1-8
    .byte   $2a,$3f,$3f,$2a                 ; $fde1 (*) ; game 9 only

startFloorLayout
    .byte   $21,$1a,$25,$00                 ; $fde5 (D)
    .byte   $2a,$15,$2a                     ; $fde9 (*)

creatureStartX
    .byte   $00,$fa,$fa,$06,$06             ; $fdec (D)

creatureStartY
    .byte   $00,$fe,$06,$fe,$06             ; $fdf1 (D)

initialGameVariables
    .byte   $09,$ff,$7f,$02,$ff,$0f,$02,$02,$03,$04 ; $fdf6 (D)

bitmaskThing ; $FE00 - Unsure of Exact Use
    .byte   %00000001 ; |       #|
    .byte   %00000010 ; |      # |
    .byte   %00000100 ; |     #  |
    .byte   %00001000 ; |    #   |
    .byte   %00010000 ; |   #    |
    .byte   %00100000 ; |  #     |
    .byte   %01000000 ; | #      |
    .byte   %10000000 ; |#       |
;Key $FE08 - object 0
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00000111 ; |     ###|
    .byte   %11111101 ; |###### #|
    .byte   %10100111 ; |# #  ###|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
;Scepter $FE10 - object 1    
    .byte   %00000001 ; |       #|
    .byte   %00000010 ; |      # |
    .byte   %00000100 ; |     #  |
    .byte   %00101000 ; |  # #   |
    .byte   %10010000 ; |#  #    |
    .byte   %01101000 ; | ## #   |
    .byte   %01100000 ; | ##     |
    .byte   %10010000 ; |#  #    |
;Left Urn $FE18 - object 2
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00100000 ; |  #     |
    .byte   %11110000 ; |####    |
    .byte   %10100000 ; |# #     |
    .byte   %11100000 ; |###     |
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
;Center Urn $FE20 - object 3
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
    .byte   %00011000 ; |   ##   |
    .byte   %00001000 ; |    #   |
    .byte   %00011000 ; |   ##   |
    .byte   %00010000 ; |   #    |
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
;Right Urn $FE28 - object 4
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00000100 ; |     #  |
    .byte   %00000111 ; |     ###|
    .byte   %00000110 ; |     ## | ; this is a graphic inconsistancy
    .byte   %00001111 ; |    ####|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
;Partial Urn $FE30 - object 5 (2+3)    
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
    .byte   %00111000 ; |  ###   |
    .byte   %11111000 ; |#####   |
    .byte   %10111000 ; |# ###   |
    .byte   %11110000 ; |####    |
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
;Partial Urn $FE38 - object 6 (2+4)    
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00100100 ; |  #  #  |
    .byte   %11110111 ; |#### ###|
    .byte   %10100101 ; |# #  # #|
    .byte   %11101111 ; |### ####|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
;Partial Urn $FE40 - object 7 (3+4)
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
    .byte   %00011100 ; |   ###  |
    .byte   %00001111 ; |    ####|
    .byte   %00011101 ; |   ### #|
    .byte   %00011111 ; |   #####|
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
;Complete Urn $FE48 - object 8
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
    .byte   %00111100 ; |  ####  |
    .byte   %11111111 ; |########|
    .byte   %10111101 ; |# #### #|
    .byte   %11111111 ; |########|
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |

creatureGraphics
;ghost_0 $FE50
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00001100 ; |    ##  |
    .byte   %00111100 ; |  ####  |
    .byte   %01111100 ; | #####  |
    .byte   %01111101 ; | ##### #|
    .byte   %01111110 ; | ###### |
    .byte   %11010100 ; |## # #  |
    .byte   %10111000 ; |# ###   |
;ghost_1 $FE5A
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11100000 ; |###     |
    .byte   %01111000 ; | ####   |
    .byte   %00111100 ; |  ####  |
    .byte   %00111110 ; |  ##### |
    .byte   %00111110 ; |  ##### |
    .byte   %10111110 ; |# ##### |
    .byte   %01101011 ; | ## # ##|
    .byte   %00011101 ; |   ### #|
;bat_0 $FE64
    .byte   %10000001 ; |#      #|
    .byte   %10000001 ; |#      #|
    .byte   %11000011 ; |##    ##|
    .byte   %01100110 ; | ##  ## |
    .byte   %01111110 ; | ###### |
    .byte   %00111100 ; |  ####  |
blank ; $FE6A
    .byte   %00000000 ; |        | blank is shared data (8 zero bytes)
    .byte   %00000000 ; |        | used to show 'no object'
    .byte   %00000000 ; |        | in status display area
    .byte   %00000000 ; |        |
;bat_1 $FE6E
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %00111100 ; |  ####  |
    .byte   %01111110 ; | ###### |
    .byte   %01100110 ; | ##  ## |
    .byte   %11000011 ; |##    ##|
    .byte   %10000001 ; |#      #|
    .byte   %10000001 ; |#      #|
;spider_0 $FE78
    .byte   %00100100 ; |  #  #  |
    .byte   %00100100 ; |  #  #  |
    .byte   %00100100 ; |  #  #  |
    .byte   %10011001 ; |#  ##  #|
    .byte   %01111110 ; | ###### |
    .byte   %00011000 ; |   ##   |
    .byte   %01100110 ; | ##  ## |
    .byte   %10000001 ; |#      #|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
;spider_1 $FE82
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11100111 ; |###  ###|
    .byte   %00011000 ; |   ##   |
    .byte   %01111110 ; | ###### |
    .byte   %10011001 ; |#  ##  #|
    .byte   %00100100 ; |  #  #  |
    .byte   %01000010 ; | #    # |
    .byte   %01000010 ; | #    # |
    .byte   %00000000 ; |        |

stairGraphics
;stairs_Ulr $FE8C
    .byte   %10110111 ; |# ## ###| LSB/MSB's are calculated (no LSB table)
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
    .byte   %10110111 ; |# ## ###|
;stairs_Dlr $FE9D
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
    .byte   %11101101 ; |### ## #|
;stairs_Dtb $FEAE
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %00000000 ; |        |
;stairs_Utb $FEBF
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %00000000 ; |        |

pf0Data
    .byte   %11111111 ; |********| $FED0 (P)
pf1Data
    .byte   %11110000 ; |****    | $FED1 (P)
pf2Data
    .byte   %11111111 ; |********| $FED2 (P)
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %00010000 ; |   *    |
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %00000000 ; |        |
    .byte   %11111111 ; |********|
    .byte   %00000000 ; |        |
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|
    .byte   %11110000 ; |****    |
    .byte   %11111111 ; |********|

	ORG $FF00 ; number lsb/msb are calulated (no lsb table)

;zero $FF00
    .byte   %00011100 ; |   ###  |
    .byte   %00111110 ; |  ##### |
    .byte   %01110011 ; | ###  ##|
    .byte   %01100011 ; | ##   ##|
    .byte   %01100011 ; | ##   ##|
    .byte   %01100111 ; | ##  ###|
    .byte   %00111110 ; |  ##### |
    .byte   %00011100 ; |   ###  |

;one $FF08
    .byte   %00111110 ; |  ##### |
    .byte   %00011100 ; |   ###  |
    .byte   %00001100 ; |    ##  |
    .byte   %00001100 ; |    ##  |
    .byte   %00001100 ; |    ##  |
    .byte   %00111100 ; |  ####  |
    .byte   %00011100 ; |   ###  |
    .byte   %00001100 ; |    ##  |

;two $FF10
    .byte   %01111111 ; | #######|
    .byte   %01100000 ; | ##     |
    .byte   %01110000 ; | ###    |
    .byte   %00111110 ; |  ##### |
    .byte   %00000011 ; |      ##|
    .byte   %00110011 ; |  ##  ##|
    .byte   %01100111 ; | ##  ###|
    .byte   %00111110 ; |  ##### |

;three $FF18
    .byte   %00111110 ; |  ##### |
    .byte   %01100111 ; | ##  ###|
    .byte   %00110011 ; |  ##  ##|
    .byte   %00000110 ; |     ## |
    .byte   %00001100 ; |    ##  |
    .byte   %00000111 ; |     ###|
    .byte   %00110011 ; |  ##  ##|
    .byte   %00011110 ; |   #### |

;four $FF20
    .byte   %00001111 ; |    ####|
    .byte   %00000110 ; |     ## |
    .byte   %11111111 ; |########|
    .byte   %11000110 ; |##   ## |
    .byte   %01100110 ; | ##  ## |
    .byte   %01100110 ; | ##  ## |
    .byte   %11100110 ; |###  ## |
    .byte   %00000111 ; |     ###|

;five $FF28
    .byte   %00111110 ; |  ##### |
    .byte   %01100111 ; | ##  ###|
    .byte   %00110011 ; |  ##  ##|
    .byte   %00000011 ; |      ##|
    .byte   %00111110 ; |  ##### |
    .byte   %00110000 ; |  ##    |
    .byte   %00111000 ; |  ###   |
    .byte   %00011111 ; |   #####|

;six $FF30
    .byte   %00111110 ; |  ##### |
    .byte   %01110011 ; | ###  ##|
    .byte   %01100111 ; | ##  ###|
    .byte   %01111110 ; | ###### |
    .byte   %01100000 ; | ##     |
    .byte   %01100111 ; | ##  ###|
    .byte   %00110011 ; |  ##  ##|
    .byte   %00011110 ; |   #### |

;seven $FF38
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
    .byte   %00001100 ; |    ##  |
    .byte   %00001100 ; |    ##  |
    .byte   %00000110 ; |     ## |
    .byte   %00000110 ; |     ## |
    .byte   %11000011 ; |##    ##|
    .byte   %01111111 ; | #######|

;eight $FF40
    .byte   %00111110 ; |  ##### |
    .byte   %01100111 ; | ##  ###|
    .byte   %11000011 ; |##    ##|
    .byte   %11001111 ; |##  ####|
    .byte   %01111110 ; | ###### |
    .byte   %01110011 ; | ###  ##|
    .byte   %01100110 ; | ##  ## |
    .byte   %00111100 ; |  ####  |

;nine $FF48
    .byte   %00111100 ; |  ####  |
    .byte   %01100110 ; | ##  ## |
    .byte   %00000011 ; |      ##|
    .byte   %00111111 ; |  ######|
    .byte   %01110011 ; | ###  ##|
    .byte   %01100011 ; | ##   ##|
    .byte   %01100110 ; | ##  ## |
    .byte   %00111100 ; |  ####  |

torchGraphics
torch_0
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |

torch_1
    .byte   %00011000 ; |   ##   |
    .byte   %00011000 ; |   ##   |
    .byte   %00011000 ; |   ##   |
    .byte   %00011000 ; |   ##   |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %11111111 ; |########|
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %01111110 ; | ###### |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00111100 ; |  ####  |
    .byte   %00011000 ; |   ##   |
    .byte   %00011000 ; |   ##   |
    .byte   %00011000 ; |   ##   |
    .byte   %00011000 ; |   ##   |
    
torchClippingValues
    .byte   $80,$80,$ee,$03

stairHPositionsTable
    .byte   111,31,139,4,111,31

rollingEyesTable
    .byte   $e0,$a0,$b0,$90,$d0,$50,$70,$60

torchLSBValues
    .byte   <torch_0,<torch_1,<torch_0,<torch_0

endingMusic
    .byte   19,18,19,23 ; sorta like 'twilight zone'

stairwaySound
    .byte   9,9,11,18 ; played forward and reversed when ascending/descending

randomLocationsTableOffsets
    .byte   $00,$06,$0d,$0d

randomLocationsTableH
    .byte   $74,$24,$74,$24,$74,$24,$74,$24 ; "random" horiz locations of items
    .byte   $74,$24,$4C,$4C,$4C,$74,$24,$94
    .byte   $04,$74,$24

randomLocationsTableV
    .byte   $24,$24,$84,$84,$C4,$C4,$54,$54 ; "random" vert locations of items
    .byte   $A4,$A4,$24,$84,$C4,$00,$00,$84
    .byte   $84,$F4,$F4

roomToRoomOffsets
    .byte   1,-1,2,-2 ; used when moving up/down stairways to another room 

wallBitPattern
       .byte %10111101 ; |X XXXX X| used to check for door openings
       .byte %00011000 ; |   XX   |
       .byte %00000000 ; |        |
       .byte %00011000 ; |   XX   |
       .byte %00011000 ; |   XX   |
       .byte %10111101 ; |X XXXX X|
       .byte %00011000 ; |   XX   |
       .byte %00011000 ; |   XX   |
       .byte %00000000 ; |        |
       .byte %00011000 ; |   XX   |
       .byte %10111101 ; |X XXXX X|
       .byte %00011000 ; |   XX   |
       .byte %00000000 ; |        |
       .byte %00011000 ; |   XX   |
       .byte %00011000 ; |   XX   |
       .byte %10111101 ; |X XXXX X|

gameColorTable
    .byte   BLACK              ; stairs
    .byte   WHITE              ; ghost
    .byte   GREEN_YELLOW + 7   ; bat
    .byte   RED + 7            ; first spider
    .byte   ORANGE + 7         ; second spider (and torch color)
    .byte   BLUE_CYAN + 7      ; third spider
    .byte   CYAN + 10          ; urn piece
    .byte   GREEN + 8          ; key
    .byte   BLUE + 2           ; floor 1
    .byte   ORANGE + 2         ; floor 2
    .byte   CYAN_GREEN + 2     ; floor 3
    .byte   BEIGE + 6          ; floor 4

    .byte   BLACK              ; BW Versions of Above
    .byte   WHITE
    .byte   WHITE - 3
    .byte   WHITE - 3
    .byte   WHITE - 3
    .byte   WHITE - 3
    .byte   CYAN + 10
    .byte   WHITE
    .byte   BLACK + 2
    .byte   BLACK + 4
    .byte   BLACK + 6
    .byte   BLACK + 8

	ORG $FFFC ; no bytes free at all

 	.word start
	.word start

