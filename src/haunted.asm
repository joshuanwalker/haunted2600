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

;=====================================
roomStairsStatus              = $9b    ; Bitfield: status of stairs in current room
playerCurrentRoom             = $9c    ; Player's current room number
; (5) | (4)
; ----+----
; (3) | (2) ; Room layout illustration
; ----+----
; (1) | (0)
urnAssembly0                  = $9d    ; Urn assembly progress (equals 8 when complete)
urnAssembly1                  = $9e    ; Urn assembly progress (equals $ff when complete)
urnAssembly2                  = $9f    ; Urn assembly progress (equals $ff when complete)
;=====================================


;=====================================
objFloorLoc                   = $a0    ; Floor location for objects (key, scepter, urn pieces)
masterKeyFloorLocation        = $a0    ; Floor location for master key
                                ; $a1 ; Scepter
                                ; $a2 ; Urn Left
                                ; $a3 ; Urn Center
                                ; $a4 ; Urn Right
;-------------------------------------
randFloorLoc                  = $a5    ; Randomized floor locations for objects
randFloorLoc0                 = $a5    ; Random floor location 0
randFloorLoc1                 = $a6    ; Random floor location 1
                                ; $a7 ; 2
                                ; $a8 ; 3
                                ; $a9 ; 4
;======================================

playerPosX                    = $aa    ; Player horizontal position

;======================================
objPosX                       = $ab    ; Horizontal position for objects (key, scepter, urn pieces)
masterKeyPosX                 = $ab    ; Horizontal position for master key
                                ; $ac ; Scepter
                                ; $ad ; Urn Left
                                ; $ae ; Urn Center
                                ; $af ; Urn Right
;-------------------------------------
randPosX                      = $b0    ; Randomized horizontal positions for objects
randPosX1                     = $b1    ; Random horizontal position 1
                                ; $b2 ; 2
                                ; $b3 ; 3
                                ; $b4 ; 4
;======================================

playerScrollY                 = $b5    ; Player vertical scroll position

;======================================
objPosY                       = $b6    ; Vertical position for objects (key, scepter, urn pieces)
masterKeyPosY                 = $b6    ; Vertical position for master key
                                ; $b7 ; Scepter
                                ; $b8 ; Urn Left
                                ; $b9 ; Urn Center
                                ; $ba ; Urn Right
;-------------------------------------
randPosY                      = $bb    ; Randomized vertical positions for objects
randPosY1                     = $bc    ; Random vertical position 1
                                ; $bd ; 2
                                ; $be ; 3
                                ; $bf ; 4
;======================================
objectRoomLocations           = $c0    ; Room location for objects (key, scepter, urn pieces)
                                ; $c1 ; Scepter
                                ; $c2 ; Urn Left
                                ; $c3 ; Urn Center
                                ; $c4 ; Urn Right
;-------------------------------------
randomRoomLocations           = $c5    ; Randomized room locations for objects
                                ; $c6 ; 1
                                ; $c7 ; 2
                                ; $c8 ; 3
                                ; $c9 ; 4
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
SpriteHorizPositions            = $dc    ; Sprite horizontal positions
p0PosY                        = $dc    ; Player 0 position Y
Player1HorizPosition            = $dd    ; Player 1 horizontal position
Missile0HorizPosition           = $de    ; Missile 0 horizontal position
torchTimer                      = $df    ; Torch timer (Missile 1 unused)
BallHorizPosition               = $e0    ; Ball horizontal position
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

Start           = $f000

    SEG     CODE
    ORG     $f000

Start
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
    jsr     SetSelectionVariables

MainGameLoop
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
    jsr     ReadSwitches            ; read console switches        
    lda     gameState
    and     #$43
    bne     .skipSubroutines
    jsr     Lf1a5
    jsr     CheckForWinning
.skipSubroutines
    jsr     Lf2b7
.waitTime1  
    lda     INTIM
    bne     .waitTime1
    sta     VBLANK
    lda     #(228)
    sta     TIM64T
    jsr     PlayfieldKernel
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
    jsr     HandlePlayerDeath 
    jsr     Lfb68
    jsr     CheckP0P1Collision
    jsr     CheckDoorwayPassages
.waitTime3
    lda     INTIM 
    bne     .waitTime3
    sta     VBLANK 
    beq     MainGameLoop            ; unconditional

; ******************************************************************

ReadSwitches SUBROUTINE
    lda     SWCHB                   ; read console switches        
    ror                             ; put reset into carry        
    bcc     resetPressed      
    jmp     ResetNotPressed
    
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
    lda     InitialGameVariables,x
    sta     playerLives,x
    dex
    bpl     .initVariablesLoop

    jsr     ScatterTheItems
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
    jsr     Lfd0a
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
    
ScatterTheItems SUBROUTINE
    jsr     nextRandom			; get a random value
    and     #$07				; make it from 0 to 7
    cmp     #$06				
    bcs     ScatterTheItems		; keep value from 0 to 5
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
    lda     RandomLocationsTableH,y
    sta     randPosX,x		; and random horiz positions
    lda     RandomLocationsTableV,y
    sta     randPosY,x		; and random vert positions
    dex
    bpl     .randomizeLoop
    rts

; ******************************************************************
    
ResetNotPressed SUBROUTINE
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
SetSelectionVariables
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
    
CheckForWinning SUBROUTINE
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
    jsr     Lf88c
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
    
Lf1a5 SUBROUTINE
    jsr     Lf1bf
    lda     #0
    sta     doorwayMovementFlag
    lda     playerScrollY
    cmp     #$26
    bcc     Lf1bc
    cmp     #$d7
    bcs     Lf1ba
    lda     #$26
    bne     Lf1bc                   ; unconditional
Lf1ba
    sbc     #$af
Lf1bc
    sta     playerAbsPosY
    rts

; ******************************************************************
    
Lf1bf SUBROUTINE
    lda     #0
    sta     playerDeltaX
    sta     playerDeltaY
    lda     roomStairsStatus
    and     #$7f
    sta     roomStairsStatus
    bit     doorwayMovementFlag
    bvs     Lf206
    bit     movementValue 
    bvs     Lf1e3
    bpl     Lf206
    inc     playerDeltaX
    lda     playerPosX
    cmp     #$94
    beq     Lf254
    jsr     Add_8_toHPos
    jmp     Lf1ee
    
Lf1e3
    dec     playerDeltaX
    lda     playerPosX
    cmp     #$04
    beq     Lf254
    jsr     DoPlayerFineCalc
Lf1ee
    jsr     Add_2_toVPos
    bne     Lf201                   ; unconditional?
     
    jsr     Add_0_toVPos
    bne     Lf201                   ; unconditional?
    
    lda     playerPosX
    clc
    adc     playerDeltaX
    sta     playerPosX
    bne     Lf206                   ; unconditional?
Lf201
    ldx     #NOISY3SOUND		    ; sound #3
    jsr     Lf88c
Lf206
    bit     doorwayMovementFlag
    bmi     .noMovement
    lda     movementValue
    and     #$30
    beq     .noMovement
    cmp     #$20
    beq     Lf22e
    inc     playerDeltaY
    lda     playerScrollY
    cmp     #$fb
    beq     Lf260
    jsr     Add_7_toHPos
    jsr     Add_3_toVPos
    bne     Lf24e
    jsr     Add_0_toHPos
    jsr     Add_3_toVPos
    bne     Lf24e
    beq     Lf246                   ; unconditional
Lf22e
    dec     playerDeltaY
    lda     playerScrollY
    cmp     #$01
    beq     Lf260
    jsr     Add_7_toHPos
    jsr     Lf28e
    bne     Lf24e
    jsr     Add_0_toHPos
    jsr     Lf28e
    bne     Lf24e
Lf246
    lda     playerScrollY 
    clc 
    adc     playerDeltaY
    sta     playerScrollY
    rts
Lf24e
    ldx     #NOISY3SOUND			; sound #3
    jsr     Lf88c
.noMovement
    rts

Lf254
    lda     roomStairsStatus
    ora     #$80
    sta     roomStairsStatus
    cmp     #$8f
    beq     Lf201
    bne     Lf206                   ; unconditional

Lf260
    lda     roomStairsStatus
    ora     #$80
    sta     roomStairsStatus
    cmp     #$8f
    beq     Lf24e
    rts

; ******************************************************************
    
Add_7_toHPos
    lda     playerPosX
    clc
    adc     #$07
    bne     .doFineCalculation       ; unconditional?
Add_8_toHPos
    lda     playerPosX
    clc
    adc     #$08
    bne     .doFineCalculation       ; unconditional?
Add_0_toHPos
    lda     playerPosX
    bne     .doFineCalculation       ; unconditional?

DoPlayerFineCalc
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
    
Add_0_toVPos
    lda     playerScrollY
    bne     DoorCheck               ; unconditional?
Lf28e
    ldy     playerScrollY
    dey
    tya
    jmp     DoorCheck
Add_2_toVPos
    lda     playerScrollY
    clc
    adc     #$02
    bne     DoorCheck               ; unconditional?
Add_3_toVPos
    lda     playerScrollY
    clc
    adc     #$03

DoorCheck
    lsr
    lsr
    lsr
    lsr
    tax
    ldy     calculatedFineValue
    tya
    and     #$08
    beq     DoorCheckBitmask
    cpx     #$08
    rts
DoorCheckBitmask
    lda     WallBitPattern,x
    and     BitmaskThing,y
    rts

; ******************************************************************
    
Lf2b7
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
    beq     Lf2d8
    and     #$04
    beq     DoRollingEyes
    lda     frameCount
    bne     Lf2d8
    jsr     SetSelectionVariables
Lf2d8
    jmp     Lf315
    
DoRollingEyes
    lda     rollingEyesTimer
    beq     .rollingDone
    jsr     Lf57d
    lda     frameCount
    lsr
    and     #$07
    tax
    lda     RollingEyesTable,x
    bne     ResetEyeGraphics
.rollingDone
    ldx     itemGatheredFlag
    beq     Lf300
    ldx     itemActionIndex
    lda     #H_FONT
    sta     spriteHeight
    lda     playerCurrentFloor
    sta     objFloorLoc,x
    jsr     Lf526
    bpl     Lf315
Lf300
    bit     torchAnimationIdx
    bvc     Lf312
    bit     torchAnimationIdx
    bmi     Lf30d
    jsr     Lf4f1
    bpl     Lf315
Lf30d
    jsr     Lf57d
    bpl     Lf34d
Lf312
    jsr     Lf57d 
Lf315
    lda     SWCHA                   ; get joystick value

ResetEyeGraphics
    ldy     #%11100111              ; no pupils
    sty     eyeRAM
    sty     eyeRAM + 1
    sty     eyeRAM + 2
    ldy     #%10100101              ; center pupils
    ldx     #$01
    rol
    bcs     Lf329
    ldy     #%11000110              ; right pupils
Lf329
    rol
    bcs     Lf32e
    ldy     #%01100011              ; left pupils
Lf32e
    rol
    bcs     Lf332
    dex 
Lf332
    rol
    bcs     Lf336
    inx
Lf336
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
    bne     Lf397                   ; unconditional?

Lf34d
    lda     tmpBallHorizPosition
    sta     BallHorizPosition
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
    lda     TorchLSBValues,x
    sta     sprite1GraphicPTRs
    lda     #>TorchGraphics
    sta     sprite1GraphicPTRs + 1
    lda     playerPosX
    sec
    sbc     #$0d
    bcs     Lf37b
    adc     #$a0
    ldx     #XMAX
    bne     Lf381                   ; unconditional

Lf37b
    cmp     #$84
    bcc     Lf387
    ldx     #XMIN
Lf381
    stx     BallHorizPosition
    ldx     #ENABLE_BM
    stx     ENABL
Lf387
    tax
    lda     #H_TORCH
    sta     playerVertSize
    lda     #QUAD_SIZE
    sta     NUSIZ1
    ldy     #$04
    lda     playerAbsPosY
    sec
    sbc     #$0c
Lf397
    sta     playerVertOffset
    stx     Player1HorizPosition
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
    bne     Lf3c8                   ; unconditional

.notRolling
    lda     creaturesInRoom
    beq     .skipRandomLightning
    bit     SWCHB
    bvs     .skipRandomLightning
    lda     #$27
Lf3c8
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
    beq     Lf3ff
    lda     frameCount
    bne     Lf3f6
    inc     colorCycTimer
Lf3f6
    lda     colorCycTimer
    ora     #$10
    sta     colorCycTimer
    tay 
    bne     Lf401
Lf3ff
    ldx     #$ff
Lf401
    sty     colorEOR
    stx     colorCycleMode
        
    ldx     #$04
.setColorsLoop
    lda     colorTableRAM,x
    clc 
    adc     tmpColorTableIndex		; add palette index
    tay 
    lda     GameColorTable,y
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
    beq     PositionObjects
    lda     frameCount
    and     lightningColorMask
    sta     wallColor

PositionObjects
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
    adc     SpriteHorizPositions,x 
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
.PositionObjectsDly
    dey                             ;2        
    bpl     .PositionObjectsDly     ;2/3      
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
    bvc     Lf4a1                   ;2/3      
    bit     doorwayCrossingStatus   ;3        
    bmi     Lf4a1                   ;2/3      
    lda     gameSelection           ;3        
    beq     Lf4a1                   ;2/3      
    lda     pfScrollOffsetB                  ;3        
    beq     Lf48c                   ;2/3      
    jsr     HandlePlayfieldScrolling ;6        
    bmi     Lf48c                   ;2/3      
    sta     calculatedFineValue     ;3 = 47
Lf48c
    lda     pfScrollOffsetA                  ;3        
    beq     Lf4a1                   ;2/3      
    tax                             ;2        
    jsr     HandlePlayfieldScrolling ;6        
    cmp     #$50                    ;2        
    bcs     Lf4a1                   ;2/3      
    sta     lightningColorMask      ;3        
    lda     #$08                    ;2        
    sta     temporaryOne            ;3        
    txa                             ;2        
    bne     Lf4af                   ;2/3 = 29
Lf4a1
    lda     playerScrollY     ;3        playfield gfx scoll calculations
    cmp     #$26                    ;2        
    bcc     Lf4bd                   ;2/3      
    cmp     #$d6                    ;2        
    bcs     Lf4c3                   ;2/3      
    adc     #$2a                    ;2        
    sta     temporaryOne            ;3 = 16
Lf4af
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
Lf4bd
    ldx     #$1e                    ;2        
    lda     #$00                    ;2        
    beq     Lf4c7                   ;2/3 = 6 unconditional

Lf4c3
    ldx     #$00                    ;2        
    lda     #$0f                    ;2 = 4
Lf4c7
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
    
ItemIDTable
    .byte   $07,$01,$06,$06,$06    ; key, scepter, urn, urn, urn

; ******************************************************************
    
HandlePlayfieldScrolling SUBROUTINE
    clc                             ;2        
    adc     playerAbsPosY        ;3        
    sec                             ;2        
    sbc     playerScrollY     ;3        
    rts                             ;6 = 16

; ******************************************************************
    
SetInvItemPTRs SUBROUTINE
    txa                             ;2        
    cpx     #$02                    ;2        
    bcc     .keyOrScepter           ;2/3      
    lda     roomStairsStatus,x      ;4 = 10  (x must = 2,3, or 4) for urn pieces
.keyOrScepter
    asl                             ;2        
    asl                             ;2        
    asl                             ;2        
    adc     #H_FONT                 ;2        
    ldx     #>CreatureGraphics      ;2        
    rts                             ;6 = 16

; ******************************************************************
    
Lf4f1 SUBROUTINE
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
    jsr     TorchLightUpItem        ;6        
    beq     Lf4fa                   ;2/3!= 17
Lf526
    ldy     itemGatheredFlag        ;3        
    bne     .notGathered            ;2/3      
    lda     ItemIDTable,x           ;4        
    sta     object1Color            ;3 = 12
.notGathered
    stx     itemLastSeen            ;3        
    jsr     SetInvItemPTRs          ;6        
    sta     sprite0GraphicPTRs      ;3        
    stx     sprite0GraphicPTRs + 1  ;3        
    lda     #H_FONT                 ;2        
    sta     spriteHeight            ;3        
    rts                             ;6 = 26

; ******************************************************************
    
TorchLightUpItem SUBROUTINE
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
    bcs     Lf55e                   ;2/3      
    lsr                             ;2        
    bpl     Lf560                   ;2/3 = 12 unconditional
Lf55e
    lsr     temporaryOne            ;5 = 5
Lf560
    clc                             ;2        
    adc     temporaryOne            ;3        
    cmp     #$11                    ;2   torch's effective light range      
    bcs     Lf595                   ;2/3      
    lda     objPosX,x  ;4        
    sta     p0PosY    ;3        
    lda     objPosY,x   ;4 = 20
Lf56d
    jsr     HandlePlayfieldScrolling ;6        
    cmp     #$50                    ;2        
    bcc     Lf578                   ;2/3      
    cmp     #$f9                    ;2        
    bcc     Lf595                   ;2/3 = 14
Lf578
    sta     playerPFScrollValue     ;3        
    lda     #$01                    ;2        
    rts                             ;6 = 11

; ******************************************************************
    
Lf57d SUBROUTINE
    ldx     collisionIndex
Lf57f
    dex
    bpl     Lf598
    ldx     numberOfCreatures
    bit     torchAnimationIdx
    bvc     Lf598
    inx
    jsr     CheckForStairs
    beq     Lf591
    stx     collisionIndex
    rts
Lf591
    cpx     collisionIndex
    bne     Lf57f
Lf595
    jmp     Lf4fe
Lf598
    jsr     Lf59e
    beq     Lf591
    rts

; ******************************************************************
    
Lf59e SUBROUTINE
    lda     randFloorLoc,x
    cmp     playerCurrentFloor
    bne     Lf595
    lda     gameSelection
    beq     Lf5ba
    lda     rollingEyesTimer
    bne     Lf5ba
    lda     creaturesInRoom
    beq     Lf595
    lda     randomRoomLocations,x
    cmp     playerCurrentRoom
    beq     Lf5ba
    cmp     playerDoorCrossing
    bne     Lf595
Lf5ba
    inx
    stx     object1Color
    dex
    lda     randPosX,x
    sta     p0PosY
    lda     randPosY,x
    jsr     Lf56d
    beq     Lf591
    stx     collisionIndex
    lda     #<CreatureGraphics
    clc
    adc     CreatureLSBOffsetTable,x
    bit     randomSeed
    bvs     .randomAnimateCreature
    adc     #H_CREATURE				; randomly does frame 1 or frame 2
.randomAnimateCreature
    sta     sprite0GraphicPTRs
    lda     #>CreatureGraphics
    sta     sprite0GraphicPTRs + 1
    lda     #H_CREATURE
    sta     spriteHeight
    rts
    
CreatureLSBOffsetTable
    .byte H_CREATURE * 2 * ID_GHOST
    .byte H_CREATURE * 2 * ID_BAT
    .byte H_CREATURE * 2 * ID_SPIDER
    .byte H_CREATURE * 2 * ID_SPIDER
    .byte H_CREATURE * 2 * ID_SPIDER

; ******************************************************************
    
CheckForStairs SUBROUTINE
    lda     roomStairsStatus
    and     #$0f
    cmp     #$0f
    beq     .noStairs
    tay
    cpy     #$04
    bcc     CalculateStairGraphic
    tya
    and     #$03
    eor     #$01
CalculateStairGraphic
    sta     temporaryOne
    asl
    asl
    asl
    asl
    clc
    adc     temporaryOne
    adc     #<StairGraphics
    sta     sprite0GraphicPTRs
    lda     #0
    adc     #>StairGraphics
    sta     sprite0GraphicPTRs + 1
    tya
    and     #$03
    tay
    lda     TorchClippingValues,y
    jsr     HandlePlayfieldScrolling
    sta     playerPFScrollValue
    ldy     playerCurrentRoom
    lda     StairHPositionsTable,y
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
    
PlayfieldKernel
    sta     CXCLR                   ;3        
    ldy     #$00                    ;2 = 5
.blackLinesLoop
    lda     scanline                ;3        
    cmp     lightningColorMask      ;3        
    beq     PlayFieldLoop           ;2/3      
    sta     WSYNC                   ;3 = 11
;---------------------------------------
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    dec     scanline                ;5        
    bpl     .blackLinesLoop         ;2/3 = 7
PlayFieldLoop
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
    lda     PF0Data,x               ;4        
    sta     PF0                     ;3        
    lda     PF1Data,x               ;4        
    sta     PF1                     ;3        
    lda     PF2Data,x               ;4        
    sta     PF2                     ;3        
    lda     scanline                ;3        
    sec                             ;2        
    sbc     playerPFScrollValue     ;3        
    cmp     spriteHeight            ;3        
    bcs     .turnSpriteOff0         ;2/3      
    tay                             ;2        
    lda     (sprite0GraphicPTRs),y  ;5        
    tay                             ;2 = 52
PlayFieldLoop2
    dec     scanline                ;5        
    lda     scanline                ;3        
    cmp     torchesHNumberPTRs      ;3        
    beq     EndPlayfieldArea        ;2/3      
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
    jmp     PlayFieldLoop           ;3 = 28
    
.turnSpriteOff1
    ldy     #$00                    ;2        
    beq     PlayFieldLoop           ;3 = 5 unconditional

.turnSpriteOff0
    ldy     #$00                    ;2        
    beq     PlayFieldLoop2          ;3 = 5 unconditional

EndPlayfieldArea
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
    bmi     StatusKernel            ;2/3      
    sta     WSYNC                   ;3 = 10
;---------------------------------------
    sta     WSYNC                   ;3 = 3
;---------------------------------------
    bpl     .finishPlayfieldBottom  ;2/3 = 2 unconditional

StatusKernel
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
    jsr     SetInvItemPTRs          ;6 set PTRs for Inventory Item        
    stx     temporaryOne + 1        ;3 set MSB for item       
    bit     itemBeingCarried        ;3        
    bpl     .carryingSomething      ;2/3      
    lda     #<Blank                 ;2 = 32 set 'blank' if no item being carried
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
TorchCountLoop
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
    bpl     TorchCountLoop          ;2/3      
    iny                             ;2        
    sty     GRP1                    ;3     kernel just 'ends' here?  and goes into audio stuff
   
    lda     gameState               ;3        
    bne     Lf7a3                   ;2/3      
    lda     creaturesInRoom                  ;3        
    bne     Lf773                   ;2/3      
    ldx     windSoundCounter                  ;3        
    cpx     #$7f                    ;2        
    beq     SilenceAudioZero        ;2/3      
    sta     audioWindSoundBit       ;3 = 49
Lf773
    lda     windSoundCounter                  ;3        
    tay                             ;2        
    cmp     #$7f                    ;2        
    beq     Lf78a                   ;2/3      
    and     #$0f                    ;2        
    bne     Lf78c                   ;2/3      
    tya                             ;2        
    beq     Lf78a                   ;2/3      
    bmi     Lf78a                   ;2/3      
    ldx     randomSeed              ;3        
    lda     Start,x                 ;4        ; using program code as random wind values (!)
    and     #$01                    ;2 = 28
Lf78a
    sta     audioWindSoundBit
Lf78c
    lda     audioWindSoundBit
    bne     Lf794
    inc     windSoundCounter
    bne     Lf796 
Lf794
    dec     windSoundCounter
Lf796
    ldx     #NOISESOUND
    tya
    lsr
    lsr
    lsr
    ora     #$10
    tay
    eor     #$0f
    bpl     SetAudioZero			; unconditional?
Lf7a3
    lsr
    bcc     Lf7c4
    lda     audioFrequency0Value
    bne     Lf7b7
    lda     randomSeed
    and     #$70
    lsr
    lsr
    clc
    adc     #$10
    sta     audioFrequency0Value
    bpl     SilenceAudioZero 
Lf7b7
    lda     audioFrequency0Value
    eor     #$ff
    tay 
    dec     audioFrequency0Value
    ldx     #NOISESOUND
    lda     #$0c
    bpl     SetAudioZero            ; unconditional

Lf7c4
    cmp     #$22
    bne     SilenceAudioZero
    lda     frameCount
    lsr
    lsr
    lsr
    lsr
    and     #$03
    tax
    lda     EndingMusic,x
    tay
    lda     #$09
    ldx     #TWILIGHTSOUND
    bpl     SetAudioZero            ; unconditional

SilenceAudioZero
    lda     #0
SetAudioZero
    stx     AUDC0
    sty     AUDF0
    sta     AUDV0
    lda     gameState
    bne     SilenceAudioOne
    ldy     audioSoundIndex
    lda     audioVolume1Value
    bne     Lf7f4
    sta     AUDV1
    sta     audioSoundIndex
    jmp     .skipSoundEleven

; CHECK SOUND #2
    
Lf7f4
    dec     audioVolume1Value
    cpy     #2
    bne     .skipSoundTwo
    ldx     #BASSSOUND
    lda     audioVolume1Value
    eor     #$03
Lf800
    tay
    lda     #$08
    bne     SetAudioOne             ; unconditional

; CHECK SOUND #3

.skipSoundTwo
    cpy     #3
    bne     .skipSoundThree
    ldx     #THUNKSOUND
Lf80b
    lda     audioVolume1Value
    bpl     Lf800

; CHECK SOUND #4

.skipSoundThree
    cpy     #4
    bne     .skipSoundFour
    lda     #$06
    ldx     #NOISESOUND
    ldy     #$0f
    bne     SetAudioOne             ; unconditional

; CHECK SOUND #5

.skipSoundFour
    cpy     #5
    bne     .skipSoundFive
    lda     audioVolume1Value
    eor     #$03
Lf823
    ldy     #$08
    ldx     #BASSSOUND
    bne     SetAudioOne             ; unconditional

; CHECK SOUND #6

.skipSoundFive
    cpy     #6
    bne     .skipSoundSix
    lda     audioVolume1Value
    bpl     Lf823

; CHECK SOUND #7

.skipSoundSix
    cpy     #7
    bne     .skipSoundSeven
    ldx     #URNCENTERSOUND
    bne     Lf80b                   ; unconditional

; CHECK SOUND #8

.skipSoundSeven
    cpy     #8
    bne     .skipSoundEight
    ldx     #NOISESOUND
    bne     Lf80b                   ; unconditional

SilenceAudioOne
    ldy     #0
SetAudioOne
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
    lda     StairwaySound,x
    ldx     #BASSSOUND
    ldy     #$0a
    bne     SetAudioOne             ; unconditional

.skipSoundTen
    cpy     #11
    bne     .skipSoundEleven
    lda     audioVolume1Value
    tay
    and     #$04
    beq     SilenceAudioOne
    ldx     #BASSSOUND
    lda     #$01
    bne     SetAudioOne             ; unconditional

.skipSoundEleven
    lda     SWCHA
    eor     #$ff
    beq     SilenceAudioOne
    lda     frameCount
    and     #$07
    cmp     #$03
    bcs     SilenceAudioOne
    ldy     #$0f
    ldx     #FOOTSOUND
    lda     #$18                    ; setup for footsteps sound
    bne     SetAudioOne             ; unconditional

Lf88c
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
    rts						; I think this returns from 'PlayfieldKernel' itself
    
audioVolume1ValueTable
    .byte   $ff,$ff,$15,$15,$01,$04,$04,$0a,$15,$3f,$3f,$1f

; ******************************************************************
    
HandlePlayerDeath SUBROUTINE
    lda     gameState
    and     #$fd
    sta     gameState
    ror
    bcc     Lf8ec
    dec     rollingEyesTimer
    bne     Lf8ec
    lda     #0
    sta     gameState
    sta     audioSoundIndex
    lda     playerLives
    bne     .playerStillHasLives
    jsr     SetSelectionVariables
.playerStillHasLives
    jsr     ScatterTheItems
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
    
Lf8ec SUBROUTINE
    lda     frameCount
    and     #$07
    sta     scanline
    tay
    lda     #$88
    and     BitmaskThing,y
    sta     floorNumberPTRs
    lda     #0
    sta     creaturesInRoom
    ldx     numberOfCreatures
Lf900
    lda     randFloorLoc,x
    cmp     playerCurrentFloor
    bne     Lf945
    lda     randomRoomLocations,x
    cmp     playerCurrentRoom
    beq     Lf910
    cmp     playerDoorCrossing
    bne     Lf945
Lf910
    inc     creaturesInRoom
    lda     rollingEyesTimer
    bne     Lf94c
    txa
    bne     Lf91f
    lda     gameSelection
    cmp     #$07
    bcs     Lf925
Lf91f
    lda     itemBeingCarried
    cmp     #$01
    beq     Lf94c
Lf925
    jsr     Lfa6f
    beq     Lf9a0
    lda     playerPosX
    sec
    sbc     Lfdec,x
    bcs     Lf934
    lda     playerPosX
Lf934
    sta     temporaryOne
    lda     playerScrollY
    sec
    sbc     Lfdf1,x
    bcs     Lf940
    lda     playerScrollY
Lf940
    sta     temporaryOne + 1
    jmp     Lf983
Lf945
    jsr     Lfa68
    beq     Lf9a0
    bne     Lf950                   ; unconditional
Lf94c
    lda     floorNumberPTRs
    beq     Lf9a0
Lf950
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
    lda     RandomLocationsTableOffsets,y
    clc
    adc     temporaryOne
    tay
    lda     RandomLocationsTableH,y
    sta     temporaryOne
    lda     RandomLocationsTableV,y
    sta     temporaryOne + 1
    cmp     randPosY,x
    bne     Lf983
    lda     randPosX,x
    cmp     temporaryOne
    bne     Lf983
    lda     creatureProcessMask
    ora     BitmaskThing,x
    sta     creatureProcessMask
    bne     Lf9a0
Lf983
    lda     randPosX,x
    cmp     temporaryOne
    bcc     Lf98f
    beq     Lf991
    dec     randPosX,x
    bne     Lf991
Lf98f
    inc     randPosX,x
Lf991
    lda     randPosY,x
    cmp     temporaryOne + 1
    bcc     Lf99e 
    beq     Lf9a0
    dec     randPosY,x
    jmp     Lf9a0
Lf99e
    inc     randPosY,x
Lf9a0
    dex 
    bmi     Lf9a6
    jmp     Lf900
Lf9a6
    ldx     #$00
    lda     creatureProcessMask
Lf9aa
    ror
    bcs     Lf9b5
    inx
    cpx     numberOfCreatures
    beq     Lf9aa
    bcc     Lf9aa
    rts

Lf9b5 SUBROUTINE
    lda     creatureProcessMask
    eor     BitmaskThing,x
    sta     creatureProcessMask
    lda     objectRoomLocations,x
    tay
    and     #$30
    beq     Lf9f3
    cmp     #$10
    bne     Lf9db
    tya
    and     #$c0
    sta     temporaryOne
    clc
    rol
    rol
    rol
    tay
    lda     RoomToRoomOffsets,y
    clc
    adc     randomRoomLocations,x
    sta     randomRoomLocations,x
    bpl     Lf9ec
Lf9db
    cmp     #$30
    beq     Lf9e3
    dec     randFloorLoc,x
    bpl     Lf9e5
Lf9e3
    inc     randFloorLoc,x
Lf9e5
    tya
    and     #$c0
    eor     #$40
    sta     temporaryOne
Lf9ec
    lda     randomRoomLocations,x
    ora     temporaryOne
    sta     objectRoomLocations,x
    rts

; ******************************************************************
    
Lf9f3 SUBROUTINE
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
    bpl     Lfa13
Lfa07
    inc     colorCycleMode
    lda     colorCycleMode
    and     #$03
    sta     colorCycleMode
    cmp     lightningColorMask
    beq     Lfa1d
Lfa13
    cmp     torchesHNumberPTRs
    beq     Lfa07
    jsr     Lfa1f
    beq     Lfa07
    rts
Lfa1d
    lda     torchesHNumberPTRs
Lfa1f
    sta     torchesLNumberPTRs
    lda     randFloorLoc,x
    sta     tmpFloorNumber
    lda     randomRoomLocations,x
    sta     temporaryOne
    jsr     Lfdae
    bmi     Lfa41
    beq     Lfa39
    lda     gameSelection
    cmp     #$08
    beq     Lfa39
    txa
    bne     skipOnOut
Lfa39
    lda     tmpRoomNumber
    sta     temporaryOne
    lda     #$01
    bne     Lfa56                   ; unconditional

Lfa41
    cmp     #$ff
    beq     skipOnOut
    ldy     randFloorLoc,x
    lda     randomRoomLocations,x
    jsr     Lfd4c
    bcs     skipOnOut
    bne     Lfa54
    lda     #$02
    bne     Lfa56                   ; unconditional

Lfa54
    lda     #$03
Lfa56
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
    
Lfa68 SUBROUTINE
    lda     creaturesPresentMask
    and     BitmaskThing,x
    beq     Lfa83
Lfa6f
    lda     gameSelection
    cmp     #$07
    bcc     .skipHardCreatureSpeeds
    lda     HardCreatureSpeeds,x
    bne     .doSetCreatureSpeeds
.skipHardCreatureSpeeds
    lda     EasyCreatureSpeeds,x
.doSetCreatureSpeeds
    ldy     scanline
    and     BitmaskThing,y
    rts
Lfa83
    lda     floorNumberPTRs
    rts
    
EasyCreatureSpeeds
    .byte   $aa ; fast ghost
    .byte   $91 ; med bat
    .byte   $88 ; slow spider
    .byte   $88 ; slow spider
    .byte   $88 ; slow spider

HardCreatureSpeeds
    .byte   $aa ; fast ghost
    .byte   $aa ; fast bat
    .byte   $91 ; med spider
    .byte   $91 ; med spider
    .byte   $91 ; med spider

; ******************************************************************
    
CheckP0P1Collision SUBROUTINE
    lda     rollingEyesTimer
    bne     skipOnOut
    bit     CXPPMM|$30
    bpl     skipOnOut
    lda     itemGatheredFlag
    bne     skipOnOut
    bit     torchAnimationIdx
    bvc     Lfaa3
    bpl     Lfaac
    rts
Lfaa3
    lda     collisionIndex
    cmp     numberOfCreatures
    beq     DeadPlayerHandler
    bcc     DeadPlayerHandler
    rts
Lfaac
    ldx     #6				; sound #6
    jsr     Lf88c
    lda     itemBeingCarried
    bmi     Lfafe
    cmp     itemLastSeen
    beq     Lfafe
    cmp     #$02
    bcc     Lfafe
    lda     itemLastSeen
    cmp     #$02
    bcc     Lfafe
    clc
    adc     itemBeingCarried
    tay
    ldx     #7				; sound #7
    jsr     Lf88c
;^^^^^^^^^^^^ section dealing with urn piece assembly
    lda     urnAssembly1
    ora     urnAssembly2
    bpl     Lfae5
    ldx     #11				; sound #11
    jsr     Lf88c
    lda     #$08			; the following stuff sets "urn complete"
    sta     urnAssembly0		;
    lda     #$ff			;
    sta     urnAssembly1		;
    sta     urnAssembly2		;
    lda     #$02
    bne     Lfb00			; unconditional
Lfae5
    ldx     #$02
    cpy     #$05
    bne     Lfaec
    dex 
Lfaec
    lda     #$ff
    sta     urnAssembly0,x
    ldx     #$00
    cpy     #$07
    bne     Lfaf7
    inx 
Lfaf7
    sty     urnAssembly0,x
    inx 
    inx 
    txa
    bne     Lfb00
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Lfafe
    lda     itemLastSeen
Lfb00
    pha
    bit     itemBeingCarried
    bmi     Lfb08
    jsr     Lfbe2
Lfb08
    pla
    sta     itemBeingCarried 
allDone
    rts

; ******************************************************************
    
DeadPlayerHandler SUBROUTINE
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
    
Lfb2b SUBROUTINE
    lda     gameSelection
    cmp     #$05
    bcc     Lfb92
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
Lfb47
    ldx     numberOfCreatures
Lfb49
    lda     #0
    sta     creaturesPresentMask
Lfb4d
    lda     randFloorLoc,x
    cmp     playerCurrentFloor
    bne     .continueLoop
    lda     randomRoomLocations,x
    cmp     playerCurrentRoom
    bne     .continueLoop
    lda     temporaryOne
    sta     objectRoomLocations,x
    lda     creaturesPresentMask
    ora     BitmaskThing,x
    sta     creaturesPresentMask
.continueLoop
    dex
    bpl     Lfb4d
    rts

; ******************************************************************
    
Lfb68 SUBROUTINE
    lda     roomStairsStatus
    and     #$0f
    cmp     #$0f
    beq     Lfb92
    bit     roomStairsStatus
    bpl     Lfb90
    bvs     Lfb92
    jsr     Lfb2b
    ldx     #9				; sound #9
    lda     roomStairsStatus
    and     #$04 
    beq     Lfb85
    inc     playerCurrentFloor 
    bne     Lfb88
Lfb85
    dec     playerCurrentFloor
    inx					; inc to sound #10
Lfb88
    jsr     Lf88c
    jsr     Lfd0a
    ora     #$40
Lfb90
    sta     roomStairsStatus
Lfb92
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
    jsr     Lf88c
    jsr     Lfbe2
    lda     #$ff
    sta     itemBeingCarried	; set to 'no item held'
    bne     .goToRTS
.noItemHeld
    lda     itemGatheredFlag
    beq     .goToRTS
    bit     CXP0FB|$30
    bmi     Lfbfb
    lda     #$00
    sta     itemGatheredFlag
    beq     .goToRTS
Lfbe2
    lda     #$05
    sta     itemGatheredFlag
    lda     itemBeingCarried
    sta     itemActionIndex
    lda     playerDeltaX
    tay
    ora     playerDeltaY
    beq     Lfbfb
    iny
    iny
    ldx     playerDeltaY
    inx
    jsr     Lfc0b
    bcc     .goToRTS
Lfbfb
    dec     itemGatheredFlag
    beq     .goToRTS
    ldx     itemGatheredFlag
    dex
    ldy     itemGatheredFlag
    dey 
    jsr     Lfc0b
    bcs     Lfbfb
.goToRTS
    rts

; ******************************************************************
    
Lfc0b SUBROUTINE
    lda     PlayerScrollOffsets + 1,x
    ldx     playerScrollY
    cpx     #$f4
    bcc     Lfc18
    cmp     #$0a
    beq     Lfc3a
Lfc18
    clc
    adc     playerScrollY
    cmp     #$f4
    bcs     Lfc3a
    ldx     itemActionIndex
    sta     objPosY,x
    jsr     HandlePlayfieldScrolling
    sta     playerPFScrollValue
    lda     PlayerScrollOffsets,y
    clc
    adc     playerPosX
    cmp     #XMAX
    bcs     Lfc3a
    ldx     itemActionIndex
    sta     objPosX,x
    sta     p0PosY
    clc
    rts
Lfc3a
    sec
    rts

; ******************************************************************
    
CheckDoorwayPassages SUBROUTINE
    lda     #$05
    sta     temporaryOne
    ldx     #$00
Lfc42
    ldy     temporaryOne
    lda     JoystickValuesIndex,y
    tay 
    lda     movementValue
    and     JoystickValues,y
    beq     Lfc61
    lda     playerPosX
    cpy     #$02
    bcc     Lfc57
    lda     playerScrollY
Lfc57
    cmp     DoorwayBoundaryTable,x
    beq     Lfca5
    cmp     DoorwayBoundaryTable + 1,x
    beq     Lfc7e
Lfc61
    inx
    inx
    dec     temporaryOne
    bpl     Lfc42
jmpIntoAnRTS
    rts
    
DoorwayBoundaryTable
    .byte   $54,$5c
    .byte   $a4,$ac
    .byte   $58,$4f
    .byte   $a8,$9f
    .byte   $4f,$47
    .byte   $48,$50

JoystickValuesIndex
    .byte   $01,$00,$03,$03,$02,$02

JoystickValues
    .byte   ~MOVE_LEFT,~MOVE_RIGHT,~MOVE_UP,~MOVE_DOWN

; ******************************************************************
    
Lfc7e SUBROUTINE
    bit     doorwayCrossingStatus
    bpl     jmpIntoAnRTS
    tya
    ora     #$80
    cmp     doorwayCrossingStatus
    beq     Lfc8d
    lda     playerDoorCrossing
    sta     playerCurrentRoom
Lfc8d
    jsr     Lfcd8
    sty     doorwayCrossingStatus
    lda     RoomToRoomOffsets,y
    clc
    adc     playerCurrentRoom
    sta     playerCurrentRoom
    lda     #$ff
    sta     playerDoorCrossing
    jsr     Lfd0a
    ldx     #$08
    bne     Lfcd1
Lfca5
    bit     doorwayCrossingStatus
    bmi     jmpIntoAnRTS
    tya
    jsr     Lfda6
    beq     Lfcb3
    lda     itemBeingCarried
    bne     Lfcc6
Lfcb3
    ldy     torchesLNumberPTRs
    lda     RoomToRoomOffsets,y
    clc 
    adc     playerCurrentRoom
    sta     playerDoorCrossing
    tya 
    ora     #$80
    sta     doorwayCrossingStatus
    ldx     #$04
    bne     Lfcd1                   ; unconditional
Lfcc6
    lda     torchesLNumberPTRs
    lsr
    tax
    lda     BitmaskThing + 6,x 
    sta     doorwayMovementFlag
    ldx     #2					; sound #2
Lfcd1
    lda     gameSelection
    beq     jmpIntoAnRTS
    jmp     Lf88c

; ******************************************************************
    
Lfcd8 SUBROUTINE
    sty     tmpYRegisterSaveLocation
    lda     gameSelection
    cmp     #$05
    bcc     .restoreYRegisterValue
    lda     Lfd06,y
    ora     #$10
    sta     temporaryOne
    tya
    jsr     Lfda6
    pha
    lda     temporaryOne
    ora     tmpRoomNumber
    sta     temporaryOne
    pla
    beq     Lfcfc
    ldx     #0
    jsr     Lfb49
    bmi     .restoreYRegisterValue			; unconditional (surely...)
Lfcfc
    jsr     Lfb47
.restoreYRegisterValue
    ldy     tmpYRegisterSaveLocation
    rts

; ******************************************************************
    
PlayerScrollOffsets
    .byte   0, 10, 0, -10

Lfd06
    .byte   $00,$40,$80,$c0                 ; $fd06 (*)
    
Lfd0a SUBROUTINE
    lda     #2
    jsr     Lfd6c
    sta     pfScrollOffsetA
    lda     #3
    jsr     Lfd6c
    sta     pfScrollOffsetB
    lda     #0
    jsr     Lfd6c
    beq     .rightSideDoor
    lda     #DOOR_LEFT_XPOS + 8		; Ball & M0 positions
    ldx     #DOOR_LEFT_XPOS		; door on left
    bne     .setDoorPosition		; unconditional
.rightSideDoor
    lda     #DOORrIGHT_XPOS		; Ball & M0 positions
    ldx     #DOORrIGHT_XPOS + 8	; door on right
.setDoorPosition
    sta     tmpBallHorizPosition
    stx     Missile0HorizPosition
    ldy     playerCurrentFloor
    lda     playerCurrentRoom
    jsr     Lfd4c
    bcs     Lfd42
    beq     Lfd3a
    lda     #$04
Lfd3a
    ldx     playerCurrentRoom
    ora     DoorsByRoom,x
Lfd3f
    sta     roomStairsStatus
    rts
Lfd42
    lda     #$0f
    bne     Lfd3f                   ; unconditional
    
DoorsByRoom
    .byte   3,3,1,0,2,2             ; indexed by playerCurrentRoom

; ******************************************************************
    
Lfd4c SUBROUTINE
    sta     tmpRoomNumber
    tya
    jsr     Game9FloorPlanner
    sty     tmpFloorNumber
    lda     Lfddd,y
    ldy     tmpRoomNumber
    and     BitmaskThing,y
    beq     Lfd6a
    ldy     tmpFloorNumber
    lda     Lfde5,y
    ldy     tmpRoomNumber
    clc
    and     BitmaskThing,y
    rts
Lfd6a
    sec
    rts

; ******************************************************************
    
Lfd6c SUBROUTINE
    sta     torchesLNumberPTRs
    lda     playerCurrentFloor
    sta     tmpFloorNumber
    lda     playerCurrentRoom
    jsr     Lfd9a
    bmi     gotoRTSWithZero
    lsr
    tay
    lda     Lfd7f,y
    rts

; ******************************************************************
    
Lfd7f
    .byte   $57,$a7,$4f                     ; $fd7f (D)
Lfd82
    .byte   $04,$ff,$00,$80,$ff,$04,$01,$81 ; $fd82 (D)
    .byte   $05,$82,$02,$00,$83,$05,$03,$01 ; $fd8a (D)
    .byte   $06,$ff,$84,$02,$ff,$06,$85,$03 ; $fd92 (D)

; ******************************************************************
    
Lfd9a SUBROUTINE
    asl
    asl
    clc
    adc     torchesLNumberPTRs
    tay
    lda     Lfd82,y
    sta     livesNumberPTRs
    rts

; ******************************************************************
    
Lfda6 SUBROUTINE
    sta     torchesLNumberPTRs
    lda     playerCurrentFloor
    sta     tmpFloorNumber
    lda     playerCurrentRoom
Lfdae
    jsr     Lfd9a
    bmi     .gotoRTS
    ldy     gameSelection
    cpy     #$02
    bcc     gotoRTSWithZero
    lda     tmpFloorNumber
    jsr     Game9FloorPlanner
    lda     Lfdd5,y
    ldy     tmpRoomNumber
    and     BitmaskThing,y
    rts
gotoRTSWithZero
    lda     #0
.gotoRTS
    rts

; ******************************************************************
    
Game9FloorPlanner SUBROUTINE
    ldy     gameSelection
    cpy     #8
    bne     .notOnGame9
    clc
    adc     #$04
.notOnGame9
    tay
    rts

; ******************************************************************
    
Lfdd5
    .byte   $29,$47,$34,$29,$2e,$1d,$2b,$0a ; $fdd5 (*)

Lfddd
    .byte   $21,$3b,$3f,$25                 ; $fddd (D) ; games 1-8
    .byte   $2a,$3f,$3f,$2a                 ; $fde1 (*) ; game 9 only

Lfde5
    .byte   $21,$1a,$25,$00                 ; $fde5 (D)
    .byte   $2a,$15,$2a                     ; $fde9 (*)

Lfdec
    .byte   $00,$fa,$fa,$06,$06             ; $fdec (D)

Lfdf1
    .byte   $00,$fe,$06,$fe,$06             ; $fdf1 (D)

InitialGameVariables
    .byte   $09,$ff,$7f,$02,$ff,$0f,$02,$02,$03,$04 ; $fdf6 (D)

BitmaskThing ; $FE00 - Unsure of Exact Use
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

CreatureGraphics
;Ghost_0 $FE50
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
;Ghost_1 $FE5A
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
;Bat_0 $FE64
    .byte   %10000001 ; |#      #|
    .byte   %10000001 ; |#      #|
    .byte   %11000011 ; |##    ##|
    .byte   %01100110 ; | ##  ## |
    .byte   %01111110 ; | ###### |
    .byte   %00111100 ; |  ####  |
Blank ; $FE6A
    .byte   %00000000 ; |        | blank is shared data (8 zero bytes)
    .byte   %00000000 ; |        | used to show 'no object'
    .byte   %00000000 ; |        | in status display area
    .byte   %00000000 ; |        |
;Bat_1 $FE6E
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
;Spider_0 $FE78
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
;Spider_1 $FE82
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

StairGraphics
;Stairs_Ulr $FE8C
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
;Stairs_Dlr $FE9D
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
;Stairs_Dtb $FEAE
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
;Stairs_Utb $FEBF
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

PF0Data
    .byte   %11111111 ; |********| $FED0 (P)
PF1Data
    .byte   %11110000 ; |****    | $FED1 (P)
PF2Data
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

TorchGraphics
Torch_0
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

Torch_1
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
    
TorchClippingValues
    .byte   $80,$80,$ee,$03

StairHPositionsTable
    .byte   111,31,139,4,111,31

RollingEyesTable
    .byte   $e0,$a0,$b0,$90,$d0,$50,$70,$60

TorchLSBValues
    .byte   <Torch_0,<Torch_1,<Torch_0,<Torch_0

EndingMusic
    .byte   19,18,19,23 ; sorta like 'twilight zone'

StairwaySound
    .byte   9,9,11,18 ; played forward and reversed when ascending/descending

RandomLocationsTableOffsets
    .byte   $00,$06,$0d,$0d

RandomLocationsTableH
    .byte   $74,$24,$74,$24,$74,$24,$74,$24 ; "random" horiz locations of items
    .byte   $74,$24,$4C,$4C,$4C,$74,$24,$94
    .byte   $04,$74,$24

RandomLocationsTableV
    .byte   $24,$24,$84,$84,$C4,$C4,$54,$54 ; "random" vert locations of items
    .byte   $A4,$A4,$24,$84,$C4,$00,$00,$84
    .byte   $84,$F4,$F4

RoomToRoomOffsets
    .byte   1,-1,2,-2 ; used when moving up/down stairways to another room 

WallBitPattern
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

GameColorTable
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

 	.word Start
	.word Start

