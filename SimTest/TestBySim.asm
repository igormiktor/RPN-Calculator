
 .device "ATmega328p"


; ***************************************
;  P I N - O U T
; ***************************************
;
; Package = 28-pin-PDIP
; 1: Reset
; 2: PD0 RXD0 PCINT16
; 3: PD1 TXD0 PCINT17
; 4: PD2 INT0 PCINT18
; 5: PD3 INT1 OC2B PCINT19
; 6: PD4 T0 XCK0 PCINT20
; 7: Vcc
; 8: Gnd
; 9: PB6 TOSC1 XTAL1 PCINT6
; 10: PB7 TOSC2 XTAL2 PCINT7
; 11: PD5 T1 OC0B PCINT21
; 12: PD6 AIN0 OC0A PCINT22
; 13: PD7 AIN1 PCINT23
; 14: PB0 ICP1 CLKO PCINT0
; 15: PB1 OC1A PCINT1
; 16: PB2 /SS OC1B PCINT2
; 17: PB3 MOSI OC2A PCINT3
; 18: PB4 MISO PCINT4
; 19: PB5 USCK PCINT5
; 20: AVCC
; 21: AREF
; 22: Gnd
; 23: PC0 ADC0 PCINT8
; 24: PC1 ADC1 PCINT9
; 25: PC2 ADC2 PCINT10
; 26: PC3 ADC3 PCINT11
; 27: PC4 ADC4 SDA PCINT12
; 28: PC5 ADC5 SCL PCINT13;



; **********************************
;  P O R T S   A N D   P I N S
; **********************************

; Red LED pin
.equ pRedLedDirD                    = DDRD
.equ pRedLedDirDBit                 = DDD2
.equ pRedLedPort                    = PORTD
.equ pRedLedPortBit                 = PORTD2
.equ pRedLedPin                     = PIND
.equ pRedLedPinBit                  = PIND2

; Green LED pin
.equ pGreenLedDirD                  = DDRD
.equ pGreenLedDirDBit               = DDD3
.equ pGreenLedPort                  = PORTD
.equ pGreenLedPortBit               = PORTD3
.equ pGreenLedPin                   = PIND
.equ pGreenLedPinBit                = PIND3


; LCD uses pins PC0-PC5

; LCD data (register) select pin
.equ pLcdDataSelectDirD             = DDRC
.equ pLcdDataSelectDirDBit          = DDC5
.equ pLcdDataSelectPort             = PORTC
.equ pLcdDataSelectPortBit          = PORTC5

; LCD enable pin
.equ pLcdEnableDirD                 = DDRC
.equ pLcdEnableDirDBit              = DDC4
.equ pLcdEnablePort                 = PORTC
.equ pLcdEnablePortBit              = PORTC4

; LCD data 7 pin
.equ pLcdD7DirD                     = DDRC
.equ pLcdD7DirDBit                  = DDC3
.equ pLcdD7Port                     = PORTC
.equ pLcdD7PortBit                  = PORTC3

; LCD data 6 pin
.equ pLcdD6DirD                     = DDRC
.equ pLcdD6DirDBit                  = DDC2
.equ pLcdD6Port                     = PORTC
.equ pLcdD6PortBit                  = PORTC2

; LCD data 5 pin
.equ pLcdD5DirD                     = DDRC
.equ pLcdD5DirDBit                  = DDC1
.equ pLcdD5Port                     = PORTC
.equ pLcdD5PortBit                  = PORTC1

; LCD data 4 pin
.equ pLcdD4DirD                     = DDRC
.equ pLcdD4DirDBit                  = DDC0
.equ pLcdD4Port                     = PORTC
.equ pLcdD4PortBit                  = PORTC0



; Keypad uses D4-D7 (columns) and B0-3 (rows) an D2 for INT0

; Keypad row pins are Port B pins 0-3
.equ pRowDirD                       = DDRB
.equ pRowPort                       = PORTB
.equ pRowPin                        = PINB
.equ kRowBitsOnes                   = 0x0F
.equ kRowBitsZeros                  = 0xF0
; Keypad row pin bits
.equ kRow1                          = 3
.equ kRow2                          = 2
.equ kRow3                          = 1
.equ kRow4                          = 0

; Keypad column pins are Port D pins 4-7
.equ pColDirD                       = DDRD
.equ pColPort                       = PORTD
.equ pColPin                        = PIND
.equ kColBitsOnes                   = 0xF0
.equ kColBitsZeros                  = 0x0F
; Keypad columns pin bits
.equ kCol1                          = 7
.equ kCol2                          = 6
.equ kCol3                          = 5
.equ kCol4                          = 4




; **********************************
;  C O N S T A N T S
; **********************************

; Message and display related lengths
.equ kDisplayMsgLen                 = 16
.equ kDecimalDigitsLen              = 5

; LCD commands
.equ kLcdClearDisplay               = 0x01
.equ kLcdReturnHome                 = 0x02
.equ kLcdEntryModeSet               = 0x04
.equ kLcdDisplayControl             = 0x08
.equ kLcdCursorShift                = 0x10
.equ kLcdFunctionSet                = 0x20
.equ kLcdSetCgramAddr               = 0x40
.equ kLcdSetDdramAddr               = 0x80

; LCD flags for display entry mode
.equ kLcdEntryRight                 = 0x00
.equ kLcdEntryLeft                  = 0x02
.equ kLcdEntryShiftIncrement        = 0x01
.equ kLcdEntryShiftDecrement        = 0x00

; LCD flags for display on/off control
.equ kLcdDisplayOn                  = 0x04
.equ kLcdDisplayOff                 = 0x00
.equ kLcdCursorOn                   = 0x02
.equ kLcdCursorOff                  = 0x00
.equ kLcdBlinkOn                    = 0x01
.equ kLcdBlinkOff                   = 0x00

; LCD flags for display/cursor shift
.equ kLcdDisplayMove                = 0x08
.equ kLcdCursorMove                 = 0x00
.equ kLcdMoveRight                  = 0x04
.equ kLcdMoveLeft                   = 0x00

; LCD flags for function set
.equ kLcd8BitMode                   = 0x10
.equ kLcd4BitMode                   = 0x00
.equ kLcd2Line                      = 0x08
.equ kLcd1Line                      = 0x00
.equ kLcd5x10Dots                   = 0x04
.equ kLcd5x8Dots                    = 0x00

; Second row addressing offset
.equ kLcdSecondRowOffset            = 0x40

; Sign bit number for signed values
.equ kSignBitNbr                    = 7

; Decimal constants (for conversion to BCD values)
.equ kDecimal_1e9                   = 1000000000
.equ kDecimal_1e8                   = 100000000
.equ kDecimal_1e7                   = 10000000
.equ kDecimal_1e6                   = 1000000
.equ kDecimal_1e5                   = 100000
.equ kDecimal_1e4                   = 10000
.equ kDecimal_1e3                   = 1000
.equ kDecimal_1e2                   = 100
.equ kDecimal_1e1                   = 10
.equ kDecimal_1e0                   = 1



; ***************************************
;  R E G I S T E R  P O L I C Y
; ***************************************

.def rDisplayTmp                    = r5        ; Used as temporary in low-level LCD related routines

.def rScratch0                      = r6        ; Scratch (low) register
.def rScratch1                      = r7        ; Scratch (low) register
.def rScratch2                      = r8        ; Scratch (low) register
.def rScratch3                      = r9        ; Scratch (low) register

.def rNbrByte0                      = r10       ; Assemble a 16 bit number from keypad entry here
.def rNbrByte1                      = r11       ; Assemble a 16 bit number from keypad entry here
.def rNbrByte2                      = r12       ; Assemble a 16 bit number from keypad entry here
.def rNbrByte3                      = r13       ; Assemble a 16 bit number from keypad entry here

.def rLoop1                         = r14       ; Loop counter

.def rSREG                          = r15       ; Save/Restore status port

.def rDWMSOuter                     = r16       ; Subroutine delayMilliSeconds
.def rTmp1                          = r16       ; Multipurpose register
.def rTmp2                          = r17       ; Multipurpose register

.def rKey                           = r18       ; Index of key hit, used to look-up value in Key Table

.def rState                         = r19       ; State of operation
.equ kDigitEntryBit                 = 0x01      ; Bit 0 set = digit entry mode; clear = not digit entry mode
.equ kDigitEntryBitNbr              = 0         ; Bit number 0
.equ kNbrSignBit                    = 0x02      ; Bit 1 set = number has negative sign; clear = no negative sign
.equ kNbrSignBitNbr                 = 1         ; Bit number 1


.def rArgByte0                      = r22       ; First byte arg, LSB
.def rArgByte1                      = r23       ; Second byte arg
.def rArgByte2                      = r24       ; Third byte arg
.def rArgByte3                      = r25       ; Fourth byte arg, MSB

.def rDelayUsL                      = r22       ; Subroutine delayMicroSeconds
.def rDelayUsH                      = r23       ; Subroutine delayMicroSeconds
.def rMillisL                       = r22       ; Subroutine delayMilliSeconds
.def rMillisH                       = r23       ; Subroutine delayMilliSeconds
.def r10ths                         = r22       ; Subroutine delayTenthsOfSeconds
.def rDTSOuter                      = r23       ; Subroutine delayTenthsOfSeconds

.def rDWMSInnerL                    = r24       ; Subroutine delayMilliSeconds
.def rDWMSInnerH                    = r25       ; Subroutine delayMilliSeconds
.def rDTSInnerL                     = r24       ; Subroutine delayTenthsOfSeconds
.def rDTSInnerH                     = r25       ; Subroutine delayTenthsOfSeconds



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = tmp reg to use (upper half)
.macro initializeStack

    .ifdef SPH
        ldi @0, High( RAMEND )
        out SPH, @0                             ; Upper byte of stack pointer (always load high-byte first)
    .endif
    ldi @0, Low( RAMEND )
    out SPL, @0                                 ; Lower byte of stack pointer

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = register base name, @1 = 16-bit constant
.macro ldiw

   ldi @0H, High( @1 )
   ldi @0L, Low( @1 )

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = register base name to be pushed onto stack (L first, H second)
.macro pushw

   push @0L
   push @0H

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = register base name to be popped from stack (H first, L second)
.macro popw

   pop @0H
   pop @0L

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = number of microseconds to delay (16-bit word)
.macro delayMicroSecondsM
    ldi rArgByte1, High( @0 )
    ldi rArgByte0, Low( @0 )
    call delayMicroSeconds
.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = number of milliseconds to delay (word value)
.macro delayMilliSecondsM
    ldi rArgByte1, High( @0 )
    ldi rArgByte0, Low( @0 )
    call delayMilliSeconds
.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = number of tenths of seconds to delay (byte value)
.macro delayTenthsOfSecondsM
    ldi rArgByte0, Low( @0 )
    call delayTenthsOfSeconds
.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  none
.macro clearLcd

    ldi rArgByte0, kLcdClearDisplay
    rcall sendCmdToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = row  (byte), @1 = column (byte)
.macro setLcdRowColM

    ldi rArgByte0, @0
    ldi rArgByte1, @1
    rcall setLcdRowCol

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = LCD command (byte)
.macro sendCmdToLcdM

    ldi rArgByte0, @0
    rcall sendCmdToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = Data to display (byte)
.macro sendDataToLcdM

    ldi rArgByte0, @0
    rcall sendDataToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = Data to display (byte)
.macro sendDataToLcdMR

    mov rArgByte0, @0
    rcall sendDataToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = address of 16 bit message to display on LCD
.macro displayMsgOnLcdM

    ldiw Z, @0
    call displayMsgOnLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None (doubles rNbrByte3:rNbrByte0)
.macro multiplyNbrBy2

    lsl rNbrByte0
    rol rNbrByte1
    rol rNbrByte2
    rol rNbrByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro clearEntryNbr

    clr rNbrByte0
    clr rNbrByte1
    clr rNbrByte2
    clr rNbrByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveEntryNbr2RpnX

    ldiw Z, sRpnX
    st Z+, rNbrByte0
    st Z+, rNbrByte1
    st Z+, rNbrByte2
    st Z+, rNbrByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveEntryNbr2ArgByte

    mov rArgByte0, rNbrByte0
    mov rArgByte1, rNbrByte1
    mov rArgByte2, rNbrByte2
    mov rArgByte3, rNbrByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveArgByte2EntryNbr

    mov rNbrByte0, rArgByte0
    mov rNbrByte1, rArgByte1
    mov rNbrByte2, rArgByte2
    mov rNbrByte3, rArgByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveRpnX2ArgBtye

    ldiw Z, sRpnX
    ld rArgByte0, Z+
    ld rArgByte1, Z+
    ld rArgByte2, Z+
    ld rArgByte3, Z+

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveArgBtye2RpnX

    ldiw Z, sRpnX
    st Z+, rArgByte0
    st Z+, rArgByte1
    st Z+, rArgByte2
    st Z+, rArgByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveRpnY2ArgBtye

    ldiw Z, sRpnY
    ld rArgByte0, Z+
    ld rArgByte1, Z+
    ld rArgByte2, Z+
    ld rArgByte3, Z+

.endm




; **********************************
;  D A T A   S E G M E N T
;        ( S R A M )
; **********************************

.dseg
.org SRAM_START


sStaticDataBegin:

    sJumpTable:
        .byte 16*2

    sLcdGreeting:
        .byte 16

    sBankLine:
        .byte 16

    sOverflowMsg:
        .byte 16

sStaticDataEnd:


; RPN stack: storage for RPN registers X, Y, Z, and T

sRpnX:
sRpnX0:
    .byte 1
sRpnX1:
    .byte 1
sRpnX2:
    .byte 1
sRpnX3:
    .byte 1

sRpnY:
sRpnY0:
    .byte 1
sRpnY1:
    .byte 1
sRpnY2:
    .byte 1
sRpnY3:
    .byte 1

sRpnZ:
sRpnZ0:
    .byte 1
sRpnZ1:
    .byte 1
sRpnZ2:
    .byte 1
sRpnZ3:
    .byte 1

sRpnT:
sRpnT0:
    .byte 1
sRpnT1:
    .byte 1
sRpnT2:
    .byte 1
sRpnT3:
    .byte 1


; Working storage for conversion to decimal form for display
sDisplayNbrStr:
sLeadingSpaces:
    .byte 3             ; 3 leading blank spaces
sAsciiNumberStr:
    .byte 1
sBcdNumberArray:
    .byte 10
sTrailingSpaces:
    .byte 2             ; 2 trailing blank spaces







; **********************************
;         C O D E
; **********************************
;
.cseg
.org 000000
;
; **********************************
; R E S E T  &  I N T - V E C T O R S
; **********************************
	rjmp Main ; Reset vector
	nop
	reti ; INT0
	nop
	reti ; INT1
	nop
	reti ; PCI0
	nop
	reti ; PCI1
	nop
	reti ; PCI2
	nop
	reti ; WDT
	nop
	reti ; OC2A
	nop
	reti ; OC2B
	nop
	reti ; OVF2
	nop
	reti ; ICP1
	nop
	reti ; OC1A
	nop
	reti ; OC1B
	nop
	reti ; OVF1
	nop
	reti ; OC0A
	nop
	reti ; OC0B
	nop
	reti ; OVF0
	nop
	reti ; SPI
	nop
	reti ; URXC
	nop
	reti ; UDRE
	nop
	reti ; UTXC
	nop
	reti ; ADCC
	nop
	reti ; ERDY
	nop
	reti ; ACI
	nop
	reti ; TWI
	nop
	reti ; SPMR
;
; **********************************
;  I N T - S E R V I C E   R O U T .
; **********************************
;
; (Add all interrupt service routines here)
;
; **********************************
;  M A I N   P R O G R A M   I N I T
; **********************************
;
Main:

  initializeStack rTmp1
;
; **********************************
;    P R O G R A M   L O O P
; **********************************
;

.equ kNbr  = 123

  ldi rArgByte0, BYTE1( kNbr )
  ldi rArgByte1, BYTE2( kNbr )
  ldi rArgByte2, BYTE3( kNbr )
  ldi rArgByte3, BYTE4( kNbr )

  moveArgBtye2RpnX
  nop

.equ kNbr2  = 1234

  ldi rArgByte0, BYTE1( kNbr2 )
  ldi rArgByte1, BYTE2( kNbr2 )
  ldi rArgByte2, BYTE3( kNbr2 )
  ldi rArgByte3, BYTE4( kNbr2 )
  call rollRpnStackUp
  moveArgBtye2RpnX
  nop

.equ kNbr3  = 12345

  ldi rArgByte0, BYTE1( kNbr3 )
  ldi rArgByte1, BYTE2( kNbr3 )
  ldi rArgByte2, BYTE3( kNbr3 )
  ldi rArgByte3, BYTE4( kNbr3 )
  call rollRpnStackUp
  moveArgBtye2RpnX
  nop

 .equ kNbr4  = 123456

  ldi rArgByte0, BYTE1( kNbr4 )
  ldi rArgByte1, BYTE2( kNbr4 )
  ldi rArgByte2, BYTE3( kNbr4 )
  ldi rArgByte3, BYTE4( kNbr4 )
  call rollRpnStackUp
  moveArgBtye2RpnX
  nop

  call rollRpnStackDown
  nop

  call rollRpnStackDown
  nop

  call rollRpnStackDown
  nop

  call rollRpnStackDown
  nop



  call convertDwordToAscStr
  nop

  call doDword2sComplement
  call convertDwordToAscStr
  nop

  call doDword2sComplement
  call convertDwordToAscStr
  nop

  moveArgByte2EntryNbr
  call multiplyBy10
  moveEntryNbr2ArgByte
  call convertDwordToAscStr
  nop

  call doDword2sComplement
  moveArgByte2EntryNbr
  call multiplyBy10
  moveEntryNbr2ArgByte
  call convertDwordToAscStr
  nop


Loop:
	rjmp loop


doOverflow:
  nop







; **********************************
;  S U B R O U T I N E
; **********************************

rollRpnStackUp:

; Roll RPN stack up

; sRpnX -> sRpnY -> sRpnZ -> sRpnT -> <discard>

;   X                       = used as source ptr
;   Z                       = used as destination ptr
;   rTmp1                   = used

    ; Z -> T
    ldiw X, sRpnZ
    ldiw Z, sRpnT
    ldi rTmp1, 4
    mov rLoop1, rTmp1
rollRpnStackUp_Z2T:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne rollRpnStackUp_Z2T

    ; Y -> Z
    ldiw X, sRpnY
    ldiw Z, sRpnZ
    ldi rTmp1, 4
    mov rLoop1, rTmp1
rollRpnStackUp_Y2Z:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne rollRpnStackUp_Y2Z

    ; X -> Y
    ldiw X, sRpnX
    ldiw Z, sRpnY
    ldi rTmp1, 4
    mov rLoop1, rTmp1
rollRpnStackUp_X2Y:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne rollRpnStackUp_X2Y

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

rollRpnStackDown:

; Roll RPN stack down

; sRpnT -> sRpnZ -> sRpnY -> sRpnX -> <discard>

;   X                       = used as source ptr
;   Z                       = used as destination ptr
;   rTmp1                   = used


    ; Y -> X
    ldiw X, sRpnY
    ldiw Z, sRpnX
    ldi rTmp1, 4
    mov rLoop1, rTmp1
    rollRpnStackDown_Y2X:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne rollRpnStackDown_Y2X

    ; Z -> Y
    ldiw X, sRpnZ
    ldiw Z, sRpnY
    ldi rTmp1, 4
    mov rLoop1, rTmp1
rollRpnStackDown_Z2Y:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne rollRpnStackDown_Z2Y

    ; T -> Z
    ldiw X, sRpnT
    ldiw Z, sRpnZ
    ldi rTmp1, 4
    mov rLoop1, rTmp1
rollRpnStackDown_T2Z:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne rollRpnStackDown_T2Z

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

multiplyBy10:

; Multiple a DWORD number by 10 using doubling and repeated additions

; Registers rNbrByte3:rNbrByte0 passed in as arguments and returned (changed)

;   rNbrByte3:rNbrByte0     = input & output
;   rTmp1                   = used

; Sequence is:
;   - double the number (x2)
;   - store double number on the stack
;   - double the number a second time (x4)
;   - double the number a third time (x8)
;   - add the doubled number from the stack (net x10)

    multiplyNbrBy2
    brvs multiplyBy10_Overflow

    push rNbrByte3
    push rNbrByte2
    push rNbrByte1
    push rNbrByte0

    multiplyNbrBy2
    brvs multiplyBy10_Overflow
    multiplyNbrBy2
    brvs multiplyBy10_Overflow

    pop rTmp1
    add rNbrByte0, rTmp1
    pop rTmp1
    adc rNbrByte1, rTmp1
    pop rTmp1
    adc rNbrByte2, rTmp1
    pop rTmp1
    adc rNbrByte3, rTmp1
    brvs multiplyBy10_Overflow

    ret

multiplyBy10_Overflow:                          ; TODO think about error handling/propagation
    call doOverflow
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doDword2sComplement:

; Convert a DWORD (32-bits, signed) to its 2s complement

; Registers rArgByte3:rArgByte0 passed in as arguments
; Results returned in rArgByte3:rArgByte0 (conversion done in place)

; rArgByte3:rArgByte0   = 32-bit quantity to convert (conversion done in place)

    com rArgByte3
    com rArgByte2
    com rArgByte1
    neg rArgByte0
    sbci rArgByte1, 0xff
    sbci rArgByte2, 0xff
    sbci rArgByte3, 0xff
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

convertDwordToAscStr:

    ; Convert a DWORD (32-bits, signed) to an 11 char decimal ASCII string

    ; Registers rArgByte3:rArgByte0 passed in as arguments
    ; Result returned in 11-bytes in SRAM starting at sAsciiNumberStr

    ; rArgByte3:rArgByte0   = 32-bit quantity to convert (not changed)
    ; rTmp1                 = working register (changed)
    ; rTmp2                 = working register (changed)
    ; rScratch0             = working register (changed)
    ; Z                     = working register (changed)

    push rArgByte0                      ; Save the number
	push rArgByte1
    push rArgByte2
    push rArgByte3

    ldiw Z, sAsciiNumberStr
    ldi rTmp1, ' '                      ; Put a space in the leading spot
    st Z+, rTmp1                        ; (will be overwritten with a '-' if needed)
                                        ; Z now points to sBcdNumberArray
    clr rTmp2                           ; rTmp2 is a flag for negative numbers
    sbrs rArgByte3, kSignBitNbr         ; Check for negative number
    rjmp convertDwordToAscStr_0         ; If not negative, go right to conversion
    sbr rTmp2, 0x01                     ; Set flag for negative number
    rcall doDword2sComplement           ; Convert to the corresponding unsigned number

convertDwordToAscStr_0:
    rcall convertDwordToBcdArray        ; Convert binary DWORD to BCD
	ldi rTmp1, 9
    mov rScratch0, rTmp1                ; Counter, rScratch0, is set to 9 for skip zeros loop

convertDwordToAscStr_1:                 ; This loop is to skip leading zeros....
    ld rTmp1, Z                         ; Read a BCD digit
    tst rTmp1                           ; Check if leading zero
    brne convertDwordToAscStr_2         ; No, we found non-zero digit
    ldi rTmp1, ' '                      ; Overwrite the 0 with a blank
    st Z+, rTmp1                        ; Store and advance Z to next position
    dec rScratch0                       ; Decrement counter of how many BCD digits we processed
    brne convertDwordToAscStr_1         ; Branch if more BCD digits to read (this loop counts down from 9)
    ld rTmp1, Z                         ; If get here, read the last BCD digit and then convert to ASCII, even if 0

convertDwordToAscStr_2:                 ; This segment deals with the first non-zero (or last) BCD digit...
	inc rScratch0                       ; Add 1 because (non-zero BCD)->ASCII loop below counts "from 10"
    sbrs rTmp2, 0                       ; Check for negative number
    rjmp convertDwordToAscStr_3         ; Not negative so jmp directly to conversion to ASCII
    ldi rTmp2, '-'                      ; Get here, so we have a negative number
    st -Z, rTmp2                        ; Back up one and store a minus sign
    st Z+, rTmp2                        ; Do it again but advance Z to restore the Z position

convertDwordToAscStr_3:                 ; This loop converts non-zero BCD digits to ASCII...
    subi rTmp1, -'0'                    ; Add '0' to get ASCII version of number
    st Z+, rTmp1                        ; Store the ASCII digit and advance Z
    ld rTmp1, Z                         ; Read next BCD digit
    dec rScratch0                       ; Still more BCD digits to go?
    brne convertDwordToAscStr_3         ; Yes, go to top of this loop

    pop rArgByte3                       ; Restore original number
    pop rArgByte2
    pop rArgByte1
    pop rArgByte0

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

convertDwordToBcdArray:

    ; Convert a DWORD (32 bits) to a 10-digit-BCD array

    ; Registers rArgByte3:rArgByte0 and Z passed in as arguments
    ; Result returned in 10-bytes starting where Z points

    ; rArgByte3:rArgByte0   = 32-bit quantity to convert (changed)
    ; Z                     = pointer to first (highest) digit of BCD result
    ;                         (1 digit per byte, 10 bytes total, with leading zeros)
    ; rScratch3:rScratch0   = scratch registers used as a 32-bit divisor (changed)

    pushw Z                             ; Save Z

    ldi rTmp1, Byte4( kDecimal_1e9 )    ; Start by multiples of 1,000,000,000
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e9 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e9 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e9 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e8 )    ; Next multiples of 100,000,000
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e8 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e8 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e8 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e7 )    ; Next multiples of 10,000,000
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e7 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e7 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e7 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e6 )    ; Next multiples of 1,000,000
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e6 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e6 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e6 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e5 )    ; Next multiples of 100,000
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e5 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e5 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e5 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e4 )    ; Next multiples of 10,000
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e4 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e4 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e4 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e3 )    ; Next multiples of 1,000
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e3 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e3 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e3 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e2 )    ; Next multiples of 100
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e2 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e2 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e2 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    ldi rTmp1, Byte4( kDecimal_1e1 )    ; Next multiples of 10
    mov rScratch3, rTmp1
    ldi rTmp1, Byte3( kDecimal_1e1 )
    mov rScratch2, rTmp1
    ldi rTmp1, Byte2( kDecimal_1e1 )
    mov rScratch1, rTmp1
    ldi rTmp1, Byte1( kDecimal_1e1 )
    mov rScratch0, rTmp1
    rcall getOneDecimalDigit

    st Z, rArgByte0                     ; Remainder is just the ones

    popw Z                              ; Restore Z

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

getOneDecimalDigit:

    ; Determine one decimal digit by continued subtraction of a decimal value

    ; Registers rArgByte3:rArgByte0, rScratch3:rScratch0, and Z passed in as arguments
    ; Result returned where Z points; Z incremented, rArgByte3:rArgByte0 contains remainder

    ; rArgByte3:rArgByte0   = 32-bit quantity to be decimated (changed)
    ; Z                     = pointer to store resulting BCD digit  (changed)
    ; rScratch3:rScratch0   = 32-bit binary decimal divisor (unchanged)
    ; rTmp1                 = Used

    clr rTmp1                           ; Counts number of multiples subtracted, initialize to 0

getOneDecimalDigit_1:
    cp rArgByte3, rScratch3             ; Compare Byte3 to decimal byte
    brcs getOneDecimalDigit_3           ; Byte3 smaller than decimal byte -> done (digit = 0)
    brne getOneDecimalDigit_2           ; Byte3 bigger than decimal byte -> subtract
    cp rArgByte2, rScratch2             ; Byte3 equal, so compare Byte2
    brcs getOneDecimalDigit_3           ; Byte2 smaller than decimal -> done (digit = 0)
    brne getOneDecimalDigit_2           ; Byte2 bigger than decimal byte -> subtract
    cp rArgByte1, rScratch1             ; Byte2 equal, so compate Byte1
    brcs getOneDecimalDigit_3           ; Byte1 smaller than decimal -> done (digit = 0)
    brne getOneDecimalDigit_2           ; Byte1 bigger than decimal byte -> subtract
    cp rArgByte0, rScratch0             ; Byte1 equal so compare Byte0
    brcs getOneDecimalDigit_3           ; Byte0 smaller than decimal -> done (digit = 0)

getOneDecimalDigit_2:
    sub rArgByte0, rScratch0            ; Subtract the decimal value from the number
    sbc rArgByte1, rScratch1
    sbc rArgByte2, rScratch2
    sbc rArgByte3, rScratch3
    inc rTmp1                           ; Increment digit count
    rjmp getOneDecimalDigit_1           ; Next loop back to try to subtract again

getOneDecimalDigit_3:
    st Z+, rTmp1                        ; Count of multiples subtracted is the digit, save and increment Z

	ret

