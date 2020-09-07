; ***********************************************************************************
;
;    An RPN calculator an 4x4 KeyPad and an LCD display
;
;    The MIT License (MIT)
;
;    Copyright (c) 2020 Igor Mikolic-Torreira
;
;    Permission is hereby granted, free of charge, to any person obtaining a copy
;    of this software and associated documentation files (the "Software"), to deal
;    in the Software without restriction, including without limitation the rights
;    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;    copies of the Software, and to permit persons to whom the Software is
;    furnished to do so, subject to the following conditions:
;
;    The above copyright notice and this permission notice shall be included in all
;    copies or substantial portions of the Software.
;
;    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;    SOFTWARE.
;
; ***********************************************************************************



.device "ATmega328p"


; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  P I N - O U T
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  P O R T S   A N D   P I N S
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  C O N S T A N T S
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  R E G I S T E R  P O L I C Y
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


.def rProd0                         = r2        ; Used to store product of DWORD multiplication
.def rProd1                         = r3        ; Used to store product of DWORD multiplication
.def rProd2                         = r4        ; Used to store product of DWORD multiplication

.def rDisplayTmp                    = r5        ; Used as temporary in low-level LCD related routines
.def rProd3                         = r5        ; Used to store product of DWORD multiplication

.def rScratch0                      = r6        ; Scratch (low) register
.def rProd4                         = r6        ; Used to store product of DWORD multiplication
.def rScratch1                      = r7        ; Scratch (low) register
.def rProd5                         = r7        ; Used to store product of DWORD multiplication
.def rScratch2                      = r8        ; Scratch (low) register
.def rProd6                         = r8        ; Used to store product of DWORD multiplication
.def rScratch3                      = r9        ; Scratch (low) register
.def rProd7                         = r9        ; Used to store product of DWORD multiplication

.def rNbrByte0                      = r10       ; Assemble a 16 bit number from keypad entry here
.def rNbrByte1                      = r11       ; Assemble a 16 bit number from keypad entry here
.def rNbrByte2                      = r12       ; Assemble a 16 bit number from keypad entry here
.def rNbrByte3                      = r13       ; Assemble a 16 bit number from keypad entry here

.def rLoop1                         = r14       ; Loop counter
.def rZero                          = r14       ; Used to store 0

.def rSREG                          = r15       ; Save/Restore status port

.def rDWMSOuter                     = r16       ; Subroutine delayMilliSeconds
.def rTmp1                          = r16       ; Multipurpose register
.def rTmp2                          = r17       ; Multipurpose register

.def rKey                           = r18       ; Index of key hit, used to look-up value in Key Table

.def rState                         = r19       ; State of operation
.equ kDigitEntryBit                 = 0x01      ; Bit 0 set = digit entry mode; clear = not digit entry mode
.equ kDigitEntryBitNbr              = 0         ; Number entry = Bit number 0
.equ kPriorEnterBit                 = 0x02      ; Bit 1 set = Enter key hit immediately prior
.equ kPriorEnterBitNbr              = 1         ; Enter key hit = Bit number 1
.equ kOverflowBit                   = 0x04      ; Bit 2 set = overflow occurred in prior op
.equ kOverflowBitNbr                = 2         ; Overflow = Bit number 2
.equ kOverflowSignBit               = 0x08      ; Bit 3 set = overflow has a negative sign
.equ kOverflowSignBitNbr            = 3         ; Overflow sign = Bit number 3

.def rLcdArg0                       = r20       ; LCD related argument #0
.def rLcdArg1                       = r21       ; LCD related argument #1

.def rArgByte0                      = r22       ; First byte arg, LSB
.def rArgByte1                      = r23       ; Second byte arg
.def rArgByte2                      = r24       ; Third byte arg
.def rArgByte3                      = r25       ; Fourth byte arg, MSB

.def r10ths                         = r22       ; Subroutine delayTenthsOfSeconds
.def rDTSOuter                      = r23       ; Subroutine delayTenthsOfSeconds

.def rMillisL                       = r24       ; Subroutine delayMilliSeconds
.def rMillisH                       = r25       ; Subroutine delayMilliSeconds
.def rDelayUsL                      = r24       ; Subroutine delayMicroSeconds
.def rDelayUsH                      = r25       ; Subroutine delayMicroSeconds
.def rDTSInnerL                     = r24       ; Subroutine delayTenthsOfSeconds
.def rDTSInnerH                     = r25       ; Subroutine delayTenthsOfSeconds

.def rDWMSInnerL                    = r26       ; Subroutine delayMilliSeconds
.def rDWMSInnerH                    = r27       ; Subroutine delayMilliSeconds




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  M A C R O S
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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
    ldi rDelayUsH, High( @0 )               ; Same as rArgByte3
    ldi rDelayUsL, Low( @0 )                ; Same as rArgByte2
    call delayMicroSeconds
.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = number of milliseconds to delay (word value)
.macro delayMilliSecondsM
    ldi rMillisH, High( @0 )                ; Same as rArgByte3
    ldi rMillisL, Low( @0 )                 ; Same as rArgByte2
    call delayMilliSeconds
.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = number of tenths of seconds to delay (byte value)
.macro delayTenthsOfSecondsM
    ldi r10ths, Low( @0 )                ; Same as rArgByte0
    call delayTenthsOfSeconds
.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  none
.macro clearLcd

    ldi rLcdArg0, kLcdClearDisplay
    rcall sendCmdToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = row  (byte), @1 = column (byte)
.macro setLcdRowColM

    ldi rLcdArg0, @0
    ldi rLcdArg1, @1
    rcall setLcdRowCol

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = LCD command (byte)
.macro sendCmdToLcdM

    ldi rLcdArg0, @0
    rcall sendCmdToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = Data to display (byte)
.macro sendDataToLcdM

    ldi rLcdArg0, @0
    rcall sendDataToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = Data to display (byte)
.macro sendDataToLcdMR

    mov rLcdArg0, @0
    rcall sendDataToLcd

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  none
.macro turnOnGreenLed

    sbi pGreenLedPort, pGreenLedPortBit

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  none
.macro turnOnRedLed

    sbi pRedLedPort, pRedLedPortBit

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  none
.macro turnOffGreenLed

    cbi pGreenLedPort, pGreenLedPortBit

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  none
.macro turnOffRedLed

    cbi pRedLedPort, pRedLedPortBit

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  @0 = address of 16 bit message to display on LCD
.macro displayMsgOnLcdM

    ldiw Z, @0
    rcall displayMsgOnLcd

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
.macro moveEntryNbrToRpnX

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
.macro moveNbrByteToArgByte

    mov rArgByte0, rNbrByte0
    mov rArgByte1, rNbrByte1
    mov rArgByte2, rNbrByte2
    mov rArgByte3, rNbrByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveArgByteToNbrByte

    mov rNbrByte0, rArgByte0
    mov rNbrByte1, rArgByte1
    mov rNbrByte2, rArgByte2
    mov rNbrByte3, rArgByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveRpnXToArgBtye

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
.macro moveArgByteToRpnX

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
.macro moveRpnYToArgBtye

    ldiw Z, sRpnY
    ld rArgByte0, Z+
    ld rArgByte1, Z+
    ld rArgByte2, Z+
    ld rArgByte3, Z+

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveArgByteToRpnY

    ldiw Z, sRpnY
    st Z+, rArgByte0
    st Z+, rArgByte1
    st Z+, rArgByte2
    st Z+, rArgByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveRpnXToScratch

    ldiw Z, sRpnX
    ld rScratch0, Z+
    ld rScratch1, Z+
    ld rScratch2, Z+
    ld rScratch3, Z+

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro moveArgByteToScratch

    mov rScratch0, rArgByte0
    mov rScratch1, rArgByte1
    mov rScratch2, rArgByte2
    mov rScratch3, rArgByte3

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro clearEnterKeyHitFlag

    cbr rState, kPriorEnterBit                  ; Clear the Enter key flag

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro setEnterKeyHitFlag

    sbr rState, kPriorEnterBit                  ; Set the Enter key flag

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro setOverflowCondition

    sbr rState, kOverflowBit                    ; Set the overflow flag
    turnOnRedLed
.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro clearOverflowCondition

    cbr rState, kOverflowBit                    ; Clear the overflow flag
    turnOffRedLed

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro loadArgByteMaxPosValue

    ldi rArgByte0, 0xff
    ldi rArgByte1, 0xff
    ldi rArgByte2, 0xff
    ldi rArgByte3, 0x7f

.endm



; **********************************
;  M A C R O
; **********************************

; Arguments:  None
.macro loadArgByteMaxNegValue

    ldi rArgByte0, 0x01
    ldi rArgByte1, 0x00
    ldi rArgByte2, 0x00
    ldi rArgByte3, 0x80

.endm




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  D A T A   S E G M E N T   ( S R A M )
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;  C O D E  S E G M E N T
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

.cseg
.org 0x00


; ************************************
;  I N T E R R U P T  V E C T O R S
; ************************************

.org 0x00
	rjmp main                  ; Reset vector
.org 0x02
	reti                       ; INT0
.org 0x04
	reti                       ; INT1
.org 0x06
	reti                       ; PCI0
.org 0x08
	reti                       ; PCI1
.org 0x0A
	reti                       ; PCI2
.org 0x0C
	reti                       ; WDT
.org 0x0E
	reti                       ; OC2A
.org 0x10
	reti                       ; OC2B
.org 0x12
	reti                       ; OVF2
.org 0x14
	reti                       ; ICP1
.org 0x16
	reti                       ; OC1A
.org 0x18
	reti                       ; OC1B
.org 0x1A
	reti                       ; OVF1
.org 0x1C
	reti                       ; OC0A
.org 0x1E
	reti                       ; OC0B
.org 0x20
	reti                       ; OVF0
.org 0x22
	reti                       ; SPI
.org 0x24
	reti                       ; URXC
.org 0x26
	reti                       ; UDRE
.org 0x28
	reti                       ; UTXC
.org 0x2A
	reti                       ; ADCC
.org 0x2C
	reti                       ; ERDY
.org 0x2E
	reti                       ; ACI
.org 0x30
	reti                       ; TWI
.org 0x32
	reti                       ; SPMR
.org 0x34




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  D A T A   I N   C O D E S E G
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; Rem: data in codeseg stored and addressed by words (not bytes)

dStaticDataBegin:

; Jump table
.dw  doKey0,  doKey1,  doKey2,  doKey3
.dw  doKey4,  doKey5,  doKey6,  doKey7
.dw  doKey8,  doKey9,  doKey10, doKey11
.dw  doKey12, doKey13, doKey14, doKey15

; LCD Message
.db ' ', 'R', 'P', 'N', ' ', 'C', 'a', 'l', 'c', 'u', 'l', 'a', 't', 'o', 'r', ' '

; Blank Line
.db ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '

; Overflow msg
.db ' ', 'O', 'v', 'e', 'r', 'f', 'l', 'o', 'w', ' ', ' ', ' ', ' ', ' ', ' ', ' '

dStaticDataEnd:

.equ kdStaticDataLen = 2 * ( dStaticDataEnd - dStaticDataBegin )



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  I N T E R R U P T   H A N D L E R S
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; None




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  M A I N   ( R E S E T )
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

main:

    initializeStack rTmp1

    rcall initializeStaticData                  ; Move static data from PROGMEM to SRAM

    ; Start the LCD display
    rcall initializeLcd

    ; Initialize LEDs
    sbi pGreenLedDirD, pGreenLedDirDBit
    cbi pGreenLedPort, pGreenLedPortBit
    sbi pRedLedDirD, pRedLedDirDBit
    cbi pRedLedPort, pRedLedPortBit

    ; Flash the LEDs
    turnOnGreenLed
    turnOnRedLed
    delayTenthsOfSecondsM 20
    turnOffGreenLed
    turnOffRedLed

    ; Prepare the LCD display
    setLcdRowColM 0, 0
    displayMsgOnLcdM sLcdGreeting

    rcall clearRpnStack
    clr rState

    ; Configure the keypad to accept inputs
    rcall configureKeypad

    mainLoop:
        ; Look for rows to go low
        in rTmp1, pRowPin
        andi rTmp1, kRowBitsOnes
        cpi rTmp1, kRowBitsOnes
        breq mainLoop
            rcall doKeyHit
            rjmp mainLoop




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  H I G H - L E V E L   K E Y   H A N D L E R S
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

doNumericKey:

    ; rKey = (inbound) the numerical value of number key

    sbrc rState, kDigitEntryBitNbr              ; Skip next if not already entering a number
    rjmp doNumericKey_Continuing                ; Yes we are entering a number, so jmp
                                                ; Not previously entering, so we are starting entry of a number
    rcall beginNumberEntryMode                  ; Set that we are in number entry mode
    sbrc rState, kPriorEnterBitNbr              ; Skip next if the previous key was not "Enter"
    rjmp doNumericKey_PriorEnter                ; Yes it was Enter, so jmp (and skip stack lift)

    rcall liftRpnStack                          ; Not previously an Enter, so we need to do a stack lift
    rcall displayRpnY                           ; Display the new RPN Y

doNumericKey_PriorEnter:
    clearEnterKeyHitFlag
    clearEntryNbr                               ; Clear the registers we accumulate the number in
    mov rNbrByte0, rKey
    rcall displayEntryNbr                       ; Display the entry so far
    ret

doNumericKey_Continuing:                        ; We are adding another digit to an on-going entry
    rcall multiplyBy10                          ; Multiply the existing number by 10 to incorporate a new digit
    brts doNumericKey_Overflow                  ; T flag is set if we had an overflow

    add rNbrByte0, rKey                         ; Add the current digit
    clr rKey                                    ; Doesn't affect carry flag
    adc rNbrByte1, rKey
    adc rNbrByte2, rKey
    adc rNbrByte3, rKey
    brvs doNumericKey_Overflow

    rcall displayEntryNbr
    ret

doNumericKey_Overflow:
    rcall doOverflowDisplay                     ; Do a stripped-down version of overflow (only display portion)
    clearEntryNbr                               ; Discard the number but stay in input mode
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doChangeSignKey:

    clearEnterKeyHitFlag

    ; If entering number, change sign of number being entered
    sbrs rState, kDigitEntryBitNbr              ; Skip next if we are in number entry mode
    rjmp doChangeSignKey_NotEnteringNumber      ; Not number entry mode, so jmp...

    moveNbrByteToArgByte                        ; Skip to here, so entering a number: negate it
    rcall doDword2sComplement
    moveArgByteToNbrByte
    setLcdRowColM 1, 0
    rcall displayArgByte
    ret

doChangeSignKey_NotEnteringNumber:
    moveRpnXToArgBtye                           ; Not in number entry mode, so negate RPN X
    rcall doDword2sComplement
    moveArgByteToRpnX
    setLcdRowColM 1, 0
    rcall displayArgByte
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doEnterKey:

    rcall endNumberEntryMode

    rcall liftRpnStack                          ; Always lift stack
    rcall displayRpnY
    rcall displayRpnX                           ; May not be displayed (e.g., after overflow)
    setEnterKeyHitFlag

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doPlusKey:

    clearEnterKeyHitFlag
    rcall endNumberEntryMode

    rcall addRpnXandY                           ; Do the addition
    brts doPlusKey_Overflow                     ; Branch if overflow

    rcall displayRpnY                           ; RPN X and Y have the right values, display
    rcall displayRpnX
    ret

doPlusKey_Overflow:                             ; Sign of overflow determined by sign of rScratch or original rArgByte
    sbrc rScratch3, kSignBitNbr                 ; Skip next if it is a positive overflow
    rjmp doPlusKey_OverflowNeg                  ; Negative overflow, so jmp...

    loadArgByteMaxPosValue
    rjmp doPlusKey_OverflowFinish

doPlusKey_OverflowNeg:
    loadArgByteMaxNegValue

doPlusKey_OverflowFinish:
    moveArgByteToRpnX
    rcall doOverflow
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doMinusKey:

    clearEnterKeyHitFlag
    rcall endNumberEntryMode

    moveRpnXToArgBtye
    rcall doDword2sComplement
    moveArgByteToScratch
    rcall dropRpnStack
    rcall displayRpnY
    moveRpnXToArgBtye

    add rArgByte0, rScratch0
    adc rArgByte1, rScratch1
    adc rArgByte2, rScratch2
    adc rArgByte3, rScratch3
    brvs doMinusKey_Overflow

    moveArgByteToRpnX
    setLcdRowColM 1, 0
    rcall displayArgByte
    ret

doMinusKey_Overflow:
    rcall doOverflow
    ret


;                        ; Sign of overflow determined by sign of of rScratch or original rArgByte
;    sbrc rScratch3, kSignBitNbr                 ; Skip next if it is a positive overflow
;    rjmp doMinusKey_OverflowNeg                 ; Negative overflow, so jmp...
;
;    loadArgByteMaxPosValue
;    rjmp doMinusKey_OverflowFinish
;
;doMinusKey_OverflowNeg:
;    loadArgByteMaxNegValue
;
;doMinusKey_OverflowFinish:
;    moveArgByteToRpnX
;    rcall doOverflow
;    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doMultiplyKey:

    clearEnterKeyHitFlag
    rcall endNumberEntryMode

    rcall multiplyRpnXandY                      ; Do the multiplication
    brts doMultiplyKey_Overflow                 ; Branch if multiplication overflow

    rcall displayRpnY                           ; RPN X and Y have the right values, display
    rcall displayRpnX
    ret

doMultiplyKey_Overflow:
    rcall doOverflow
    ret

;    sbrc rState, kOverflowSignBitNbr            ; If kOverflowSignBit of rState is clear, result is positive so skip next
;    rjmp doMultiplyKey_OverflowNeg              ; Negative overflow, so jmp...
;
;    loadArgByteMaxPosValue
;    rjmp doMinusKey_OverflowFinish
;
;doMultiplyKey_OverflowNeg:
;    loadArgByteMaxNegValue
;
;doMultiplyKey_OverflowFinish:
;    moveArgByteToRpnX                           ; Update RPN X
;    rcall doOverflow
;    ret



; **********************************
;  S U B R O U T I N E
; **********************************

beginNumberEntryMode:

    sbr rState, kDigitEntryBit                  ; Set that we are in number entry mode
    turnOnGreenLed
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

endNumberEntryMode:

    sbrs rState, kDigitEntryBitNbr              ; Are we in number entry mode?
    ret                                         ; No, we are not: return
                                                ; Yes, we are: so...
    cbr rState, kDigitEntryBit                  ; Clear number entry mode state
    turnOffGreenLed
    moveEntryNbrToRpnX
    rcall displayRpnX                           ; Always display RPN X (might have been a prior overflow)

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doOverflow:

    sbrc rState, kOverflowSignBitNbr            ; If kOverflowSignBit of rState is clear, result is positive so skip next
    rjmp doOverflow_Negative                    ; Negative overflow, so jmp...

    loadArgByteMaxPosValue
    rjmp doOverflow_Finish

doOverflow_Negative:
    loadArgByteMaxNegValue

doOverflow_Finish:
    moveArgByteToRpnX                           ; Update RPN X
    rcall displayRpnY                           ; Update the display of RPN Y

doOverflowDisplay:                              ; Entry point for doNumericKey on overflow
    setOverflowCondition                        ; Turns on red LED
    setLcdRowColM 1, 0                          ; Display the overflow msg instead of RPN X
    displayMsgOnLcdM sOverflowMsg
    ret



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  C A L C U L A T O R   D I S P L A Y
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

displayEntryNbr:
    ; Move the entry number to display routine argument
    setLcdRowColM 1, 0                  ; Uses rArgByte0 & rArgByte1
    moveNbrByteToArgByte

displayArgByte:
    ; Convert the number to decimal ASCII string and display
    rcall convertDwordToAscStr
    displayMsgOnLcdM sDisplayNbrStr
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

displayRpnX:
    ; Move RPN X to display routine argument
    setLcdRowColM 1, 0                  ; Uses rArgByte0 & rArgByte1
    moveRpnXToArgBtye
    rcall displayArgByte
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

displayRpnY:
   ; Move the entry number to  display routine argument
   setLcdRowColM 0, 0                  ; Uses rArgByte0 & rArgByte1
   moveRpnYToArgBtye
   rcall displayArgByte
   ret



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  S T A C K   M A N A G E M E N T
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

clearRpnStack:

    ldiw Z, sRpnX0
    ldi rTmp2, 16
    clr rTmp1
clearRpnStack_Loop:
        st Z+, rTmp1
        dec rTmp2
        brne clearRpnStack_Loop

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

liftRpnStack:

; Lift RPN stack up

; sRpnX -> sRpnY -> sRpnZ -> sRpnT -> <discard>
; sRpnX unchanged

;   X                       = used as source ptr
;   Z                       = used as destination ptr
;   rTmp1                   = used
;   rLoop1                  = used

    ; Z -> T
    ldiw X, sRpnZ
    ldiw Z, sRpnT
    ldi rTmp1, 4
    mov rLoop1, rTmp1
liftRpnStack_Z2T:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne liftRpnStack_Z2T

    ; Y -> Z
    ldiw X, sRpnY
    ldiw Z, sRpnZ
    ldi rTmp1, 4
    mov rLoop1, rTmp1
liftRpnStack_Y2Z:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne liftRpnStack_Y2Z

    ; X -> Y
    ldiw X, sRpnX
    ldiw Z, sRpnY
    ldi rTmp1, 4
    mov rLoop1, rTmp1
liftRpnStack_X2Y:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne liftRpnStack_X2Y

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

dropRpnStack:

; Drop RPN stack down

; sRpnT -> sRpnZ -> sRpnY -> sRpnX -> <discard>
; sRpnT unchanged

;   X                       = used as source ptr
;   Z                       = used as destination ptr
;   rTmp1                   = used

    ; Y -> X
    ldiw X, sRpnY
    ldiw Z, sRpnX
    ldi rTmp1, 4
    mov rLoop1, rTmp1
    dropRpnStack_Y2X:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne dropRpnStack_Y2X

    ; Z -> Y
    ldiw X, sRpnZ
    ldiw Z, sRpnY
    ldi rTmp1, 4
    mov rLoop1, rTmp1
dropRpnStack_Z2Y:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne dropRpnStack_Z2Y

    ; T -> Z
    ldiw X, sRpnT
    ldiw Z, sRpnZ
    ldi rTmp1, 4
    mov rLoop1, rTmp1
dropRpnStack_T2Z:
    ld rTmp1, X+
    st Z+, rTmp1
    dec rLoop1
    brne dropRpnStack_T2Z

    ret



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  M A T H   S U P P O R T  &  C O N V E R S I O N
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

addRpnXandY:

; Add two signed DWORD numbers from RPN X and Y in the RPN stack

; Registers rScratch3:rScratch0 and rArgByte3:rArgByte0 used as summands and
; result is stored in rArgByte3:rArgByte0.

; If no overflow, T flag is clear and product stored in RPN X
; If overflow, T flag is set and rState kOverflowSignBit indicates a negative overflow if set
; In all cases, stack is dropped

;   rNbrByte3:rNbrByte0     = used
;   rArgByte3:rArgByte0     = used
;   rTmp1                   = used
;   T flag                  = used (set = overflow; clear = no overflow)
;   rState                  = overflow sign bit used (set = negative overflow, clear = positive overflow)

    clt                                         ; Clear T flag (no overflow)
    cbr rState, kOverflowSignBit                ; Clear the overflow sign bit (presume positive product)
    ldi rTmp1, kOverflowSignBit                 ; Need this for toggling the sign bit
    clr rZero

    moveRpnXToScratch
    rcall dropRpnStack
    moveRpnXToArgBtye

    add rArgByte0, rScratch0
    adc rArgByte1, rScratch1
    adc rArgByte2, rScratch2
    adc rArgByte3, rScratch3

    brvs addRpnXandY_Overflow                     ; Check for overflow

    moveArgByteToRpnX
    ret

addRpnXandY_Overflow:
    set                                         ; Set T flag to signal overflow
    sbrc rScratch3, kSignBitNbr                 ; Sign of overflow determined by sign of rScratch or original rArgByte
    eor rState, rTmp1                           ; Set rState kOverflowSignBit (skipped if rScratch is positive)
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

subtractRpnXfromY:

; Subtract two signed DWORD numbers, RPN Y - RPN X taken from the RPN stack

; Registers rScratch3:rScratch0 and rArgByte3:rArgByte0 used as arguments and
; result is stored in rArgByte3:rArgByte0.

; If no overflow, T flag is clear and product stored in RPN X
; If overflow, T flag is set and rState kOverflowSignBit indicates a negative overflow if set
; In all cases, stack is dropped

;   rNbrByte3:rNbrByte0     = used
;   rArgByte3:rArgByte0     = used
;   rTmp1                   = used
;   T flag                  = used (set = overflow; clear = no overflow)
;   rState                  = overflow sign bit used (set = negative overflow, clear = positive overflow)

    clt                                         ; Clear T flag (no overflow)
    cbr rState, kOverflowSignBit                ; Clear the overflow sign bit (presume positive product)
    ldi rTmp1, kOverflowSignBit                 ; Need this for toggling the sign bit
    clr rZero

    moveRpnXToArgBtye
    rcall doDword2sComplement
    moveArgByteToScratch
    rcall dropRpnStack
    rcall displayRpnY
    moveRpnXToArgBtye

    add rArgByte0, rScratch0
    adc rArgByte1, rScratch1
    adc rArgByte2, rScratch2
    adc rArgByte3, rScratch3

    brvs subtractRpnXfromY_Overflow             ; Check for overflow

    moveArgByteToRpnX
    ret

subtractRpnXfromY_Overflow:
    set                                         ; Set T flag to signal overflow
    sbrc rScratch3, kSignBitNbr                 ; Sign of overflow determined by sign of rScratch or original rArgByte
    eor rState, rTmp1                           ; Set rState kOverflowSignBit (skipped if rScratch is positive)
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

multiplyRpnXandY:

; Multiply two signed DWORD numbers from RPN X and Y in the RPN stack

; Registers rNbrByte3:rNbrByte0 and rArgByte3:rArgByte0 used as multiplicands and
; results is temporarily stored in rProd7:rProd0.

; If no overflow, T flag is clear and product stored in RPN X
; If overflow, T flag is set and rState kOverflowSignBit indicates a negative product if set
; In all cases, stack is dropped

;   rNbrByte3:rNbrByte0     = used
;   rArgByte3:rArgByte0     = used
;   rProd7:rProd0           = used
;   r1:r0                   = used (mul instruction outputs to r1:r0)
;   rTmp1                   = used
;   rZero                   = used (to store 0)
;   T flag                  = used (set = overflow; clear = no overflow)
;   rState                  = overflow sign bit used (set = negative overflow, clear = positive overflow)

    clt                                         ; Clear T flag (no overflow)
    cbr rState, kOverflowSignBit                ; Clear the overflow sign bit (presume positive product)
    ldi rTmp1, kOverflowSignBit                 ; Need this for toggling the sign bit
    clr rZero

    clr rProd0
    clr rProd1
    clr rProd2
    clr rProd3
    clr rProd4
    clr rProd5
    clr rProd6
    clr rProd7

    ; Deal with X
    moveRpnXToArgBtye
    sbrs rArgByte3, kSignBitNbr                 ; Skip next if it is negative
    rjmp multiplyRpnXandY_XisPositive

    eor rState, rTmp1                           ; Got a negative number, toggle the rState kOverflowSignBit
    rcall doDword2sComplement                   ; Make the number positive

multiplyRpnXandY_XisPositive:
    moveArgByteToNbrByte                        ; Move it to rNbrByte3:rNbrByte0

    ; Deal with Y
    moveRpnYToArgBtye
    sbrs rArgByte3, kSignBitNbr                 ; Skip next if it is negative
    rjmp multiplyRpnXandY_YisPositive

    eor rState, rTmp1                           ; Got a negative number, toggle the rState kOverflowSignBit
    rcall doDword2sComplement                   ; Make the number positive

multiplyRpnXandY_YisPositive:
    rcall dropRpnStack                          ; Both RPN X and Y are extracted, drop the stack

    ; Do the multiplication (both factors are positive and rState kOverflowSignBit has the sign of product)

    ; Index sum = 0
    mul rArgByte0, rNbrByte0
    movw rProd0, r0

    ; Index sum = 6
    mul rArgByte3, rNbrByte3
    movw rProd6, r0

    ; Index sum = 1
    mul rArgByte1, rNbrByte0
    add rProd1, r0
    adc rProd2, r1
    adc rProd3, rZero

    mul rArgByte0, rNbrByte1
    add rProd1, r0
    adc rProd2, r1
    adc rProd3, rZero

    ; Index sum = 2
    mul rArgByte2, rNbrByte0
    add rProd2, r0
    adc rProd3, r1
    adc rProd4, rZero

    mul rArgByte1, rNbrByte1
    add rProd2, r0
    adc rProd3, r1
    adc rProd4, rZero

    mul rArgByte0, rNbrByte2
    add rProd2, r0
    adc rProd3, r1
    adc rProd4, rZero

    ; Index sum = 3
    mul rArgByte3, rNbrByte0
    add rProd3, r0
    adc rProd4, r1
    adc rProd5, rZero

    mul rArgByte2, rNbrByte1
    add rProd3, r0
    adc rProd4, r1
    adc rProd5, rZero

    mul rArgByte1, rNbrByte2
    add rProd3, r0
    adc rProd4, r1
    adc rProd5, rZero

    mul rArgByte0, rNbrByte3
    add rProd3, r0
    adc rProd4, r1
    adc rProd5, rZero

    ; Index sum = 4
    mul rArgByte3, rNbrByte1
    add rProd4, r0
    adc rProd5, r1
    adc rProd6, rZero

    mul rArgByte2, rNbrByte2
    add rProd4, r0
    adc rProd5, r1
    adc rProd6, rZero

    mul rArgByte1, rNbrByte3
    add rProd4, r0
    adc rProd5, r1
    adc rProd6, rZero

    ; Index sum = 5
    mul rArgByte3, rNbrByte2
    add rProd5, r0
    adc rProd6, r1
    adc rProd7, rZero

    mul rArgByte2, rNbrByte3
    add rProd5, r0
    adc rProd6, r1
    adc rProd7, rZero

    ; Check for Overflow
    tst rProd7
    brne multiplyRpnXandY_Overflow
    tst rProd6
    brne multiplyRpnXandY_Overflow
    tst rProd5
    brne multiplyRpnXandY_Overflow
    tst rProd4
    brne multiplyRpnXandY_Overflow

    ldi rTmp1, 0x7f                             ; Max positive byte
    cp rTmp1, rProd3
    brlo multiplyRpnXandY_Overflow

    ; No overflow...
    mov rArgByte0, rProd0
    mov rArgByte1, rProd1
    mov rArgByte2, rProd2
    mov rArgByte3, rProd3

    sbrc rState, kOverflowSignBitNbr            ; If bit 0 of rTmp2 is clear, result is positive so skip next
    rcall doDword2sComplement                   ; Product should be negative, make it so

multiplyRpnXandY_Done:
    moveArgByteToRpnX
    ret

multiplyRpnXandY_Overflow:
    set                                         ; To indicate an overflow
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

    clt                                         ; Clear T flag

    multiplyNbrBy2
    brvs multiplyBy10_Overflow

    push rNbrByte3
    push rNbrByte2
    push rNbrByte1
    push rNbrByte0

    multiplyNbrBy2
    brvs multiplyBy10_Overflow_Pop
    multiplyNbrBy2
    brvs multiplyBy10_Overflow_Pop

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

multiplyBy10_Overflow_Pop:
    pop rTmp1                                   ; Need to pop values off the stack
    pop rTmp1
    pop rTmp1
    pop rTmp1

multiplyBy10_Overflow:
    set                                         ; Set T flag to indicate overflow
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
    ; rScratch03:rScratch0  = working register (not changed)
    ; Z                     = working register (changed)

    push rArgByte0                      ; Save the number
    push rArgByte1
    push rArgByte2
    push rArgByte3

    push rScratch0
    push rScratch1
    push rScratch2
    push rScratch3

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

    pop rScratch3                       ; Restore rScratch
    pop rScratch2
    pop rScratch1
    pop rScratch0

    pop rArgByte3                       ; Restore rArgByte
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



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  K E Y P A D   S U P P O R T
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

doKeyHit:

    rcall scanKeyPad
    delayMilliSecondsM 200                      ; Delay for button de-bounce
    clearOverflowCondition                      ; Any key hit clears overflow
    rcall dispatchKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

dispatchKey:
    ldiw X, sJumpTable                          ; Read number corresponding to key from SRAM
    lsl rKey                                    ; Multiply by 2 (jump addresses are words)
    add XL, rKey                                ; Add the offset (possible carry required)
    clr rTmp2                                   ; Doesn't affect carry flag
    adc XH, rTmp2                               ; X now points to the low byte in SRAM of the jump address
    ld ZL, X+
    ld ZH, X                                    ; Z now contains the address to call
    icall
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

; Returns the number of the key hit in register rKey
; Numbered as:
;   0   1   2   3
;   4   5   6   7
;   8   9   10  11
;   12  13  14  15

scanKeyPad:
    sbis pRowPin, kRow1                         ; Find row of keypress
    ldi rKey, 0                                 ; Set Row pointer
    sbis pRowPin, kRow2
    ldi rKey, 4
    sbis pRowPin, kRow3
    ldi rKey, 8
    sbis pRowPin, kRow4
    ldi rKey, 12

    ; To read the column value need to flip the configuration of rows & columns
    ; Reconfigure rows
    in rTmp1, pRowDirD                          ; Change Rows to outputs
    ori rTmp1, kRowBitsOnes
    out pRowDirD, rTmp1
    in rTmp1, pColDirD                          ; Change Columns to inputs
    andi rTmp1, kColBitsZeros
    out pColDirD, rTmp1
    ; Reconfigure columns
    in rTmp1, pRowPort                          ; Set Rows low
    andi rTmp1, kRowBitsZeros
    out pRowPort, rTmp1
    in rTmp1, pColPort                          ; Set pull-up resistors on Cols
    ori rTmp1, kColBitsOnes
    out pColPort, rTmp1

    delayMicroSecondsM 200                      ; Allow time for port to settle

    sbis pColPin, kCol1                         ; Find column of keypress
    ldi rTmp1, 0
    sbis pColPin, kCol2
    ldi rTmp1, 1
    sbis pColPin, kCol3
    ldi rTmp1, 2
    sbis pColPin, kCol4
    ldi rTmp1, 3

    add rKey, rTmp1                             ; Combine ROW and COL for pointer

    ; Re-initialize columns and rows
    rcall configureKeypad

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

configureKeypad:

    ; Configure the keybad to accept inputs

    ; rTmp1     = used as a scratch register

    ; Configure keypad column pins
    in rTmp1, pColDirD                          ; Set PD4-PD7, columns, as output (others unchanged)
    ori rTmp1, kColBitsOnes
    out pColDirD, rTmp1
    in rTmp1, pColPort                          ; Set PD4-PD7 as low
    andi rTmp1, kColBitsZeros
    out pColPort, rTmp1

    ; Configure keypad row pins
    in rTmp1, pRowDirD                          ; Set PB0-PB3, rows, as input
    andi rTmp1, kRowBitsZeros
    out pRowDirD, rTmp1
    in rTmp1, pRowPort                          ; Enable pull ups on PB0-PB3
    ori rTmp1, kRowBitsOnes
    out pRowPort, rTmp1

    delayMicroSecondsM 200                      ; Allow time for port to settle

    ret




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  L O W - L E V E L   K E Y   H A N D L E R S
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

doKey0:
    ldi rKey, 1
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey1:
    ldi rKey, 2
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey2:
    ldi rKey, 3
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey3:
    ldi rTmp2, '/'
    setLcdRowColM 1, 0                          ; Display the key hit second row, first column
    sendDataToLcdMR rTmp2
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey4:
    ldi rKey, 4
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey5:
    ldi rKey, 5
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey6:
    ldi rKey, 6
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey7:
    rcall doMultiplyKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey8:
    ldi rKey, 7
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey9:
    ldi rKey, 8
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey10:
    ldi rKey, 9
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey11:
    rcall doMinusKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey12:
    rcall doChangeSignKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey13:
    ldi rKey, 0
    rcall doNumericKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey14:
    rcall doEnterKey
    ret



; **********************************
;  S U B R O U T I N E
; **********************************

doKey15:
    rcall doPlusKey
    ret




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  L C D   D R I V E R S
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

initializeLcd:

    ; Configure the LCD pins for output
    sbi pLcdD4DirD, pLcdD4DirDBit
    sbi pLcdD5DirD, pLcdD5DirDBit
    sbi pLcdD6DirD, pLcdD6DirDBit
    sbi pLcdD7DirD, pLcdD7DirDBit
    sbi pLcdEnableDirD, pLcdEnableDirDBit
    sbi pLcdDataSelectDirD, pLcdDataSelectDirDBit

    ; Set LCD pins LOW
    cbi pLcdD4Port, pLcdD4PortBit
    cbi pLcdD5Port, pLcdD5PortBit
    cbi pLcdD6Port, pLcdD6PortBit
    cbi pLcdD7Port, pLcdD7PortBit
    cbi pLcdEnablePort, pLcdEnablePortBit
    cbi pLcdDataSelectPort, pLcdDataSelectPortBit

    ; Wait 50 milliseconds to ensure full voltage rise
    delayMilliSecondsM 50

    cbi pLcdDataSelectPort, pLcdDataSelectPortBit   ; Pull DS pin Low (sending commands)
    cbi pLcdEnablePort, pLcdEnablePortBit           ; Pull E pin Low

    ; Need to send 0x03 three times

    ; First time
    ldi rLcdArg0, 0x03
    rcall write4BitsToLcd

    ; Wait > 4.1 ms
    delayMicroSecondsM  4500

    ; Second time
    ldi rLcdArg0, 0x03
    rcall write4BitsToLcd

    ; Wait > 4.1 ms
    delayMicroSecondsM  4500

    ; Third try and go...
    ldi rLcdArg0, 0x03
    rcall write4BitsToLcd

    ; Wait >150 us
    delayMicroSecondsM  200

    ; This actually sets the 4-bit interface
    ldi rLcdArg0, 0x02
    rcall write4BitsToLcd

    ; Set nbr of lines and font
    sendCmdToLcdM  ( kLcdFunctionSet | kLcd2Line | kLcd5x8Dots )

    ; Turn on display, with cursor off and blinking off
    sendCmdToLcdM ( kLcdDisplayControl | kLcdDisplayOn | kLcdCursorOff | kLcdBlinkOff )

    ; Set text entry more (L to R)
    sendCmdToLcdM  ( kLcdEntryModeSet | kLcdEntryLeft )

    ; Clear display
    sendCmdToLcdM kLcdClearDisplay
    delayMicroSecondsM 2000                     ; Clear cmd takes a long time...

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

write4BitsToLcd:

    ; Register rLcdArg0 is passed as parameter (the 4-bits to write)

    ; rLcdArg0 = the 4-bits to write to the LCD in lower nibble (modified)
    ; rTmp1 used as a temporary register

    ; Protect rDelayUsH:rDelayUsL (a.k.a. rArgByte3:rArgByte2)

    push rDelayUsL
    push rDelayUsH

    ; First write the pins with the 4-bit value;
    ; The 4 pins are on the lower nibble of a single PORT
    andi rLcdArg0, 0x0F                         ; Mask out just the lower nibble
    in rTmp1, pLcdD4Port
    andi rTmp1, 0xF0                            ; Save just the upper nibble of PORTC in @1
    or rLcdArg0, rTmp1                          ; Combine upper nibble of PORTC with lower nibble value
    out pLcdD4Port, rLcdArg0

    ; Now pulse the enable pin to have the LCD read the value
    cbi pLcdEnablePort, pLcdEnablePortBit       ; Enable pin LOW
    delayMicroSecondsM 2
    sbi pLcdEnablePort, pLcdEnablePortBit       ; Enable pin HIGH (actual enable pulse)
    delayMicroSecondsM 2                        ; Enable pulse must be > 450ns
    cbi pLcdEnablePort, pLcdEnablePortBit       ; Enable pin LOW
    delayMicroSecondsM 100                      ; Seems like a lot but didn't work with 70us... Command needs > 37us to settle

    pop rDelayUsH
    pop rDelayUsL

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

sendDataToLcd:

    ; Register rLcdArg0 is passed as parameter

    ; rLcdArg0 = the byte to write to the LCD (modified)
    ; rDisplayTmp used as a temporary register
    ; (rTmp1 used as a temporary by write4BitsToLcd)


    sbi pLcdDataSelectPort, pLcdDataSelectPortBit   ; Pin on to send data
    rjmp send8BitsToLcd

sendCmdToLcd:

    cbi pLcdDataSelectPort, pLcdDataSelectPortBit   ; Pin off to send command
    ; Intentional fall through

send8BitsToLcd:
    mov rDisplayTmp, rLcdArg0                       ; Save the value
    swap rLcdArg0
    rcall write4BitsToLcd                           ; Send the upper nibble
    mov rLcdArg0, rDisplayTmp                       ; Restore the value
    rcall write4BitsToLcd                           ; Send the lower nibble

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

setLcdRowCol:

    ; rLcdArg0 and rLcdArg1 passed as parameters (row, col)

    ; rLcdArg0 = LCD row (0-1)
    ; rLcdArg1 = LCD col (0-15)

    cpi rLcdArg0, 0                             ; Compare row to 0
    breq NoOffsetRequired                       ; If row == 0, skip offset
    subi rLcdArg1, -kLcdSecondRowOffset         ; Add the offset for second row to column
NoOffsetRequired:
    ori rLcdArg1, kLcdSetDdramAddr              ; Incorporate the command itself
    mov rLcdArg0, rLcdArg1                      ; Move the cmd to rLcdArg0
    rcall sendCmdToLcd

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

displayMsgOnLcd:

    ; Z passed as parameter (pointer to message, modified)

    ; Z             = pointer to SRAM to 16 character (byte) message to display
    ; rTmp1         = temporary
    ; rLoop1        = loop counter
    ; rLcdArg0      = Used

    ; Position display is assumed to be set previously

    ; Set up loop and display
    ldi rTmp1, kDisplayMsgLen
    mov rLoop1, rTmp1
displayMsgLoop:
        ld rLcdArg0, Z+
        rcall sendDataToLcd
        dec rLoop1
        brne displayMsgLoop

    ret




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  I N I T I A L I Z A T I O N
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

initializeStaticData:

    ; Copy the static strings into SRAM

    ; Z             = pointer to program memory
    ; X             = pointer to SRAM
    ; rTmp1         = counter
    ; rTmp2         = transfer register

    ; Set up pointers to read from PROGMEM to SRAM
    ldi rTmp1, kdStaticDataLen
    ldiw Z, dStaticDataBegin << 1
    ldiw X, sStaticDataBegin
initializeStaticData_Loop:                               ; Actual transfer loop from PROGMEM to SRAM
        lpm rTmp2, Z+
        st X+, rTmp2
        dec rTmp1
        brne initializeStaticData_Loop

    ; Also write the leading and trailing blanks for number strings
    ldi rTmp1, ' '
    ldiw X, sLeadingSpaces
    st X+, rTmp1
    st X+, rTmp1
    st X+, rTmp1
    ldiw X, sTrailingSpaces
    st X+, rTmp1
    st X+, rTmp1

    ret




; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
;  T I M I N G  &  D E L A Y
;
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


; **********************************
;  S U B R O U T I N E
; **********************************

delayMicroSeconds:

    ; Register r25:24 is passed as parameter (the number of microseconds to delay)

    ; r24 = LSB microseconds to delay
    ; r25 = MSB microseconds to delay

    ; 1 microsecond = 16 cycles.
    ; Call/return overhead takes 7-8 cycles (depending on rcall or call).

    ; So burn up 8 more cycles (not counting the ret) to make a whole microsecond, including
    ; a check to see if we are done (i.e., the request was a 1us delay).
    ; Then do a loop that burns 16 cycles each time

    nop                                 ; 1 cycle
    nop                                 ; 1 cycle
    nop                                 ; 1 cycle
    nop                                 ; 1 cycle
    sbiw rDelayUsH:rDelayUsL, 1         ; 2 cycles
    breq delayMicroseconds_Ret          ; 1 cycle if false/continue, 2 cycles (8 total) if true/branch
    nop                                 ; 1 cycle (8 total)

    delayMicroseconds_Loop:
        nop                             ; 1 cycle
        nop                             ; 1 cycle
        nop                             ; 1 cycle
        nop                             ; 1 cycle

        nop                             ; 1 cycle
        nop                             ; 1 cycle
        nop                             ; 1 cycle
        nop                             ; 1 cycle

        nop                             ; 1 cycle
        nop                             ; 1 cycle
        nop                             ; 1 cycle
        nop                             ; 1 cycle

        sbiw rDelayUsH:rDelayUsL, 1     ; 2 cycles
        brne delayMicroseconds_Loop     ; 2 cycles (16 total) on true/loop, 1 cycle on false/exit_loop
    nop                                 ; 1 cycle (so total 16 on exit from last loop)

delayMicroseconds_Ret:
    ret



;; **********************************
;  S U B R O U T I N E
; **********************************

delayMilliSeconds:

    ; Register r25:r24 (milliSecCounter) is passed as parameter

    ; r25:r24 = number of milliseconds to count (comes in as argument)
    ;     = number of times to execute the outer+inner loops combined
    ; r16 = outer loop counter byte
    ; r26 = low byte of inner loop counter word
    ; r27 = high byte of inner loop counter word

    ; Executing the following combination of inner and outer loop cycles takes almost precisely 1 millisecond at 16 MHz
    .equ kDWMSOuterCount    = 2
    .equ kDWMSInnerCount    = 1997

    ; Top of loop for number of milliseconds
    DWMS_Loop1:

        ; Initialize outer loop (uses a byte counter and counts down)
        ldi rDWMSOuter, kDWMSOuterCount

        ; Top of outer loop
        DWMS_Loop2:

            ; Initialze inner loop (uses a word counter and counts down)
            ldiw rDWMSInner, kDWMSInnerCount

            ; Top of inner loop
            DWMS_Loop3:

                ; Decrement and test inner loop
                sbiw rDWMSInnerH:rDWMSInnerL, 1
                brne DWMS_Loop3
                ; Done with inner loop

            ; Decrement and test outer loop
            dec rDWMSOuter
            brne DWMS_Loop2
            ; Done with outer loop

        ; Decrement and test millisecond loop
        sbiw rMillisH:rMillisL, 1
        brne DWMS_Loop1
        ; Done with the requested number of milliseconds

    ret



; **********************************
;  S U B R O U T I N E
; **********************************

delayTenthsOfSeconds:

    ; Register r24 (tenthOfSecCounter) is passed as parameter
    ; r24 = number of tenths-of-seconds to count (comes in as argument)
    ;     = number of times to execute the outer+inner loops combined
    ; r25 = outer loop counter byte
    ; r26 = low byte of inner loop counter word
    ; r27 = high byte of inner loop counter word

    ; Executing the following combination of inner and outer loop cycles takes almost precisely 0.1 seconds at 16 Mhz
    .equ kDTSOuterCount     = 7
    .equ kDTSInnerCount     = 57142

    ; Top of loop for number of tenths-of-seconds
    DTS_Loop1:
        ; Initialize outer loop (uses a byte counter and counts down)
        ldi rDTSOuter, kDTSOuterCount

        ; Top of outer loop
        DTS_Loop2:
            ; Initialze inner loop (uses a word counter and counts down)
            ldiw rDTSInner, kDTSInnerCount

            ; Top of inner loop
            DTS_Loop3:
                ; Decrement and test inner loop
                sbiw rDTSInnerH:rDTSInnerL, 1
                brne DTS_Loop3
                ; Done with inner loop

            ; Decrement and test outer loop
            dec rDTSOuter
            brne DTS_Loop2
            ; Done with outer loop

        ; Decrement and test tenth-of-second loop
        dec r10ths
        brne DTS_Loop1
        ; Done with the requested number of tenths-of-seconds

    ret
