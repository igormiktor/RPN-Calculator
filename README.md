# RPN-Calculator
An ATmega328-based RPN calculator in assembler.

This creates a simple four-function (+, - , *, /) RPN calculator out of a bare ATmega328p chip that is
wired to a 4x4 button keypad (with classic row-column wiring) and a 2x16 LCD, all powered by a 9V battery.

The calculator implements 32-bit signed integer arithmetic (sorry, no floating point) and a traditional
four-level RPN stack (X, Y, Z, and T as in the classic HP calculators).  The LCD displays
the RPN X register on the lower row and the RPN Y register on the upper row.

There are two LEDs.  They both flash at start-up just to confirm operation.  While using the calculator, the
green LED lights while a number is being entered.  The red LED lights up when an overflow occurs (and the
word "Overflow" displays on the lower row of the LCD).

The keypad is set up as follows:

| | Col 1 | Col 2 | Col 3 | Col 4 |
| :---: | :---: | :---: | :---: | :---: |
| **Row 1** | 1 | 2 | 3 | / |
| **Row 2** | 4 | 5 | 6 | x |
| **Row 3** | 7 | 8 | 9 | - |
| **Row 4** | +/- | 0 | Enter | + |

The code detects key presses by polling (perhaps to be replaced
in the future by an interrupt handler).  Keys are dispatched
using a jump table with the addresses of 16 key-handling functions
and using the jump table to indirect-call to the appropriate one.  

Double-word (32-bit) signed
arithmetic is implemented manually in assembler, as is conversion
of 32-bit quantities to decimal ASCII strings for display.

A schematic of the circuit and a Fritzing wiring diagram (both in PDF) are available in the Schematic folder, along with the Fritzing file.

Various design notes appear in the Notes folder.

The Images directory has photos of the calculator.
