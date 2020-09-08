# RPN-Calculator
An ATmega328-based RPN calculator in assembler

This creates a simple four-function (+, - , *, /) RPN calculator out of a bare ATmega328p chip that is
wired to a 4x4 button keypad (with classic row-column wiring) and 2x16 LCD, all powered by a 9V battery.

The calculator implements 32-bit signed integer arithmetic (sorry, no floating point) and a traditional
four-level RPN stack (X, Y, Z, and T as in the classic HP calculators).  The LCD displays
the RPN X register on the lower row and the RPN Y register on the upper row.

There are two LEDs.  They both flash at start-up just to confirm operation.  While using the calculator, the
green LED lights while a number is being entered.  The red LED lights up when an overflow occurs (and the
word "Overflow" displays on the lower row of the LCD).

The keypad is set up as follows:

The keypad values are:

| | Col 1 | Col 2 | Col 3 | Col 4 |
| :---: | :---: | :---: | :---: | :---: |
| **Row 1** | 1 | 2 | 3 | / |
| **Row 2** | 4 | 5 | 6 | x |
| **Row 3** | 7 | 8 | 9 | - |
| **Row 4** | +/- | 0 | Enter | + |

The value "s" is shorthand for "+/-" (change sign) and the value "E" is shorthand for
"Enter" (as might appear on an RPN calculator).

A Fritzing schematic is available in the Schematic folder, along with a PDF export.

Various design notes appear in the Notes folder.
