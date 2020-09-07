# Makefile


RpnCalc = RpnCalculator
RpnCalcAsm = $(RpnCalc).asm
RpnCalcHex = $(RpnCalc).hex

Options = -BE

ASM = gavrasm


%.hex : %.asm
	$(ASM) $(Options) $<


.PHONY: burn
burn: $(RpnCalcHex)
	avrdude -c usbtiny -p atmega328p -U flash:w:$(RpnCalcHex)


.PHONY: hex
hex: $(RpnCalcHex)


.PHONY: clean
clean:
	-rm -f *.lst *.hex
