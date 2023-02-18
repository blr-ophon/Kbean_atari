ASM := dasm
FLAGS := -f3 -v0
SFILE := Kbean.s
BUILD := $(SFILE:%.s=%.bin)
LST := $(SFILE:%.s=%.lst)
SYM := $(SFILE:%.s=%.sym)
EMULATOR := stella

${BUILD}:  
	${ASM} ${SFILE} ${FLAGS} -o${BUILD} -l${LST} -s${SYM}

clean:
	rm -rf ${BUILD} ${LST} ${SYM}

run: ${BUILD} 
	${EMULATOR} $^
