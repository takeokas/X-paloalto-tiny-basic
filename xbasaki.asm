; 
; Xtended    PALO ALTO TINY BASIC
;               original        by Dr.WANG
;               for Super-Aki80    by S.TAKEOKA  (JQ3KLL)
;               for CP/M 2.2    by S.TAKEOKA (JQ3KLL)
; 
; Extented binary logocal operator: |(or),&(and),^(xor)
; 
; Input Line Editor, <-^A, <^B, DEL^D, >^F, ->^E
; on the prompt, "EDIT LineNo" : edit the specified line.
;
; AKI80 version:
; RST1〜7 jump to 0ffe4h〜0fffch on RAM, Those are not initialized.
; NMI jumps to 0ffe0h.It's default is "jmp START" as jumps to XTB's HotStart.

	MACLIB Z80

AKI80	equ 1
CPM equ 0

if  AKI80
CPM equ 0
;RAMTOP equ 08000h
;RAMEND equ 0FFFFh
rsttable equ    0FFE0h		;RST jump table
STACK   EQU     rsttable-2
endif

if CPM	
STACK   EQU     0FEFEh
endif

LINEDX equ 1 ;0
NEVER equ 0
ledtest equ 0
;
BOOT    EQU     0 
if CPM
BDOS    EQU     BOOT+5
TPA     EQU     100H
endif

;VTOP    EQU     6F00H   ;VARIABLE TOP ADRS
;VTOP    EQU     8F00H   ;VARIABLE TOP ADRS
;VTOP    EQU     0FE00H   ;VARIABLE TOP ADRS
VTOP    EQU     0FD00H   ;VARIABLE TOP ADRS
OTOP    equ     VTOP+(('['-'@')*2)
OBTM    equ     VTOP+((']'-'@')*2)
CHUPF    equ     VTOP+(('\'-'@')*2) ; getch Character to_upper flag
;;(format t "~x" (+ #x8000 (* (- (char-code #\])  (char-code #\@)) 2)))
;; => 803A
;WORKTOP equ 8000h
; 
;;; Stack may be  placed last of RAM,
;STKLMT    EQU     7000H
;STACK   EQU     8000H
;STACK   EQU     0b000H
;STACK   EQU     VTOP
;STACK   EQU     0FEFEh
;STKLMT  EQU     STACK - 254      ;TOP LIMIT FOR STACK
STKLMT  EQU     STACK - 82      ;TOP LIMIT FOR STACK
;
;;; TextArea must be  placed before VTOP
TextArea equ    8000h       ;basic text starts here 
;
LBUFSIZ EQU	80
;
BS      equ     08H
DELBS     equ     7fH
CR      equ     0DH	; \r
LF      equ     0AH	; \n
CAN     equ     18H	;^X
ESC     equ     1BH
;
prompt  equ     '+' 
EOL     equ     00h 
; 
if AKI80
	org 0
	jmp TPA

rstjmp1	equ 0ffe4h
	org 8
	jmp rstjmp1

rstjmp2	equ 0ffe8h
	org 8*2
	jmp rstjmp1
	;
rstjmp3	equ 0ffech
	org 8*3
	jmp rstjmp3
	;
rstjmp4	equ 0fff0h
	org 8*4
	jmp rstjmp4
	;
rstjmp5	equ 0fff4h
	org 8*5
	jmp rstjmp5
	;
rstjmp6	equ 0fff8h
	org 8*6
	jmp rstjmp6
	;
rstjmp7	equ 0fffch
	org 8*7
	jmp rstjmp7
	;
nmijmp	equ 0ffe0h
	org 66h
	jmp nmijmp
	;
TPA equ $
endif

        ORG     TPA 
; 
        LXI     SP,STACK
	call sioinit
if AKI80
        ; put "jmp START" at 'nmijmp'
        mvi a,0C3H ;jmp inst.
	sta nmijmp
	lxi h,START
	shld (nmijmp+1)
endif
	;; 
        lxi     h,TextArea
        shld    OTOP
        call    newText 
        CALL    INIT
	mvi a,0FFh
	sta (CHUPF) 		;getch to_upper flag ON
; 
START:  ;DI             ;RESTART FROM ERR 
        LXI     SP,STACK
        JMP     ENTRY 
; 
COMPR:  MOV     A,H             ;COMPAIR DE,HL
        CMP     D 
        RNZ 
        MOV     A,L 
        CMP     E 
        RET 
; 
SKPBL:  LDAX    D 
        CPI     ' ' 
        RNZ 
        INX     D 
        jmp     skpbl 
        RET 
; 
; 
TSTV:   CALL    SKPBL   ;TEST VARIABLE
        SUI     '@' 
        RC
        JNZ     TSTV1 
        INX     D 
        CALL    PARN 
        DAD     H 
        JC      QHOW 
        PUSH    D 
        XCHG
        CALL    RSIZE 
        CALL    COMPR 
        JC      ASORRY 
        LXI     H,VTOP
        CALL    DIFF
        POP     D 
        RET 
; 
TSTV1:  CPI     '_'-'@'+1       ;'@' TO '_' ? 
        CMC 
        RC
        INX     D 
        LXI     H,VTOP
        add     a 
        ADD     L 
        MOV     L,A 
        MVI     A,0 
        ADC     H 
        MOV     H,A 
        RET 
; 
TEST:   XTHL            ;TEST CHARACTOR 
        CALL    SKPBL 
        CMP M 
TEST1:  INX     H 
        JZ      TSTEQ 
        PUSH    B 
        MOV     C,M     ;FETCH SKIP NUM.
        MVI     B,0 
        DAD     B 
        POP     B 
        DCX     D 
TSTEQ:  INX     D 
        INX     H 
        XTHL
        RET 
; 
GINT:   lxi     h,0     ;GET INTEGER
        MOV     B,H 
        CALL    SKPBL 
        cpi     '$' 
        jnz     GETI1 
        ;hex
gethex: 
        inx     d 
        ldax    d 
        sui     '0' 
        rc
        cpi     9+1 
        jc      gethex1 
        cpi     'A'-'0' 
        rc
        cpi     'F'+1-'0' 
        rnc 
        sui     'A'-'0'-10
gethex1:
        dad     h 
        dad     h 
        dad     h 
        dad     h 
        add     l 
        mov     l,a 
        mvi     a,0 
        adc     h 
        mov     h,a 
        inr     b 
        jmp     gethex
; 
;       decimal 
; 
getdec: 
        lxi     h,0 
        mov     b,h 
        call    SKPBL 
GETI1:  sui     '0' 
        RC              ;NOT NUM THEN RET 
        CPI     9+1 
        RNC 
        INR     B 
        PUSH    B 
        MOV     B,H 
        MOV     C,L 
        DAD     H 
        DAD     H 
        DAD     B 
        DAD     H 
        ADD     L 
        MOV     L,A 
        MVI     A,0 
        ADC     H 
        MOV     H,A 
        POP     B 
        inx     d 
        LDAX    D 
        JmP     GETI1 
; 
QHOW:	PUSH    D 
AHOW:	LXI     D,HOWMS 
        JMP     ERROR
; 
HOWMS:  DB      'HOW?',cr,0
OKMES:  DB      'OK',cr,0
WHTMS   DB      'WHAT?',cr,0
SRYMS:  DB      'SORRY',cr,0
if NEVER
HOWMS:  DB      'HOW?',CR 
OKMES:  DB      'OK',CR 
WHTMS   DB      'WHAT?',CR
SRYMS:  DB      'SORRY',CR
endif
;
;nulLine: db 00,00,00,0ffh,0ffh
; 
;;;;;;;;;;;
reinit:
        LXI     H,ST2+1 ;LITERAL 0
        ;LXI     H,nullLine ;LITERAL =null line,Line#0
        SHLD    CURRNT 	;CURRENT->LINE # = 0
ST2:    LXI     H,0 
        SHLD    LOPVAR 
        SHLD    STKGOS 
	ret
;;;;;;;;;;;;;;;;;;;
ENTRY:  CALL    CRLF
        LXI     D,OKMES 
        XRA     A 		;MSG end mark is 00h
        CALL    MSG 
	call reinit
	;jmp GETCmdl
; 
GETCmdl:MVI     A,prompt
        CALL    GETLine
gcmd1:	PUSH    D 
        LXI     D,LBUF
        CALL    getdec
        CALL    SKPBL 
        POP     B       ;BC:=END OF LINE
        mov     a,h 
        ora     l 
        JZ      DIRECT   ;LINE NO.=0 THEN CMD
; 
;       editor
; 
INSRT:  DCX     D 		;put binary LineNo on top of line
        MOV     A,H 
        STAX    D 
        DCX     D 
        MOV     A,L 
        STAX    D 
        PUSH    B       ;SAVE L END 
        PUSH    D       ;SAVE L TOP 
        MOV     A,C 
        SUB     E 
        PUSH    PSW     ;SAVE L LEN
        CALL    SRCHLine	;FIND THIS LINE IN text area
        PUSH    D       ;SAVE CURRENT LINE
        JNZ     MOVE	;NZ: not found, insert
        ; 
        PUSH    D       ;EXIST SAME LINE(Erase the line)
        CALL    SKIPL	;DE->NEXT LINE
        POP     B	;BC->LINE TO BE DELETED
        CALL    TRNSF   ;ERASE OLD LINE ,MVUP
        MOV     H,B 
        MOV     L,C 
        SHLD    OBTM    ;UPDATE OBTM
; 
MOVE:   POP     B       ;BC:=CURRENT LINE 
        LHLD    OBTM
        POP     PSW     ;L LEN
        PUSH    H       ;SAVE CURRENT OBTM
        CPI     3 
        JZ      START   ;3=LNO.&CR(delete line) ;MUST CLEAR THE STACK
        ; 
        ADD     L       ;CALC MOV.BTTM
        MOV     L,A 
        MVI     A,0 
        ADC     H 
        MOV     H,A 
        LXI     D,VTOP
        CALL    COMPR 
        JNC     QSORRY 
        SHLD    OBTM
        POP     D       ;POP CURRENT BTTM 
        CALL    TR2     ;MOV FOR INSERTION, MVDOWN
        POP     D       ;POP L TOP
        POP     H       ;POP L END
        CALL    copyline	;move new line to text area
        JMP     GETCmdl
; 
copyline: 
        LDAX    D       ;line No.L
        STAX    B 
        INX     D 
        INX     B 
        LDAX    D       ;line No.H
        STAX    B 
        INX     D 
        INX     B 
cpyl1:  LDAX    D 
        STAX    B 
        INX     D 
        INX     B 
        cpi     EOL 
        jnz     cpyl1 
        ret 
; 
TRNSF:
        LDAX    D       ;line No.L
        STAX    B 
        INX     D 
        INX     B 
        LDAX    D       ;line No.H
        STAX    B 
        INX     D 
        INX     B 
        ora     a 
        rm
trnsf1: LDAX    D 
        STAX    B 
        INX     D 
        INX     B 
        cpi     EOL 
        jnz     trnsf1
        JMP     TRNSF 
; 
TR2:    MOV     A,B 
        SUB     D 
        JNZ     TR2E
        MOV     A,C 
        SUB     E 
        RZ
TR2E:   DCX     D 
        DCX     H 
        LDAX    D 
        MOV     M,A 
        JMP     TR2 
; 
newText:
        lhld    OTOP
        mvi     m,0FFh
        inx     h 
        mvi     m,0FFh
        inx     h 
        shld    OBTM
        ret 
; 
cschEnd:
        push    d 
        lxi     h,8000h  ;7fffh   ;0h   ;08000h	;??????????
        call    srchx 
        inx     d 
        inx     d 
        xchg
        shld    OBTM
        xchg
        pop     d 
        jmp     ENDL
; 
; 
CMDKW:  DB 'LIST' 
        DB      (LIST SHR 8) OR 80H 
        DB      LIST AND 0FFH 
        DB 'EDIT'
        DB      (editl SHR 8) OR 80H
        DB      editl AND 0FFH
        DB 'RUN'
        DB      (RUN SHR 8) OR 80H
        DB      RUN AND 0FFH
        DB 'NEW'
        DB      (NEW SHR 8) OR 80H
        DB      NEW AND 0FFH
	;
if CPM
;        DB 'system' 
        DB 'SYSTEM' 
        DB      (BOOT SHR 8)OR 80H
        DB      BOOT AND 0FFH 
endif
	;
STMKW:  DB 'NEXT' 
        DB      (NEXT SHR 8) OR 80H 
        DB      NEXT AND 0FFH 
        DB 'LET'
        DB      (LET SHR 8) OR 80H
        DB      LET AND 0FFH
        DB 'IF' 
        DB      (IFSTM SHR 8) OR 80H
        DB      IFSTM AND 0FFH
        DB 'GOTO' 
        DB      (GOTO SHR 8) OR 80H 
        DB      GOTO AND 0FFH 
        DB 'GOSUB'
        DB      (GOSUB SHR 8) OR 80H
        DB      GOSUB AND 0FFH
        DB 'RETURN' 
        DB      (RETURN SHR 8) OR 80H 
        DB      RETURN AND 0FFH 
        DB 'REM'
        DB      (REM SHR 8)OR 80H 
        DB      REM AND 0FFH
        DB 'FOR'
        DB      (FOR SHR 8)OR 80H 
        DB      FOR AND 0FFH
        DB 'INPUT'
        DB      (INPUT SHR 8)OR 80H 
        DB      INPUT AND 0FFH
        DB '$=' 
        DB      (prcha SHR 8)OR 80H 
        DB      prcha AND 0FFH
        DB 'PRINT'
        DB      (PRINT SHR 8)OR 80H 
        DB      PRINT AND 0FFH
        DB '??' 
        DB      (pr4hex SHR 8)OR 80H
        DB      pr4hex AND 0FFH 
        DB '?$' 
        DB      (pr2hex SHR 8)OR 80H
        DB      pr2hex AND 0FFH 
        DB '?'
        DB      (PRINT SHR 8)OR 80H 
        DB      PRINT AND 0FFH
if AKI80
        DB 'OUT' 
        DB      (OUTPUT SHR 8)OR 80H
        DB      OUTPUT AND 0FFH 
endif
        DB 'POKE' 
        DB      (POKE SHR 8)OR 80H
        DB      POKE AND 0FFH 
        DB 'CALL' 
        DB      (MCALL SHR 8)OR 80H 
        DB      MCALL AND 0FFH
        DB '==' 
        DB      (cschEnd SHR 8)OR 80H 
        DB      cschEnd AND 0FFH
        DB 'STOP' 
        DB      (STOP SHR 8)OR 80H
        DB      STOP AND 0FFH 
; 
        DB      (CMDER SHR 8)OR 80H 
        DB      CMDER AND 0FFH
; 
FNKW: 
        DB 'RND'
        DB      (RND SHR 8)OR 80H 
        DB      RND AND 0FFH
        DB 'ABS'
        DB      (FNABS SHR 8)OR 80H 
        DB      FNABS AND 0FFH
        DB 'PEEK' 
        DB      (PEEK SHR 8)OR 80H
        DB      PEEK AND 0FFH 
        DB 'INP' 
        DB      (INPRT SHR 8)OR 80H
        DB      INPRT AND 0FFH 
        DB 'SIZE' 
        DB      (RSIZE SHR 8)OR 80H 
        DB      RSIZE AND 0FFH
; 
        DB      (FACT2 SHR 8)OR 80H 
        DB      FACT2 AND 0FFH
; 
KWTO:   DB 'TO' 
        DB      (FORTO SHR 8)OR 80H 
        DB      FORTO AND 0FFH
        DB      (QWHAT SHR 8)OR 80H 
        DB      QWHAT AND 0FFH
KWSTP:  DB 'STEP' 
        DB      (FSTEP SHR 8) OR 80H
        DB      FSTEP AND 0FFH
        DB      (FSTP1 SHR 8) OR 80H
        DB      FSTP1 AND 0FFH
; 
ROPKW:
        DB '>>' 
        DB      (oshr SHR 8)OR 80H
        DB      oshr AND 0FFH 
        DB '<<' 
        DB      (oshl SHR 8)OR 80H
        DB      oshl AND 0FFH 
        DB '>=' 
        DB      (OGE SHR 8)OR 80H 
        DB      OGE AND 0FFH
        DB '#'
        DB      (ONE SHR 8)OR 80H 
        DB      ONE AND 0FFH
        DB '>'
        DB      (OGT SHR 8)OR 80H 
        DB      OGT AND 0FFH
        DB '='
        DB      (OEQ SHR 8)OR 80H 
        DB      OEQ AND 0FFH
        DB '<=' 
        DB      (OLE SHR 8)OR 80H 
        DB      OLE AND 0FFH
        DB '<'
        DB      (OLT SHR 8)OR 80H 
        DB      OLT AND 0FFH
        DB      (NOROP SHR 8)OR 80H 
        DB      NOROP AND 0FFH
; 
DIRECT:	LXI     H,CMDKW-1 
exec:	CALL    SKPBL 
NXTKW:  PUSH    D       ;SAVE L TOP 
KWC1:   LDAX    D 
        INX     D 
        CPI     '.' 
        JZ      MATKW 
        INX     H 
        CMP     M 
        JZ      KWC1
        MVI     A,7FH 
        DCX     D 
        CMP     M 
        JC      EXEQT   ;MATCH FULL SPELL 
KWSK1:  INX     H 
        CMP     M 
        JNC     KWSK1 
        INX     H 
        POP     D       ;POP L TOP
        JMP     NXTKW 
MATKW:  MVI     A,7FH 
KWSK2:  INX     H 
        CMP     M 
        JNC     KWSK2 
EXEQT:  CALL    SKPBL 
        MOV     A,M     ;FETCH EXEC ADRS
        INX     H 
        MOV     L,M 
        ANI     7FH 
        MOV     H,A 
        POP     PSW     ;DUMMY POP
        PCHL
; 
NEW:    CALL    TSCR2 
        call    newText 
STOP:   CALL    TSCR2 
        JMP     START 
; 
RUN:    CALL    TSCR2 
        xchg
        lhld    OTOP
        xchg
; 
RUN1:   LXI     H,0 
        CALL    SRCH1   ;SRCH MIN-LABEL 
        JC      START 
; 
RUN2: 
        XCHG
        SHLD    CURRNT
        XCHG
        INX     D 
        INX     D 
; 
NXTGO:  CALL    BREAK 
        call    SKPBL 
        LXI     H,STMKW-1 
        JMP     NXTKW 
; 
GOTO:   CALL    EEXPR 
        PUSH    D 
        CALL    TSCR2 
        CALL    SRCHLine
        JNZ     AHOW
        POP     PSW     ;DMY POP
        JMP     RUN2
; 
editl:	CALL    GINT
        CALL    TSCR2 
        CALL    SRCHLine
	JC      START 
	;
	LDAX    D ; HL <- LineNo at (DE)
        MOV     L,A 
        INX     D 
        LDAX    D 
        MOV     H,A 
        INX     D 
	;
        CALL    decnum		;HL -> dec num string into lbuf
	mvi m,' '
	inx h
	call cpy	;line -> lbuf
	dcx h
	push h
        call   crlf
	lxi h,lbuf
	call puts
	pop h
	mov e,l	;DE <-HL
	mov d,h
	call linedloop		;DE=END OF LINE
	call reinit
	JMP     gcmd1
        ;JMP     START
; 
LIST:   CALL    GINT
        CALL    TSCR2 
        CALL    SRCHLine
LISTL:  JC      START 
        CALL    WLINE 
        call    crlf
        CALL    BREAK 
        CALL    SRCH1 
        JMP     LISTL 
; 
OUTPUT: 
        call    eexpr 
        call    skpbl 
        cpi     ',' 
        jnz     QWHAT 
        inx     d 
        push    h 
        call    eexpr 
        pop     b 
	;mov a,l
if AKI80
        outp	l;out (c),L
endif
        jmp     ENDL
POKE: 
        call    eexpr 
        call    skpbl 
        cpi     ',' 
        jnz     QWHAT 
        inx     d 
        push    h 
        call    eexpr 
        push    h 
        pop     b 
        pop     h 
        mov     m,c 
        jmp     ENDL
; 
MCALL:
        call    eexpr 
        push    d 
        lxi     d,mreturn       ;return address 
        push    d 
        pchl
        ;return here
mreturn:
        pop     d 
        jmp     ENDL
; 
prcha:
        call    eexpr 
        mov     a,l 
        call    PUTC
        jmp     ENDL
; 
pr4hex: 
        call    eexpr 
        push    h 
        mov     a,h 
        call    pr2hexsub 
        pop     h 
        mov     a,l 
        call    pr2hexsub 
        jmp     ENDL
; 
pr2hex: 
        call    eexpr 
        mov     a,l 
        call    pr2hexsub 
        jmp     ENDL
; 
pr2hexsub:
        push    psw 
        rrc 
        rrc 
        rrc 
        rrc 
        call    prhex 
        pop     psw 
        call    prhex 
        ret 
        ; 
prhex:  ani     0Fh 
        cpi     10
        jnc     prhex1
        adi     '0' 
        call    PUTC
        ret 
prhex1: adi     'A'-10
        call    PUTC
        ret 
; 
PRINT:  MVI     C,6     ;COLUMN 
; 
        CALL    TEST
        DB      ':' 
        DB      PRNT1-$-1 
        CALL    CRLF
        JMP     NXTGO 
; 
PRNT1:  CALL    TEST
        DB      EOL 
        DB      PRNT2-$-1 
        CALL    CRLF
        JMP     RUN1
; 
PRNT2:  CALL    TEST
        DB      '#' 
        DB      PRNT3-$-1 
        CALL    EEXPR 
        MOV     C,L 
        JMP     PRNT4 
PRNT3:  CALL    PR10
        JMP     PRNT6 
PRNT4:  CALL    TEST
        DB      ',' 
        DB      PRNT5-$-1 
        CALL    TSTSC 
        JMP     PRNT2 
; 
PRNT5:  CALL    CRLF
        JMP     ENDL
; 
PRNT6:  CALL    EEXPR 
        PUSH    B 
        CALL    WINT
        POP     B 
        JMP     PRNT4 
; 
GOSUB:  CALL    PSHV
        CALL    EEXPR 
        PUSH    D 
        CALL    SRCHLine
        JNZ     AHOW
        LHLD    CURRNT
        PUSH    H 
        LHLD    STKGOS 
        PUSH    H 
        LXI     H,0 
        SHLD    LOPVAR 
        DAD     SP
        SHLD    STKGOS 
        JMP     RUN2
; 
RETURN: CALL    TSCR2 
        LHLD    STKGOS 
        MOV     A,H 
        ORA     L 
        JZ      QWHAT   ;IF RSTACK_TOP=0 ERR
        SPHL
        POP     H 
        SHLD    STKGOS 
        POP     H 
        SHLD    CURRNT
        POP     D 
        CALL    POPV
        JMP     ENDL
; 
FOR:    CALL    PSHV
        CALL    LTSUB 
        DCX     H 
        SHLD    LOPVAR 
        LXI     H,KWTO-1
        JMP     NXTKW 
; 
FORTO:  CALL    EEXPR 
        SHLD    LOPLMT
        LXI     H,KWSTP-1 
        JMP     NXTKW 
; 
FSTEP:  CALL    EEXPR 
        JMP     FOR0
; 
FSTP1:  LXI     H,1 
FOR0:   SHLD    LOPINC 
        LHLD    CURRNT
        SHLD    LOPLN 
        XCHG
        SHLD    LOPPT
        LXI     B,0AH 

        LHLD    LOPVAR 
        XCHG
        MOV     H,B 
        MOV     L,B 
        DAD     SP
        DB      3EH     ;<SKIP NEXT INSTRUCTION>
FOR3:   DAD     B 
        MOV     A,M 
        INX     H 
        ORA     M 
        JZ      FOR10 
        MOV     A,M 
        DCX     H 
        CMP     D 
        JNZ     FOR3
        MOV     A,M 
        CMP     E 
        JNZ     FOR3
        XCHG
        LXI     H,0 
        DAD     SP
        MOV     B,H 
        MOV     C,L 
        LXI     H,0AH 
        DAD     D 
        CALL    TR2 
        SPHL
FOR10:  LHLD    LOPPT
        XCHG
        JMP     ENDL
; 
NEXT:   CALL    TSTV
        JC      QWHAT 
        SHLD    NCNTR 
NEXT1:  PUSH    D 
        XCHG
        LHLD    LOPVAR 
        MOV     A,H 
        ORA     L 
        JZ      AWHAT 
        CALL    COMPR 
        JZ      NEXT2 
        POP     D 
        CALL    POPV
        LHLD    NCNTR 
        JMP     NEXT1 
NEXT2:  MOV     E,M 
        INX     H 
        MOV     D,M     ;DE OLD FOR VALUE 
        LHLD    LOPINC 
        PUSH    H       ;HL STEP VALUE
        DAD     D 
        XCHG
        LHLD    LOPVAR 
        MOV     M,E 
        INX     H 
        MOV     M,D     ;FOR VAR. :=NEW VALUE 
        LHLD    LOPLMT
        POP     PSW 
        ORA     A 
        JP      NEXT4 
        XCHG
NEXT4:  CALL    CMINT 
        POP     D 
        JC      NEXT5 
        LHLD    LOPLN 
        SHLD    CURRNT
        LHLD    LOPPT
        XCHG
        JMP     ENDL
; 
NEXT5:  CALL    POPV
        JMP     ENDL
; 
REM:    LXI     H,0 
        JMP     IFST2 
; 
IFSTM:  CALL    EEXPR 
        MOV     A,H 
        ORA     L 
        JNZ     NXTGO 
IFST2:  CALL    SKPL2 
        JNC     RUN2
        JMP     START 
; 
ERRIN:  LHLD    NCNTR 
        SPHL
        POP     H 
        SHLD    CURRNT
        POP     D 
        POP     D 
INPUT:  PUSH    D 
        CALL    PR10
        JMP     INPT2 
; 
INPT1:  CALL    TSTV
        JC      INPT6 
        JMP     INPT4 
INPT2:  PUSH    D 
        CALL    TSTV
        JC      QWHAT 
        LDAX    D 
        MOV     C,A 
        XRA     A 
        STAX    D 
        POP     D 
        CALL    MSG 
        MOV     A,C 
        DCX     D 
        STAX    D 
; 
INPT4:  PUSH    D 
        XCHG
        LHLD    CURRNT
        PUSH    H 
        LXI     H,INPUT 
        SHLD    CURRNT
        LXI     H,0 
        DAD     SP
        SHLD    NCNTR 
        PUSH    D 
        MVI     A,':' 
        CALL    GETLine
        LXI     D,LBUF
        CALL    EEXPR 
        POP     D 
        XCHG
        MOV     M,E 
        INX     H 
        MOV     M,D 
        POP     H 
        SHLD    CURRNT
        POP     D 
; 
INPT6:  POP     PSW 
        CALL    TEST
        DB      ',' 
        DB      LTEND-$-1 
        JMP     INPUT 
; 
CMDER:  LDAX    D 
        CPI     EOL 
        JZ      ENDL
; 
LET:    CALL    LTSUB 
        CALL    TEST
        DB      ',' 
        DB      LTEND-$-1 
        JMP     LET 
LTEND:  JMP     ENDL
; 
; 
EEXPR:  CALL    EXPR    ;EXTENDED EXP.
        PUSH H
        ;JMP     RST31 
RST31:  LXI     H,ROPKW-1 
        JMP     NXTKW 
; 
OGE:    CALL    IFEXQ 
        RC
        MOV     L,A 
        RET 
; 
ONE:    CALL    IFEXQ 
        RZ
        MOV     L,A 
        RET 
; 
OGT:    CALL    IFEXQ 
        RZ
        RC
        MOV     L,A 
        RET 
; 
OLE:    CALL    IFEXQ 
        MOV     L,A 
        RZ
        RC
        MOV     L,H 
        RET 
; 
OEQ:    CALL    IFEXQ 
        RNZ 
        MOV     L,A 
        RET 
; 
OLT:    CALL    IFEXQ 
        RNC 
        MOV     L,A 
        RET 
; 
NOROP:  POP     H 
        RET 
; 
IFEXQ:  MOV     A,C 
        POP     H 
        POP     B 
        PUSH    H 
        PUSH    B 
        MOV     C,A 
        CALL    EXPR
        XCHG
        XTHL
        CALL    CMINT 
        POP     D 
        LXI     H,0 
        MVI     A,1 
        RET 
; 
oshr: 
        call    EXPR
        mov     a,h 
        ora     a 
        jz      oshr2 
        pop     h       ;avoid b
        lxi     h,0 
        ret 
oshr2:  mov     b,l 
        pop     h 
        inr     b 
oshr1:  dcr     b 
        rz
        mov     a,h 
        ana     a 
        rar 
        mov     h,a 
        mov     a,l 
        rar 
        mov     l,a 
        jmp     oshr1 
; 
oshl: 
        call    EXPR
        mov     a,h 
        ora     a 
        jz      oshl2 
        pop     h       ;dummy pop
        lxi     h,0 
        ret 
oshl2:  mov     b,l 
        pop     h 
        inr     b 
oshl1:  dcr     b 
        rz
        dad     h 
        jmp     oshl1 
; 
EXPR:   CALL    TEST
        DB      '-' 
        DB      EXPR1-$-1 
        LXI     H,0 
        JMP     NEGA1 
; 
EXPR1:  CALL    TEST
        DB      '+' 
        DB      EXPR3-$-1 
EXPR3:  CALL    TERM
; 
EXPR2:  CALL    TEST
        DB      '+' 
        DB      NEGA0-$-1 
        PUSH    H 
        CALL    TERM
ADDDBL: XCHG
        XTHL
        DAD     D 
        POP     D 
        jmp     EXPR2 
; 
NEGA0:  CALL    TEST
        DB      '-' 
        DB      oor-$-1 
; 
NEGA1:  PUSH    H 
        CALL    TERM
        CALL    CHGSGN 
        JMP     ADDDBL
; 
oor:    CALL    TEST
        DB      '|' 
        DB      oand-$-1
        PUSH    H 
        CALL    TERM
        XCHG
        XTHL
        mov     a,d 
        ora     h 
        mov     h,a 
        mov     a,e 
        ora     l 
        mov     l,a 
        POP     D 
        jmp     EXPR2 
; 
oand:   CALL    TEST
        DB      '&' 
        DB      oxor-$-1
        PUSH    H 
        CALL    TERM
        XCHG
        XTHL
        mov     a,d 
        ana     h 
        mov     h,a 
        mov     a,e 
        ana     l 
        mov     l,a 
        POP     D 
        jmp     EXPR2 
; 
oxor:   CALL    TEST
        DB      '^' 
        DB      EXPRT-$-1 
        PUSH    H 
        CALL    TERM
        XCHG
        XTHL
        mov     a,d 
        xra     h 
        mov     h,a 
        mov     a,e 
        xra     l 
        mov     l,a 
        POP     D 
        jmp     EXPR2 
; 
; 
TERM:   CALL    FACTR 
; 
MULT:   CALL    TEST
        DB      '*' 
        DB      DIV-$-1 
        push    b 
        PUSH    H 
        CALL    FACTR 
;       mult NEW routine
        pop     b 
        push    d 
        xchg
        mvi     a,16
mul2:   dad     h 
        xchg
        dad     h 
        xchg
        jnc     mul1
        dad     b 
mul1:   dcr     a 
        jnz     mul2
        pop     d 
        pop     b 
        jmp     MULT
; 
DIV:    CALL    TEST
        DB      '/' 
        DB      EXPRT-$-1 
        PUSH    H 
        CALL    FACTR 
        MVI     B,0 
        CALL    ABS 
        XCHG
        XTHL
        CALL    ABS 
        MOV     A,D 
        ORA     E 
        JZ      AHOW
        PUSH    B 
        CALL    DIVID 
        MOV     H,B 
        MOV     L,C 
        POP     B 
TERM1:  POP     D 
        MOV     A,H 
        ORA     A 
        JM      QHOW 
        MOV     A,B 
        ORA     A 
        CM      CHGSGN 
        JMP     MULT
; 
FACTR:  LXI     H,FNKW-1
        JMP     NXTKW 
; 
FACT2:  CALL    TSTV
        JC      FNUM
        MOV     A,M 
        INX     H 
        MOV     H,M 
        MOV     L,A 
        RET 
; 
FNUM:   CALL    GINT
        MOV     A,B 
        ORA     A 
        RNZ 
; 
PARN:  CALL    TEST
        DB      '(' 
        DB      EXPER-$-1 
        CALL    EEXPR 
        CALL    TEST
        DB      ')' 
        DB      EXPER-$-1 
EXPRT:  RET 
EXPER:  JMP     QWHAT 
; 
RND:    CALL    PARN 
        MOV     A,H 
        ORA     A 
        JM      QHOW 
        ORA     L 
        JZ      QHOW 
        PUSH    D 
        XCHG
        LHLD    RWRK
        MOV     A,L 
        RAR 
        XRA     L 
        DAD     H 
        ACI     0 
        CMA 
        ANI     01H 
        ORA     L 
        MOV     L,A 
        SHLD    RWRK
        PUSH    B 
        CALL    DIVID 
        POP     B 
        POP     D 
        INX     H 
        RET 
; 
FNABS:  CALL    PARN 
        CALL    ABS 
        RET 
; 
RSIZE:  LHLD    OBTM
        PUSH    D 
        XCHG
        LXI     H,VTOP
        CALL    DIFF
        POP     D 
        RET 
; 
PEEK: 
        call    PARN
        mov     l,m 
        mvi     h,0 
        ret 
INPRT: 
        call    PARN
if	AKI80
	mov c,l
	inp  l		;in l(c)
        mvi     h,0 
endif
        ret 
; 
DIVID:  PUSH    H	;DIVIDE H BY DE
        MOV     L,H 
        MVI     H,0 
        CALL    DIVSB 
        MOV     B,C	;SAVE RESULT IN B
        MOV     A,L	;(REMINDER+L)/DE
        POP     H 
        MOV     H,A 
DIVSB:  MVI     C,0FFh	;(-1) RESULT IN C
DIVS1:  INR     C	;DUMB ROUTINE
        CALL    DIFF	;DIVIDE BY SUBTRACT
        JNC     DIVS1	;AND COUNT
        DAD     D 
        RET 
; 
DIFF:   MOV     A,L 		;HL-DE
        SUB     E 
        MOV     L,A 
        MOV     A,H 
        SBB     D 
        MOV     H,A 
        RET 
; 
ABS:    MOV     A,H 
        ORA     A 
        RP
CHGSGN:  XRA     A 	; HL <- -HL
        SUB     L 
        MOV     L,A 
        SBB     H 
        SUB     L 
        MOV     H,A 
        MOV     A,B ; B <- B ^ 80h
        XRI     80H 
        MOV     B,A 
        RET 
; 
CMINT:  MOV     A,H 
        XRA     D 
        JP      CPIN1 
        XCHG
CPIN1:  CALL    COMPR 
        RET 
; 
LTSUB:  CALL    TSTV
        JC      QWHAT 
        PUSH    H 
        CALL    TEST
        DB      '=' 
        DB      LTERR-$-1 
        CALL    EEXPR 
        MOV     B,H 
        MOV     C,L 
        POP     H 
        MOV     M,C 
        INX     H 
        MOV     M,B 
        RET 
LTERR:  JMP     QWHAT 
; 
ENDL:   CALL    SKPBL 
        CALL    TSTSC   ;TEST END OF LINE 
        JMP     QWHAT 
; 
TSTSC:  CALL    TEST
        DB      ':' 
        DB      TSCR1-$-1 
        POP     PSW 
        JMP     NXTGO 
; 
TSCR1:  CALL    TEST
        DB      EOL 
        DB      TSRTN-$-1 
        POP     PSW 
        JMP     RUN1
TSRTN:  RET 
; 
TSCR2:  CALL    SKPBL 
        CPI     EOL 
        RZ
; 
QWHAT:  PUSH    D 
AWHAT:  LXI     D,WHTMS 
ERROR:  SUB     A 
        CALL    MSG 
        POP     D 
        LDAX    D 
        PUSH    PSW 
        XRA     A 
        STAX    D 
        LHLD    CURRNT
        PUSH    H 
        MOV     A,M 
        INX     H 
        ORA     M 
        POP     D 
        JZ      START 
        MOV     A,M 
        ORA     A 
        JM      ERRIN 
        CALL    WLINE 
        DCX     D 
        POP     PSW 
        STAX    D 
        XRA     A 
        PUSH    D 
        LXI     D,ERRQ
        CALL    MSG 
        POP     D 
        CALL    MSG 
        JMP     START 
; 
;ERRQ:   DB      ESC,')?',ESC,'(',0 
ERRQ:   DB      '?',0 

; 
QSORRY:  PUSH    D 
ASORRY:  LXI     D,SRYMS 
        JMP     ERROR 
; 
SRCHLine:   MOV     A,H 
        ORA     A 
        JM      QHOW 
srchx   xchg
        lhld    OTOP
        xchg
SRCH1:  ldax    d 
        mov     c,a 
        inx     d 
        ldax    d 
        inx     d 
        mov     b,a 
        ora     a 
        jp      srch2
        dcx     d 		; MSB==1 then text end
        dcx     d 
        stc 			;Not found and Set Carry
        ret 
srch2:  mov     a,c 
        SUB     L 
        MOV     c,A 
        mov     a,b 
        SBB     H 
        JC      SKPL1 		; BC<HL? then SKPL1
        DCX     D 		; BC>=HL
        dcx     d 
        ora     c		; BC-HL =Zero?
        RET 
; 
SKIPL:  INX     D 
SKPL1:  INX     D 
SKPL2:  LDAX    D 
        CPI     EOL 
        JNZ     SKPL1 
        INX     D 
        JMP     SRCH1 
; 
MSG:    MOV     B,A 
MSG1:   LDAX    D 
        INX     D 
        CMP     B 
        RZ
        CALL    PUTC
        CPI     CR
        JNZ     MSG1
        RET 
; 
PR10:   CALL    TEST
        DB      '"' 		;"
        DB      PR13-$-1
PR11:   CALL    MSG 
        CPI     EOL 
        POP     H 
        JZ      RUN1
PR12:   INX     H 
        INX     H 
        INX     H 
        PCHL
; 
PR13:   CALL    TEST
        DB      ''''
        DB      PR15-$-1
        MVI     A,27H 
        JMP     PR11
; 
; 
PR15:   RET 
; 
decnum:;make decimal num string, C=pre Spaces count
	PUSH    D 
        LXI     D,10	;000Ah
	push d
        ;MOV     B,D ;B<-0
        ;DCR     C 
	lxi b,0004h
	push b ;SAVE SIGN & SPACE
dcn2:	CALL    DIVID	; B=HL/DE,H=REMINDER
        MOV     A,B 
        ORA     C 
	JZ      dcn3	;shou BC == 0?
        XTHL		;NO, SAVE REMAINDER
        DCR     L	;C'--,COUNT SPACE
        PUSH    H	;HL IS OLD BC
        MOV     H,B	;HL <-BC shou,;MOVE RESULT TO BC
        MOV     L,C
        JMP     dcn2	;AND DIVIDE BY 10
	;
dcn3:	pop b		;WE GOT ALL DIGITS I THE STACK,BC=sign,space count
	;; 
	MOV     E,L	;LAST REMAINDER IN E
	lxi h,lbuf
dcn4:	MOV     A,E 
        CPI     10	;stop mark?
        POP     D 
        RZ
        ADI     '0' 
        ;CALL    PUTC
	mov m,a
	inx h
        JMP     dcn4
	;; 
WINT:;make decimal num string, C=pre Spaces count
	PUSH    D 
        LXI     D,10	;000Ah
        PUSH    D	;push stop mark E==10
        MOV     B,D ;B<-0
        DCR     C ;C=SPACES
        CALL    ABS 
        JP      WINT1 
        MVI     B,'-' 
        DCR     C ;'-' TAKES SPACE
WINT1:  PUSH    B ;SAVE SIGN & SPACE
WINT2:  CALL    DIVID 
        MOV     A,B 
        ORA     C
	JZ      WINT3	;RESULT 0?
        XTHL		;NO, SAVE REMAINDER
        DCR     L	;AND COUNT SPACE
        PUSH    H	;HL IS OLD BC
        MOV     H,B	;MOVE RESULT TO BC
        MOV     L,C 
        JMP     WINT2 ;AND DIVIDE BY 10
	;
WINT3:  POP     B 	;WE GOT ALL DIGITS I THE STACK,BC=sign,space count
WINT4:  DCR     C	; print leading spaces, count C reg
        ;MOV     A,C 
        ;ORA     A 
        JM      WINT5 
        MVI     A,' ' 
        CALL    PUTC
        JMP     WINT4 
	;
WINT5:  MOV     A,B ;PRINT SIGN
        CALL    PUTC
        MOV     E,L ;LAST REMAINDER IN E
WINT6:  MOV     A,E 
        CPI     10		;stop mark?
        POP     D 
        RZ
        ADI     '0' 
        CALL    PUTC
        JMP     WINT6 
; 
WLINE:  LDAX    D 
        MOV     L,A 
        INX     D 
        LDAX    D 
        MOV     H,A 
        INX     D 
        MVI     C,4 
        CALL    WINT
        MVI     A,' ' 
        CALL    PUTC
        XRA     A 
        CALL    MSG 
        RET 
; 
POPV:   POP     B                   ;BC = RETURN ADDR.
        POP     H                   ;RESTORE LOPVAR, BUT
        SHLD    LOPVAR              ;=0 MEANS NO MORE
        MOV     A,H 
        ORA     L 
        JZ      NPOP                ;YEP, GO RETURN
        POP     H                   ;NOP, RESTORE OTHERS
        SHLD    LOPINC 
        POP     H 
        SHLD    LOPLMT
        POP     H 
        SHLD    LOPLN 
        POP     H 
        SHLD    LOPPT
NPOP:   PUSH    B                    ;BC = RETURN ADDR.
        RET 
; 
PSHV:   LXI     H,STKLMT                   ;*** PUSHA ***
        CALL    CHGSGN ; -HL
        POP     B                          ;BC=RETURN ADDRESS
        DAD     SP                         ;IS STACK NEAR THE TOP?
        JNC     QSORRY                      ;YES, SORRY FOR THAT
        LHLD    LOPVAR                      ;ELSE SAVE LOOP VAR'S
        MOV     A,H                         ;BUT IF LOPVAR IS 0
        ORA     L                          ;THAT WILL BE ALL
        JZ      NPSH
        LHLD    LOPPT                      ;ELSE, MORE TO SAVE
        PUSH    H 
        LHLD    LOPLN
        PUSH    H 
        LHLD    LOPLMT
        PUSH    H 
        LHLD    LOPINC 
        PUSH    H 
        LHLD    LOPVAR 
NPSH:   PUSH    H 
        PUSH    B 
        RET 
; 
; 
if aki80
BREAK:
	call getkey
        ORA     A 
        RZ
        CPI     3       ;CTRL_C 
        JZ      START 
        CPI     13H     ;CTRL_S 
        RNZ 
	;
GETC:
	call getch
        CPI     3 		;ctl C
        JZ      START 
        RET 
endif
	;; 
if cpm
sioinit:ret
	;; 
CRLF:   MVI     A,CR
PUTC:   CALL    PUTC1
        CPI     CR
        RNZ 
        PUSH    PSW 
        MVI     A,LF
        JMP     CRLF1 
PUTC1:  ORA     A 
        RZ
        PUSH    PSW 
CRLF1:  PUSH    B 
        PUSH    D 
        PUSH    H 
        MVI     C,6 
        MOV     E,A 
        CALL    BDOS
        POP     H 
        POP     D 
        POP     B 
        POP     PSW 
        RET 
; 
BREAK:  PUSH    B 
        PUSH    D 
        PUSH    H 
        MVI     C,06H 
        MVI     E,0FFH
        CALL    BDOS
        POP     H 
        POP     D 
        POP     B 
        ORA     A 
        RZ
        CPI     3       ;CTRL_C 
        JZ      START 
        CPI     13H     ;CTRL_S 
        RNZ 
	; 
GETC:
	call getch
        CPI     3 		;ctl C
        JZ      START 
        RET 
endif
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;
; simple input line
;;;;;;;;;;
if not LINEDX
GETLine:CALL    PUTC
        LXI     D,LBUF
GETL1:  CALL    GETC
        CPI     BS       ;BACK-SPACE 
        JZ      RUBout
        CPI     DELBS      ;
        JZ      RUBout
        CPI     CAN     ;CAN
        JZ      CANX 
	push psw
        MOV     A,E 
        CPI     7FH     ;BUF FULL?
        jc     GETL11
	pop psw
        MVI     A,7     ;BELL 
        CALL    PUTC
	jmp     GETL1 
GETL11:	
	pop psw
	;call excase
        CALL    PUTC
        STAX    D 
        INX     D 
        CPI     CR
        ;jz      entline         ;CRLF & RET
	jnz      GETL1         ;CRLF & RET do exec


entline:
        dcx     d 
        xra     a       ;EOL
        stax    d 
        inx     d 
        ; 
        mvi     a,0FFh  ;put stop-mark
        stax    d 
        inx     d 
        stax    d 
        dcx     d 
        ret 
; 
; 
RUBout: MOV     A,E 
        CPI     LBUF AND 0FFH 
        JZ      CANX1
        MVI     A,BS
        CALL    PUTC
        MVI     A,' '
        CALL    PUTC
        MVI     A,BS
        CALL    PUTC
        DCX     D 
        JMP     GETL1 
; 
CANX:    MVI     A,'\' 
        CALL    PUTC
CANX1:   CALL    CRLF
        MVI     A,prompt
        JMP     GETLine
endif
;;;;
if NEVER
excase:				;exchange alphabet case, upper <-> lower
	cpi	'A'
	rc	;nonalpha
	cpi	'Z'+1
	jnc	excase2
	xri	020h	;tolower 'A'~'Z'
	ret	
	;
excase2:	
	cpi	'a'
	rc	;nonalpha
	cpi	'z'+1
	rnc	;nonalpha
	xri	020h	;toupper 'a'~'z'
	ret
endif
;;;;;;;;;;;;;;; 
; 
;
INIT:   LXI     D,INIMSG
        XRA     A 
        CALL    MSG 
        RET 
; 
INIMSG: 
        DB      'X Palo Alto Tiny BASIC' 
        DB      '(JQ3KLL,01/JUN/2025, Jul/01/1983)',00h
        ;DB      CR
;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
; 	line editor
;	copyright by S.TAKEOKA  (JQ3KLL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;TEST equ 1

left equ  'B' and 1fh
right equ  'F' and 1fh
kltop equ  'A' and 1fh
klend equ  'E' and 1fh
klpre equ  'P' and 1fh
delc equ  'D' and 1fh
;
ctlC equ  'C' and 1fh
BS	equ 8 ;back space
CR equ  0dh
LF equ  0ah
DELBS	equ 7Fh ;back space
;DELBS	equ 0FFh ;back space
;
	;;;;
	;public lined
	;;;
	;aseg
;
if LEDTEST
;;;;;;; Line editor test
lbufsiz equ 80
;lbuf equ 8000h
lbuf equ 8022h
	org lbuf
	ds lbufsiz
	;
	org 100h
main:
	call crlf
	mvi a,'?'
	call putc
	call lined
	lxi h,lbuf
	call crlf
	mvi a,' '
	call putc
	call puts
	jmp main
	;
endif


;;;;;
if LINEDX
GETLine:
endif
	CALL    PUTC
        ;LXI     D,LBUF
lined:
	lxi h,lbuf	;HL holds current insert point
	mov e,l		;DE holds end of line
	mov d,h
	mvi m,0	;put EOL
	;xra a	;put EOL
	;stax d
	;
linedloop:
	call getch
	cpi  ' '
	jc   ctrl
	;
	cpi DELBS
	jz ctrbs3	;Back Space
	;
	;normal char, insert
normal:	mov b,a
	mov a,e
	sui lbuf MOD 256	;e-low(lbuf) *****
	cpi lbufsiz
	jnc linedloop	;buf full
	;
	mov a,b
	CALL putc
	;
	xchg
	mvi m,0 ;put EOL at (DE)
	xchg
	call strrcpy	;from DE, 1 char ushiro zurasi copy
	;mov a,b
	;mov m,a
	mov m,b
	inx h	;inr l
	inx d	;inr e
	xra a
	stax d
	call puts
	call backspcs
	jmp linedloop
	;
ctrl:
	cpi ctlC ;break
	;;;jz 0 ;exit to CP/M
        JZ      START 
	;
	cpi left
	jnz ctl0
	mov a,l
	sui lbuf MOD 256	;l-low(lbuf) *****
	jz linedloop ;at top of line
	;dcr l
	;jp ctrl1
	;inr l ;at top of line
	;jmp linedloop
	dcx h	;dcr l
ctrl1:	mvi a,BS
	call putc	; move cursor Left
	jmp linedloop
ctl0:
	cpi CR
	jnz ctl8
	xchg
	mvi m,0	;put EOL at (DE)
	inx h
        mvi     m,0FFh  ;put stop-mark1
	inx h
        mvi     m,0FFh  ;put stop-mark2
        ;mvi     m,07Fh  ;put stop-mark2
	dcx h
	;dcx h
	xchg
	;
	push d
	lxi d,lbuf
	lxi h,lbuf2
	call cpy
	call crlf
	pop d
	ret
	;
ctl8:
	cpi klpre
	jnz ctl1
	lxi d,lbuf2
	lxi h,lbuf   ; copy from history buf(lbuf2)
        CALL    cpy	;HL points (end of line)+1
	dcx h
	push h
	call crlf
	lxi h,lbuf
	call puts
	pop h
	mov e,l	;DE <-HL
	mov d,h
	;lxi h,lbuf
	jmp linedloop
	;
ctl1:
	cpi right
	jnz ctl2
	mov a,m
	ora a	;EOL?
	jz linedloop
	mov a,m
	inx h	;inr l
	CALL    putc
ctl2:
	cpi kltop
	jnz ctl3
	mov a,l
	sui lbuf MOD 256	;l-low(lbuf) *****
	mov c,a
ctrlx3:
	jz linedloop
	;jp ctrlx2
	;jmp linedloop
ctrlx2:	mvi a,BS
	call putc	; move cursor Left
	dcx h	;dcr l
	dcr c
	jmp ctrlx3
	;
ctl3:
	cpi klend
	jnz ctl4
	call puts
	mov l,e
	mov h,d
	jmp linedloop
	;
ctl4:
	cpi delc
	jnz ctl5
ctrbs2:	;delete 1char
	mov a,m
	ora a
	jz linedloop
	;
	call strcpy	;MAE zurasi copy
	dcx d	;dcr e
	call puts
	mvi a,' '
	call putc
	;mvi a,BS
	;call putc
	inr c
	call backspcs
	jmp linedloop
	;
ctl5:
;ctl6:
	cpi BS
	jnz ctl7
ctrbs3:
	mov a,l
	sui lbuf MOD 256	;l-low(lbuf) *****
	jz linedloop ;at top of line
	dcx h	;dcr l
ctrbs1:	mvi a,BS	; back 1char
	call putc
	jmp ctrbs2	;del char
	;
ctl7:
	jmp linedloop
;;


;;;;;;;;;;;;;;;
if AKI80
PCTCC3	EQU	13H
PSIOAD	EQU	18H
PSIOAC	EQU	19H
PSIOBD	EQU	1AH
PSIOBC	EQU	1BH

;getch:				;
inpkey:	
;	SIO RXdata -> A reg
	xra a
	out	PSIOAC	;select RR0
rx1:	IN	PSIOAC	;CHECK RECEIVE
	ani 00000001B	;01h
	jz rx1
	IN	PSIOAD	;GET RX DATA
	ret
	;;;;
getkey: ; check SIO RX, 0 == NO key
	xra a
	out	PSIOAC	;select RR0
	IN	PSIOAC	;CHECK RECEIVE
	ani 00000001B		; 0 == NO key
	rz
	IN	PSIOAD	;GET RX DATA
	ret
;;;;;;;;

CRLF:   MVI     A,CR
putc:
	call putc1
	cpi CR
	rnz
	mvi a,LF
	push psw
	jmp CRLF1
	;; 
;	A reg -> SIO TX
putc1:
TXA:	PUSH	psw		;SAVE DATA
CRLF1:
TXLOOP:	IN	PSIOAC	;CHECK STATUS
	;BIT	2,A		;IF BUFFER NOT EMPTY
	ani	00000100B		;IF BUFFER NOT EMPTY
	jz txloop
	POP	psw		;RESTORE DATA
	OUT	PSIOAD	;TRANSFER
	RET
;
;	Init CTC, SIO
;This routine gotten from https://github.com/vintagechips/saki80basic
;	SIOA COMMAND CHAIN
SIOACD:
	DB	00011000B	;RESET
	;DB	01H,00010000B	;RX INTERRUPT ENABLE
	DB	01H,00000000B	;RX INTERRUPT disABLE
	DB	04H,01000100B	;FORMAT
	DB	05H,11101010B	;TX ENABLE
	DB	03H,11000001B	;RX ENABLE
SIOACL	EQU	$-SIOACD
;
;	SIOB COMMAND CHAIN
SIOBCD:
	DB	00011000B		;RESET
	DB	01H,00000000B		;DISABLE STATUS/AFFECTS VECTOR
	;DB	02H,INTVCT AND 00FFH	;SET INTERRUPT VECTOR
	DB	02H, 00FFH	;SET INTERRUPT VECTOR (gomi)
SIOBCL	EQU	$-SIOBCD
	;; 
;
;	CTC INITIALIZE
;
sioinit:	
	mvi	A,00000111B
	OUT	PCTCC3
	mvi	A,4		;9.8304MHz
;	LD	A,3		;7.3728MHz
;	LD	A,2		;4.9152MHz
;	LD	A,1		;2.4576MHz
	OUT	PCTCC3
;
;	SIO INITIALIZE
	;; SIO-A
	mvi	B,SIOACL	;LENGTH
	mvi	C,PSIOAC	;I/O ADDRESS
	lxi	H,SIOACD	;COMMAND ADDRESS
	outir	;OTIR
	;; SIO-B
	mvi	B,SIOBCL	;LENGTH
	mvi	C,PSIOBC	;I/O ADDRESS
	lxi	H,SIOBCD	;COMMAND ADDRESS
	outir	;OTIR
	;; 
	ret
endif
;;;;;;;;
;
;;;;;;;;
getch:
        CALL    inpkey
	push psw
	lda (CHUPF)
	ora a
	jnz alpha1		;Not Zero Upper
	pop psw
	ret
alpha1:	pop psw
	cpi	'A'
	;jc	nonalpha
	rc
	cpi	'Z'+1
	jnc	alpha2
	xri	020h	;tolower 'A'~'Z'
	;jmp	nonalpha
	ret
	;
alpha2:	
	cpi	'a'
	;jc	nonalpha
	rc
	cpi	'z'+1
	;jnc	nonalpha
	rnc
	xri	020h	;toupper 'a'~'z'
nonalpha:	
	ret
;;;
if cpm
;;;
inpkey:
        call    getkey
        ora      a
        jz	inpkey
        ret
;
getkey:
        push    b
        push    d
        push    h
        mvi      e,0ffh
        mvi      c,6
        call    5
        pop     h
        pop     d
        pop     b
        ret
endif
;;;;;;;;;
	;from DE , 1 char ushiro zurasi copy
strrcpy:
	push h
	push d
	mov a,e
	sub l
	jz strcpret
	inr a
	mov c,a
	mov l,e
	mov h,d
	inx h
strcp1:	ldax d
	mov m,a
	dcx h
	dcx d
	dcr c
	jnz strcp1
strcpret:
	pop d
	pop h
	ret
;
strcpy:	;MAE zurasi copy
	push h
	push d
	mov e,l
	mov d,h
	inx d	;inr e
	call cpy
	pop d
	pop h
	ret
	;jmp strcpret
	;
cpy:	; (DE)->(HL), HL=EOL+1 on return 
	ldax d
	mov m,a
	inx h
	inx d
	ora a
	rz
	jmp cpy

;
backspcs:
	mvi a,BS
	inr c
bcksps1:
	dcr c
	rz
	call	putc
	jmp bcksps1
;;;
puts:	push h
	mvi c,0	;count chars
puts1:	mov a,m
	ora a
	jz putsret
	call	putc
	inx h
	inr c
	jmp puts1	
putsret:pop h
	ret
;
;;;;
;;; line editor test
;;
if LEDTEST
;
crlf:	mvi a,CR
	call putc
	mvi a,LF
	call putc
	ret
;
endif
;;
;; 	org RAMTOP
;TextArea equ    $       ;basic text starts here 
;; 
	org VTOP
	DS 60 ; @ ~ ]

	;org WORKTOP
WORKTOP equ $
;;;;;;;;;;;;;;;;;;;;;
CURRNT    DS      2       ;Top of current Line
STKGOS   DS      2 
NCNTR   DS      2 
LOPVAR   DS      2 
LOPINC   DS      2 
LOPLMT    DS      2 
LOPLN   DS      2 
LOPPT    DS      2 
RWRK    DS      2 
; 
; 
;        ORG     ($ AND 0FF00H)+0f8H
;        ORG     ($ AND 0FF00H)+100H
LBUF    DS      LBUFSIZ
LBUF2    DS      LBUFSIZ
; 
ALLEND	equ $

	org	stack
if AKI80
	org rsttable
	ds 4*8
endif
	
        END 

