.model tiny
.data
    STARTING_IP DW ?   
    PA EQU 00H
    PB EQU 02H
    PC EQU 04H  
    CR_8255 EQU 06H
    MODENO DB 00H
    STACK DW 100 DUP(?)
    TOP_STACK LABEL WORD   
.code
.startup

    
     LEA SP, TOP_STACK ;---STORE THE ISR ADDRESS OF THE NMI(STOP) IN THE IVT
    MOV AX,0
    MOV ES,AX ;calculate vector address for interrupt 02H(NMI)
    MOV AL,02H
    MOV BL,04H
    MUL BL
    MOV BX,AX
    
    MOV SI,OFFSET [STOP_BUTTON]
    MOV ES:[BX],SI
    ADD BX,2
    
	MOV AX,0000
    MOV ES:[BX],AX  
	
    MOV AL,10010000B        ;programming the 8255 10010000b
    OUT CR_8255,AL
	MOV AH ,00h
	MOV MODENO , AH
	
	
	BEGIN:
		MOV AL , 00h
		OUT PC , AL
		MOV AL , 00h
		OUT PB , AL
		MOV AL , 00h
		MOV MODENO , AL
		
    LOAD:	;polling the LOAD button and DOOR_LOCK switch
		 MOV AL , MODENO
		 OUT PC , AL
		 IN  AL, PA
         CMP AL, 11101110B   
         JZ PRESTART
		 CMP AL, 11111011B
		 JZ INCR
		 CMP AL, 11101011B
		 JZ INCR
		 JMP LOAD
    INCR: INC BYTE PTR MODENO ;if LOAD button is pressed increase the MODE number
		  MOV AH , MODENO
		  CMP AH , 03H
		  JA RSCNT
          CALL RELEASE_DELAY ;one press of LOAD button should only raise MODE number by 1
		  JMP LOAD
	RSCNT:MOV AH , 00H
     	  MOV MODENO , AH
		  CALL RELEASE_DELAY
		  JMP LOAD
		
	PRESTART: MOV AH , MODENO
			  CMP AH , 00H
			  JZ LOAD
			  JMP OUT1
	
    OUT1: 
		MOV AL, MODENO
		CMP AL, 01H             ;displaying on the 7 segment display
		JNE OUT2
		MOV AL, 01H
		OUT PC, AL
		JMP LIGHT
		
    OUT2:
		CMP AL, 02H
		JNE OUT3
		MOV AL, 02H
		OUT PC, AL
		JMP MEDIUM
			
	OUT3:
		MOV AL, 03H
		OUT PC, AL
		JMP HEAVY
		
	LIGHT:                  ;LIGHT MODE
        CALL WATER_MAX      ;sensing if water level is max
	
        MOV AL,01H          ;rinse cycle
        OUT PB,AL        ;activating the agitator
		MOV CX,2
        X1:	CALL DELAY_1m    ;rinse cycle runs for 2 minutes
		LOOP X1
			
			
        MOV AL,00H
        OUT PB,AL        ;stop rinse cycle(i.e. stop agitator)
        CALL BUZZER_RINSE   ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY ;only when resume button comes up, proceed
        
		
		
        MOV AL,01H          ;wash cycle
        OUT PB,AL
		MOV CX,3
        X2: CALL DELAY_1m    ;wash cycle runs for 3 minutes
		LOOP X2
			
        MOV AL,00H
        OUT PB,AL
        CALL BUZZER_WASH    ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
		CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY  
		
		
        MOV AL,01H          ;rinse cycle
        OUT PB,AL        ;activating the agitator
		MOV CX,2
        X3:CALL DELAY_1m    ;rinse cycle runs for 2 minutes
		LOOP X3
			
        MOV AL,00H
        OUT PB,AL        ;stop rinse cycle(i.e. stop agitator)
        CALL BUZZER_RINSE   ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY ;only when resume button comes up, proceed
		
        MOV AL,02H          ;dry cycle
        OUT PB,AL        ;activating the revolving tub
		
		MOV CX,2
        X4:
			CALL DELAY_1m    ;dry cycle runs for 2 minutes
		LOOP X4
			
        MOV AL,00H
        OUT PB,AL
        CALL BUZZER_DRY
        JMP DONE_WASHING
        
    MEDIUM: 
	    ;MEDIUM MODE
        CALL WATER_MAX      ;sensing if water level is max
        MOV AL,01H          ;rinse cycle
        OUT PB,AL        ;activating the agitator
		
        MOV CX,3
        X5:
			CALL DELAY_1m    ;rinse cycle runs for 3 minutes
			LOOP X5
			
        MOV AL,00H
        OUT PB,AL        ;stop rinse cycle(i.e. stop agitator)
		
        CALL BUZZER_RINSE   ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY ;only when resume button comes up, proceed
        
        
        MOV AL,01H          ;wash cycle
        OUT PB,AL
		
        MOV CX,5
        X6:
			CALL DELAY_1m    ;wash cycle runs for 5 minutes
			LOOP X6
			
        MOV AL,00H
        OUT PB,AL
		
        CALL BUZZER_WASH    ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY  
        
        MOV AL,01H          ;rinse cycle
        OUT PB,AL        ;activating the agitator
		
        MOV CX,3
        X7:
			CALL DELAY_1m    ;rinse cycle runs for 3 minutes
			LOOP X7
			
        MOV AL,00H
        OUT PB,AL        ;stop rinse cycle(i.e. stop agitator)
        CALL BUZZER_RINSE   ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY ;only when resume button comes up, proceed
        
        MOV AL,02H          ;dry cycle
        OUT PB,AL        ;activating the revolving tub
		
        MOV CX,4
        X8:
			CALL DELAY_1m    ;dry cycle runs for 4 minutes
			LOOP X8
			
		
        MOV AL,00H
        OUT PB,AL
        CALL BUZZER_DRY
        JMP DONE_WASHING
		
		
		
    HEAVY:                  ;HEAVY MODE
        CALL WATER_MAX      ;sensing if water level is max
        MOV AL,01H          ;rinse cycle
        OUT PB,AL        ;activating the agitator
		
		MOV CX,3
        X9:
			CALL DELAY_1m    ;rinse cycle runs for 3 minutes
			LOOP X9
			
        MOV AL,00H
        OUT PB,AL        ;stop rinse cycle(i.e. stop agitator)
        CALL BUZZER_RINSE   ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY ;only when resume button comes up, proceed
       
        
        MOV AL,01H          ;wash cycle
        OUT PB,AL
		
		
        MOV CX,5
        X10:
			CALL DELAY_1m    ;wash cycle runs for 5 minutes
			LOOP X10
			
			
        MOV AL,00H
        OUT PB,AL
        CALL BUZZER_WASH    ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY
        CALL WATER_MAX      ;sensing if water level is max
		
		
        MOV AL,01H          ;rinse cycle
        OUT PB,AL        ;activating the agitator
		
        MOV CX,3
        X11:
			CALL DELAY_1m    ;rinse cycle runs for 3 minutes
			LOOP X11
			
        MOV AL,00H
        OUT PB,AL        ;stop rinse cycle(i.e. stop agitator)
        CALL BUZZER_RINSE   ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY ;only when resume button comes up, proceed
        
        
		MOV AL,01H          ;wash cycle
        OUT PB,AL
		
        MOV CX,5
        X12:
			CALL DELAY_1m    ;wash cycle runs for 5 minutes
			LOOP X12
			
        MOV AL,00H
        OUT PB,AL
        CALL BUZZER_WASH    ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL WATER_MAX      ;check if water is at max level again for wash cycle       
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY
        CALL WATER_MAX      ;sensing if water level is max
		
        MOV AL,01H          ;rinse cycle
        OUT PB,AL        ;activating the agitator
		
        MOV CX,3
        X13:
			CALL DELAY_1m    ;rinse cycle runs for 3 minutes
			LOOP X13
			
        MOV AL,00H
        OUT PB,AL        ;stop rinse cycle(i.e. stop agitator)
        CALL BUZZER_RINSE   ;play the buzzer for 1 minute
        CALL WATER_MIN      ;check if water has drained fully
        CALL CHECK_RESUME   ;check if resume button is pressed   
        CALL RELEASE_DELAY ;only when resume button comes up, proceed
        
        MOV AL,02H          ;dry cycle
        OUT PB,AL        ;activating the revolving tub
		
        MOV CX,4
        X14:
			CALL DELAY_1m   ;dry cycle runs for 4 minutes
			LOOP X14
        MOV AL,00H
        OUT PB,AL
        CALL BUZZER_DRY
        JMP DONE_WASHING
                
    DONE_WASHING:
		MOV AL, 0EH
		OUT PC, AL
		CALL DELAY_1m
		MOV AL , 00H
		MOV MODENO , AL
		
		JMP BEGIN
		
	 ;INF:
    ;JMP INF
    STOP_BUTTON:              ;this procedure is an ISR for NMI(STOP button)
        MOV BP,SP
        MOV AL,00H
        OUT PB,AL
        OUT PC,AL
        MOV AX,STARTING_IP ;this will put in stack the IP address of the starting line of program
        MOV [BP],AX
        IRET               ;now the IP address popped will be of the starting line of program
		
	JMP DONE_WASHING
	
.exit
		
	
STORE_IP PROC NEAR          ;this procedure will store the IP address
    MOV BP,SP               ;of the label POLL_START
    MOV AX,[BP]
    MOV STARTING_IP,AX
    RET
STORE_IP ENDP



RELEASE_DELAY PROC NEAR    ;this procedure checks all the buttons and
    RELEASE:               ;returns only of all the buttons are up
        IN AL,PA
        OR AL,11110000B
        CMP AL,11111111B
        JNZ RELEASE
    RET
RELEASE_DELAY ENDP

INITIALIZE_INT PROC NEAR
    MOV AX, 0
    MOV ES, AX
    CLI
    MOV WORD PTR ES:[320], OFFSET INT50H
    MOV WORD PTR ES:[322], CS
    STI  
    MOV AX, 0
    RET
INITIALIZE_INT ENDP 

INT50H PROC FAR
    ;int 3
    MOV AL, 08H
    OUT PC, AL
    IRET
INT50H ENDP 


DELAY_1m PROC NEAR  ;this procedure is used to generate a delay of 1 minute
		MOV DX , 001Fh
		P2 :
			MOV BX , 0FFFFh 
			P1 : 
			DEC BX
			NOP
			JNZ P1
		DEC DX
		JNZ P2
		RET
DELAY_1m ENDP




WATER_MAX PROC NEAR         ;this procedure checks if water level is max
                            ;water level is max when the pressure sensitive switch(WATER_MAX) is pressed
    CHECK1:
        IN AL,PA
        CMP AL,11001111B
    JNE CHECK1 
    RET
WATER_MAX ENDP 

WATER_MIN PROC NEAR         ;this procedure checks if water level is min
                            ;water level is min when the pressure sensitive switch(WATER_MIN) is pressed
    CHECK2:
        IN AL,PA
        CMP AL,10101111B
    JNE CHECK2 
    RET
WATER_MIN ENDP

BUZZER_RINSE PROC NEAR      ;this procedure activates a buzzer after rinse cycle in complete
    MOV AL,10H
    OUT PB,AL
    CALL DELAY_1m
    MOV AL,00H
    OUT PB,AL
    RET
BUZZER_RINSE ENDP

BUZZER_WASH PROC NEAR       ;this procedure activates a buzzer after wash cycle in complete
    MOV AL,08H
    OUT PB,AL
    CALL DELAY_1m
    MOV AL,00H
    OUT PB,AL
    RET
BUZZER_WASH ENDP 

BUZZER_DRY PROC NEAR        ;this procedure activates a buzzer after dry cycle in complete
    MOV AL,04H
    OUT PB,AL
    CALL DELAY_1m
    MOV AL,00H
    OUT PB,AL
    RET
BUZZER_DRY ENDP
                            
CHECK_RESUME PROC NEAR      ;this procedure checks if resume button is pressed or not
    
    CHECKR:
        IN AL,PA
        OR AL,11100111B
        CMP AL,11100111B
        JNE CHECKR     
    RET
CHECK_RESUME ENDP


END


