; KEYPAD ASCII DOT MATRIX
; 3375
; Fall2025
; Programming
; Git URL

;Device Setup
;-------------------------------------------------------------------------

;Configuration
    ; PIC16F883 Configuration Bit Settings

    ; Assembly source line config statements

    ; CONFIG1
	CONFIG  FOSC = XT             ; Oscillator Selection bits (XT oscillator: Crystal/resonator on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN)
	CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
	CONFIG  PWRTE = OFF           ; Power-up Timer Enable bit (PWRT disabled)
	CONFIG  MCLRE = ON            ; RE3/MCLR pin function select bit (RE3/MCLR pin function is MCLR)
	CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
	CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)
	CONFIG  BOREN = OFF           ; Brown Out Reset Selection bits (BOR disabled)
	CONFIG  IESO = OFF            ; Internal External Switchover bit (Internal/External Switchover mode is disabled)
	CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is disabled)
	CONFIG  LVP = OFF             ; Low Voltage Programming Enable bit (RB3 pin has digital I/O, HV on MCLR must be used for programming)

    ; CONFIG2
	CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
	CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)

;Include Statements
	
// config statements should precede project file includes.
#include <xc.inc>
	
;Code Section
;-------------------------------------------------------------------------
      
; Register Setup
    STATUS_SAVE EQU 0x20
    W_SAVE EQU 0x21
    DELAY_TIME EQU 0x22
    TEST_KEYPAD EQU 0x23
    SAVED_VALUE EQU 0x24
    EEPROM_ADDRESS EQU 0x25
    DELAY_FLAG EQU 0x26
    IDLE EQU 0x28
    IDLE_COUNT EQU 0x29    
    INPUT_RECEIVED EQU 0x30
    R_MODE EQU 0x31
    P_MODE EQU 0x32
    PLAYBACK_COUNTER EQU 0x33
 ;Asign A Value To A Variable

;Start Of Program
    PSECT resetVect,class=CODE,delta=2	;Reset Vector Adress
    GOTO Start 

    PSECT isrVect,class=CODE,delta=2
    GOTO INTERRUPT_HANDLER
    
; Setup Code That Runs Once At Power Up/Reset
    PSECT code,class=CODE,delta=2
;------------------------------------------------------------------------
Start:
;PORTB
;Bank 3
    BSF STATUS,5	    ;Bit Set File Address -> To STATUS Register -> RP0 corresponds to bit 5
    BSF STATUS,6	    ;Bit Set File Address -> To STATUS Register -> RP0 corresponds to bit 5
    CLRF ANSELH		    ;Sets Inputs to Digital  
    CLRF INTCON		    ;Disables interrupt control register
    CLRF OPTION_REG	    ;Clears adrress OPTION_REG(0x81)
    MOVLW 0xF0		    ;Write's literal number to w(accumulator)
    MOVWF TRISB		    ;Sets PORTB as half inputs and half outputs
    CLRF EECON1
    CLRF EECON2
    BANKSEL EEADRH
    CLRF   EEADRH
;BANK 2
    BCF STATUS,5	    ;Changes bank bit to 1
    BSF STATUS,6	    ;Clears bank bit to 0
    CLRF CM2CON1	    ;Sets all bits in CM2CON1 to 0
    
;Bank 1
    BSF STATUS,5
    BCF STATUS,6
    MOVLW 0x00		    ;Sets all bits to 0, disabling Weak Pull Ups
    MOVWF WPUB
    CLRF IOCB		    ;Sets all bits to 0, disables interruption on change B

;Bank 0
    BCF STATUS,5
    BCF STATUS,6
    CLRF CCP1CON
;-------------------------------------------------------------------------
;PORTC
;Bank 3
    BSF STATUS,5
    BSF STATUS,6
    MOVLW 0x03
    MOVWF ANSEL		    ;Enables Analog Inputs for AN0
    
;Bank 1
    BSF STATUS,5
    BCF STATUS,6
    CLRF PSTRCON
    MOVLW 0x00
    MOVWF TRISC
;Bank 0
    BCF STATUS,5
    BCF STATUS,6
    CLRF SSPCON
    CLRF RCSTA
    CLRF T1CON
    CLRF CCP1CON
    CLRF CCP2CON
    CLRF PORTC
    CLRF PORTB 
;------------------------------------------------------------------------
;PORTA
;Bank 3
    BSF STATUS,5
    BSF STATUS,6
    CLRF ANSEL		    ;Disables analog inputs PORTA
    
;BANK 2
    BCF STATUS,5	    ;Changes bank bit to 1
    BSF STATUS,6	    ;Clears bank bit to 0
    CLRF CM2CON0	    ;Disables comparator C2 control register 0
    CLRF CM1CON0	    ;Disables comparator C1 control register 0
    
;BANK1
    BSF STATUS,5
    BCF STATUS,6
    CLRF PCON
    MOVLW 0x03
    MOVWF TRISA
   
;BANK0
    BCF STATUS,5
    BCF STATUS,6
    CLRF SSPCON
    CLRF ADCON0
    MOVLW 0x00
    MOVWF PORTA
;=======================================================
    MOVLW 0x64
    MOVWF IDLE_COUNT
    BSF IDLE,0		    ;IDLE,B0 - SET AT BEGINNING TO DEFAULT IDLE
    CLRF R_MODE
    CLRF SAVED_VALUE
    CLRF INPUT_RECEIVED
    CLRF TEST_KEYPAD
    CLRF DELAY_FLAG
    CLRF P_MODE
    CLRF PLAYBACK_COUNTER
;---------------------------------------------------------
;Timer2
    CLRF PIR1
    CLRF TMR2
    CLRF T2CON		    ;INIT ALL 0's on Startup
   
    BSF STATUS,5
    MOVLW 0x02
    MOVWF PIE1		    ;Enable TMR2 INT Flag
    
    MOVLW 0xD0
    MOVWF PR2		    ;Sets TMR2 to compare to max count
   
    MOVLW 0xC0
    MOVWF INTCON
  
    BCF STATUS,5
    MOVLW 0x27
    MOVWF T2CON		    ;Sets PreSclr.1:16 & PostSclr.1:5, Turns on TMR2

GOTO MAIN
;---------------------------------------------------------------------
WRITE:			    ;IN CHARGE OF WRITING DATA TO EEPROM REGISTERS
    BANKSEL EEDATA	    ;
    MOVWF EEDATA	    ;PICKS DATA FROM EEPROM REGISTER
    BANKSEL EECON1	    ;
    BCF EECON1,7	    ;ACCESS DATA MEMORY NOT PROGRAM MEMORY
    BSF EECON1,2	    ;ALLOWS EEPROM WRITE ENABLE	      
    BCF INTCON,7	    ;DISABLES GIE, WE DO NOT WANT TO INTERRUPTED
    BTFSC INTCON,7
    GOTO $-2
    MOVLW 0x55
    MOVWF EECON2
    MOVLW 0xAA
    MOVWF EECON2
    BSF EECON1,1
    WAIT:		    ;LOOP TO ENSURE WR IS COMPLETE
    BTFSC EECON1,1	    ;WRITE CONTROL BIT, 1 = INIT WR, 0 = WR COMPLETE
    GOTO WAIT		    ;
    BCF EECON1,2	    ;NO MORE WRITING IS ALLOWED
    BANKSEL PIR2	    ;
    BCF PIR2,4		    ;CLEARS THE EEIF FLAG, OPERATION COMPLETE
    ;GOTO MAIN
    GOTO RETURN_FROM_ISR    ;EXIT
    
READ:			    ;IN CHARGE OF READING & DISPLAYING DATA
    BANKSEL EECON1	    ;
    BCF EECON1,7	    ;ACCESS DATA MEMORY NOT PROGRAM MEMORY
    BSF EECON1,0	    ;ALLOWS EEPROM READ ENABLE
    NOP
    NOP
    BANKSEL EEDATA
    MOVF EEDATA,0	    ;READS DATA & PLACES IT IN W
    BANKSEL PORTC	    ;
    MOVWF PORTC		    ;DISPLAY DATA ON PORTC
    GOTO RETURN_FROM_ISR    ;EXIT
      
INTERRUPT_HANDLER:	    ;ISR
    MOVWF W_SAVE	    ;PLACES W INTO GENTLY INTO A WARM AND COMFY SPOT
    MOVF STATUS,0	    ;MOVES STATUS -> W
    MOVWF STATUS_SAVE	    ;PLACES STATUS GENTLY INTO A WARM AND COMFY SPOT
    BANKSEL PIR1	    ;
    BTFSS PIR1,1	    ;CHECKS TMR2IF
    GOTO RETURN_FROM_ISR    ;IF TMR2IF NOT SET, EXIT
    BTFSC IDLE,0	    ;TMR2 FLAG WAS SET, ARE WE SUPPOSE TO IDLE?
    GOTO MODE_CHANGE	    ;MODE_CHANGE WILL DETERMINE IDLE,RECORD, OR PLAYBACK
    BTFSC PORTA,2	    ;CHECKING TO SEE IF THE STOP BUTTON WAS PRESSED DURING OPERATION
    GOTO END_RECORD_PLAYBACK;THIS IS FIRST BECAUSE IT HAS PRIORITY
    BTFSC P_MODE,0	    ;CHECK IF WE ARE PLAYBACK MODE
    GOTO CONTINUE_PLAYBACK  ;
    BTFSC R_MODE,0	    ;CHECK IF R_MODE IS ON, 
    GOTO CONTINUE_RECORDING ;
    GOTO RETURN_FROM_ISR    ;
    
MODE_CHANGE:		    ;DETERMINES WHAT MODE PROGRAM WILL EXECUTE
    BANKSEL PORTA	    ;
    BTFSC PORTA,0	    ;DETERMINES IF THE RECORD BUTTON WAS PRESSED
    GOTO START_RECORD	    ;IF BUTTON WAS PRESSED, GO TO START RECORD
    BTFSC PORTA,1	    ;CHECKING PORTA,1 IF PLAYBACK BUTTON WAS PRESSED
    GOTO START_PLAYBACK	    ;INIT PLAYBACK MODE
    DECFSZ IDLE_COUNT	    ;DECREMENT IDLE_COUNT 
    GOTO RETURN_FROM_ISR    ;WILL SKIP IF IDLE COUNT =/= 0
    NOP			    ;IF NOTHING, CONTINUE IDLING
    BTFSC PORTC,0	    ;CHECKS IF DISPLAY IS BLANK(0)
    GOTO CLEAR_SCREEN	    ;IF SCREEN HAS S, CLEAR
    MOVLW 0x53		    ;IF OUTPUT IS BLANK(0), MAKE IT AN S
    MOVWF PORTC		    ;
    GOTO IDLE_RESET	    ;
CLEAR_SCREEN:		    ;EXPLICITLY FOR CLEARING THE SCREEN
    MOVLW 0x20		    ;DLG HEX FOR A BLANK SCREEN
    MOVWF PORTC		    ;
IDLE_RESET:		    ;SEE COMMENT 2 LINES BELOW, IF YOU WILL
    MOVLW 0x64		    ;
    MOVWF IDLE_COUNT	    ;RELOADS TIMER COUNT
    GOTO RETURN_FROM_ISR    ;
    
RETURN_FROM_ISR:	    ;MAIN METHOD TO EXITING THE ISR
    BANKSEL PIR1
    BCF PIR1,1		    ;CLEARS TMR2 INTERRUPT FLAG
    MOVF STATUS_SAVE,0	    ;Moves F -> W
    MOVWF STATUS	    ;Restores Status
    MOVF W_SAVE,0	    ;Restores W
    RETFIE		    ;RETURNS WITH DATA
    
START_RECORD:		    ;RESPONSIBLE FOR SETTING PREREQUISITES, FLAGS, DISPLAY, ADDRESS
    BANKSEL PORTC
    MOVLW 0x32
    MOVWF DELAY_TIME
    BSF TEST_KEYPAD,0	    ;TEST_KEYPAD FLAG TO ALLOW PIC TO POLL KEYPAD
    BCF DELAY_FLAG,0
    BCF IDLE,0		    ;RESETS IDLE FLAG, AFTER RECORD, IDLE AGAIN
    BSF R_MODE,0	    ;RECORDING MODE ON
    BCF P_MODE,0	    ;PLAYBACK MODE OFF
    BCF INPUT_RECEIVED,0    ;NO UNEXPECTED DATA TO EEPROM
    BANKSEL EEPROM_ADDRESS
    CLRF EEPROM_ADDRESS	    ;CLEARS EEPROM ADDRESS SO WE START AT ADDRESS 0
    MOVLW 0x52		    ;
    MOVWF PORTC		    ;DLG HEX R DISPLAYED
    GOTO RETURN_FROM_ISR    ;
    
 CONTINUE_RECORDING:	    ;RESPONSIBLE FOR SETTING DELAY TIME AND EEPROM ADDRESS
    BANKSEL PORTC
    BTFSC DELAY_FLAG,0	    ;FIRST ITERATION IS SKIPPED, THERAFTER, DELAY OCCURS
    GOTO DELAY		    ;DELAYS INPUT UNTIL DELAY = DONE
    BTFSS INPUT_RECEIVED,0  ;CHECKS IF ANY KEY WAS PRESSED PRIOR
    GOTO RETURN_FROM_ISR    ;ON THE FIRST ITERATION, FLAG WILL BE CLEAR, SO EXIT ISR
    NOP			    ;GETTING HERE INDICATIONS A VALID INPUT WAS RECEIVED
    BSF DELAY_FLAG,0    
    BCF INPUT_RECEIVED,0    ;CLEARS FLAG, READY FOR NEW INPUT AGAIN 
    MOVLW 0x55		    ;
    MOVWF DELAY_TIME	    ;MOVES VALUE INTO DELAY TIME SO EVERY .5 SECONDS, WE SEE THE INPUT 
    BCF PIR2,4		    ;WRITE OPERATION HAS NOT COMPLETED OR HAS NOT STARTED
    BANKSEL EEPROM_ADDRESS
    MOVF EEPROM_ADDRESS,0   ;TAKE EEPROM ADDRESS 0-10 AND MOVE IT INTO W
    BANKSEL EEADR	    ;
    MOVWF EEADR		    ;MOVE ADDRESS INTO EEMPROM REGISTER ADDRESS
    BANKSEL PORTC	    ;
    MOVF SAVED_VALUE,0	    ;AFTER DELAY, MOVE SAVED KEYPAD PRESS INTO W
    MOVWF PORTC		    ;DISPLAY KEYPAD PRESS ON DLG DOT MATRIX
    GOTO WRITE		    ;
 
START_PLAYBACK:		    ;SETS PRE-PREREQUISITES - FLAGS, COUNT, LOADS READ DATA
    BANKSEL PORTC
    MOVLW 0x64		    ;
    MOVWF PLAYBACK_COUNTER  ;LOADS COUNT INTO THE PLAYBACK COUNTER FOR 1S DISPLAY
    BCF IDLE,0		    ;NO MORE IDLE ANIMATION
    BSF P_MODE,0	    ;PLAYBACK MODE ON
    BANKSEL EEPROM_ADDRESS
    CLRF EEPROM_ADDRESS	    ;CLEARS ADDRESS TO START AT 0
    MOVF EEPROM_ADDRESS,0   ;LOADS NOW CLEAR REGISTER ADDRESS -> W
    BANKSEL EEADR	    ;
    MOVWF EEADR		    ;SETS ADDRESS 0 FOR EEPROM
    GOTO READ		    ;
 
CONTINUE_PLAYBACK:
    BANKSEL PORTC
    DECFSZ PLAYBACK_COUNTER ;WHEN PLAYBACK COUNTER = 0, AROUND 1S, SKIP NEXT LINE
    GOTO RETURN_FROM_ISR    ;UNTIL PLAYBACK COUNTER = 0 THIS LINE IS EXECUTED
    BANKSEL EEPROM_ADDRESS
    INCF EEPROM_ADDRESS	    ;WHEN PLAYBACK CNTR = 0, INC THE ADDRESS
    MOVLW 0x0A		    ;HEX 10
    SUBWF EEPROM_ADDRESS,0    ;SAME PRESMISE AS BEFORE, STATUS,0 WILL DETERMINE IF
    BTFSC STATUS,0	    ;CARRY OUT OCCURED, UNTIL IT IS 0, KEEP LOOPING
    GOTO END_RECORD_PLAYBACK;ALL REGISTERS ARE READ, EXIT
    MOVLW 0x64		    ;
    MOVWF PLAYBACK_COUNTER  ;RELOAD PLAYBACK COUNTER FOR ANOTHER SECOND
    MOVF EEPROM_ADDRESS,0   ;LOAD CURRENT ADDRESS -> W
    BANKSEL EEADR	    ;
    MOVWF EEADR		    ;MOVES EEPROM_ADDRESS DATA TO EEADR
    GOTO READ
    
DELAY:			    ;ALSO, WHERE WE INCR. EEPROM ADRESS
    BANKSEL PORTC
    DECFSZ DELAY_TIME	    ;DECREMENTS DELAY_TIME UNTIL 0
    GOTO RETURN_FROM_ISR    ;
    NOP			    ;DELAY FINISHED
    BSF TEST_KEYPAD,0	    ;ALLOWS KEYPAD POLLING AGAIN
    BCF DELAY_FLAG,0	    ;ALLOWS FOR ANOTHER DELAY
    MOVLW 0x52		    ;
    MOVWF PORTC		    ;DLG HEX R BACK INTO DISPLAY AFTER DELAY
    BANKSEL EEPROM_ADDRESS
    INCF EEPROM_ADDRESS	    ;CHOOSES ADDRESS 0-10		    
    MOVLW 0x0A		    ;HAVE WE READ ALL 10 REGISTERS?
    SUBWF EEPROM_ADDRESS,0
    BTFSC STATUS,0	    ;WHEN STATUS,0 = 1, A CARRY-OUT OCCURED IN THE MSB
    GOTO END_RECORD_PLAYBACK;CARRY OUT DID OCCUR
    GOTO RETURN_FROM_ISR    ;CARRY OUT DID NOT OCCUR

    
END_RECORD_PLAYBACK:
    BSF IDLE,0		    ;PROGRAMM IDLES AFTER RECORDING IS COMPLETE
    BANKSEL EEPROM_ADDRESS
    CLRF EEPROM_ADDRESS	    ;CLEARS SO DEFAULT ADDRESS IS 0
    MOVLW 0x64		    ;
    MOVWF IDLE_COUNT	    ;RESETS 1 SECOND IDLE COUNT
    BCF R_MODE,0	    ;CLEARS RECORD MODE, INDICATING WE ARE DONE
    BCF INPUT_RECEIVED,0    ;CLEARS INPUT, NO MORE DATA
    MOVLW 0x53		    ;
    MOVWF PORTC		    ;DISPLAYS S ON PORTC
    GOTO RETURN_FROM_ISR    ;
    

    ;===================================================================
KEY_0:
    MOVLW 0x30		    ;'0'
    GOTO SAVE_KEY
KEY_1: 
    MOVLW 0x31		     ;'1'
    GOTO SAVE_KEY
KEY_2: 
    MOVLW 0x32		      ;'2'
    GOTO SAVE_KEY
KEY_3: 
    MOVLW 0x33		      ;'3'
    GOTO SAVE_KEY
KEY_4: 
    MOVLW 0x34		     ;'4'
    GOTO SAVE_KEY
KEY_5: 
    MOVLW 0x35		     ;'5'
    GOTO SAVE_KEY
KEY_6: 
    MOVLW 0x36		     ;'6'
    GOTO SAVE_KEY
KEY_7: 
    MOVLW 0x37		    ;'7'
    GOTO SAVE_KEY
KEY_8: 
    MOVLW 0x38		    ;'8'
    GOTO SAVE_KEY
KEY_9: 
    MOVLW 0x39		     ;'9'
    GOTO SAVE_KEY
KEY_A: 
    MOVLW 0x41		     ;'A'
    GOTO SAVE_KEY
KEY_B: 
    MOVLW 0x42		     ;'B'
    GOTO SAVE_KEY   
KEY_C: 
    MOVLW 0x43		     ;'C'
    GOTO SAVE_KEY
KEY_D: 
    MOVLW 0x44		     ;'D'
    GOTO SAVE_KEY
KEY_E: 
    MOVLW 0x45		     ;'E'
    GOTO SAVE_KEY
KEY_F: 
    MOVLW 0x46		     ;'F'
    GOTO SAVE_KEY
;===================================================================
MAIN:
    BANKSEL PORTB
    BTFSS TEST_KEYPAD,0
    GOTO MAIN
    
    BCF STATUS,5
    BCF STATUS,6
;ROW 4(BOTTOM)
    MOVLW 0x08		    ;SETS RB3 HIGH
    MOVWF PORTB		    ;
    BTFSC PORTB,7	    ;CHECKS COLUMN 4, RB7
    GOTO KEY_F		    
    BTFSC PORTB,6	    ;CHECKS COLUMN 3, RB6
    GOTO KEY_E
    BTFSC PORTB,5	    ;CHECKS COLUMN 2, RB5
    GOTO KEY_D
    BTFSC PORTB,4	    ;CHECKS COLUMN 1, RB4
    GOTO KEY_C
    ;CALL BUTTON_RELEASED    ;AND's KEYS TO ENSURE BUTTON IS RELEASED

;ROW 3
    MOVLW 0x04		    ;SETS RB2 HIGH
    MOVWF PORTB		    ;
    BTFSC PORTB,7	    ;CHECKS COLUMN 4, RB7
    GOTO KEY_B		    ;
    BTFSC PORTB,6	    ;CHECKS COLUMN 3, RB6
    GOTO KEY_A		    ;
    BTFSC PORTB,5	    ;CHECKS COLUMN 2, RB5
    GOTO KEY_9		    ;
    BTFSC PORTB,4	    ;CHECKS COLUMN 1, RB4
    GOTO KEY_8		    ;
    ;CALL BUTTON_RELEASED    ;AND's KEYS TO ENSURE BUTTON IS RELEASED
    
;ROW 2
    MOVLW 0x02		    ;SETS RB1 HIGH
    MOVWF PORTB		    ;
    BTFSC PORTB,7	    ;CHECKS COLUMN 4, RB7
    GOTO KEY_7		    ;
    BTFSC PORTB,6	    ;CHECKS COLUMN 3, RB6
    GOTO KEY_6		    ;
    BTFSC PORTB,5	    ;CHECKS COLUMN 2, RB5
    GOTO KEY_5		    ;
    BTFSC PORTB,4	    ;CHECKS COLUMN 1, RB4
    GOTO KEY_4		    ;
    ;CALL BUTTON_RELEASED    ;AND's KEYS TO ENSURE BUTTON IS RELEASED
    
;ROW 1
    MOVLW 0x01		    ;SETS RB0 HIGH
    MOVWF PORTB		    ;
    BTFSC PORTB,7	    ;CHECKS COLUMN 4, RB7
    GOTO KEY_3		    ;
    BTFSC PORTB,6	    ;CHECKS COLUMN 3, RB6 
    GOTO KEY_2		    ;
    BTFSC PORTB,5	    ;CHECKS COLUMN 2, RB5
    GOTO KEY_1		    ;
    BTFSC PORTB,4	    ;CHECKS COLUMN 1, RB4
    GOTO KEY_0		    ;
    ;CALL BUTTON_RELEASED    ;AND's KEYS TO ENSURE BUTTON IS RELEASED
    ;BTFSS INPUT_RECEIVED,0  ;IF NO BUTTON WAS PRESSED
    ;BSF TEST_KEYPAD,0
    GOTO MAIN
    
SAVE_KEY:
    MOVWF SAVED_VALUE
    BCF TEST_KEYPAD,0
    BSF INPUT_RECEIVED,0
GOTO MAIN
    
BUTTON_RELEASED:
    MOVLW 0xF0		    ;MOVES (1111|0000) into W
    ANDWF PORTB,0	    ;'ANDS' PORTB & W
    BTFSS STATUS,2	    ;CHECK ZERO FLAG, IF 0 CHECK AGAIN
    GOTO BUTTON_RELEASED
    RETURN
END 

