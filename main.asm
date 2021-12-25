.MODEL SMALL
.STACK 100H
.DATA
MESSAGE1 DB 'ENTER ELEMENTS : $' 
MESSAGE2 DB 'AFTER SORTING : $'
MESSAGE3 DB 'PRESS : 1-FOR DESCENDING ORDER  2-FOR ASCENDING ORDER ; $'
MESSAGE4 DB 'INVALID NUMBER $'
MESSAGE5 DB 'PRESS : 1-FOR BUBBLE SORTING  2-FOR SELECTION SORTING ; $'
ARR DB 100 dup (0)
.CODE  



 
MAIN PROC
    MOV AX,@DATA
    MOV DS,AX
    MOV AH,9       ;DISPLAY MESSAGE
    lea DX,MESSAGE1
    INT 21H
    MOV CX,0 
    ;FILLING ARRAY
    MOV AH,1 ;FIRST INPUT
    INT 21H
    MOV SI,0
    WHILE_:
        CMP AL,0DH      ;COMPARE INPUT WITH CR ;ENTER BUTTON
        JE END_WHILE    ;IF EQUAL JMP TO END_WHILE
        MOV ARR[SI],AL ;MOVE INPUT INTO ARRAY
        INC SI          ;SI+=1
        INC CX
        MOV AH,2
        MOV DX,' '     ;DISPLAY SPACE
        INT 21H
        MOV AH,1
        INT 21H
    JMP WHILE_          ;JMP WHILE_ TO CONTINUE ENTER THE INPUTS
    END_WHILE:
        MOV AH,2
        MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
        INT 21H
        MOV DL,0AH  ;LINE FEED IN ASCII TABLE
        INT 21H
    JCXZ EXIT ;IF CX == 0 JMP EXIT (NO INPUTS ENTERED) 
    
    LEA SI,ARR ;OFFSET OF ARRAY INTO SI
    MOV BX,CX 
    

    
 ;==================================================   
    
    ;TYPE OF SORTING
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
    MOV AH,9
    LEA DX,MESSAGE5
    INT 21H
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
    
    MOV AH,1
    INT 21H
    SUB AL,30H 
    CMP AL,1      ;COMPARE INPUT WITH 1
    JB INVALID 
    JE T1    ;IF EQUAL JMP TO T1 
    CMP AL,2      ;COMPARE INPUT WITH 2
    JA INVALID
    JE T2    ;IF EQUAL JMP TO T2

    
 ;======================================================= 
 
 
  ;==================================================   
    T1: ;BUBBLE TYPE
    ;TYPE OF ORDER
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
    MOV AH,9
    LEA DX,MESSAGE3
    INT 21H
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
    
    MOV AH,1
    INT 21H
    SUB AL,30H 
    CMP AL,1      ;COMPARE INPUT WITH 1
    JB INVALID 
    JE P1    ;IF EQUAL JMP TO P1 AND THEN CALL DESCENDING SORT
    CMP AL,2      ;COMPARE INPUT WITH 2
    JA INVALID
    JE P2    ;IF EQUAL JMP TO P2 AND THEN CALL ASCENDING SORT
    P1: CALL BUBBLE_DESCENDING_SORT
    JMP PRINT_ARRAY 
    P2: CALL BUBBLE_ASCENDING_SORT  
    JMP PRINT_ARRAY
    
 
    
 ;==================================================   
    T2: ;SELECTION TYPE
    ;TYPE OF ORDER
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
    MOV AH,9
    LEA DX,MESSAGE3
    INT 21H
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
    
    MOV AH,1
    INT 21H
    SUB AL,30H 
    CMP AL,1      ;COMPARE INPUT WITH 1
    JB INVALID 
    JE P3    ;IF EQUAL JMP TO P3 AND THEN CALL DESCENDING SORT
    CMP AL,2      ;COMPARE INPUT WITH 2
    JA INVALID
    JE P4    ;IF EQUAL JMP TO P4 AND THEN CALL ASCENDING SORT
    P3: CALL SELECTION_DESCENDING_SORT
    JMP PRINT_ARRAY 
    P4: CALL SELECTION_ASCENDING_SORT  
    JMP PRINT_ARRAY
    
    

    
 ;=======================================================
      
    ;PRINT AFTER SORTING
    PRINT_ARRAY:
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
        
    MOV AH,9
    LEA DX,MESSAGE2
    INT 21H
    MOV SI,0
    PRINT_SORTED_ARRAY:
        MOV AH,2
        MOV DL,ARR[SI]
        INT 21H
        MOV DL,' '
        INT 21H
        INC SI
        LOOP PRINT_SORTED_ARRAY
    EXIT:
        MOV AH,4CH
        INT 21H
        MAIN ENDP


;==========================================================


BUBBLE_DESCENDING_SORT PROC
   ;THIS PROCEDURE WILL SORT THE ARRAY IN DESCENDING ORDER
   ;INPUT : SI=OFFSET ADDRESS OF THE ARRAY
   ;      : BX=ARRAY SIZE
   PUSH AX
   PUSH BX
   PUSH CX
   PUSH DX
   PUSH DI
   PUSH SI
   
   MOV AX,SI
   MOV DX,CX

  DEC CX
  JCXZ BUBBLESORT_END                         
  @OUTER_LOOP1:
      MOV BX,CX           ;SET BX=CX
      MOV SI,AX           ;SET SI=AX (IN EACH LOOP IN OUTER LOOP SET SI THE BEGINNIG OFFSET OF THE AARAY)
      MOV DI,AX           ;SET DI=AX
      INC DI              ;SET DI=DI+1
      @INNER_LOOP1:        ;
          MOV DL,[SI]     ;SET DL=[SI]   (EXAMPLE: DL=[SI]=ARR[0],[DI]=ARR[1]}  
          CMP DL,[DI]     ;COMPARE DL WITH [DI]
          JNG @NOT_SWAP1   ;JMP IF DL<=[DI] ([DI] IS GREATER THAN DL )
          XCHG DL,[DI]    ;IF DL > [DI] EXCHANGE THE VALUE OD DL AND [DI]
          MOV [SI],DL
          @NOT_SWAP1:
          INC SI 
          INC DI
          DEC BX
      JNZ @INNER_LOOP1 ;JMP IF BX!=0
      ;======================================
        PUSH CX
        PUSH AX            ;THIS BLOCK OF CODE TO PRINT ARRAY AFTER EACH OUTER LOOP
        MOV CX,DX
        MOV AH,2
        MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
        INT 21H
        MOV DL,0AH  ;LINE FEED IN ASCII TABLE
        INT 21H 
                                 

        MOV SI,0
        PRINT_SORTED_ARRAY_:
            MOV AH,2
            MOV DL,ARR[SI]
            INT 21H
            MOV DL,' '
            INT 21H
            INC SI
            LOOP PRINT_SORTED_ARRAY_
        POP AX
        POP CX
        ;====================================       
      LOOP @OUTER_LOOP1
  
       
  BUBBLESORT_END:
  POP SI
  POP DI
  POP DX
  POP CX
  POP BX
  POP AX
  RET   ;RETURN TO RHE CALLING PROCEDURE
  BUBBLE_DESCENDING_SORT ENDP

   
    
;======================================================    
   

BUBBLE_ASCENDING_SORT PROC
   ;THIS PROCEDURE WILL SORT THE ARRAY IN ASCENDING ORDER
   ;INPUT : SI=OFFSET ADDRESS OF THE ARRAY
   ;      : BX=ARRAY SIZE
   PUSH AX
   PUSH BX
   PUSH CX
   PUSH DX
   PUSH DI
   PUSH SI
   
   MOV AX,SI
   MOV DX,CX

  DEC CX
  JCXZ BUBBLESORT2_END                         
  @OUTER_LOOP2:
      MOV BX,CX           ;SET BX=CX
      MOV SI,AX           ;SET SI=AX (IN EACH LOOP IN OUTER LOOP SET SI THE BEGINNIG OFFSET OF THE AARAY)
      MOV DI,AX           ;SET DI=AX
      INC DI              ;SET DI=DI+1
      @INNER_LOOP2:        ;
          MOV DL,[SI]     ;SET DL=[SI]   (EXAMPLE: DL=[SI]=ARR[0],[DI]=ARR[1]}  
          CMP [DI],DL     ;COMPARE DL WITH [DI]
          JNG @NOT_SWAP2   ;JMP IF [DI]<=DL (DL IS GREATER THAN [DI] )
          XCHG DL,[DI]    ;IF DL > [DI] EXCHANGE THE VALUE OD DL AND [DI]
          MOV [SI],DL
          @NOT_SWAP2:
          INC SI 
          INC DI
          DEC BX
      JNZ @INNER_LOOP2 ;JMP IF BX!=0
      ;======================================
        PUSH CX
        PUSH AX                ;THIS BLOCK OF CODE TO PRINT ARRAY AFTER EACH OUTER LOOP
        MOV CX,DX
        MOV AH,2
        MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
        INT 21H
        MOV DL,0AH  ;LINE FEED IN ASCII TABLE
        INT 21H 
                                 

        MOV SI,0
        PRINT_SORTED_ARRAY__:
            MOV AH,2
            MOV DL,ARR[SI]
            INT 21H
            MOV DL,' '
            INT 21H
            INC SI
            LOOP PRINT_SORTED_ARRAY__
        POP AX
        POP CX
      ;==========================================       
       
      LOOP @OUTER_LOOP2

   
  BUBBLESORT2_END:
  POP SI
  POP DI
  POP DX
  POP CX
  POP BX
  POP AX
  RET   ;RETURN TO RHE CALLING PROCEDURE
  BUBBLE_ASCENDING_SORT ENDP
;==============================================================================


SELECTION_DESCENDING_SORT PROC
   ;THIS PROCEDURE WILL SORT THE ARRAY IN DESCENDING ORDER
   ;INPUT : SI=OFFSET ADDRESS OF THE ARRAY
   ;      : BX=ARRAY SIZE
   PUSH AX
   PUSH BX
   PUSH CX
   PUSH DX
   PUSH DI
   PUSH SI
   MOV BX,CX
   DEC CX
   JCXZ SELECTION_END      ;JMP IF CX == 0, THIS FOR CONDITION IF THERE IS ONLY 1 ELEMENT IN ARRAY THEN END SORT                  
   @OUTER_LOOP3:
       PUSH BX     
       MOV BX,CX           ;SET BX=CX
       MOV DI,SI
       INC DI              ;ADDRESS OF THE NEXT ELEMENT
       MOV DX,0            ;DX=0
 
       @INNER_LOOP3:      
          MOV AL,[DI]       ; AL= [DI]
          CMP [SI],AL      ;COMPARE [SI] WITH [DI] ; COMPARE THE CURRENT VALUE TO THE NEXT VALUE
          JNG @NOT_SWAP3   ;JMP TO @NOT_SWAP1 IF [SI]<=[DI]    CURRENT < NEXT
          PUSH DI          ;IF NOT PUSH DI & SI CUZ WE WILL CHANGE THIM 
          PUSH SI
         
         
          ADD SI,DX        ;ADD THE INDEX OF MINUMUM ELEMENT TO SI TO GET THE ADDRESS OF MINMUM ELEMENT
          CMP [SI],AL      ;COMPARE THE MINMUM ELEMENT TO  THE CURRENT ELEMENT
          JNG @NOT_SWAP___   ;IF THE MINMUM ELEMENT IS STILL SMALLER JUMP TO @NOT_SWAP_
          POP SI           ;RETURN THE ORIGNAL SI FROM STACK
          SUB DI,SI        ;SUBTRACT THE ADDRESS OF THE CURRENT ELEMENT FROM THE SI ADDRESS TO GET THE CURRENT ELEMENT INDEX 
          MOV DX,DI        ;PUT THE INDEX OF THE MINIMUM ADDRESS INTO DX
          POP DI           ;RETURN THE ORIGNAL DI FROM STACK
          JMP @NOT_SWAP3   ;JMP TO @NOT_SWAP1
          
          
          @NOT_SWAP___:      ;THIS LABLE IF THE MINMUM ELEMENT IS STILL SMALLER
          POP SI           ;THEN REURN THE OERIGNAL SI & DI
          POP DI
          @NOT_SWAP3:
          INC DI           ;INCREMENT DI TO COMPARE THE NEXT ELEMENT
          DEC BX
       JNZ @INNER_LOOP3    ;JMP IF BX!=0
       MOV DI,SI   
       ADD SI,DX
       MOV AL,[SI]  
       XCHG [DI],AL        ;THIS BLOCK OF CODE IS TO EXCHANGE THE MINIMUM VALUE TO THE CORRECT INDEX
       MOV [SI],AL
       MOV SI,DI
       INC SI
      ;======================================
        POP BX
        PUSH SI
        PUSH CX
        PUSH AX
        MOV CX,BX
        MOV AH,2
        MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
        INT 21H
        MOV DL,0AH  ;LINE FEED IN ASCII TABLE
        INT 21H 
                                 ;THIS BLOCK OF CODE TO PRINT ARRAY AFTER EACH OUTER LOOP

        MOV SI,0
        PRINT_SORTED_ARRAY___:
            MOV AH,2
            MOV DL,ARR[SI]
            INT 21H
            MOV DL,' '
            INT 21H
            INC SI
            LOOP PRINT_SORTED_ARRAY___
        POP AX
        POP CX
        POP SI
      ;==========================================        
       
       LOOP @OUTER_LOOP3
   
        
   SELECTION_END:
   POP SI
   POP DI
   POP DX
   POP CX
   POP BX
   POP AX
   RET   ;RETURN TO RHE CALLING PROCEDURE
   SELECTION_DESCENDING_SORT ENDP

   
    
;=====================================================    
   

SELECTION_ASCENDING_SORT PROC
   ;THIS PROCEDURE WILL SORT THE ARRAY IN ASCENDING ORDER
   ;INPUT : SI=OFFSET ADDRESS OF THE ARRAY
   ;      : BX=ARRAY SIZE
   PUSH AX
   PUSH BX
   PUSH CX
   PUSH DX
   PUSH DI
   PUSH SI
   MOV BX,CX

  DEC CX                    
                            
  JCXZ SELECTION2_END       ;JMP IF CX == 0, THIS FOR CONDITION IF THERE IS ONLY 1 ELEMENT IN ARRAY THEN END SORT              
  @OUTER_LOOP4:
       PUSH BX                                                                                                              
       MOV BX,CX            ;SET BX=CX                                                                                       
       MOV DI,SI                                                                                                             
       INC DI               ;ADDRESS OF THE NEXT ELEMENT                                                                     
       MOV DX,0             ;DX=0                                                                                            
       @INNER_LOOP4:                                                                                                         
          MOV AL,[DI]        ; AL= [DI]                                                                                      
          CMP AL,[SI]       ;COMPARE [SI] WITH [DI] ; COMPARE THE CURRENT VALUE TO THE NEXT VALUE                            
          JNG @NOT_SWAP4    ;JMP TO @NOT_SWAP1 IF [SI]<=[DI]    CURRENT < NEXT                                               
          PUSH DI           ;IF NOT PUSH DI & SI CUZ WE WILL CHANGE THIM                                                     
          PUSH SI                                                                                                            
          ADD SI,DX           ;ADD THE INDEX OF MINUMUM ELEMENT TO SI TO GET THE ADDRESS OF MINMUM ELEMENT                                                                                                                     
          CMP AL,[SI]       ;COMPARE THE MINMUM ELEMENT TO  THE CURRENT ELEMENT                                                                                                                                                
          JNG @NOT_SWAP____ ;IF THE MINMUM ELEMENT IS STILL SMALLER JUMP TO @NOT_SWAP____                                      
          POP SI            ;RETURN THE ORIGNAL SI FROM STACK                                                                 
          SUB DI,SI         ;SUBTRACT THE ADDRESS OF THE CURRENT ELEMENT FROM THE SI ADDRESS TO GET THE CURRENT ELEMENT INDEX   
          MOV DX,DI         ;PUT THE INDEX OF THE MINIMUM ADDRESS INTO DX                                                     
          POP DI              ;RETURN THE ORIGNAL DI FROM STACK                                                               
          JMP @NOT_SWAP4    ;JMP TO @NOT_SWAP4                                                                                
          @NOT_SWAP____:     
          POP SI            
          POP DI                                                                                                             
          @NOT_SWAP4:                                                                                                        
          INC DI            ;THIS LABLE IF THE MINMUM ELEMENT IS STILL SMALLER                                               
          DEC BX            ;THEN REURN THE OERIGNAL SI & DI                                                                 
       JNZ @INNER_LOOP4     ;JMP IF BX!=0                                                                                                 
       MOV DI,SI                                                                                                             
       ADD SI,DX            ;INCREMENT DI TO COMPARE THE NEXT ELEMENT                                                        
       MOV AL,[SI]          ;THIS BLOCK OF CODE IS TO EXCHANGE THE MINIMUM VALUE TO THE CORRECT INDEX                                                                                                 
       XCHG [DI],AL                                                                                             
       MOV [SI],AL                                                                                                           
       MOV SI,DI                                                                                                             
       INC SI
      ;======================================
        POP BX
        PUSH SI
        PUSH CX
        PUSH AX
        MOV CX,BX
        MOV AH,2
        MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
        INT 21H
        MOV DL,0AH  ;LINE FEED IN ASCII TABLE
        INT 21H 
                                 ;THIS BLOCK OF CODE TO PRINT ARRAY AFTER EACH OUTER LOOP

        MOV SI,0
        PRINT_SORTED_ARRAY____:
            MOV AH,2
            MOV DL,ARR[SI]
            INT 21H
            MOV DL,' '
            INT 21H
            INC SI
            LOOP PRINT_SORTED_ARRAY____
        POP AX
        POP CX
        POP SI
      ;==========================================                                                                                                                       
      LOOP @OUTER_LOOP4                             

   
  SELECTION2_END:
  POP SI
  POP DI
  POP DX
  POP CX
  POP BX
  POP AX
  RET   ;RETURN TO RHE CALLING PROCEDURE
  SELECTION_ASCENDING_SORT ENDP

;===================================================


    INVALID:
    MOV AH,2
    MOV DL,0DH  ;CARRIAGE RETURN IN ASCII TABLE
    INT 21H
    MOV DL,0AH  ;LINE FEED IN ASCII TABLE
    INT 21H 
    MOV AH,9
    LEA DX,MESSAGE4
    INT 21H
    JMP EXIT
 
END MAIN  

;===============================================================================