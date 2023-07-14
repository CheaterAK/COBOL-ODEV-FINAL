       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FINALCBL
       AUTHOR.        AHMET KOCABAS aka CheaterAK
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE ASSIGN TO OUTFILE
                           FILE STATUS IS STATUS-OUT.
           SELECT INP-FILE ASSIGN TO INPFILE
                           FILE STATUS IS STATUS-INP.
           SELECT ERR-OUT ASSIGN TO SYSERROR
                           FILE STATUS IS STATUS-ERR.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           05 ID-O                       PIC X(5).
           05 CUR-O                      PIC X(3).
           05 SPERATOR1-O                PIC X(1) VALUE '-'.
           05 PROCESS-TYPE-O             PIC X(4).
           05 SPERATOR2-O                PIC X(1) VALUE '-'.
           05 RC-TXT-O                   PIC X(3).
           05 RC-O                       PIC X(2).
           05 SPERATOR3-O                PIC X(1) VALUE '-'.
           05 EXP-O                      PIC X(55).
           05 SPERATOR4-O                PIC X(1) VALUE ' '.
           05 OLD-FNAME-O                PIC X(15).
           05 OLD-LNAME-O                PIC X(15).
           05 NEW-FNAME-O                PIC X(15).
           05 NEW-LNAME-O                PIC X(15).
       66  FULL-OLD-NAME-O RENAMES OLD-FNAME-O THRU OLD-LNAME-O.
       66  FULL-NEW-NAME-O RENAMES NEW-FNAME-O THRU NEW-LNAME-O.
       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           05    PROCESS-TYPE-I          PIC X(1).
           05    ID-I                    PIC X(5).
           05    CUR-I                   PIC X(3).
       66  KEY-I RENAMES ID-I THRU CUR-I.
       FD  ERR-OUT RECORDING MODE F.
       01  ERR-REC.
           05 ERR-REASON                 PIC X(90).
       WORKING-STORAGE SECTION.
       01  FLAGS.
           05 KEY-CONTROL                PIC 9.
              88 KEY-VALID                         VALUE 0.
              88 ERR-ID                            VALUE 1.
              88 ERR-CUR                           VALUE 2.
              88 ERR-BOTH                          VALUE 3.
           05 STATUS-OUT                 PIC 99.
              88 OUT-SUCC                          VALUE 00 97.
           05 STATUS-INP                 PIC 99.
              88 INP-SUCC                          VALUE 00 97.
              88 INP-EOF                           VALUE 10.
           05 STATUS-ERR                 PIC 99.
              88 ERR-SUCC                          VALUE 00 97.
       77  SUB-PRG-NAME                  PIC X(8)  VALUE 'FINALSUB'.
       77  READ-CNT                      PIC 9(3)  VALUE 0.
       77  VALID-READ-CNT                PIC 9(3)  VALUE 0.
       77  INVALID-READ-CNT              PIC 9(3)  VALUE 0.
       01  WS-SUB-DATA.
           05 SUB-IDX-ID                     PIC X(5).
           05 SUB-IDX-CUR                    PIC X(3).
           05 SUB-RC                         PIC 9(2).
              88 SUB-RC-ERR-DUP                VALUE 22.
              88 SUB-RC-ERR-NF                 VALUE 23.
              88 SUB-RC-SUCC-FILE              VALUE 00 97.
           05 SUB-PRC                       PIC 9(2).
              88 SUB-RC-CLEAN                  VALUE 00.
              88 SUB-RC-SUCC-READ              VALUE 10.
              88 SUB-RC-READ-WARN              VALUE 11.
              88 SUB-RC-READ-ERR               VALUE 12.
              88 SUB-RC-SUCC-UPDT              VALUE 20.
              88 SUB-RC-UPDT-SPC               VALUE 21.
              88 SUB-RC-UPDT-CHR               VALUE 22.
              88 SUB-RC-UPDT-BTH               VALUE 23.
              88 SUB-RC-UPDT-ERR               VALUE 24.
              88 SUB-RC-SUCC-DELT              VALUE 30.
              88 SUB-RC-DELT-ERR               VALUE 31.
              88 SUB-RC-SUCC-WRTE              VALUE 40.
              88 SUB-RC-WRTE-ERR               VALUE 41.
              88 SUB-RC-INVALID                VALUE 81 82.
              88 SUB-RC-INVALID-TYPE           VALUE 81.
              88 SUB-RC-INVALID-KEY            VALUE 82.
              88 SUB-RC-UEXP-PRCSS-TYPE        VALUE 99.
           05 SUB-OLD-FNAME                  PIC X(15).
           05 SUB-OLD-LNAME                  PIC X(15).
           05 SUB-NEW-FNAME                  PIC X(15).
           05 SUB-NEW-LNAME                  PIC X(15).
           05 VSAM-FILE-PROCESS              PIC X.
              88 VSAM-OPEN                     VALUE 'O'.
              88 VSAM-CLOSE                    VALUE 'C'.
              88 VSAM-PROCESS                  VALUE 'P'.
           05 SUB-PROCESS-TYPE               PIC X.
              88 SUB-READ-P                    VALUE 'R'.
              88 SUB-WRTE-P                    VALUE 'W'.
              88 SUB-UPDT-P                    VALUE 'U'.
              88 SUB-DELT-P                    VALUE 'D'.
              88 SUB-VALD-P                    VALUE 'R' 'W' 'U' 'D'.
           66 SUB-OLD-FULLNAME RENAMES SUB-OLD-FNAME THROUGH
               SUB-OLD-LNAME.
           66 SUB-NEW-FULLNAME RENAMES SUB-NEW-FNAME THROUGH
               SUB-NEW-LNAME.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0200-PROCESS-FILE.
           PERFORM 0800-CLOSE-FILES.
           PERFORM 9999-EXIT.
      *
       0100-OPEN-FILES.
           OPEN INPUT INP-FILE.
           OPEN OUTPUT OUT-FILE.
           OPEN OUTPUT ERR-OUT.
           PERFORM 0110-FILE-CONTROL.
           SET VSAM-OPEN TO TRUE.
           CALL SUB-PRG-NAME USING WS-SUB-DATA.

      *
       0110-FILE-CONTROL.
           IF NOT INP-SUCC
              DISPLAY "INPFILE NOT FOUND. EXITING..."
              STOP RUN
           END-IF.
           IF NOT OUT-SUCC
              DISPLAY "OUTFILE NOT FOUND. EXITING..."
              STOP RUN
           END-IF.
           IF NOT ERR-SUCC
              DISPLAY "DATASET NOT FOUND TO ERR. EXITING..."
              STOP RUN
           END-IF.
      *
       0200-PROCESS-FILE.
           PERFORM P200-READ-KEY.
           PERFORM UNTIL INP-EOF
              IF KEY-VALID AND SUB-VALD-P
                 CALL SUB-PRG-NAME USING WS-SUB-DATA
              END-IF
              PERFORM P400-WRITE-AND-HANDLE
              PERFORM P200-READ-KEY
           END-PERFORM.
      *
       P200-READ-KEY.
           READ INP-FILE
           NOT AT END
              ADD 1 TO READ-CNT
           END-READ.
           PERFORM P210-KEY-CONTROL.
           PERFORM P220-TYPE-CONTROL.
      *
       P210-KEY-CONTROL.
           SET KEY-VALID TO TRUE.
           IF ID-I NOT NUMERIC
              ADD 1 TO KEY-CONTROL
           END-IF.
           IF CUR-I NOT NUMERIC
              ADD 2 TO KEY-CONTROL
           END-IF.
           MOVE PROCESS-TYPE-I TO SUB-PROCESS-TYPE
           IF KEY-VALID
              MOVE ID-I TO SUB-IDX-ID
              MOVE CUR-I TO SUB-IDX-CUR
           ELSE
              SET SUB-RC-INVALID-KEY TO TRUE
              MOVE SPACES TO ERR-REASON
              EVALUATE TRUE
                 WHEN ERR-ID
                    STRING ID-I
                       " << ID  MUST BE NUMERIC AND 5 CHARACTER"
                          DELIMITED BY SIZE INTO ERR-REASON
                 WHEN ERR-CUR
                    STRING CUR-I
                       " << CUR MUST BE NUMERIC AND 3 CHARACTER"
                          DELIMITED BY SIZE INTO ERR-REASON
                 WHEN ERR-BOTH
                    STRING ID-I " - " CUR-I " << ID MUST BE NUMERIC AND
      -    " 5 CHARACTER AND CUR MUST BE NUMERIC AND 3 CHARACTER"
                          DELIMITED BY SIZE INTO ERR-REASON
              END-EVALUATE
           END-IF.
      *
       P220-TYPE-CONTROL.
           IF NOT SUB-VALD-P AND NOT SUB-RC-INVALID-KEY
              SET SUB-RC-INVALID-TYPE TO TRUE
              MOVE SPACES TO ERR-REASON
              STRING PROCESS-TYPE-I ' - ' ID-I CUR-I ' << INVALID TYPE
      -    ' MUST BE R,W,U,D AND 1 CHARACTER'
                          DELIMITED BY SIZE INTO ERR-REASON
           END-IF.
           IF NOT SUB-VALD-P AND SUB-RC-INVALID-KEY
              WRITE ERR-REC
              MOVE SPACES TO ERR-REASON
              STRING PROCESS-TYPE-I ' << ALSO INVALID TYPE FOR PREVIOUS
      -    'LINE. TYPE MUST BE R,W,U,D AND 1 CHARACTER'
                          DELIMITED BY SIZE INTO ERR-REASON
           END-IF.
      *
       P400-WRITE-AND-HANDLE.
           INITIALIZE OUT-REC.
           MOVE SPACES TO OUT-REC.
           MOVE ID-I TO ID-O
           MOVE CUR-I TO CUR-O
           MOVE '-' TO SPERATOR1-O
           MOVE '-' TO SPERATOR2-O
           MOVE '-' TO SPERATOR3-O
           MOVE 'RC:' TO RC-TXT-O
           MOVE SUB-RC TO RC-O
           EVALUATE TRUE
              WHEN SUB-RC-SUCC-READ
                 MOVE 'OK' TO EXP-O
                 MOVE 'READ' TO PROCESS-TYPE-O
              WHEN SUB-RC-SUCC-WRTE
                 MOVE 'OK' TO EXP-O
                 MOVE 'WRTE' TO PROCESS-TYPE-O
              WHEN SUB-RC-SUCC-UPDT
                 MOVE 'OK' TO EXP-O
                 MOVE 'UPDT' TO PROCESS-TYPE-O
              WHEN SUB-RC-SUCC-DELT
                 MOVE 'OK' TO EXP-O
                 MOVE 'DELT' TO PROCESS-TYPE-O
              WHEN SUB-RC-READ-WARN
                 MOVE 'WOK-CORRUPTED DATA' TO EXP-O
                 MOVE 'READ' TO PROCESS-TYPE-O
              WHEN SUB-RC-READ-ERR
                 MOVE 'ERROR RECORD NOT FOUND' TO EXP-O
              WHEN SUB-RC-WRTE-ERR
                 MOVE 'ERROR DUPLICATE PRIMARY KEY' TO EXP-O
                 MOVE 'WRTE' TO PROCESS-TYPE-O
              WHEN SUB-RC-UPDT-SPC
                 MOVE 'SUCCESSFUL UPDATE-SPACES REMOVED' TO EXP-O
                 MOVE 'UPDT' TO PROCESS-TYPE-O
                 MOVE SUB-OLD-FULLNAME TO FULL-OLD-NAME-O
                 MOVE SUB-NEW-FULLNAME TO FULL-NEW-NAME-O
              WHEN SUB-RC-UPDT-CHR
                 MOVE 'SUCCESSFUL UPDATE-CHARACTERS UPDATED' TO EXP-O
                 MOVE 'UPDT' TO PROCESS-TYPE-O
                 MOVE SUB-OLD-FULLNAME TO FULL-OLD-NAME-O
                 MOVE SUB-NEW-FULLNAME TO FULL-NEW-NAME-O
              WHEN SUB-RC-UPDT-BTH
                 MOVE 'SUCCESSFUL UPDATE-SPACES REMOVED AND CHARACTERS
      -    'UPDATED' TO EXP-O
                 MOVE 'UPDT' TO PROCESS-TYPE-O
                 MOVE SUB-OLD-FULLNAME TO FULL-OLD-NAME-O
                 MOVE SUB-NEW-FULLNAME TO FULL-NEW-NAME-O
              WHEN SUB-RC-UPDT-ERR
                 MOVE 'ERROR RECORD NOT FOUND' TO EXP-O
                 MOVE 'UPDT' TO PROCESS-TYPE-O
              WHEN SUB-RC-DELT-ERR
                 MOVE 'ERROR RECORD NOT FOUND' TO EXP-O
                 MOVE 'DELT' TO PROCESS-TYPE-O
              WHEN SUB-RC-UEXP-PRCSS-TYPE
                 MOVE 'UNEXPECTED ERR' TO EXP-O
                 MOVE 'UEXP' TO PROCESS-TYPE-O
           END-EVALUATE.
           IF SUB-RC-INVALID
              WRITE ERR-REC
           ELSE
              WRITE OUT-REC
           END-IF.
           SET SUB-RC-CLEAN TO TRUE.
       0800-CLOSE-FILES.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           SET VSAM-CLOSE TO TRUE.
           CALL SUB-PRG-NAME USING WS-SUB-DATA.
       9999-EXIT.
           STOP RUN.
