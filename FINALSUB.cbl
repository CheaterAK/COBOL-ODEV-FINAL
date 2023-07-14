         IDENTIFICATION DIVISION.
       PROGRAM-ID.    FINALSUB
       AUTHOR.        AHMET KOCABAS aka CheaterAK
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN to IDXFILE ORGANIZATION IS INDEXED
                           ACCESS MODE IS RANDOM
                           RECORD KEY is IDX-KEY
                           FILE STATUS is STATUS-IDX.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           05    IDX-KEY.
              07    IDX-ID               PIC S9(5)  COMP-3.
              07    IDX-CUR              PIC S9(3) COMP.
           05    FIRST-NAME              PIC X(15).
           05    LAST-NAME               PIC X(15).
           05    LAST-ORDER-DATE         PIC S9(7)  COMP-3.
           05    BALANCE                 PIC S9(15) COMP-3.
       66  FULL-NAME   RENAMES FIRST-NAME THROUGH LAST-NAME.
       WORKING-STORAGE SECTION.
       01  WS-TODAY                      PIC 9(8).
       01  WS-TODAY-JUL                  PIC 9(7).
       01  WS-FLAGS.
           05 STATUS-IDX                 PIC 9(2).
              88 IDX-SUCC                          VALUE 00 97.
              88 IDX-EOF                           VALUE 10.
              88 IDX-INVALID-KEY                   VALUE 23.
       01  DATE-CONTROLLER.
           05 DATE-TEMP                  PIC 9(7).
           05 DATE-YYYYDDD.
              07 DATE-YYYY               PIC 9(4).
              07 DATE-DDD                PIC 9(3).
           05 LEAP-CONTROL.
              07 LEAP-RULE1              PIC 9(4).
              07 LEAP-RULE2              PIC 9(4).
              07 LEAP-RULE3              PIC 9(4).
              07 LEAP-TMP                PIC 9.

       77  WS-INDEX-1                    PIC 9(2).
       77  WS-INDEX-2                    PIC 9(2).
       77  WS-NAME                       PIC X(15).
       77  WS-NAME-LEN                   PIC 9(2) VALUE 15.
       77  WS-UPDATE-TYPE                PIC 9.
       77  WS-UPDATE-CONTROL             PIC 99.
       LINKAGE SECTION.
       01  LS-SUB-DATA.
           05 SUB-IDX-ID                     PIC X(5).
           05 SUB-IDX-CUR                    PIC X(3).
           05 SUB-RC                         PIC 9(2).
              88 SUB-RC-ERR-DUP                VALUE 22.
              88 SUB-RC-ERR-NF                 VALUE 23.
              88 SUB-RC-SUCC-FILE              VALUE 00 97.
           05 SUB-PRC                       PIC 9(2).
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
           66 SUB-OLD-FULLNAME RENAMES SUB-OLD-FNAME THROUGH
               SUB-OLD-LNAME.
           66 SUB-NEW-FULLNAME RENAMES SUB-NEW-FNAME THROUGH
               SUB-NEW-LNAME.

       PROCEDURE DIVISION USING LS-SUB-DATA.
       0000-MAIN.
           EVALUATE TRUE
              WHEN VSAM-OPEN
                 PERFORM 0100-OPEN-FILES
              WHEN VSAM-CLOSE
                 PERFORM 0800-CLOSE-FILES
              WHEN VSAM-PROCESS
                 EVALUATE TRUE
                    WHEN SUB-READ-P
                       PERFORM 0200-READ-RECORD
                    WHEN SUB-WRTE-P
                       PERFORM 0300-WRITE-RECORD
                    WHEN SUB-UPDT-P
                       PERFORM 0400-UPDATE-RECORD
                    WHEN SUB-DELT-P
                       PERFORM 0500-DELETE-RECORD
                    WHEN OTHER
                       SET SUB-RC-UEXP-PRCSS-TYPE TO TRUE
                 END-EVALUATE
              WHEN OTHER
                 SET SUB-RC-UEXP-PRCSS-TYPE TO TRUE
           END-EVALUATE.
           GOBACK.
      *
       0100-OPEN-FILES.
           OPEN I-O  IDX-FILE.
           PERFORM 0110-FILE-CONTROL.
           GOBACK.
      *
       0110-FILE-CONTROL.
           IF IDX-SUCC
              SET VSAM-PROCESS TO TRUE
           ELSE
              DISPLAY "IDXFILE NOT FOUND. EXITING..."
              STOP RUN
           END-IF.
      *
       0200-READ-RECORD.
           MOVE SUB-IDX-ID TO IDX-ID.
           MOVE SUB-IDX-CUR TO IDX-CUR.
           READ IDX-FILE KEY IDX-KEY.
           MOVE STATUS-IDX TO SUB-RC.
           IF NOT IDX-SUCC 
              SET SUB-RC-READ-ERR TO TRUE
           ELSE
              SET SUB-RC-SUCC-READ TO TRUE
              PERFORM 0210-DATE-CONTROL
              DISPLAY IDX-ID " - " IDX-CUR " - " FULL-NAME " - "
                       LAST-ORDER-DATE " - " BALANCE
           END-IF.
           GOBACK.
      *
       0210-DATE-CONTROL.
           MOVE LAST-ORDER-DATE TO DATE-TEMP.
           MOVE DATE-TEMP TO DATE-YYYYDDD.
           IF DATE-YYYY < 1601
              SET SUB-RC-READ-WARN TO TRUE
           ELSE
              DIVIDE DATE-YYYY BY 4 GIVING LEAP-TMP
                                         REMAINDER LEAP-RULE1
              DIVIDE DATE-YYYY BY 100 GIVING LEAP-TMP
                                         REMAINDER LEAP-RULE2
              DIVIDE DATE-YYYY BY 400 GIVING LEAP-TMP
                                         REMAINDER LEAP-RULE3
              IF ((LEAP-RULE1 = 0 AND LEAP-RULE2 NOT = 0)
                                      OR LEAP-RULE3 = 0)
                 IF DATE-DDD > 366
                    SET SUB-RC-READ-WARN TO TRUE
                  END-IF
               ELSE
                 IF DATE-DDD > 365
                    SET SUB-RC-READ-WARN TO TRUE
                 END-IF
               END-IF
           END-IF.
      *
       0300-WRITE-RECORD.
           MOVE SUB-IDX-ID TO IDX-ID.
           MOVE SUB-IDX-CUR TO IDX-CUR.
           MOVE 'A H M E T      KOCABAS        ' TO FULL-NAME.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY.
           COMPUTE WS-TODAY-JUL = FUNCTION DAY-OF-INTEGER
              (FUNCTION INTEGER-OF-DATE(WS-TODAY)).
           MOVE WS-TODAY-JUL TO LAST-ORDER-DATE.
           MOVE 0 TO BALANCE.
           WRITE IDX-REC
           MOVE STATUS-IDX TO SUB-RC.
           IF NOT IDX-SUCC
              SET SUB-RC-WRTE-ERR TO TRUE
           ELSE
              SET SUB-RC-SUCC-WRTE TO TRUE
           END-IF.
           GOBACK.
      *
       0400-UPDATE-RECORD.
           MOVE SUB-IDX-ID TO IDX-ID.
           MOVE SUB-IDX-CUR TO IDX-CUR.
           READ IDX-FILE KEY IDX-KEY.
           STRING FULL-NAME DELIMITED BY SIZE INTO SUB-OLD-FULLNAME.
           MOVE SPACES TO SUB-NEW-FULLNAME.
           MOVE 0 TO WS-UPDATE-TYPE.
           PERFORM 0410-UPDATE-NAME.
           MOVE SUB-NEW-FULLNAME TO FULL-NAME.
           REWRITE IDX-REC.
           MOVE STATUS-IDX TO SUB-RC.
           IF NOT IDX-SUCC
              SET SUB-RC-UPDT-ERR TO TRUE
           ELSE
              SET SUB-RC-SUCC-UPDT TO TRUE
              ADD WS-UPDATE-TYPE TO SUB-PRC
           END-IF.
           GOBACK.
      *
       0410-UPDATE-NAME.
           MOVE SUB-OLD-FNAME TO WS-NAME
           PERFORM 0420-REMOVE-SPACES-FUNC.
           MOVE WS-NAME TO SUB-NEW-FNAME.
           MOVE SUB-OLD-LNAME TO WS-NAME.
           PERFORM 0430-REPLACE-CHAR-FUNC.
           MOVE WS-NAME TO SUB-NEW-LNAME.
      *
       0420-REMOVE-SPACES-FUNC.
           MOVE 0 TO WS-INDEX-1 WS-INDEX-2.
           INSPECT WS-NAME TALLYING WS-INDEX-1
               FOR ALL ' '.
           COMPUTE WS-INDEX-1 = 15 - WS-INDEX-1
           PERFORM UNTIL WS-INDEX-2 > WS-INDEX-1
              IF WS-NAME(WS-INDEX-2:1) = ' '
                 MOVE WS-NAME(WS-INDEX-2 + 1: 15 - WS-INDEX-2)
                 TO WS-NAME(WS-INDEX-2: 15 - WS-INDEX-2 + 1)
                 MOVE 1 TO WS-UPDATE-TYPE
              ELSE
                 ADD 1 TO WS-INDEX-2
              END-IF
           END-PERFORM.
      *
       0430-REPLACE-CHAR-FUNC.
           MOVE 0 TO WS-UPDATE-CONTROL.
           INSPECT WS-NAME TALLYING WS-UPDATE-CONTROL FOR ALL 'A' 'E'.
           IF WS-UPDATE-CONTROL > 0
              INSPECT WS-NAME REPLACING ALL 'E' BY 'I'
              INSPECT WS-NAME REPLACING ALL 'A' BY 'E'
              ADD 2 TO WS-UPDATE-TYPE
           END-IF.
      *
       0500-DELETE-RECORD.
           MOVE SUB-IDX-ID TO IDX-ID.
           MOVE SUB-IDX-CUR TO IDX-CUR.
           READ IDX-FILE KEY IDX-KEY.
           DELETE IDX-FILE.
           MOVE STATUS-IDX TO SUB-RC.
           IF NOT IDX-SUCC
              SET SUB-RC-DELT-ERR TO TRUE
           ELSE
              SET SUB-RC-SUCC-DELT TO TRUE
           END-IF.
           GOBACK.
      *
       0800-CLOSE-FILES.
           CLOSE IDX-FILE.
           GOBACK.
