       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG005.
      *AUTHOR.       YTALO WILLIAM.
      *DATE-WRITTEN. 04/05/2021.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010001  SISTEMA MOSTRA SYSOUT
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS DE TOTAL DE CONTROLE
       01  WS-CONTROL-TOTALS.
           05  WS-TOTAL-MAN           PIC Z9.
           05  WS-TOTAL-WOMAN         PIC Z9.
           05  WS-COUNT-SIX           PIC Z9.
           05  WS-PERCENT-SIX         PIC Z9,99.
           05  WS-GENERAL-AVERAGE     PIC Z9,99.
           05  WS-AVERAGE-SIX         PIC Z9,99.
      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-MEDIA               PIC 9(02)V99.
           05  AS-TOTAL-MAN           PIC 9(02).
           05  AS-TOTAL-WOMAN         PIC 9(02).
           05  AS-AVERAGE-SIX         PIC 9(02)V99.
           05  AS-PERCENT-SIX         PIC 9(03)V99.
           05  AS-GENERAL-AVERAGE     PIC 9(02)V99.
           05  AS-SUM-GRADES          PIC 9(04)V99.
           05  AS-SUM-SIX             PIC 9(04)V99.
           05  AS-COUNT-SIX           PIC 9(02).
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-NUMERO-IN        PIC 9(04).
           05 WS-NOME-IN          PIC X(20).
           05 WS-SEXO-IN          PIC X(01).
           05 WS-IDADE-IN         PIC 9(02).
           05 WS-CURSO-IN         PIC X(12).
           05 WS-NOTA1-IN         PIC 9(02)V99.
           05 WS-NOTA2-IN         PIC 9(02)V99.
      *-----> DADOS DE SAIDA
       01  WS-REG-SYSOUT.
           05 WS-NUM              PIC 9(04).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NOM              PIC X(20).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-SEX              PIC X(01).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-IDA              PIC Z9.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-CUR              PIC X(12).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NT1              PIC Z9,99.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NT2              PIC Z9,99.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-MED              PIC Z9,99.

       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-RSPRG002.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY '---------------------------------------------'
           DISPLAY 'ATIVIDADE 5'
           DISPLAY 'YTALO WILLIAM'
           DISPLAY 'CALCULO DA MEDIA DOS ALUNOS A PARTIR DA SYSIN'
           DISPLAY '---------------------------------------------'

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

           COMPUTE WS-MEDIA = (WS-NOTA1-IN + WS-NOTA2-IN) / 2

           IF WS-SEXO-IN = 'M'
               ADD 1 TO AS-TOTAL-MAN
           ELSE
               ADD 1 TO AS-TOTAL-WOMAN
           END-IF

           ADD WS-MEDIA TO AS-SUM-GRADES

           IF WS-MEDIA < 6
               ADD WS-MEDIA TO AS-SUM-SIX
               ADD 1 TO AS-COUNT-SIX
           END-IF

           MOVE WS-NUMERO-IN          TO WS-NUM
           MOVE WS-NOME-IN            TO WS-NOM
           MOVE WS-SEXO-IN            TO WS-SEX
           MOVE WS-IDADE-IN           TO WS-IDA
           MOVE WS-CURSO-IN           TO WS-CUR
           MOVE WS-NOTA1-IN           TO WS-NT1
           MOVE WS-NOTA2-IN           TO WS-NT2
           MOVE WS-MEDIA              TO WS-MED

           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.
           COMPUTE AS-AVERAGE-SIX = AS-SUM-SIX / AS-COUNT-SIX           DER-SIX
           COMPUTE AS-GENERAL-AVERAGE = AS-SUM-GRADES / WS-CTLIDO
           COMPUTE AS-PERCENT-SIX = AS-COUNT-SIX * 100 / WS-CTLIDO      CTLIDO

           MOVE AS-AVERAGE-SIX        TO WS-AVERAGE-SIX
           MOVE AS-GENERAL-AVERAGE    TO WS-GENERAL-AVERAGE
           MOVE AS-PERCENT-SIX        TO WS-PERCENT-SIX
           MOVE AS-COUNT-SIX          TO WS-COUNT-SIX
           MOVE AS-TOTAL-MAN          TO WS-TOTAL-MAN
           MOVE AS-TOTAL-WOMAN        TO WS-TOTAL-WOMAN

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS        :' WS-CTLIDO
           DISPLAY ' * TOTAL DE MULHERES      :' WS-TOTAL-WOMAN
           DISPLAY ' * TOTAL DE HOMENS        :' WS-TOTAL-MAN
           DISPLAY ' * MEDIA GERAL DE ALUNOS  :' WS-GENERAL-AVERAGE
           DISPLAY ' * TOTAL DE ALUNOS ABAIXO DA MEDIA 6,00:'
           DISPLAY WS-COUNT-SIX
           DISPLAY ' * PORCENTAGEM DE ALUNOS ABAIXO DA MEDIA 6,00:'
           DISPLAY WS-PERCENT-SIX '%'
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA RSPRG002 <-------------------*
