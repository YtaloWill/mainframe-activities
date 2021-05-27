       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.    CGPRG008.
       AUTHOR.        YTALO WILLIAM DE JESUS DA GLORIA.
       INSTALLATION.  FATEC SAO CAETANO.
       DATE-WRITTEN.  20/10/2019.
       DATE-COMPILED. 17/05/2021.
      *--------------------------------------------------------------*
      * DISCIPLINA: PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *           GRAVAR ARQUIVO FISICO SEQUENCIAL(WRITE)
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010002  SISTEMA GERA ARQUIVO SEQUENCIAL     *
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
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO CADALUJ
                  FILE STATUS   IS WS-FS-ALU
           .
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       FD  CADALU
           LABEL RECORD STANDARD
           RECORDING MODE  F
           .
       01  REG-CADALU             PIC X(70)
           .
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-CTGRAV              PIC 9(02).
           05  WS-FS-ALU              PIC X(02).
           05  WS-MSG                 PIC X(30).
           05  WS-FS-MSG              PIC X(02).
           05  WS-MEDIA               PIC 9(02)V99.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-NUMERO-IN        PIC 9(04).
           05 WS-NOME-IN          PIC X(20).
           05 WS-SEXO-IN          PIC X(01).
           05 WS-IDADE-IN         PIC 9(02).
           05 WS-CURSO-IN         PIC X(12).
           05 WS-NOTA1-IN         PIC 9(02)V99.
           05 WS-NOTA2-IN         PIC 9(02)V99.
      *-----> SAIDA - DADOS PARA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-NUM              PIC 9(04).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NOME             PIC X(20).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-SEXO             PIC X(01).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-IDADE            PIC 9(02).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-CURSO            PIC X(12).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NOTA1            PIC Z9,99.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NOTA2            PIC Z9,99.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-MED              PIC Z9,99.
      *-----> SAIDA - ARQ. FISICO SEQUENCIAL
       01  WS-REG-CADALU.
           05 WS-NUMERO-S         PIC 9(04).
           05 WS-NOME-S           PIC X(20).
           05 WS-SEXO-S           PIC X(01).
           05 WS-IDADE-S          PIC 9(02).
           05 WS-CURSO-S          PIC X(12).
           05 WS-NOTA1-S          PIC 9(02)V99.
           05 WS-NOTA2-S          PIC 9(02)V99.
           05 WS-MEDIA-S          PIC 9(02)V99.
           05 FILLER              PIC X(19)       VALUE SPACES.

       01  WS-HIFEN               PIC X(80)       VALUE ALL '-'.

       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG008.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

	   DISPLAY "PROGRAMA 8 - FATEC SCS"
           DISPLAY "YTALO WILLIAM DE JESUS DA GLORIA"
           DISPLAY "-----------------------------------------------"

           OPEN OUTPUT CADALU
           IF WS-FS-ALU  NOT = '00'
              MOVE  'ERRO AO ABRIR O CADALU'  TO WS-MSG
              MOVE   WS-FS-ALU                TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

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
           MOVE WS-REG-SYSIN   TO   WS-REG-CADALU
           MOVE WS-MEDIA       TO   WS-MEDIA-S
           WRITE REG-CADALU   FROM  WS-REG-CADALU
           IF  WS-FS-ALU  NOT = '00'
               MOVE 'ERRO NA GRAVACAO DO CADALUN'  TO WS-MSG
               MOVE  WS-FS-ALU                     TO WS-FS-MSG
               GO TO 999-ERRO
           ELSE
               ADD 1 TO WS-CTGRAV
           END-IF

           MOVE WS-NUMERO-S       TO    WS-NUM
           MOVE WS-NOME-S         TO    WS-NOME
           MOVE WS-SEXO-S         TO    WS-SEXO
           MOVE WS-IDADE-S        TO    WS-IDADE
           MOVE WS-CURSO-S        TO    WS-CURSO
           MOVE WS-NOTA1-S        TO    WS-NOTA1
           MOVE WS-NOTA2-S        TO    WS-NOTA2
           MOVE WS-MEDIA          TO    WS-MED
           
           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG008        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS    - SYSIN  = ' WS-CTLIDO
           DISPLAY ' * REGISTROS GRAVADOS - CADALU = ' WS-CTGRAV
           DISPLAY ' *========================================*'

           CLOSE  CADALU
           IF WS-FS-ALU  NOT = '00'
              MOVE  'ERRO AO FECHAR O CADALU'  TO WS-MSG
              MOVE   WS-FS-ALU                 TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG008        *'
           DISPLAY ' *----------------------------------------*'
           .
      *--------------------------------------------------------------*
      *    ROTINA DE ERRO
      *--------------------------------------------------------------*
       999-ERRO.

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *           PROGRAMA CANCELADO           *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * MENSAGEM    = ' WS-MSG
           DISPLAY ' * FILE STATUS = ' WS-FS-MSG
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *       TERMINO ANORMAL DO CGPRG008      *'
           DISPLAY ' *----------------------------------------*'
           STOP RUN
           .
      *---------------> FIM DO PROGRAMA XXPRG002 <-------------------*
