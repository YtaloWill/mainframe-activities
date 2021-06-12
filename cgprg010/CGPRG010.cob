       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG010.
       AUTHOR.       YTALO WILLIAM.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-WRITTEN. 20/10/2019.
       DATE-COMPILED. 01/06/2021.
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
       FILE-CONTROL.
           SELECT CADGRAVA ASSIGN TO CADGRAVJ
                  FILE STATUS     IS WS-COD-STATUS
           .
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       FD  CADGRAVA
           LABEL RECORD STANDARD
           RECORDING MODE F
           .
       01  REG-CADGRAVA           PIC X(27)
           .
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-MED-POR-SP          PIC ZZ9,99.
           05  WS-QTD-ACIDS-SP        PIC ZZZ.ZZ9.
           05  WS-CID-MAIOR-ACID      PIC 9(5).
           05  WS-QTDACID-MAIOR       PIC Z.ZZ9.
           05  WS-CID-MENOR-OBITO     PIC 9(5).
           05  WS-PORCOBITO-MENOR     PIC ZZ9,99.
           05  AS-DATA                PIC 9999/99/99.
           05  AS-PORACID             PIC 9(03)V99.
           05  AS-COUNTPORCSP         PIC 9V99.
           05  AS-QTDACIDSSP          PIC 9(06).
           05  AS-QTDCIDSSP           PIC 9(02).
           05  AS-QTDACID-MAIOR       PIC 9(04).
           05  AS-PORCOBITO-MENOR     PIC 99V99.
           05  AS-PORCOBITO           PIC 99V99.
           05  WS-CTGRAV              PIC 9(02).
           05  WS-COD-STATUS          PIC X(02).
           05  WS-MSG                 PIC X(30).
           05  WS-FS-MSG              PIC X(02).
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(04).
           05 WS-ESTADO           PIC X(2).
           05 WS-QTD-VEICULOS     PIC 9(07).
           05 WS-BAFOMETRO        PIC X(01).
           05 WS-QTD-ACIDENTES    PIC 9(04).
           05 WS-QTD-OBITOS       PIC 9(04).
      *-----> SAIDA - DADOS PARA SYSOUT
       01  WS-REG-SYSOUT.
           05 CID                 PIC 99999.
           05 FILLER              PIC X(01)        VALUE '-'.
           05 UF                  PIC XX.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 VEICS               PIC Z.ZZZ.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 BAFO                PIC X.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 ACIDS               PIC Z.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 OBITOS              PIC Z.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 PORC-ACIDS          PIC ZZ9,99.
           05 FILLER              PIC X(01)        VALUE '%'.
      *-----> OUTPUT PARA ARQUIVO
       01  WS-REG-CADGRAVA.
           05 WS-CIDADEG          PIC 9(04).
           05 WS-ESTADOG          PIC X(2).
           05 WS-QTD-VEICULOSG    PIC 9(07).
           05 WS-BAFOMETROG       PIC X(01).
           05 WS-QTD-ACIDENTESG   PIC 9(04).
           05 WS-QTD-OBITOSG      PIC 9(04).
           05 WS-PORCS-ACIDSG     PIC 9(03)V99.
       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG010.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "YTALO WILLIAM"
           DISPLAY "PROGRAMA 10 - FATEC SCS"
           DISPLAY "-----------------------------------------------"
           OPEN OUTPUT CADGRAVA
           IF WS-COD-STATUS NOT = "00"
              MOVE "ERRO AO ABRIR O CADGRAVA" TO WS-MSG
              MOVE WS-COD-STATUS              TO WS-FS-MSG
              GO TO 999-ERRO
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

           COMPUTE AS-PORACID = 100 * WS-QTD-ACIDENTES / WS-QTD-VEICULOS

           MOVE WS-CIDADE           TO CID
           MOVE WS-ESTADO           TO UF
           MOVE WS-QTD-VEICULOS     TO VEICS
           MOVE WS-BAFOMETRO        TO BAFO
           MOVE WS-QTD-ACIDENTES    TO ACIDS
           MOVE WS-QTD-OBITOS       TO OBITOS
           MOVE AS-PORACID          TO PORC-ACIDS

           MOVE WS-CIDADE           TO WS-CIDADEG
           MOVE WS-ESTADO           TO WS-ESTADOG
           MOVE WS-QTD-VEICULOS     TO WS-QTD-VEICULOSG
           MOVE WS-BAFOMETRO        TO WS-BAFOMETROG
           MOVE WS-QTD-ACIDENTES    TO WS-QTD-ACIDENTESG
           MOVE WS-QTD-OBITOS       TO WS-QTD-OBITOSG
           MOVE AS-PORACID          TO WS-PORCS-ACIDSG

           WRITE REG-CADGRAVA     FROM WS-REG-CADGRAVA

           IF WS-ESTADO = 'SP'
               PERFORM 040-PROCESSAR-SP
           END-IF

           PERFORM 045-PROCESSAR-MAIOR
           PERFORM 047-PROCESSAR-MENOR

           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    CALCULAR DADOS SP
      *--------------------------------------------------------------*
       040-PROCESSAR-SP.
           ADD 1  TO AS-QTDCIDSSP
           COMPUTE AS-QTDACIDSSP = AS-QTDACIDSSP + WS-QTD-ACIDENTES
           COMPUTE AS-COUNTPORCSP = AS-COUNTPORCSP + AS-PORACID
           COMPUTE WS-MED-POR-SP = AS-COUNTPORCSP / AS-QTDCIDSSP

           MOVE AS-QTDACIDSSP    TO WS-QTD-ACIDS-SP
           .
      *--------------------------------------------------------------*
      *    PROCESSAR MAIOR
      *--------------------------------------------------------------*
       045-PROCESSAR-MAIOR.

           IF WS-QTD-ACIDENTES > AS-QTDACID-MAIOR
              MOVE WS-CIDADE        TO WS-CID-MAIOR-ACID
              MOVE WS-QTD-ACIDENTES TO AS-QTDACID-MAIOR
              MOVE WS-QTD-ACIDENTES TO WS-QTDACID-MAIOR
           END-IF

           .
      *--------------------------------------------------------------*
      *    PROCESSAR MENOR
      *--------------------------------------------------------------*
       047-PROCESSAR-MENOR.

           COMPUTE AS-PORCOBITO = 100 * WS-QTD-OBITOS / WS-QTD-ACIDENTES

           IF AS-PORCOBITO < AS-PORCOBITO-MENOR OR WS-CTLIDO = 1
              MOVE WS-CIDADE        TO WS-CID-MENOR-OBITO
              MOVE AS-PORCOBITO     TO AS-PORCOBITO-MENOR
              MOVE AS-PORCOBITO     TO WS-PORCOBITO-MENOR
           END-IF

           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY '-----------------------------------------'
           DISPLAY 'MEDIA DAS PORCENTAGENS DE SP............: '
      -    WS-MED-POR-SP '%'
           DISPLAY 'QTDE. DE ACIDENTES TOTAIS EM SP.........: '
      -    WS-QTD-ACIDS-SP
           DISPLAY 'QTDE. DE CIDADES DE SP PESQUISADAS......: '
      -    AS-QTDCIDSSP

           DISPLAY '-----------------------------------------'
           DISPLAY 'CIDADE COM MAIOR QUANTIDADE DE ACIDENTES: '
      -    WS-CID-MAIOR-ACID
           DISPLAY 'QTDE. DE ACIDENTES DESTA CIDADE.........: '
      -    WS-QTDACID-MAIOR
           DISPLAY 'QTDE. TOTAL DE CIDADES PESQUISADAS......: '
      -    WS-CTLIDO

           DISPLAY '-----------------------------------------'
           DISPLAY 'CIDADE COM MENOR PORCENTAGEM DE OBITOS..: '
      -    WS-CID-MENOR-OBITO
           DISPLAY 'PORCENTAGEM OBITOS/ACIDENTE DESTA CIDADE: '
      -    WS-PORCOBITO-MENOR '%'

           CLOSE CADGRAVA
           IF WS-COD-STATUS NOT = "00"
              MOVE "ERRO AO FECHAR O CADGRAVA" TO WS-MSG
              MOVE WS-COD-STATUS TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF
           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG010        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS    - SYSIN  = ' WS-CTLIDO
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG010        *'
           DISPLAY ' *----------------------------------------*'
           .
       095-FECHAR-ARQ.
           CLOSE CADGRAVA
           IF WS-COD-STATUS NOT = "00"
              MOVE "ERRO AO FECHAR O CADGRAVA" TO WS-MSG
              MOVE WS-COD-STATUS               TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF
           .
       999-ERRO.
           DISPLAY " *-------------------------------------*"
           DISPLAY " *            PROGRAMA CANCELADO       *"
           DISPLAY " *-------------------------------------*"
           DISPLAY " * MENSAGEM    = " WS-MSG
           DISPLAY " * FILE STATUS = " WS-FS-MSG
           DISPLAY " *-------------------------------------*"
           DISPLAY " *       TERMINO ANORMAL DO CGPRG010   *"
           DISPLAY " *-------------------------------------*"
           STOP RUN
           .
      *---------------> FIM DO PROGRAMA CGPRG006 <-------------------*
