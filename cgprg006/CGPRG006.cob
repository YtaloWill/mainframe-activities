       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG006.
       AUTHOR.       YTALO WILLIAM.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-WRITTEN. 03/09/2019.
       DATE-COMPILED. 06/04/2021.
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
           05  AS-PORACID             PIC 9V99.
           05  AS-COUNTPORCSP         PIC 9V99.
           05  AS-QTDACIDSSP          PIC 9(06).
           05  AS-QTDCIDSSP           PIC 9(02).
           05  AS-QTDACID-MAIOR       PIC 9(04).
           05  AS-PORCOBITO-MENOR     PIC 99V99.
           05  AS-PORCOBITO           PIC 99V99.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(05).
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

       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG006.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.
           ACCEPT AS-DATA FROM DATE

           DISPLAY "YTALO WILLIAM"
           DISPLAY "ATIVIDADE 6"
           DISPLAY "ESTATISTICAS - DATA DO CALCULO: " AS-DATA
           DISPLAY "-----------------------------------------------"

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

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG006        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS    - SYSIN  = ' WS-CTLIDO
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG006        *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA CGPRG006 <-------------------*
