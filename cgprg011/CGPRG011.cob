       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG011.
       AUTHOR.       YTALO WILLIAM.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-WRITTEN. 06/07/2021.
       DATE-COMPILED. 06/07/2021.
      *--------------------------------------------------------------*
      * DISIPLINA: PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: LER DADOS DO CADASTRO DE ACIDENTES E GERAR
      *           RELATORIO COM A AVALIACAO FINAL DAS CIDADES
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010001  SISTEMA LER ARQUIVO E GERA RELATORIO
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
                  FILE STATUS   IS WS-FS-ARQ
           .
           SELECT RELACID ASSIGN TO RELACIDJ
                  FILE STATUS   IS WS-FS-REL
           .
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       FD  CADGRAVA
           LABEL RECORD STANDARD
           RECORDING MODE  F
           .
       01  REG-CADGRAVA           PIC X(27)
           .
       FD  RELACID
           LABEL RECORD OMITTED
           RECORDING MODE  F
           .
       01  REG-RELCID             PIC X(80)
           .
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)       VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-CTLIDO              PIC 9(02).
           05  WS-CTIMPR              PIC 9(02).
           05  WS-CTLIN               PIC 9(02)  VALUE 65.
           05  WS-DTSYS               PIC 9(06).
           05  WS-HRSYS               PIC 9(08).
           05  WS-DTEDI               PIC X(10).
           05  WS-HREDI               PIC X(11).
           05  WS-FS-ARQ              PIC X(02).
           05  WS-FS-REL              PIC X(02).
           05  WS-MSG                 PIC X(30).
           05  WS-FS-MSG              PIC X(02).

      *-----> DADOS DE ENTRADA - CADASTRO DE CIDADES
       01  WS-REG-CADGRAVA.
           05 WS-CIDADE             PIC 9(04).
           05 WS-ESTADO             PIC X(2).
           05 WS-QTD-VEICULOS       PIC 9(07).
           05 WS-BAFOMETRO          PIC X(01).
           05 WS-QTD-ACIDENTES      PIC 9(04).
           05 WS-QTD-OBITOS         PIC 9(04).
           05 WS-PORCS-ACIDS        PIC 9(03)V99.

      *-----> DADOS DE SAIDA - IMPRESSAO DO RELATORIO
       01  WS-CAB1.
           05 FILLER                PIC X(01) VALUE SPACES.
           05 WS-DATA-CAB           PIC X(10).
           05 FILLER                PIC X(04) VALUE SPACES.
           05 FILLER                PIC X(36) VALUE
              'RELATORIO DA AVALIACAO DOS ACIDENTES'.
           05 FILLER                PIC X(17) VALUE SPACES.
           05 FILLER                PIC X(05) VALUE 'PAG. '.
           05 WS-PAG-CAB            PIC 9(02) VALUE ZEROS.
           05 FILLER                PIC X(05) VALUE SPACES.

       01  WS-CAB2.
           05 FILLER                PIC X(01) VALUE SPACES.
           05 FILLER                PIC X(10) VALUE 'COD-CIDADE'.
           05 FILLER                PIC X(02) VALUE SPACES.
           05 FILLER                PIC X(02) VALUE 'UF'.
           05 FILLER                PIC X(02) VALUE SPACES.
           05 FILLER                PIC X(09) VALUE 'QTD-VEICS'.
           05 FILLER                PIC X(01) VALUE SPACES.
           05 FILLER                PIC X(09) VALUE 'BAFOMETRO'.
           05 FILLER                PIC X(01) VALUE SPACES.
           05 FILLER                PIC X(09) VALUE 'QTD-ACIDS'.
           05 FILLER                PIC X(02) VALUE SPACES.
           05 FILLER                PIC X(10) VALUE 'QTD-OBITOS'.
           05 FILLER                PIC X(02) VALUE SPACES.
           05 FILLER                PIC X(06) VALUE '%ACIDS'.
           05 FILLER                PIC X(02) VALUE SPACES.
           05 FILLER                PIC X(10) VALUE 'AVALIACAO'.
           05 FILLER                PIC X(02) VALUE SPACES.

       01  WS-REG-RELACID.
           05 FILLER              PIC X(06) VALUE SPACES.
           05 WS-CIDADE-R         PIC 9(04).
           05 FILLER              PIC X(05) VALUE SPACES.
           05 WS-ESTADO-R         PIC X(02).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-QTD-VEICULOS-R   PIC Z.ZZZ.ZZ9.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 WS-BAFOMETRO-R      PIC X(01).
           05 FILLER              PIC X(06) VALUE SPACES.
           05 WS-QTD-ACIDENTES-R  PIC Z.ZZ9.
           05 FILLER              PIC X(07) VALUE SPACES.
           05 WS-QTD-OBITOS-R     PIC Z.ZZ9.
           05 FILLER              PIC X(06) VALUE SPACES.
           05 WS-PORCS-ACIDS-R    PIC ZZ9,99.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 WS-AVALIACAO-R      PIC X(08).
           05 FILLER              PIC X(01) VALUE SPACES.

       01  WS-HIFEN               PIC X(80)       VALUE ALL '-'.

       01  FILLER                 PIC X(35)       VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *

       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG011.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FS-ARQ = '10'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "*--------------------------*"
           DISPLAY "* PROGRAMA 11 - FATEC SCS  *"
           DISPLAY "* YTALO WILLIAM            *"

           PERFORM 015-DATA-HORA

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * INICIO : ' WS-DTEDI ' AS ' WS-HREDI
           DISPLAY ' *----------------------------------------*'

           PERFORM 020-ABRIR-ARQ

           PERFORM 025-LER-CADGRAVA

           IF WS-FS-ARQ = '10'
              MOVE 'ERRO - CADGRAVA VAZIO'  TO  WS-MSG
              MOVE WS-FS-ARQ              TO  WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           ACCEPT WS-REG-CADGRAVA FROM  SYSIN
           MOVE   WS-DTEDI        TO    WS-DATA-CAB
           .
      *--------------------------------------------------------------*
      *    OBTER A DATA E HORA DO SISTEMA
      *--------------------------------------------------------------*
       015-DATA-HORA.

           ACCEPT  WS-DTSYS  FROM DATE
           STRING  WS-DTSYS  (5:2) '/'
                   WS-DTSYS  (3:2) '/20'
                   WS-DTSYS  (1:2)
           DELIMITED BY SIZE INTO WS-DTEDI

           ACCEPT  WS-HRSYS  FROM TIME
           STRING  WS-HRSYS  (1:2) ':'
                   WS-HRSYS  (3:2) ':'
                   WS-HRSYS  (5:2) ':'
                   WS-HRSYS  (7:2)
           DELIMITED BY SIZE INTO WS-HREDI
           .
      *--------------------------------------------------------------*
      *    ABERTURA DOS ARQUIVOS
      *--------------------------------------------------------------*
       020-ABRIR-ARQ.

           OPEN INPUT CADGRAVA
           IF WS-FS-ARQ  NOT = '00'
              MOVE  'ERRO AO ABRIR O CADGRAVA'  TO WS-MSG
              MOVE   WS-FS-ARQ                TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

           OPEN OUTPUT RELACID
           IF WS-FS-REL  NOT = '00'
              MOVE  'ERRO AO ABRIR O RELACID'  TO WS-MSG
              MOVE   WS-FS-REL                TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF
           .
      *--------------------------------------------------------------*
      *    LEITURA DO CADASTRO DE CIDADES
      *--------------------------------------------------------------*
       025-LER-CADGRAVA.

           READ CADGRAVA  INTO  WS-REG-CADGRAVA

           IF WS-FS-ARQ  NOT = '00' AND '10'
              MOVE  'ERRO NA LEITURA DO CADGRAVA'  TO WS-MSG
              MOVE   WS-FS-ARQ                   TO WS-FS-MSG
              GO TO  999-ERRO
           ELSE
              IF WS-FS-ARQ = '00'
                 ADD   1  TO  WS-CTLIDO
              END-IF
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DO CADGRAVA
      *--------------------------------------------------------------*
       030-PROCESSAR.

           IF WS-PORCS-ACIDS  <  1
              MOVE 'SEGURA' TO  WS-AVALIACAO-R
           ELSE
              IF WS-PORCS-ACIDS < 5
                  MOVE 'ALERTA ' TO  WS-AVALIACAO-R
              ELSE
                  MOVE 'INSEGURA' TO WS-AVALIACAO-R
              END-IF
           END-IF

           MOVE WS-CIDADE            TO  WS-CIDADE-R
           MOVE WS-ESTADO            TO  WS-ESTADO-R
           MOVE WS-QTD-VEICULOS      TO  WS-QTD-VEICULOS-R
           MOVE WS-BAFOMETRO         TO  WS-BAFOMETRO-R
           MOVE WS-QTD-ACIDENTES     TO  WS-QTD-ACIDENTES-R
           MOVE WS-QTD-OBITOS        TO  WS-QTD-OBITOS-R
           MOVE WS-PORCS-ACIDS       TO  WS-PORCS-ACIDS-R

           PERFORM 035-IMPREL

           PERFORM 025-LER-CADGRAVA
           .
      *--------------------------------------------------------------*
      *    IMPRIMIR LINHAS DO RELATORIO
      *--------------------------------------------------------------*
       035-IMPREL.

           IF WS-CTLIN > 8
              PERFORM 040-IMPCAB
           END-IF

           WRITE REG-RELCID   FROM  WS-REG-RELACID
           IF  WS-FS-REL  NOT = '00'
               MOVE 'ERRO NA GRAVACAO DO RELACID'   TO WS-MSG
               MOVE  WS-FS-ARQ                     TO WS-FS-MSG
               GO TO 999-ERRO
           ELSE
               ADD 1 TO WS-CTIMPR
               ADD 1 TO WS-CTLIN
           END-IF
           .
      *--------------------------------------------------------------*
      *    IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       040-IMPCAB.

           ADD    1        TO WS-PAG-CAB

           WRITE REG-RELCID FROM WS-CAB1 AFTER PAGE
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO GRAVACAO CAB1' TO WS-MSG
              MOVE WS-FS-REL TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           WRITE REG-RELCID FROM WS-HIFEN
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO GRAVACAO HIFEN-1' TO WS-MSG
              MOVE WS-FS-REL TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           WRITE REG-RELCID FROM WS-CAB2
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO GRAVACAO CAB2' TO WS-MSG
              MOVE WS-FS-REL            TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           WRITE REG-RELCID FROM WS-HIFEN
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO GRAVACAO HIFEN-2' TO WS-MSG
              MOVE WS-FS-REL               TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           MOVE 4 TO WS-CTLIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           PERFORM 015-DATA-HORA.

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * TERMINO: ' WS-DTEDI ' AS ' WS-HREDI
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG011        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS     - CADGRAVA = ' WS-CTLIDO
           DISPLAY ' * REGISTROS IMPRESSOS - RELACID = ' WS-CTIMPR
           DISPLAY ' *========================================*'

           PERFORM 095-FECHAR-ARQ

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG011        *'
           DISPLAY ' *----------------------------------------*'
           .
      *--------------------------------------------------------------*
      *    FECHAR OS ARQUIVOS
      *--------------------------------------------------------------*
       095-FECHAR-ARQ.

           CLOSE  CADGRAVA
           IF WS-FS-ARQ  NOT = '00'
              MOVE  'ERRO AO FECHAR O CADGRAVA'  TO WS-MSG
              MOVE   WS-FS-ARQ                 TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

           CLOSE  RELACID
           IF WS-FS-REL  NOT = '00'
              MOVE  'ERRO AO FECHAR O RELACID'  TO WS-MSG
              MOVE   WS-FS-REL                 TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF
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
           DISPLAY ' *       TERMINO ANORMAL DO CGPRG011      *'
           DISPLAY ' *----------------------------------------*'
           STOP RUN
           .
      *---------------> FIM DO PROGRAMA CGPRG011 <
