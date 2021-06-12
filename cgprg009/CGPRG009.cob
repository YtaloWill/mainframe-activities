       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.    CGPRG009.
       AUTHOR.        YTALO WILLIAM DE JESUS DA GLORIA.
       DATE-WRITTEN.  29/06/2021.
      *--------------------------------------------------------------*
      * DISIPLINA: PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: LER DADOS DO CADASTRO DE ALUNOS E GERAR RELATORIO
      *           COM A AVALIACAO FINAL DOS ALUNOS DA TURMA
      *           SOLICITADA NO PARAMETRO VIA SYSIN
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
           SELECT CADALU ASSIGN TO CADALUJ
                  FILE STATUS   IS WS-FS-ALU
           .
           SELECT RELALU ASSIGN TO RELALUJ
                  FILE STATUS   IS WS-FS-REL
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
       FD  RELALU
           LABEL RECORD OMITTED
           RECORDING MODE  F
           .
       01  REG-RELALU             PIC X(80)
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
           05  WS-FS-ALU              PIC X(02).
           05  WS-FS-REL              PIC X(02).
           05  WS-MSG                 PIC X(30).
           05  WS-FS-MSG              PIC X(02).

      *-----> DADOS DE ENTRADA - CADASTRO DE ALUNOS
       01  WS-REG-CADALU.
           05 WS-NUMERO-E         PIC 9(04).
           05 WS-NOME-E           PIC X(20).
           05 WS-SEXO-E           PIC X(01).
           05 WS-IDADE-E          PIC 9(02).
           05 WS-CURSO-E          PIC X(12).
           05 WS-NOTA1-E          PIC 9(02)V99.
           05 WS-NOTA2-E          PIC 9(02)V99.
           05 WS-MEDIA-E          PIC 9(02)V99.
           05 FILLER              PIC X(19).

      *-----> DADOS DE SAIDA - IMPRESSAO DO RELATORIO
       01  WS-CAB1.
           05 FILLER                PIC X(01) VALUE SPACES.
           05 WS-DATA-CAB           PIC X(10).
           05 FILLER                PIC X(02) VALUE SPACES.
           05 FILLER                PIC X(40) VALUE
              'RELATORIO DA AVALIACAO DOS ALUNOS DE ADS'.
           05 WS-CURSO-CAB          PIC X(15).
           05 FILLER                PIC X(04) VALUE SPACES.
           05 FILLER                PIC X(05) VALUE 'PAG. '.
           05 WS-PAG-CAB            PIC 9(02) VALUE ZEROS.
           05 FILLER                PIC X(01) VALUE SPACES.

       01  WS-CAB2.
           05 FILLER                PIC X(01) VALUE SPACES.
           05 FILLER                PIC X(07) VALUE 'NUM.   '.
           05 FILLER                PIC X(04) VALUE 'NOME'.
           05 FILLER                PIC X(19) VALUE SPACES.
           05 FILLER                PIC X(05) VALUE 'CURSO'.
           05 FILLER                PIC X(10) VALUE SPACES.
           05 FILLER                PIC X(08) VALUE 'NOTA1   '.
           05 FILLER                PIC X(08) VALUE 'NOTA2   '.
           05 FILLER                PIC X(08) VALUE 'MEDIA   '.
           05 FILLER                PIC X(09) VALUE 'AVALIACAO'.
           05 FILLER                PIC X(01) VALUE SPACES.

       01  WS-REG-RELALU.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 WS-NUMERO-R         PIC 9(04).
           05 FILLER              PIC X(03) VALUE SPACES.
           05 WS-NOME-R           PIC X(20).
           05 FILLER              PIC X(03) VALUE SPACES.
           05 WS-CURSO-R          PIC X(12).
           05 FILLER              PIC X(03) VALUE SPACES.
           05 WS-NOTA1-R          PIC Z9,99.
           05 FILLER              PIC X(03) VALUE SPACES.
           05 WS-NOTA2-R          PIC Z9,99.
           05 FILLER              PIC X(03) VALUE SPACES.
           05 WS-MEDIA-R          PIC Z9,99.
           05 FILLER              PIC X(03) VALUE SPACES.
           05 WS-AVALIACAO-R      PIC X(09).
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
       000-CGPRG009.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FS-ALU = '10'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "PROGRAMA 9 - FATEC SCS"
           DISPLAY "YTALO WILLIAM DE JESUS DA GLORIA"
           DISPLAY "-----------------------------------------------"

           PERFORM 015-DATA-HORA

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * INICIO : ' WS-DTEDI ' AS ' WS-HREDI
           DISPLAY ' *----------------------------------------*'

           PERFORM 020-ABRIR-ARQ

           PERFORM 025-LER-CADALU

           IF WS-FS-ALU = '10'
              MOVE 'ERRO - CADALU VAZIO'  TO  WS-MSG
              MOVE WS-FS-ALU              TO  WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           ACCEPT WS-CURSO-CAB  FROM  SYSIN
           MOVE   WS-DTEDI      TO    WS-DATA-CAB
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

           OPEN INPUT CADALU
           IF WS-FS-ALU  NOT = '00'
              MOVE  'ERRO AO ABRIR O CADALU'  TO WS-MSG
              MOVE   WS-FS-ALU                TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

           OPEN OUTPUT RELALU
           IF WS-FS-REL  NOT = '00'
              MOVE  'ERRO AO ABRIR O RELALU'  TO WS-MSG
              MOVE   WS-FS-REL                TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF
           .
      *--------------------------------------------------------------*
      *    LEITURA DO CADASTRO DE ALUNOS
      *--------------------------------------------------------------*
       025-LER-CADALU.

           READ CADALU  INTO  WS-REG-CADALU

           IF WS-FS-ALU  NOT = '00' AND '10'
              MOVE  'ERRO NA LEITURA DO CADALU'  TO WS-MSG
              MOVE   WS-FS-ALU                   TO WS-FS-MSG
              GO TO  999-ERRO
           ELSE
              IF WS-FS-ALU = '00'
                 ADD   1  TO  WS-CTLIDO
              END-IF
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DO CADALU
      *--------------------------------------------------------------*
       030-PROCESSAR.

           IF WS-MEDIA-E  <  7
              MOVE 'REPROVADO' TO  WS-AVALIACAO-R
           ELSE
              MOVE 'APROVADO ' TO  WS-AVALIACAO-R
           END-IF

           MOVE  WS-NUMERO-E   TO  WS-NUMERO-R
           MOVE  WS-NOME-E     TO  WS-NOME-R
           MOVE  WS-CURSO-E    TO  WS-CURSO-R
           MOVE  WS-CURSO-E    TO  WS-CURSO-R
           MOVE  WS-NOTA1-E    TO  WS-NOTA1-R
           MOVE  WS-NOTA2-E    TO  WS-NOTA2-R
           MOVE  WS-MEDIA-E    TO  WS-MEDIA-R

           PERFORM 035-IMPREL

           PERFORM 025-LER-CADALU
           .
      *--------------------------------------------------------------*
      *    IMPRIMIR LINHAS DO RELATORIO
      *--------------------------------------------------------------*
       035-IMPREL.

           IF WS-CTLIN > 8
              PERFORM 040-IMPCAB
           END-IF

           WRITE REG-RELALU   FROM  WS-REG-RELALU
           IF  WS-FS-REL  NOT = '00'
               MOVE 'ERRO NA GRAVACAO DO RELALU'   TO WS-MSG
               MOVE  WS-FS-ALU                     TO WS-FS-MSG
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

           WRITE REG-RELALU FROM WS-CAB1 AFTER PAGE
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO GRAVACAO CAB1' TO WS-MSG
              MOVE WS-FS-REL TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           WRITE REG-RELALU FROM WS-HIFEN
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO GRAVACAO HIFEN-1' TO WS-MSG
              MOVE WS-FS-REL TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           WRITE REG-RELALU FROM WS-CAB2
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO GRAVACAO CAB2' TO WS-MSG
              MOVE WS-FS-REL            TO WS-FS-MSG
              GO TO 999-ERRO
           END-IF

           WRITE REG-RELALU FROM WS-HIFEN
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
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG009        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS     - CADALU = ' WS-CTLIDO
           DISPLAY ' * REGISTROS IMPRESSOS - RELALU = ' WS-CTIMPR
           DISPLAY ' *========================================*'

           PERFORM 095-FECHAR-ARQ

           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG009        *'
           DISPLAY ' *----------------------------------------*'
           .
      *--------------------------------------------------------------*
      *    FECHAR OS ARQUIVOS
      *--------------------------------------------------------------*
       095-FECHAR-ARQ.

           CLOSE  CADALU
           IF WS-FS-ALU  NOT = '00'
              MOVE  'ERRO AO FECHAR O CADALU'  TO WS-MSG
              MOVE   WS-FS-ALU                 TO WS-FS-MSG
              GO TO  999-ERRO
           END-IF

           CLOSE  RELALU
           IF WS-FS-REL  NOT = '00'
              MOVE  'ERRO AO FECHAR O RELALU'  TO WS-MSG
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
           DISPLAY ' *       TERMINO ANORMAL DO CGPRG009      *'
           DISPLAY ' *----------------------------------------*'
           STOP RUN
           .
      *---------------> FIM DO PROGRAMA CGPRG009 <
