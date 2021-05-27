       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG003.
       AUTHOR. CELSO GALLAO.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-WRITTEN. 02/03/2021.
       DATE-COMPILED. 02/03/2021.
       SECURITY. NIVEL BASICO.
      *---------------------*
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
      *==> LOCAL PARA O SELECT DOS ARQUVOS

       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
      *==> LOCAL PARA A FD (DESCRICAO DOS ARQUIVOS)

       WORKING-STORAGE SECTION.
      *-----------------------*

      *-----> AREA AUXILIAR
       77  WS-FIM                 PIC X(01) VALUE "N".
       77  WS-CTEXIB              PIC 9(02).
       77  AS-N1                  PIC 99V99.
       77  AS-N2                  PIC 99V99.
       77  AS-MED                 PIC 99V99.

      *-----> DADOS DE SAIDA VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-ALUNO            PIC A(25).
           05 FILLER              PIC X(2)        VALUE SPACES.
           05 WS-RA               PIC 9(13).
           05 FILLER              PIC X(2)        VALUE SPACES.
           05 WS-NOTA1            PIC Z9,99.
           05 FILLER              PIC X(2)        VALUE SPACES.
           05 WS-NOTA2            PIC Z9,99.
           05 FILLER              PIC X(2)        VALUE SPACES.
           05 WS-MEDIA            PIC Z9,99.

       LINKAGE SECTION.
      *----------------*
       01  LK-PARAMETROS.
           05 LK-NR-DPTO             PIC 9(04).
           05 LK-NOME-DPTO           PIC X(15).
           05 LK-COD-RETORNO         PIC 99.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL                                        *
      *--------------------------------------------------------------*

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 050-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS                                    *
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "** PROGRAMA 3 **"

           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.

           DISPLAY '** YTALO WILLIAM **'

           MOVE 'ZE GABARITO JR'   TO   WS-ALUNO
           MOVE '1680481117111'    TO   WS-RA
           MOVE 8,25               TO   AS-N1
           MOVE 10,00              TO   AS-N2
           COMPUTE AS-MED = (AS-N1 + AS-N2)/ 2
           MOVE AS-N1              TO   WS-NOTA1
           MOVE AS-N2              TO   WS-NOTA2
           MOVE AS-MED             TO   WS-MEDIA

           DISPLAY WS-REG-SYSOUT
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '** FIM DA EXECUCAO **'

           DISPLAY "REGISTROS EXIBIDOS = " WS-CTEXIB
           DISPLAY "TERMINO NORMAL DO PROGRAMA CGPRG003"
           .
      *---------------> FIM DO PROGRAMA CGPRG003 <-------------------*
