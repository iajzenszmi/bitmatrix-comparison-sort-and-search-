cat bcss.f
C======================================================================
C  BCSS_F77.F  - Bitmatrix Comparison Sort & Search (Fortran 77 demo)
C  Single program unit, fixed form (keep text in cols 2..72).
C  Compile (flang):   flang -std=legacy -O3 bcss_f77.f -o bcss
C  Run:               ./bcss
C  Limits: NMAX, KMAX. Keys are INTEGER*4. Pivots must be sorted asc.
C======================================================================
      PROGRAM BCSS
C----- PARAMETERS ------------------------------------------------------
      INTEGER NMAX, KMAX
      PARAMETER (NMAX=10000, KMAX=31)
C----- INPUT / WORK ARRAYS --------------------------------------------
      INTEGER N, K, I, J, B, POS, L, R, M, QVAL, FOUND, IDX
      INTEGER A(NMAX), OUT(NMAX), PIV(KMAX)
      INTEGER COUNTS(0:KMAX), BASE(0:KMAX), WRITEP(0:KMAX)
      INTEGER BUCKET, WORD, MASK, LEFT, RIGHT, MID, VAL, TMP
C----- DEMO DATA (you can replace with your own I/O) ------------------
C  Example: N=20 random-ish keys; K=5 pivots chosen & sorted.
      N = 20
      K = 5
      A(1) = 35
      A(2) = 12
      A(3) = 99
      A(4) = 58
      A(5) = 58
      A(6) =  7
      A(7) = 61
      A(8) = 14
      A(9) = 19
      A(10)= 73
      A(11)=  5
      A(12)= 58
      A(13)= 16
      A(14)= 88
      A(15)=  2
      A(16)= 40
      A(17)= 41
      A(18)= 60
      A(19)= 18
      A(20)= 57
C  Pivots (sorted): choose by sample/quantiles externally if desired.
      PIV(1)=10
      PIV(2)=20
      PIV(3)=40
      PIV(4)=60
      PIV(5)=80

C----- STEP 1: BUILD BIT-SIGNATURES AND COUNT BUCKET SIZES ------------
      DO 5 J=0,K
         COUNTS(J)=0
 5    CONTINUE

      DO 20 I=1,N
C       Compute k-bit signature: bit j set if A(i) <= PIV(j)
         WORD = 0
         DO 10 J=1,K
            IF (A(I) .LE. PIV(J)) THEN
               MASK = ISHFT(1, J-1)
               WORD = IOR(WORD, MASK)
            ENDIF
 10      CONTINUE
C       POPCOUNT by testing bits 0..K-1 (no external function)
         BUCKET = 0
         DO 15 J=0,K-1
            MASK = ISHFT(1, J)
            IF (IAND(WORD,MASK) .NE. 0) BUCKET = BUCKET + 1
 15      CONTINUE
         COUNTS(BUCKET) = COUNTS(BUCKET) + 1
C       Reuse WORD if you wish to store per-item; here we recompute later
 20   CONTINUE

C----- STEP 2: PREFIX SUMS -> BASE OFFSETS ----------------------------
      BASE(0)=0
      DO 30 J=1,K
         BASE(J)=BASE(J-1)+COUNTS(J-1)
 30   CONTINUE
      DO 35 J=0,K
         WRITEP(J)=BASE(J)
 35   CONTINUE

C----- STEP 3: STABLE SCATTER (REPEAT SIGNATURE + POPCOUNT) ----------
      DO 60 I=1,N
         WORD = 0
         DO 40 J=1,K
            IF (A(I) .LE. PIV(J)) THEN
               MASK = ISHFT(1, J-1)
               WORD = IOR(WORD, MASK)
            ENDIF
 40      CONTINUE
         BUCKET = 0
         DO 50 J=0,K-1
            MASK = ISHFT(1, J)
            IF (IAND(WORD,MASK) .NE. 0) BUCKET = BUCKET + 1
 50      CONTINUE
         POS = WRITEP(BUCKET)
         OUT(POS+1) = A(I)
         WRITEP(BUCKET) = WRITEP(BUCKET) + 1
 60   CONTINUE

C----- STEP 4: INTRA-BUCKET STABLE INSERTION SORT ---------------------
      DO 90 B=0,K
         L = BASE(B) + 1
         IF (B .EQ. K) THEN
            R = N
         ELSE
            R = BASE(B+1)
         ENDIF
         IF (R .LE. L) GOTO 90
C        Standard insertion sort on OUT(L..R)
         DO 80 I=L+1,R
            VAL = OUT(I)
            J = I - 1
 70         IF (J .GE. L .AND. OUT(J) .GT. VAL) THEN
               OUT(J+1) = OUT(J)
               J = J - 1
               GOTO 70
            ENDIF
            OUT(J+1) = VAL
 80      CONTINUE
 90   CONTINUE

C----- OUTPUT: SHOW SORTED ARRAY -------------------------------------
      WRITE(*,*) 'Sorted:'
      DO 100 I=1,N
         WRITE(*,'(1X,I6,1X)') OUT(I)
 100  CONTINUE

C----- SEARCH DEMO: MAP QUERY TO BUCKET, THEN BINARY-SEARCH LOCALLY ---
      QVAL = 58
C     Build signature for query
      WORD = 0
      DO 110 J=1,K
         IF (QVAL .LE. PIV(J)) THEN
            MASK = ISHFT(1, J-1)
            WORD = IOR(WORD, MASK)
         ENDIF
 110  CONTINUE
      B = 0
      DO 120 J=0,K-1
         MASK = ISHFT(1, J)
         IF (IAND(WORD,MASK) .NE. 0) B = B + 1
 120  CONTINUE
      L = BASE(B) + 1
      IF (B .EQ. K) THEN
         R = N
      ELSE
         R = BASE(B+1)
      ENDIF
C     Binary search in OUT(L..R)
      FOUND = 0
      LEFT = L
      RIGHT = R
 130  IF (LEFT .LE. RIGHT) THEN
         MID = (LEFT+RIGHT)/2
         IF (OUT(MID) .EQ. QVAL) THEN
            FOUND = 1
            IDX = MID
         ELSE IF (OUT(MID) .LT. QVAL) THEN
            LEFT = MID + 1
            GOTO 130
         ELSE
            RIGHT = MID - 1
            GOTO 130
         ENDIF
      ENDIF
      IF (FOUND .EQ. 1) THEN
         WRITE(*,*) 'Query ',QVAL,' found at index ',IDX
      ELSE
         WRITE(*,*) 'Query ',QVAL,' not found in its bucket.'
      ENDIF

      END
~ $ flang bcss.f -o bcss
~ $
~ $ ./bcss
 Sorted:
     88
     99
     61
     73
     41
     57
     58
     58
     58
     60
     35
     40
     12
     14
     16
     18
     19
      2
      5
      7
 Query  58  found at index  7
~ $