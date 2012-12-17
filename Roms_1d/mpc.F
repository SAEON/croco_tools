! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
! Multifunctional precompiling processor designed to work between
! CPP and FORTRAN compiler.
!-------------------------------------------------------------------
! Objectives:
!------------ 
! (1) Eliminate blank lines left by CPP and fortran style comments,
!     but keep compiler directives like C$DOACROSS in place.
!
! (2) Convert _default_ size fortran real type declarations into
!     real*8.  Any mixture of lower and upper cases latters is
!     allowed in the word real. 
!     Explicitly specified size declaration, such as real*4
!     or real*16 will be left unchanged.
!   --> Optional:: acrivated by REAL_TO_REAL8 CPP-switch.
!
! (3) Enforce double precision accuracy for the real type constants
!     appearing anywhere in the program, for example, 1. --> 1.D0;
!     .1 --> .1D0;    9.81 --> 9.81D0;   .5e-8 --> .5D-8; 
!     1.2e+14 --> 1.2D+14 etc.
!   --> Safe. Will distinguish real type constants from look alike
!     combinations like, say format descriptors F8.4 or e12.5; or
!     logical expressions like 1.eq.i etc. Will fold line of the
!     code automatically according to fortran syntax rules, when
!     this change causes line to increase beyond 72 characters. 
!   --> Optional: acrivated by DOUBLE_CONST CPP-switch.
!
!
! (4) Convert _default_ size fortran integer type declarations
!     into integer*4. Explicitly specified size declaration, such
!     as integer*2 or integer*8 will be left unchanged.
!   --> Optional: activated when _both_  INT_TO_INT4 _and_
!                 REAL_TO_REAL8 CPP-switches are defined.
!
!
! (5) Convert parallelized loops over subdomains (tiles) 
!
!          do tile=0,NSUB_X*NSUB_E-1
!
!     into two nested loops. The outer is loop over parallel threads
!     (to be parallelized), and the inner is over tiles to be
!     processed by the particular thread:
!
!          do trd=0,numthreads-1
!            do tile=forward_sweep
!     or
!          do trd=0,numthreads-1
!            do tile=backward_sweep
!
!     in such a way the inner loops in the subsequent parallel
!     regions are reversed, so that each thread "zig-zags" across
!     the tiles it is processing. Automatically append SHARED/PRIVATE
!     lists with the newly introduced variables.
!   --> Optional: acrivated by ZIG_ZAG CPP-switch. 
!
! More detailed description for each option is available below.
!
! In the case when after they were modified, lines of the fortran
! program became longer than the prescribed width of 72 characters, 
! they are automatically folded by mpc according to the fortran
! syntax rules. 
!
! Usage:
!-------
! Similar to CPP
!                  mpc source.file target.file
!             or
!                  mpc source.file > target.file
!
! WARNING: Insofar as manifestations of functional deficiencies are
! agreed upon by any and all concerned parties to be imperceivable,
! and are so stipulated, it is incumbent upon said heretofore
! mentioned parties to exercise the deferment of otherwise pertinent
! maintenance procedures.
!
! And therefore, all manifestations of functional deficiencies, such
! as bugs, evidence of nonrobust behaviour or any other problems of
! any kind shall be reported to Alexander Shchepetkin, (310)-206-9381
! or alex@atmos.ucla.edu.
!
c--#define ZIG_ZAG
#define REAL_TO_REAL8
#define INT_TO_INT4
c--#define DOUBLE_CONST
!
      implicit none
      integer full_length
      parameter(full_length=72)
      character*1 symbol(2*full_length)
      character*32 file_in,file_out
      logical end_of_file, directive
      integer last_arg,iargc, iin,iout,iocheck, length, i,j,k,m,
     &                                       ks1,ks2,ks3,ks4,ks5
      character*1 quote, double_quote
#ifdef DOUBLE_CONST
      logical dgt
      integer ndots,indx(full_length)
#endif
#ifdef ZIG_ZAG
      logical par_region, dir_switch
      integer s1,s2,l1,l2
      character*1 share_list(full_length),
     &            local_list(full_length)
      data local_list/'t','r','d',',','s','u','b','s',',',63*' '/
      data share_list/'n','u','m','t','h','r','e','a','d','s',
     &                                               ',', 61*' '/
      par_region=.FALSE.
      dir_switch=.FALSE.
#endif
      quote=char(39)
      double_quote=char(34)
!-
      file_in='                                '
      file_out='                                '
      last_arg=iargc()
      if (last_arg.gt.0) then
        iin=11
        call getarg(1,file_in)
        open(unit=iin,file=file_in,form='FORMATTED',
     &                             status='UNKNOWN')
      else
        stop 'NO FILE TO PROCESS'
      endif
      if (last_arg.eq.2) then
        iout=12
        call getarg(2,file_out)
        open(unit=iout,file=file_out,form='FORMATTED',
     &                               status='UNKNOWN')
      else
        iout=6
      endif
      end_of_file=.false.

   1   length=0                 ! RESET THE STING LENGTH
        do i=1,full_length      ! AND THE STRING ITSELF,
          symbol(i)=' '         ! THEN READ IN THE STRING.
        enddo
        read(iin,'(80A1)',iostat=iocheck,end=2)
     &                (symbol(i), i=1,full_length)
        go to 3
   2    end_of_file=.true.
        if( symbol(1).eq.'#') go to 1 
   3    if( symbol(1).eq.'C' .or. symbol(1).eq.'c' .or.
     &                             symbol(1).eq.'!') then

          if( symbol(2).eq.'$' .or. symbol(3).eq.'$' .or.
     &        symbol(4).eq.'$' .or. symbol(5).eq.'$') then
            directive=.true.
          else
            directive=.false.
            goto 1
          endif

c          if( symbol(2).ne.'$' .and. symbol(3).ne.'$' .and.
c     &        symbol(4).ne.'$' .and. symbol(5).ne.'$') then
c            directive=.false.
c            go to 1             !--> This is a comment line: IGNORE
c          else
c            directive=.true.
c          endif                 !    If the dollar sign is present, 

        endif                   !    this is a directive and it 

        i=0                     !    should be further processed.
   4     i=i+1
          if (symbol(i).eq.'!') go to 5
          if (symbol(i).ne.' ') length=i
          if (i.eq.full_length) go to 5
         go to 4
   5    continue

!
! Recognize REAL or real and turn it into REAL*8 or real*8,
! but do not convert REAL*4 or real*4 into real*8.
!
#ifdef REAL_TO_REAL8
        i=6
   6     i=i+1
          if (symbol(i  ).eq.'R' .or. symbol(i  ).eq.'r') then
           if (symbol(i+1).eq.'E' .or. symbol(i+1).eq.'e') then
            if (symbol(i+2).eq.'A' .or. symbol(i+2).eq.'a') then
             if (symbol(i+3).eq.'L' .or. symbol(i+3).eq.'l') then
              if (symbol(i-1).eq.' '.and. symbol(i+4).eq.' ') then
                do j=length,i+4,-1
                  symbol(j+2)=symbol(j)
                enddo
                symbol(i+4)='*'
                symbol(i+5)='8'
                length=length+2
                go to 7
              endif
             endif
            endif
           endif
# ifdef INT_TO_INT4
          elseif (symbol(i).eq.'I'.or.symbol(i  ).eq.'i') then
           if (symbol(i+1) .eq.'N'.or. symbol(i+1).eq.'n') then
            if (symbol(i+2).eq.'T' .or. symbol(i+2).eq.'t') then
             if (symbol(i+3).eq.'E' .or. symbol(i+3).eq.'e') then
              if (symbol(i+4).eq.'G' .or. symbol(i+4).eq.'g') then
               if (symbol(i+5).eq.'E' .or. symbol(i+5).eq.'e') then
                if (symbol(i+6).eq.'R' .or. symbol(i+6).eq.'r') then
                 if (symbol(i-1).eq.' '.and.symbol(i+7).eq.' ') then
                   do j=length,i+7,-1
                    symbol(j+2)=symbol(j)
                   enddo
                   symbol(i+7)='*'
                   symbol(i+8)='4'
                   length=length+2
                   go to 7
                 endif
                endif
               endif
              endif
             endif
            endif
           endif
# endif
          endif
          if (i.lt.length) go to 6
   7    continue
#endif

#ifdef DOUBLE_CONST
!
! Recognize numerical constants of real type in the text of the
! program and convert them into double precision constants, for
! example, 1. --> 1.D0;  .1 --> .1D0;  9.81 --> 9.81D0;
! .5e-8 --> .5D-8;  1.2e+14 --> 1.2D+14 etc.
!
! Algorithm:
!-----------
!    (1) Form list of indices of all dot symbols within the line
!        [dots which occur within quotations ' ... ' are ignored,
!        to do so , variable m is used as masking switch, it turns
!        off when meeting a quotation mark, when entering a region
!        betwee quotes and turns ond back again, when exiting];
!
! For each dot character in the list, starting from the last one,
! and moving from the right to the left: 
!
!    (2) Scan the characters adjacent from the _left_ to the dot in
!        order to find the first non-blank character which is not
!        a digit, (for this purpose digits are ASCII symbols with
!        numbers within the range (48:57) inclusive). During this
!        search also set the logical trigger variable dgt to signal
!        that at least one of the characters encountered during this
!        search is a digit. This search is terminated if 
!        either
!             the non-blank non-digital symbol is found
!        or
!             the 7th position (the starting position for the
!                fixed format fortran statements) has been reached.
!
!    (3) Check weather this symbol is _NOT_ a letter, that is
!        excluding ASCII characters with numbers within the ranges
!        of numbers (65:90) or (97:122) inclusive, _OR_ weather the 
!        7th position limit has been reached (in this case it does
!        not matter what the simbol is). If either condition is true,
!        continue processing, otherwise terminate it.  
!
!        It should be noted that in a legal fortran code a constant
!        expression may be preceded by a mathematical operation
!        symbol, bracket, comma, dot, etc; but _NOT_ with a letter.
!        If it happens, the potential candidate for the numerical
!        constant is actually a format statement descriptor, like
!        E16.8, and not a constant of real type. These are rejected
!        at this moment.
!
!    (4) Once condition (3) holds, scan the characters adjacent from
!        the _right_ to the dot in order to find the first non-blank
!        non-digital character. During this search also set the
!        logical trigger variable dgt, if digit is encountered.
!        This search is limited by the length of the line in the
!        case when no such symbol is found.
!
!    (5) Once this symbol is found, if any, or the search was
!        terminated (in this case index m is equal to lenght+1,
!        so that that symbol is blank, or ! (fortran comment), this
!        symbol may be
!        ether
!              'e', 'E', 'd' or 'D', so that it likely belongs to the
!                 constant itself. In this case scan to the right, to
!                 verify that this symbol is '+', '-' or a digit.
!                 If so, the expression is a floating point real type
!                 constant to be converted into double precision.
!        or 
!              any other character. In this case verify that it is
!                 not a letter. (In a legal fortran code a constant
!                 expression may be followed by a mathematical/
!                 logical operation, bracket, coma, etc, but _NOT_
!                 a letter. If so, the expression is a fixed point 
!                 real type constant to be converted into double
!                 precision. Paste 'D0' immediately after last digit
!                 and move the rest of the line two positions to the
!                 right to make space.
!
! NOTE: if there are more than one real constant within the line of
! the code, the order of processing is from the right to the left.
! This is done because in the case when 'D0' is pasted to the
! constant as in the step (5), the second case, the tail of the
! line is shifted to the right. Processing them in the forward order 
! will also cause shift of the dots to be processed later. So that 
! the indices indx(j+1:ndots) are no longer consistent with the
! actual position of the dots, if the dot indx(j) was found to be  
! a fixed point real type constant as defined in (5), second case.
!
! Limitations:
!----------------------
!   none known to date.
!
        ndots=0                                          ! Step (1)
        m=1
        do i=7,length
          if (symbol(i).eq.quote) m=-m
          if (m.gt.0 .and. symbol(i).eq.'.') then
            ndots=ndots+1
            indx(ndots)=i
          endif
        enddo
        do j=ndots,1,-1 ! <-- REVERSED
          dgt=.false.
          m=indx(j)                                      ! Step (2)
  8        m=m-1
           if (symbol(m).eq.' ') goto 8
           k=ichar(symbol(m))
           if (k.ge.48 .and. k.le.57) then
             dgt=.true.
             if (m.gt.6) goto 8
           endif
          if (m.lt.7 .or. k.lt.65 .or. (k.gt.90. and.    ! Cond.(3)
     &                    k.lt.97) .or. k.gt.122) then
            m=indx(j)                                    ! Step (4)
  9          m=m+1
             if (symbol(m).eq.' ') goto 9
             k=ichar(symbol(m))
             if (k.ge.48 .and. k.le.57) then
               dgt=.true.
               if (m.le.length) goto 9
             endif
            if (dgt) then                                ! Step (5)
              k=ichar(symbol(m))
              if (symbol(m).eq.'e' .or. symbol(m).eq.'E' .or.
     &            symbol(m).eq.'d' .or. symbol(m).eq.'D') then
                i=m
  91             i=i+1
                 if (i.lt.length .and. symbol(i).eq.' ') goto 91
                k=ichar(symbol(i)) 
                if (k.eq.43 .or. k.eq.45 .or.
     &              (k.ge.48 .and. k.le.57)) symbol(m)='D' 
              elseif (k.lt.65 .or. (k.gt.90. and. k.lt.97) .or.
     &                                            k.gt.122) then
  92            m=m-1
                 if (symbol(m).eq.' ') goto 92
                m=m+1
                do i=length,m,-1
                  symbol(i+2)=symbol(i)
                enddo
                symbol(m)='D'
                symbol(m+1)='0'
                length=length+2
              endif
            endif
          endif
        enddo
#endif

#ifdef DOUBLE_CONST_ALT
!
! Recognize numerical constants of real type in the text of the
! program and convert them into double precision constants, for
! example, 1. --> 1.D0;  .1 --> .1D0;  9.81 --> 9.81D0; 
! .5e-8 --> .5D-8;  1.2e+14 --> 1.2D+14 etc. 
!
!  Alternative Algorithm:
!------------------------
!    (1) Form list of indices of all dot symbols within the line
!        [dots which occur within quotations ' ... ' are ignored,
!        to do so , variable m is used as masking switch, it turns
!        off when meeting a quotation mark, when entering a region
!        betwee quotes and turns ond back again, when exiting];
!
!    (2) scan this list in order to recognize fortran logical
!        relation operators, such as .eq., .ne., .or., .gt., .lt.,
!        .ge., .le., .ne., .and., .not.; exclude the dots bounding
!        these expressions from the list of indices. Here setting of
!        indx(j,j+1)=0 signals that the associated dots should be
!        ignored during the subsequent processing; Note that it is
!        not necessary to recognize and exclude dots surrounding
!        logical constants .true. and .false., since there is no
!        possibility in a legal fortran code that there is a digit
!        in a position immediately adjacent to the left or right
!        from  these dots.
!
!    (3) Among remaining dots, exclude those for which none of the
!        characters adjacent on the left or on the right
!        is a digit [i.e. symbol with  ASCII table number between 48
!        to 57 (inclusive)]; It should be noted that (2) and (3) are
!        not interchangeable: (2) must be done before (3), because
!        algorithm (2) relies on parity [opening and closing dot
!        for the logical operator]. Algorithm (3) may destroy the
!        parity, and, as the result, some of the closing dots may
!        not be excluded in (2), if (3) is applied before (2).
!
!    (4) Process the remaining dots: Once at least one of the symbols
!        adjacent to the dot on the left or on the right is a digit
!        [i.e. a symbol with  ASCII table number between 48 to 57
!        (inclusive)], then this dot is associated with 
!        either
!               a real type constant expression.
!        or
!               a specifier in operator format like F8.4 for example.
!
!        To exclude dots associated with format specifiers, search
!        for the first nonblank non-digital character to the left
!        from the dot. If this symbol is 'f', 'F', 'e', 'E', 'd' and
!        'D', the dot should be excluded. [For the sake of simplicity
!        the algorithm is designed to exclude all letter characters,
!        i.e. ASCII symbols within the ranges (65:90) and (97:122)
!        inclusive]. So that things like B12.5 are still interpreted
!        as format specifiers, even thought they are not legal
!        fortran expressions: who cares, they should not appear
!        anyway in a correct fortran program. 
! 
!          If the dot is not excluded as a format specifier, scan
!          the symbols to the right from the dot in order to find 
!          the first non-blank non-digital symbol. This symbol may
!          be ether
!             'e' or 'E', so that it needs to be converted into 'D';
!          or
!             'd' or 'D', so that the constant is already double
!                         precision and no action is needed (it is
!                         converted to the upper case 'D', however,
!                         for stylistic reasons);.
!          or
!               any other symbol, which is not a digit. In this case
!                         'D0' is placed just after it, while the
!          rest of the line is moved two positions to the right, and
!          the length of the line in increased by two every time when
!          the expression 'D0' is being placed.
!
!        Finally, the alternative part, which prints the error
!        message is placed here just to test bullet proofness of the
!        algorithm: in principle, it should never happen, since all
!        possible possibilities are _positively_ identified in the
!        steps presented above.
!
! Limitations:
!-------------
! i.  This algorithm implies that all fortran logical relation
!     operators are written in dense mode, that is no blank spaces 
!     are allowed within fortran logical operations, e.g.,
!     .a n d. is not an acceptable format for .and.;  
!
! ii. Real type numerical constant may be written in nondense format,
!     for example  3. 141 5926 will be recognized and interpreted
!     correctly (that is converted into 3. 141 5926D0), however, at
!     least one digit should be adjacent (without leaving blank space
!     between) to the decimal point, e.g., 3 . 1415926 will not be
!     recognized as a real type constant.
!
        ndots=0                                          ! Step (1)
        m=1
        do i=7,length
          if (symbol(i).eq.quote) m=-m
          if (m.gt.0 .and. symbol(i).eq.'.') then
            ndots=ndots+1
            indx(ndots)=i
          endif
        enddo
        do j=1,ndots                                     ! Step (2)
         i=indx(j)
          if (i.gt.0) then
            if (symbol(i+3).eq.'.') then
              if (symbol(i+2).eq.'r' .or. symbol(i+2).eq.'R') then
                if (symbol(i+1).eq.'o' .or. symbol(i+1).eq.'O') then
                  indx(j)=0
                  indx(j+1)=0
                endif
              elseif(symbol(i+2).eq.'q' .or. symbol(i+2).eq.'Q') then
                if ( symbol(i+1).eq.'e' .or. symbol(i+1).eq.'E') then
                  indx(j)=0
                  indx(j+1)=0
                endif
              elseif(symbol(i+2).eq.'t' .or. symbol(i+2).eq.'T') then
                if ( symbol(i+1).eq.'g' .or. symbol(i+1).eq.'G' .or.
     &               symbol(i+1).eq.'l' .or. symbol(i+1).eq.'L') then
                  indx(j)=0
                  indx(j+1)=0
                endif
              elseif(symbol(i+2).eq.'e' .or. symbol(i+2).eq.'E') then
                if ( symbol(i+1).eq.'g' .or. symbol(i+1).eq.'G' .or.
     &               symbol(i+1).eq.'l' .or. symbol(i+1).eq.'L' .or.
     &               symbol(i+1).eq.'n' .or. symbol(i+1).eq.'N') then
                  indx(j)=0
                  indx(j+1)=0
                endif
              endif
            elseif (symbol(i+4).eq.'.') then 
              if (symbol(i+1).eq.'a' .or. symbol(i+1).eq.'A') then
                if (symbol(i+2).eq.'n' .or.symbol(i+2).eq.'N') then
                  if (symbol(i+3).eq.'d'.or.symbol(i+3).eq.'D') then
                    indx(j)=0
                    indx(j+1)=0
                  endif
                endif
              endif
              if (symbol(i+1).eq.'n' .or. symbol(i+1).eq.'N') then
                if (symbol(i+2).eq.'o'.or. symbol(i+2).eq.'O') then
                  if (symbol(i+3).eq.'t'.or.symbol(i+3).eq.'T') then
                    indx(j)=0
                    indx(j+1)=0
                  endif
                endif
              endif
            endif
          endif
        enddo
!-
        do j=1,ndots                                     ! Step (3)
         i=indx(j)
          if (i.gt.0) then
            m=ichar(symbol(i-1))
            k=ichar(symbol(i+1))
            if ((m.lt.48 .or. m.gt.57) .and.
     &          (k.lt.48 .or. k.gt.57)) then
              indx(j)=0
            endif
          endif
        enddo
!-
        do j=ndots,1,-1   !<-- REVERSED !!!              ! Step (4)
          if (indx(j).gt.0) then
            m=ichar(symbol(i-1))
            k=ichar(symbol(i+1))
            if ((m.ge.48 .and. m.le.57) .or.
     &          (k.ge.48 .and. k.le.57)) then
              m=indx(j)
  8            m=m-1
                if (symbol(m).eq.' ') go to 8 
               k=ichar(symbol(m))
               if (k.ge.48 .and. k.le.57) then
                 if (m.gt.7) goto 8
               endif
              if (k.lt.65 .or. (k.gt.90. and. k.lt.97) .or.
     &                                        k.gt.122) then
                m=indx(j)
  9              m=m+1
                  if (symbol(m).eq.' ') go to 9
                 k=ichar(symbol(m))
                if (k.ge.48 .and. k.le.57) then
                   if (m.le.length)  goto 9
                endif
                if (symbol(m).eq.'e' .or. symbol(m).eq.'E') then
                  symbol(m)='D'
                elseif (symbol(m).eq.'d' .or. symbol(m).eq.'D') then
                  symbol(m)='D'
                else
  91              m=m-1
                   if (symbol(m).eq.' ') goto 91
                  m=m+1
                  do k=length,m,-1
                    symbol(k+2)=symbol(k)
                  enddo
                  symbol(m)='D'
                  symbol(m+1)='0'
                  length=length+2
                endif
              endif
            else
              write(iout,*)'MPC ERROR: cannot recognize expression'
              write(iout,*)'  associated with (.) in the next line.'
            endif
          endif 
        enddo
#endif
#ifdef ZIG_ZAG
!
! Recognize and rearrange parallel loops.
!---------------------------------------------
! If a parallel directive is present, loop over subdomain tiles,
! that is
!          do tile=0,NSUB_X*NSUB_E-1
!
! is converted into two nested loops. The outer is loop over parallel
! threads (to be parallelized), and the inner is over tiles to be
! processed by the particular thread:
!
!          do trd=0,numthreads-1
!            do tile=forward_sweep
! or 
!          do trd=0,numthreads-1
!            do tile=backward_sweep
!
! in such a way the inner loops in the subsequent parallel regions
! are reversed, so that each thread "zig-zags" across the tiles  
! it is processing.
!
! Compared to the straightforward sequence, this measure eliminates 
! secondary cache misses, because after passing a synchronization
! point, each thread proceeds with the same tile it was processing
! just before the synchronization point. It also reduces the
! probability of mutual cache_line invalidation (by factor of two)
! in a multiprocessor machine, if multiprocessing is allowed for the
! subdomains adjacent in the XI direction. This is because after a
! subdomain has been processed by a processor and a synchronization
! point has been reached, all cache_lines which are going across the
! subdivision partitioning are coherent with the cache of _that_
! processor (and invalid with respect to the one working on the
! adjacent subdomain). And it is _that_ processor (and not the
! adjacent one), who proceeds with this subdomain, and therefore
! enjoys all its cache_lines coherent with its cache at this moment.
!
         l1=0
         l2=0
         i=0
 10       i=i+1
           if (symbol(i  ).eq.'L' .or. symbol(i  ).eq.'l') then
            if (symbol(i+1).eq.'O' .or. symbol(i+1).eq.'o') then
             if (symbol(i+2).eq.'C' .or. symbol(i+2).eq.'c') then
              if (symbol(i+3).eq.'A' .or. symbol(i+3).eq.'a') then
               if (symbol(i+4).eq.'L' .or. symbol(i+4).eq.'l') then
                j=i+4
 11              j=j+1
                  if (symbol(j).eq.'(') then
                    l1=j+1
                    k=j
 12                  k=k+1
                      if (symbol(k).eq.')') then
                        l2=k-1
                        go to 13
                      endif
                     if (k.lt.length) go to 12
                  endif
                 if (j.lt.length) go to 11
               endif
              endif
             endif
            endif
           endif
          if (i.lt.length) go to 10
 13      continue
         if (l2-l1.gt.0) then
           do j=1,1+l2-l1
             local_list(j+10)=symbol(j+l1-1)
           enddo
           s1=0
           s2=0
           i=0
 14         i=i+1
             if (symbol(i  ).eq.'S'  .or. symbol(i  ).eq.'s') then
              if (symbol(i+1).eq.'H'  .or. symbol(i+1).eq.'h') then
               if (symbol(i+2).eq.'A'  .or. symbol(i+2).eq.'a') then
                if (symbol(i+3).eq.'R'  .or. symbol(i+3).eq.'r') then
                 if (symbol(i+4).eq.'E' .or. symbol(i+4).eq.'e') then
                  j=i+4
 15                j=j+1 
                    if (symbol(j).eq.'(') then
                      s1=j+1
                      k=j
 16                    k=k+1
                        if (symbol(k).eq.')') then
                          s2=k-1
                          go to 17
                        endif
                       if (k.lt.length) go to 16
                    endif
                   if (j.lt.length) go to 15
                 endif
                endif
               endif
              endif
             endif
            if (i.lt.length) go to 14
 17        continue

           if (s2-s1.gt.0) then
             do i=1,1+s2-s1
               share_list(i+12)=symbol(i+s1-1)
             enddo
           else 
             symbol(l2+2)=','
             symbol(l2+3)=' '
             symbol(l2+4)='S'
             symbol(l2+5)='H'
             symbol(l2+6)='A'
             symbol(l2+7)='R'
             symbol(l2+8)='E'
             symbol(l2+9)='('
             s1=l2+10
             s2=s1-3
           endif
!
! Write out the modified compiler directive: combine the parts of the
! original directive, updated shared and local lists into a single
! string of symbols (which is obviously longer than the original
! one), check its length and if it exceeds the width of 72 symbols,
! split it into two. For the estetic purposes the split is done along
! a natural divider symbol, such as a intentionally placed blank
! character or a comma.
!
           do i=s1-1,l2+1,-1     !<-- inverse
             symbol(i+10)=symbol(i)
           enddo
           do i=1,l2-l1+11
             symbol(i+l1-1)=local_list(i)
           enddo
           do i=1,1+s2-s1+12
             symbol(i+s1+9)=share_list(i)
           enddo
           length=s2+12+11
           symbol(length) = ')'
           if (length.le.72) then
             write(iout,'(72A1)') (symbol(i),i=1,length)
           else
             k=73
  171         k=k-1
             if (k.gt.0 .and. symbol(k).ne.' ') goto 171
             ks1=k
             k=73
  172         k=k-1
             if (k.gt.0 .and. symbol(k).ne.',') goto 172
             ks2=k
             k=0
             if (ks1.gt.54) then
               k=ks1
               goto 173
             endif
             if (ks2.gt.6) then
               k=ks2
               goto 173
             endif
             if (k.eq.0) write(iout,*) 'MPC ERROR: cannot',
     &                                 ' split directive line.'
  173        write(iout,'(72A1)') (symbol(i),i=1,k)
# ifdef SGI
             m=3
# else
             m=6
# endif
             symbol(m)='&'
             do i=m+1,k
               symbol(i)=' '
             enddo
             m=length-k
             do i=k+1,length
               symbol(i-m)=symbol(i)
             enddo
             write(iout,'(72A1)') (symbol(i),i=1,k)
           endif
           par_region=.TRUE.
           go to 1
         endif
!
! Once a parallel region is detected and the compiler directive above
! was modified, transform the parallel loop over subdomains (tiles)
! into a set of nested loops over threads (outer loop) and subdomains
! within the work zone of each thread (inner loop), in such a way
! that the sequence of sweep of the inner loop is always reversed
! with respect to the sweep in the previous parallel region (zig-zag
! sequence).  
!
         if (par_region) then
           i=6
  18        i=i+1
             if (symbol(i  ).eq.'t') then
              if (symbol(i+1).eq.'i') then
               if (symbol(i+2).eq.'l') then
                if (symbol(i+3).eq.'e') then
                 if (symbol(i+4).eq.'=') then
                   write(iout,'(7x,A21)')
     &                   'do trd=0,numthreads-1'
                   write(iout,'(8x,A29)')
     &                   'subs=NSUB_X*NSUB_E/numthreads'
                   dir_switch=.NOT.dir_switch
                   if (dir_switch) then
                     write(iout,'(9x,A34)')
     &                   'do tile=subs*trd,subs*(trd+1)-1,+1'
                   else
                     write(iout,'(9x,A34)')
     &                   'do tile=subs*(trd+1)-1,subs*trd,-1'
                   endif
                   go to 1
                 endif
                endif
               endif
              endif
             endif
            if (i.lt.length) goto 18 

           i=6
  19        i=i+1
             if (symbol(i   ).eq.'e') then
              if (symbol(i+1 ).eq.'n') then
               if (symbol(i+2 ).eq.'d') then
                if (symbol(i+3 ).eq.'d') then
                 if (symbol(i+4 ).eq.'o') then
                    write(iout,'(9x,A5)') 'enddo'
                    write(iout,'(7x,A5)') 'enddo'
                    par_region=.FALSE.
                   goto 1
                 endif
                endif
               endif
              endif
             endif
            if (i.lt.length) goto 19
         endif
#endif
!
! Write the modified line of code into the output file.
!---------------------------------------------------------
! Because its length after modification may exceed the standard of
! 72 symbols, it may be necessary to split the line into two. In this
! case several attempts are made to find a good (from estetic point
! of view) splitting point. First attempt is made by searching for
! the first blank symbol starting from position 72 and moving to the
! left; if such is not found, then the search starts from the
! beginning and attempt is made to split along a comma; if this is
! not successful, an assignment operator is being searched; then
! a mathematical operation. 
!
        if (length.gt.0 .and. length.le.72) then
           write(iout,'(72A1)') (symbol(i),i=1,length)
        elseif (length.gt.72) then
!
! Find appropriate places in the line where it may be split into
! two. This is just a purely estetic matter: line split may be
! done if there is a natural divider in the line, such as a blank
! character in the middle; comma or a symbol of mathematical
! operation.
!
          k=73
  20       k=k-1
          if (k.gt.6 .and. symbol(k).ne.' ') goto 20
          if (length-k.lt.67) then
            ks1=k
          else
            ks1=0
          endif

          k=73
  21       k=k-1
          if (k.gt.6 .and. symbol(k).ne.',') goto 21
          if (length-k.lt.67) then
            ks2=k
          else
            ks2=0
          endif

          k=73
  22       k=k-1
          if (k.gt.6 .and. symbol(k).ne.'=') goto 22
          if (length-k.lt.67) then
            ks3=k
          else
            ks3=0
          endif

          k=73
  23       k=k-1
          if (k.gt.6 .and. symbol(k).ne.'/') goto 23
          if (length-k.lt.67) then
            ks4=k
          else
            ks4=0
          endif

          k=73
  24       k=k-1
          if (k.gt.6 .and. symbol(k).ne.'+'
     &               .and. symbol(k).ne.'-'
     &               .and. symbol(k).ne.'*') goto 24
          if (length-k.lt.67) then
            ks5=k
          else
            ks5=0
          endif
!
! Making final desision about the split: there is nothing special
! here, it is just a matter of estetics to decide which of the
! possible breaking points (if more than one are available) is the
! most appropriate; the logical sequence below is designed as
! hierarchy of preferences. 
!
          k=0

          if (ks1.gt.34) then
            k=ks1
            goto 25
          endif

          if (ks4.gt.6) then
            k=ks4-1
            goto 25
          endif

          if (ks2.gt.54) then
            k=ks2
            goto 25
          endif

          if (ks3.gt.60) then
            k=ks3
            goto 25
          endif

          if (ks5.gt.6) then
            k=ks5-1
            goto 25
          endif

          if (k.eq.0) write(iout,*) 'MPC ERROR: Cannot split line'
!
! Write out the line. First write the first part of the line; then
! create a continuation line resetting the statring symbols to blank,
! and them moving the tail to the left in such a way that its end
! will be in the position 72. Also put a continuation character into
! position 6. 
!
  25      write(iout,'(72A1)') (symbol(i),i=1,k) 

          do i=1,k
            symbol(i)=' '
          enddo
          symbol(6)='&'

          m=length-72
          do i=k+1,length
            symbol(i-m)=symbol(i)
          enddo

          write(iout,'(72A1)') (symbol(i),i=1,72)
        endif
       if (.not.end_of_file) go to 1
      close(iin)
      close(iout)
      stop
      end
       
