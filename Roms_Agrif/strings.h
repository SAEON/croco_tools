!
! $Id: strings.h,v 1.3 2004/11/30 08:21:20 pmarches Exp $
!
! A long character string to hold activated cpp-switches.
! Basically it is used to keep track of cpp-switches by placing
! them together and writing into history file.
!                                    !
      integer max_opt_size           ! NOTE: Parameter max_opt_size
      parameter (max_opt_size=2048)  ! must be equal to the length
      character*2048 Coptions,srcs   ! of character string. 
      common /strings/ Coptions,srcs !
