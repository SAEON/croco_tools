!
! $Id: lenstr.F,v 1.3 2004/03/26 15:26:07 pmarches Exp $
!
      integer function lenstr (string)
!
! Returns the character position of the last nonblank character in
! input string after removing the leading blank characters, if any.
! Algorithm: Find length of input character string using intrinsic
! function "len", then find positions of  the first "is" and the
! last "ie" nonblank characters. Define the length of the string
! based on these bounds. In the case of there are any leading blank
! characters, shift the whole string to the beginning.
!
      implicit none
      integer is, ie
      character*(*) string
      ie=len(string)
   1   if (string(ie:ie).eq.' ') then
         ie=ie-1
         if (ie.gt.0) goto 1
       endif
      is=0
   2   is=is+1
       if (string(is:is).eq.' ' .and. is.lt.ie) goto 2
      lenstr=ie-is+1
      if (is.gt.1) string=string(is:ie)
      return
      end
