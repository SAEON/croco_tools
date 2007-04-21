!
! $Id: dynparam.h,v 1.2 2003/12/17 13:56:01 pmarches Exp $
!
! Dimensions of Physical Grid and array dimensions: 
! =========== == ======== ==== === ===== ============
! LLm,MMm  Number of the internal points of the PHYSICAL grid.
!          in the XI- and ETA-directions [physical side boundary
!          points and peroodic ghost points (if any) are excluded].
!
! Lm,Mm    Number of the internal points [see above] of array
!          covering a Message Passing subdomain. In the case when
!          no Message Passing partitioning is used, these two are
!          the same as LLm,MMm. 
!
      LLm=LLm0;  MMm=MMm0      ! LLm0,MMm0 defined in param.h

