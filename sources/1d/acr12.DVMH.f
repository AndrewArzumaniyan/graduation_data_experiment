      program acr12

! DVMH declarations 
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_finish,dvm
     &h_init2,ftcntr,eiof,biof,lexit,clfdvm
      integer*8  dvmh_string,tstio,getach,getad,getaf,getal,getai
      integer*8  dvm000(13)
      character*8 ::  filenm001='acr12.f'//char (0)
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      character  ch000m(0:64)
      logical  l0000m(0:64)
      double precision  d0000m(0:64)
      real  r0000m(0:64)
      integer  i0000m(0:64)
      equivalence (l0000m,d0000m,r0000m,i0000m)
      common /mem000/i0000m
      save 
      call dvmh_line(7_8,dvmh_string (filenm001))
      dvm000(1) = dvm0c6
      dvm000(2) = getai (dvm000(1))
      dvm000(3) = getai (i0000m(0))
      dvm000(4) = getal (l0000m(0))
      dvm000(5) = getaf (r0000m(0))
      dvm000(6) = getad (d0000m(0))
      dvm000(7) = getach (ch000m(0))
      dvm000(8) = getai (dvm000(2))
      dvm000(9) = getai (i0000m(1))
      dvm000(10) = getal (l0000m(1))
      dvm000(11) = getaf (r0000m(1))
      dvm000(12) = getad (d0000m(1))
      dvm000(13) = getach (ch000m(1))
      i0000m(0) = 8
      i0000m(1) = 4
      i0000m(2) = 4
      i0000m(3) = 4
      i0000m(4) = 8
      i0000m(5) = 1
      i0000m(10) = 2
      i0000m(11) = 1
      i0000m(12) = 1
      i0000m(13) = 3
      i0000m(14) = 4
      i0000m(15) = 5
      call ftcntr(6,dvm000(2),dvm000(8),i0000m(0),i0000m(10))
      dvm000(1) = 1

!$    dvm000(1) = dvm000(1) + 8 
      call dvmh_init2(dvm000(1))
      call dvmh_scope_start()
      call dvmh_line(7_8,dvmh_string (filenm001))
      call biof()

!    TESTING OF THE ACROSS CLAUSE'.       
!    DISTRIBUTED ARRAY A(N) IS TO HAVE DIFFERENT 
!    FLOW-DEP-LENGTH ON BOTH SIDES 
      if (tstio () .ne. 0)  print *, '===START OF ACR12=================
     &======='
      call eiof()

! --------------------------------------------------
      call acr1201()

! --------------------------------------------------
      call acr1202()

! --------------------------------------------------
      call acr1203()

! -------------------------------------------------
      call acr1204()

! -------------------------------------------------
      call acr1205()

! -------------------------------------------------
      call acr1206()

! --------------------------------------------------
      call acr1207()

! --------------------------------------------------
      call acr1208()

! --------------------------------------------------
      call acr1209()

! -------------------------------------------------
      call acr1210()
      call dvmh_line(31_8,dvmh_string (filenm001))
      call biof()

! -------------------------------------------------
!     
      if (tstio () .ne. 0)  print *, '=== END OF ACR12 =================
     &======== '
      call eiof()
      call dvmh_line(32_8,dvmh_string (filenm001))
      call dvmh_scope_end()
      call dvmh_line(32_8,dvmh_string (filenm001))
      call clfdvm()
      call dvmh_finish()
      call lexit(dvm0c0)
      end


! ---------------------------------------------ACR1201
      subroutine acr1201 ()
      integer ,parameter:: n = 8,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_58_host,loop_acr12_64_host,loop_acr12_64_host_
     &1,loop_acr12_64_host_0,loop_acr12_69_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,dvmh_shadow_renew,region_set_name_array,region_register_ar
     &ray,region_execute_on_targets,region_end,crtraf,across,insshd,endp
     &l,mappl,waitrd,strtrd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(36)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1201dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(43_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1201'
      call dvmh_line(44_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 8
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 1
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(9) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm0
     &c0,dvm0c0,dvm000(8),dvm000(8))
      dvm000(10) = 1
      dvm000(11) = 1
      dvm000(12) = 0
      dvm000(13) = align (a(1),dvm000(6),dvm000(10),dvm000(11),dvm000(12
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 2,n - 1
            c(i) = c(i - 1) + c(i + 1)
         enddo  
         call dvmh_line(54_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(55_8,dvmh_string (filenm001))

! Start region (line 55)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(57_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 58)
         call dvmh_line(58_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_58_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(60_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(63_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 1
         dvm000(22) = 0
         dvm000(23) = 3
         dvm000(15) = crtshg (dvm0c0)
         dvm000(18) = crtshg (dvm0c0)
         call insshd(dvm000(15),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(18),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = 0
         dvm000(25) = 1
         dvm000(26) = 5
         dvm000(17) = crtshg (dvm0c0)
         dvm000(20) = crtshg (dvm0c0)
         call insshd(dvm000(17),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         call insshd(dvm000(20),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         dvm000(27) = getai (i)
         dvm000(28) = 1
         dvm000(29) = 2
         dvm000(30) = n - 1
         dvm000(31) = 1
         dvm000(32) = 1
         dvm000(33) = 1
         dvm000(34) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(32),dvm000(33),dvm000(34),dvm
     &000(27),dvm000(28),dvm000(29),dvm000(30),dvm000(31),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call dvmh_shadow_renew(dvm000(17))
         call across(dvm0c0,dvm000(17),dvm000(15),pipe00)

! Parallel loop (line 64)
         call dvmh_line(64_8,dvmh_string (filenm001))
         dvm000(35) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(35),dvm000(20),dvm000(18))
         dvm000(36) = 0

!$    dvm000(36) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(35),DEVICE_TYPE_HOST,dvm000(3
     &6),loop_acr12_64_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(35))
         call dvmh_line(66_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(68_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 2
         dvm000(19) = n - 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 69)
         call dvmh_line(69_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_69_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7)),
     &i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(73_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 55)
         call dvmh_line(74_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(75_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(83_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(85_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! ---------------------------------------------ACR1202     
      subroutine acr1202 ()
      integer ,parameter:: n = 16,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_111_host,loop_acr12_116_host,loop_acr12_116_ho
     &st_1,loop_acr12_116_host_0,loop_acr12_121_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,dvmh_shadow_renew,region_set_name_array,region_register_ar
     &ray,region_execute_on_targets,region_end,crtraf,across,insshd,endp
     &l,mappl,waitrd,strtrd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(33)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1202dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(96_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1202'
      call dvmh_line(97_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 16
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 1
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(9) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm0
     &c0,dvm0c0,dvm000(8),dvm000(8))
      dvm000(10) = 1
      dvm000(11) = 1
      dvm000(12) = 0
      dvm000(13) = align (a(1),dvm000(6),dvm000(10),dvm000(11),dvm000(12
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 1,n - 1
            c(i) = c(i) + c(i + 1)
         enddo  
         call dvmh_line(107_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(108_8,dvmh_string (filenm001))

! Start region (line 108)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(110_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 111)
         call dvmh_line(111_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_111_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(113_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(115_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 0
         dvm000(22) = 1
         dvm000(23) = 5
         dvm000(17) = crtshg (dvm0c0)
         dvm000(20) = crtshg (dvm0c0)
         call insshd(dvm000(17),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(20),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = getai (i)
         dvm000(25) = 1
         dvm000(26) = 1
         dvm000(27) = n - 1
         dvm000(28) = 1
         dvm000(29) = 1
         dvm000(30) = 1
         dvm000(31) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(29),dvm000(30),dvm000(31),dvm
     &000(24),dvm000(25),dvm000(26),dvm000(27),dvm000(28),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call dvmh_shadow_renew(dvm000(17))
         call across(dvm0c1,dvm000(17),dvm0c0,pipe00)

! Parallel loop (line 116)
         call dvmh_line(116_8,dvmh_string (filenm001))
         dvm000(32) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(32),dvm000(20),dvm0c0)
         dvm000(33) = 0

!$    dvm000(33) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(32),DEVICE_TYPE_HOST,dvm000(3
     &3),loop_acr12_116_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(32))
         call dvmh_line(118_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(120_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 2
         dvm000(19) = n - 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 121)
         call dvmh_line(121_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_121_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(125_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 108)
         call dvmh_line(126_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(127_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(135_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(137_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -----------------------------------------ACR1203      
      subroutine acr1203 ()
      integer ,parameter:: n = 16,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_163_host,loop_acr12_168_host,loop_acr12_168_ho
     &st_1,loop_acr12_168_host_0,loop_acr12_173_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,region_set_name_array,region_register_array,region_execute
     &_on_targets,region_end,crtraf,across,insshd,endpl,mappl,waitrd,str
     &trd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(33)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1203dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(148_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1203'
      call dvmh_line(149_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 16
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 1
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(9) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm0
     &c0,dvm0c0,dvm000(8),dvm000(8))
      dvm000(10) = 1
      dvm000(11) = 1
      dvm000(12) = 0
      dvm000(13) = align (a(1),dvm000(6),dvm000(10),dvm000(11),dvm000(12
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 2,n
            c(i) = c(i) + c(i - 1)
         enddo  
         call dvmh_line(159_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(160_8,dvmh_string (filenm001))

! Start region (line 160)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(162_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 163)
         call dvmh_line(163_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_163_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(165_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(167_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 1
         dvm000(22) = 0
         dvm000(23) = 3
         dvm000(15) = crtshg (dvm0c0)
         dvm000(18) = crtshg (dvm0c0)
         call insshd(dvm000(15),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(18),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = getai (i)
         dvm000(25) = 1
         dvm000(26) = 2
         dvm000(27) = n
         dvm000(28) = 1
         dvm000(29) = 1
         dvm000(30) = 1
         dvm000(31) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(29),dvm000(30),dvm000(31),dvm
     &000(24),dvm000(25),dvm000(26),dvm000(27),dvm000(28),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call across(dvm0c0,dvm0c0,dvm000(15),pipe00)

! Parallel loop (line 168)
         call dvmh_line(168_8,dvmh_string (filenm001))
         dvm000(32) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(32),dvm0c0,dvm000(18))
         dvm000(33) = 0

!$    dvm000(33) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(32),DEVICE_TYPE_HOST,dvm000(3
     &3),loop_acr12_168_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(32))
         call dvmh_line(170_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(172_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 2
         dvm000(19) = n - 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 173)
         call dvmh_line(173_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_173_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(177_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 160)
         call dvmh_line(178_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(179_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(187_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(189_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -------------------------------------------ACR1204   
      subroutine acr1204 ()
      integer ,parameter:: n = 16,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_217_host,loop_acr12_222_host,loop_acr12_222_ho
     &st_1,loop_acr12_222_host_0,loop_acr12_227_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,dvmh_shadow_renew,region_set_name_array,region_register_ar
     &ray,region_execute_on_targets,region_end,crtraf,across,insshd,endp
     &l,mappl,waitrd,strtrd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(36)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1204dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(202_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1204'
      call dvmh_line(203_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 16
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 2
      dvm000(9) = 2
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(10) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm
     &0c0,dvm0c0,dvm000(8),dvm000(9))
      dvm000(11) = 1
      dvm000(12) = 1
      dvm000(13) = 0
      dvm000(14) = align (a(1),dvm000(6),dvm000(11),dvm000(12),dvm000(13
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 3,n - 2
            c(i) = c(i - 1) + c(i + 1) + c(i + 2) + c(i - 2)
         enddo  
         call dvmh_line(213_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(214_8,dvmh_string (filenm001))

! Start region (line 214)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(216_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 217)
         call dvmh_line(217_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_217_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(219_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(221_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 2
         dvm000(22) = 0
         dvm000(23) = 3
         dvm000(15) = crtshg (dvm0c0)
         dvm000(18) = crtshg (dvm0c0)
         call insshd(dvm000(15),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(18),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = 0
         dvm000(25) = 2
         dvm000(26) = 5
         dvm000(17) = crtshg (dvm0c0)
         dvm000(20) = crtshg (dvm0c0)
         call insshd(dvm000(17),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         call insshd(dvm000(20),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         dvm000(27) = getai (i)
         dvm000(28) = 1
         dvm000(29) = 3
         dvm000(30) = n - 2
         dvm000(31) = 1
         dvm000(32) = 1
         dvm000(33) = 1
         dvm000(34) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(32),dvm000(33),dvm000(34),dvm
     &000(27),dvm000(28),dvm000(29),dvm000(30),dvm000(31),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call dvmh_shadow_renew(dvm000(17))
         call across(dvm0c0,dvm000(17),dvm000(15),pipe00)

! Parallel loop (line 222)
         call dvmh_line(222_8,dvmh_string (filenm001))
         dvm000(35) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(35),dvm000(20),dvm000(18))
         dvm000(36) = 0

!$    dvm000(36) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(35),DEVICE_TYPE_HOST,dvm000(3
     &6),loop_acr12_222_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(35))
         call dvmh_line(224_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(226_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 3
         dvm000(19) = n - 2
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 227)
         call dvmh_line(227_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_227_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(231_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 214)
         call dvmh_line(232_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(233_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(241_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(243_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -------------------------------------------ACR1205   
      subroutine acr1205 ()
      integer ,parameter:: n = 16,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_271_host,loop_acr12_276_host,loop_acr12_276_ho
     &st_1,loop_acr12_276_host_0,loop_acr12_281_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,dvmh_shadow_renew,region_set_name_array,region_register_ar
     &ray,region_execute_on_targets,region_end,crtraf,across,insshd,endp
     &l,mappl,waitrd,strtrd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(33)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1205dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(256_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1205'
      call dvmh_line(257_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 16
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 2
      dvm000(9) = 2
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(10) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm
     &0c0,dvm0c0,dvm000(8),dvm000(9))
      dvm000(11) = 1
      dvm000(12) = 1
      dvm000(13) = 0
      dvm000(14) = align (a(1),dvm000(6),dvm000(11),dvm000(12),dvm000(13
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 2,n - 2
            c(i) = c(i + 1) + c(i + 2)
         enddo  
         call dvmh_line(267_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(268_8,dvmh_string (filenm001))

! Start region (line 268)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(270_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 271)
         call dvmh_line(271_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_271_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(273_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(275_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 0
         dvm000(22) = 2
         dvm000(23) = 5
         dvm000(17) = crtshg (dvm0c0)
         dvm000(20) = crtshg (dvm0c0)
         call insshd(dvm000(17),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(20),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = getai (i)
         dvm000(25) = 1
         dvm000(26) = 2
         dvm000(27) = n - 2
         dvm000(28) = 1
         dvm000(29) = 1
         dvm000(30) = 1
         dvm000(31) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(29),dvm000(30),dvm000(31),dvm
     &000(24),dvm000(25),dvm000(26),dvm000(27),dvm000(28),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call dvmh_shadow_renew(dvm000(17))
         call across(dvm0c1,dvm000(17),dvm0c0,pipe00)

! Parallel loop (line 276)
         call dvmh_line(276_8,dvmh_string (filenm001))
         dvm000(32) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(32),dvm000(20),dvm0c0)
         dvm000(33) = 0

!$    dvm000(33) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(32),DEVICE_TYPE_HOST,dvm000(3
     &3),loop_acr12_276_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(32))
         call dvmh_line(278_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(280_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 2
         dvm000(19) = n - 2
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 281)
         call dvmh_line(281_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_281_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(285_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 268)
         call dvmh_line(286_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(287_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(295_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(297_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -------------------------------------------ACR1206   
      subroutine acr1206 ()
      integer ,parameter:: n = 16,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_325_host,loop_acr12_330_host,loop_acr12_330_ho
     &st_1,loop_acr12_330_host_0,loop_acr12_335_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,region_set_name_array,region_register_array,region_execute
     &_on_targets,region_end,crtraf,across,insshd,endpl,mappl,waitrd,str
     &trd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(33)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1206dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(310_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1206'
      call dvmh_line(311_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 16
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 2
      dvm000(9) = 2
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(10) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm
     &0c0,dvm0c0,dvm000(8),dvm000(9))
      dvm000(11) = 1
      dvm000(12) = 1
      dvm000(13) = 0
      dvm000(14) = align (a(1),dvm000(6),dvm000(11),dvm000(12),dvm000(13
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 3,n
            c(i) = c(i - 1) + c(i - 2)
         enddo  
         call dvmh_line(321_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(322_8,dvmh_string (filenm001))

! Start region (line 322)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(324_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 325)
         call dvmh_line(325_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_325_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(327_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(329_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 2
         dvm000(22) = 0
         dvm000(23) = 3
         dvm000(15) = crtshg (dvm0c0)
         dvm000(18) = crtshg (dvm0c0)
         call insshd(dvm000(15),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(18),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = getai (i)
         dvm000(25) = 1
         dvm000(26) = 3
         dvm000(27) = n
         dvm000(28) = 1
         dvm000(29) = 1
         dvm000(30) = 1
         dvm000(31) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(29),dvm000(30),dvm000(31),dvm
     &000(24),dvm000(25),dvm000(26),dvm000(27),dvm000(28),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call across(dvm0c0,dvm0c0,dvm000(15),pipe00)

! Parallel loop (line 330)
         call dvmh_line(330_8,dvmh_string (filenm001))
         dvm000(32) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(32),dvm0c0,dvm000(18))
         dvm000(33) = 0

!$    dvm000(33) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(32),DEVICE_TYPE_HOST,dvm000(3
     &3),loop_acr12_330_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(32))
         call dvmh_line(332_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(334_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 3
         dvm000(19) = n
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 335)
         call dvmh_line(335_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_335_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(339_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 322)
         call dvmh_line(340_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(341_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(349_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(350_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -------------------------------------------ACR1207   
      subroutine acr1207 ()
      integer ,parameter:: n = 16,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_379_host,loop_acr12_384_host,loop_acr12_384_ho
     &st_1,loop_acr12_384_host_0,loop_acr12_389_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,dvmh_shadow_renew,region_set_name_array,region_register_ar
     &ray,region_execute_on_targets,region_end,crtraf,across,insshd,endp
     &l,mappl,waitrd,strtrd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(36)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1207dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(364_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1207'
      call dvmh_line(365_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 16
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 3
      dvm000(9) = 3
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(10) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm
     &0c0,dvm0c0,dvm000(8),dvm000(9))
      dvm000(11) = 1
      dvm000(12) = 1
      dvm000(13) = 0
      dvm000(14) = align (a(1),dvm000(6),dvm000(11),dvm000(12),dvm000(13
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 4,n - 3
            c(i) = c(i - 1) + c(i + 1) + c(i + 2) + c(i - 2) + c(i - 3) 
     &+ c(i + 3)
         enddo  
         call dvmh_line(375_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(376_8,dvmh_string (filenm001))

! Start region (line 376)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(378_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 379)
         call dvmh_line(379_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_379_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(381_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(383_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 3
         dvm000(22) = 0
         dvm000(23) = 3
         dvm000(15) = crtshg (dvm0c0)
         dvm000(18) = crtshg (dvm0c0)
         call insshd(dvm000(15),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(18),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = 0
         dvm000(25) = 3
         dvm000(26) = 5
         dvm000(17) = crtshg (dvm0c0)
         dvm000(20) = crtshg (dvm0c0)
         call insshd(dvm000(17),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         call insshd(dvm000(20),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         dvm000(27) = getai (i)
         dvm000(28) = 1
         dvm000(29) = 4
         dvm000(30) = n - 3
         dvm000(31) = 1
         dvm000(32) = 1
         dvm000(33) = 1
         dvm000(34) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(32),dvm000(33),dvm000(34),dvm
     &000(27),dvm000(28),dvm000(29),dvm000(30),dvm000(31),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call dvmh_shadow_renew(dvm000(17))
         call across(dvm0c0,dvm000(17),dvm000(15),pipe00)

! Parallel loop (line 384)
         call dvmh_line(384_8,dvmh_string (filenm001))
         dvm000(35) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(35),dvm000(20),dvm000(18))
         dvm000(36) = 0

!$    dvm000(36) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(35),DEVICE_TYPE_HOST,dvm000(3
     &6),loop_acr12_384_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(35))
         call dvmh_line(386_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(388_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 4
         dvm000(19) = n - 3
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 389)
         call dvmh_line(389_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_389_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(393_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 376)
         call dvmh_line(394_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(395_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(403_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(405_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -------------------------------------------ACR1208  
      subroutine acr1208 ()
      integer ,parameter:: n = 24,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_433_host,loop_acr12_438_host,loop_acr12_438_ho
     &st_1,loop_acr12_438_host_0,loop_acr12_443_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,dvmh_shadow_renew,region_set_name_array,region_register_ar
     &ray,region_execute_on_targets,region_end,crtraf,across,insshd,endp
     &l,mappl,waitrd,strtrd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(33)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1208dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(418_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1208'
      call dvmh_line(419_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 24
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 3
      dvm000(9) = 3
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(10) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm
     &0c0,dvm0c0,dvm000(8),dvm000(9))
      dvm000(11) = 1
      dvm000(12) = 1
      dvm000(13) = 0
      dvm000(14) = align (a(1),dvm000(6),dvm000(11),dvm000(12),dvm000(13
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 2,n - 3
            c(i) = c(i + 1) + c(i + 2) + c(i + 3)
         enddo  
         call dvmh_line(429_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(430_8,dvmh_string (filenm001))

! Start region (line 430)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(432_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 433)
         call dvmh_line(433_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_433_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(435_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(437_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 0
         dvm000(22) = 3
         dvm000(23) = 5
         dvm000(17) = crtshg (dvm0c0)
         dvm000(20) = crtshg (dvm0c0)
         call insshd(dvm000(17),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(20),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = getai (i)
         dvm000(25) = 1
         dvm000(26) = 2
         dvm000(27) = n - 3
         dvm000(28) = 1
         dvm000(29) = 1
         dvm000(30) = 1
         dvm000(31) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(29),dvm000(30),dvm000(31),dvm
     &000(24),dvm000(25),dvm000(26),dvm000(27),dvm000(28),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call dvmh_shadow_renew(dvm000(17))
         call across(dvm0c1,dvm000(17),dvm0c0,pipe00)

! Parallel loop (line 438)
         call dvmh_line(438_8,dvmh_string (filenm001))
         dvm000(32) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(32),dvm000(20),dvm0c0)
         dvm000(33) = 0

!$    dvm000(33) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(32),DEVICE_TYPE_HOST,dvm000(3
     &3),loop_acr12_438_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(32))
         call dvmh_line(440_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(442_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 2
         dvm000(19) = n - 3
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 443)
         call dvmh_line(443_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_443_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(447_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 430)
         call dvmh_line(448_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(449_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(457_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(459_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -------------------------------------------ACR1209   
      subroutine acr1209 ()
      integer ,parameter:: n = 24,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_488_host,loop_acr12_493_host,loop_acr12_493_ho
     &st_1,loop_acr12_493_host_0,loop_acr12_498_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,region_set_name_array,region_register_array,region_execute
     &_on_targets,region_end,crtraf,across,insshd,endpl,mappl,waitrd,str
     &trd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(33)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1209dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(472_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1209'
      call dvmh_line(473_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 24
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 3
      dvm000(9) = 3
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(10) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm
     &0c0,dvm0c0,dvm000(8),dvm000(9))
      dvm000(11) = 1
      dvm000(12) = 1
      dvm000(13) = 0
      dvm000(14) = align (a(1),dvm000(6),dvm000(11),dvm000(12),dvm000(13
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 4,n
            c(i) = c(i - 1) + c(i - 2) + c(i - 3)
         enddo  
         call dvmh_line(484_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(485_8,dvmh_string (filenm001))

! Start region (line 485)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(487_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 488)
         call dvmh_line(488_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_488_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(490_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(492_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 3
         dvm000(22) = 0
         dvm000(23) = 3
         dvm000(15) = crtshg (dvm0c0)
         dvm000(18) = crtshg (dvm0c0)
         call insshd(dvm000(15),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(18),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = getai (i)
         dvm000(25) = 1
         dvm000(26) = 4
         dvm000(27) = n
         dvm000(28) = 1
         dvm000(29) = 1
         dvm000(30) = 1
         dvm000(31) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(29),dvm000(30),dvm000(31),dvm
     &000(24),dvm000(25),dvm000(26),dvm000(27),dvm000(28),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call across(dvm0c0,dvm0c0,dvm000(15),pipe00)

! Parallel loop (line 493)
         call dvmh_line(493_8,dvmh_string (filenm001))
         dvm000(32) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(32),dvm0c0,dvm000(18))
         dvm000(33) = 0

!$    dvm000(33) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(32),DEVICE_TYPE_HOST,dvm000(3
     &3),loop_acr12_493_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(32))
         call dvmh_line(495_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(497_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 4
         dvm000(19) = n
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 498)
         call dvmh_line(498_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_498_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(502_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 485)
         call dvmh_line(503_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(504_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(513_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(515_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! --------------------------------------------ACR1210  
      subroutine acr1210 ()
      integer ,parameter:: n = 50,nl = 1000
      character*7  tname
      integer ,allocatable:: c(:)
      integer  nloop

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1
      integer*8 ,parameter:: INTENT_IN = 1,INTENT_INOUT = 3
      external loop_acr12_544_host,loop_acr12_549_host,loop_acr12_549_ho
     &st_1,loop_acr12_549_host_0,loop_acr12_554_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_data_exit,dvmh_data_enter,loop_across,dvmh_actual_variabl
     &e,dvmh_get_actual_variable,loop_insred,loop_perform,loop_register_
     &handler,dvmh_shadow_renew,region_set_name_array,region_register_ar
     &ray,region_execute_on_targets,region_end,crtraf,across,insshd,endp
     &l,mappl,waitrd,strtrd,insred
      integer*8  dvmh_string,loop_create,region_create,getai,crtpl,crtsh
     &g,crtrg,crtrdf,align,crtda,distr,crtamv
      integer*8  dvm000(36)
      integer*8  a(26)
      character*8 ::  filenm001='acr12.f'//char (0)
      double precision  pipe00
      common /acr1210dvm/nloop
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      double precision  d0000m(0:64)
      integer  i0000m(0:64)
      equivalence (d0000m,i0000m)
      common /mem000/i0000m
      call dvmh_line(529_8,dvmh_string (filenm001))
      call dvmh_scope_start()
      tname = 'ACR1210'
      call dvmh_line(530_8,dvmh_string (filenm001))
      dvm000(4) = 0
      dvm000(5) = 50
      dvm000(6) = crtamv (dvm0c0,dvm0c1,dvm000(5),dvm0c0)
      dvm000(7) = distr (dvm000(6),dvm0c0,dvm0c0,dvm000(4),dvm000(4))
      dvm000(8) = 11
      dvm000(9) = 11
      a(2:2) = 1
      a(4) = 1
      a(5) = 6
      dvm000(10) = crtda (a(1),dvm0c1,i0000m,dvm0c1,dvm0c4,dvm000(5),dvm
     &0c0,dvm0c0,dvm000(8),dvm000(9))
      dvm000(11) = 1
      dvm000(12) = 1
      dvm000(13) = 0
      dvm000(14) = align (a(1),dvm000(6),dvm000(11),dvm000(12),dvm000(13
     &))
      allocate(c(n))
      call dvmh_data_enter(c,dvm0c0)
      nloop = nl
      do  iloop = 0,2
         nnl = nl
         call serial1(c,n,nnl)
         do  i = 12,n - 11
            c(i) = c(i - 9) + c(i + 9) + c(i + 10) + c(i - 10) + c(i - 1
     &1) + c(i + 11)
         enddo  
         call dvmh_line(540_8,dvmh_string (filenm001))
         call dvmh_actual_variable(nloop)
         call dvmh_actual_variable(c)
         call dvmh_line(541_8,dvmh_string (filenm001))

! Start region (line 541)
         dvm000(4) = region_create (dvm0c0)
         dvm000(8) = lbound (c,1)
         dvm000(10) = size (c,1)
         call crtraf(dvm000(5),dvm0c1,i0000m,dvm0c1,(-(dvm0c1)),dvm000(1
     &0),dvm0c1,dvm0c0,getai (c))
         call region_register_array(dvm000(4),INTENT_IN,dvm000(5),1_8)
         call region_set_name_array(dvm000(4),dvm000(5),'c')
         call region_register_array(dvm000(4),INTENT_INOUT,a(1),1_8)
         call region_set_name_array(dvm000(4),a(1),'a')
         call region_execute_on_targets(dvm000(4),DEVICE_TYPE_HOST)
         call dvmh_line(543_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(15) = getai (i)
         dvm000(16) = 1
         dvm000(17) = 1
         dvm000(18) = n
         dvm000(19) = 1
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(20),dvm000(21),dvm000(22),dvm
     &000(15),dvm000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(12),dvm
     &000(13),dvm000(14))

! Parallel loop (line 544)
         call dvmh_line(544_8,dvmh_string (filenm001))
         dvm000(23) = loop_create (dvm000(4),dvm000(11))
         dvm000(24) = 0

!$    dvm000(24) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(23),DEVICE_TYPE_HOST,dvm000(2
     &4),loop_acr12_544_host,dvm0c0,dvm0c3,a,i0000m(a(3)),nl)

! Loop execution
         call loop_perform(dvm000(23))
         call dvmh_line(546_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(548_8,dvmh_string (filenm001))
         dvm000(11) = crtpl (dvm0c1)
         dvm000(21) = 11
         dvm000(22) = 0
         dvm000(23) = 3
         dvm000(15) = crtshg (dvm0c0)
         dvm000(18) = crtshg (dvm0c0)
         call insshd(dvm000(15),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         call insshd(dvm000(18),a(1),dvm000(21),dvm000(22),dvm0c1,dvm000
     &(23))
         dvm000(24) = 0
         dvm000(25) = 11
         dvm000(26) = 5
         dvm000(17) = crtshg (dvm0c0)
         dvm000(20) = crtshg (dvm0c0)
         call insshd(dvm000(17),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         call insshd(dvm000(20),a(1),dvm000(24),dvm000(25),dvm0c1,dvm000
     &(26))
         dvm000(27) = getai (i)
         dvm000(28) = 1
         dvm000(29) = 12
         dvm000(30) = n - 11
         dvm000(31) = 1
         dvm000(32) = 1
         dvm000(33) = 1
         dvm000(34) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(32),dvm000(33),dvm000(34),dvm
     &000(27),dvm000(28),dvm000(29),dvm000(30),dvm000(31),dvm000(12),dvm
     &000(13),dvm000(14))
         pipe00 = iloop
         call dvmh_shadow_renew(dvm000(17))
         call across(dvm0c0,dvm000(17),dvm000(15),pipe00)

! Parallel loop (line 549)
         call dvmh_line(549_8,dvmh_string (filenm001))
         dvm000(35) = loop_create (dvm000(4),dvm000(11))
         call loop_across(dvm000(35),dvm000(20),dvm000(18))
         dvm000(36) = 0

!$    dvm000(36) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(35),DEVICE_TYPE_HOST,dvm000(3
     &6),loop_acr12_549_host,dvm0c0,dvm0c2,a,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(35))
         call dvmh_line(551_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         call dvmh_line(553_8,dvmh_string (filenm001))
         dvm000(15) = crtrg (dvm0c1,dvm0c1)
         dvm000(24) = 1
         dvm000(25) = 0
         call dvmh_get_actual_variable(nloop)
         dvm000(26) = crtrdf (dvm0c4,getai (nloop),dvm0c1,dvm000(24),dvm
     &0c0,dvm000(25),dvm0c1)
         dvm000(11) = crtpl (dvm0c1)
         dvm000(16) = getai (i)
         dvm000(17) = 1
         dvm000(18) = 12
         dvm000(19) = n - 11
         dvm000(20) = 1
         dvm000(21) = 1
         dvm000(22) = 1
         dvm000(23) = 0 - a(4)
         call mappl(dvm000(11),a(1),dvm000(21),dvm000(22),dvm000(23),dvm
     &000(16),dvm000(17),dvm000(18),dvm000(19),dvm000(20),dvm000(12),dvm
     &000(13),dvm000(14))
         call insred(dvm000(15),dvm000(26),dvm000(11),dvm0c0)

! Parallel loop (line 554)
         call dvmh_line(554_8,dvmh_string (filenm001))
         dvm000(27) = loop_create (dvm000(4),dvm000(11))
         call loop_insred(dvm000(27),dvm000(26))
         dvm000(28) = 0

!$    dvm000(28) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(27),DEVICE_TYPE_HOST,dvm000(2
     &8),loop_acr12_554_host,dvm0c0,dvm0c4,dvm000(5),a,i0000m(dvm000(7))
     &,i0000m(a(3)))

! Loop execution
         call loop_perform(dvm000(27))
         call dvmh_line(558_8,dvmh_string (filenm001))
         call endpl(dvm000(11))
         dvm000(29) = getai (nloop)
         call strtrd(dvm000(15))
         call waitrd(dvm000(15))
         call dvmh_actual_variable(nloop)
         call dvmh_delete_object(dvm000(15))

! Region end (line 541)
         call dvmh_line(559_8,dvmh_string (filenm001))
         call dvmh_delete_object(dvm000(5))
         call region_end(dvm000(4))
         call dvmh_line(560_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(nloop)
      enddo  
      if (nloop .eq. nl) then
         call ansyes(tname)
      else  
         call ansno(tname)
      endif  
      call dvmh_line(568_8,dvmh_string (filenm001))
      call dvmh_delete_object(a(1))
      call dvmh_data_exit(c,dvm0c0)
      deallocate(c)
      call dvmh_line(570_8,dvmh_string (filenm001))
      if (allocated (c))  call dvmh_data_exit(c,dvm0c0)
      call dvmh_scope_end()
      end


! -----------------------------------------------         
      subroutine serial1 (ar, n, nl)
      integer  ar(n)
      integer  nl
      do  i = 1,n
         ar(i) = nl + i
      enddo  
      end

      subroutine ansyes (name)
      character*7  name

! DVMH declarations 
      external dvmh_line,eiof,biof
      integer*8  dvmh_string,tstio
      character*8 ::  filenm001='acr12.f'//char (0)
      call dvmh_line(582_8,dvmh_string (filenm001))
      call biof()
      if (tstio () .ne. 0)  print *, name,'  -  complete'
      call eiof()
      end

      subroutine ansno (name)
      character*7  name

! DVMH declarations 
      external dvmh_line,eiof,biof
      integer*8  dvmh_string,tstio
      character*8 ::  filenm001='acr12.f'//char (0)
      call dvmh_line(586_8,dvmh_string (filenm001))
      call biof()
      if (tstio () .ne. 0)  print *, name,'  -  ***error'
      call eiof()
      end


!-----------------------------------------------------------------------


!     Host handler for loop on line 58 

      recursive subroutine loop_acr12_58_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 64 

      recursive subroutine loop_acr12_64_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i + 1)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 64 

      recursive subroutine loop_acr12_64_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i + 1)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 64 

      recursive subroutine loop_acr12_64_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_64_host_1
      external loop_acr12_64_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_64_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_64_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 69 

      recursive subroutine loop_acr12_69_host (loop_ref,c_head,a_head,c,
     &a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 111 

      recursive subroutine loop_acr12_111_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 116 

      recursive subroutine loop_acr12_116_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i) + a(i + 1)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 116 

      recursive subroutine loop_acr12_116_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i) + a(i + 1)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 116 

      recursive subroutine loop_acr12_116_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_116_host_1
      external loop_acr12_116_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_116_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_116_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 121 

      recursive subroutine loop_acr12_121_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 163 

      recursive subroutine loop_acr12_163_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 168 

      recursive subroutine loop_acr12_168_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i) + a(i - 1)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 168 

      recursive subroutine loop_acr12_168_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i) + a(i - 1)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 168 

      recursive subroutine loop_acr12_168_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_168_host_1
      external loop_acr12_168_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_168_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_168_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 173 

      recursive subroutine loop_acr12_173_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 217 

      recursive subroutine loop_acr12_217_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 222 

      recursive subroutine loop_acr12_222_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i + 1) + a(i + 2) + a(i - 2)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 222 

      recursive subroutine loop_acr12_222_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i + 1) + a(i + 2) + a(i - 2)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 222 

      recursive subroutine loop_acr12_222_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_222_host_1
      external loop_acr12_222_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_222_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_222_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 227 

      recursive subroutine loop_acr12_227_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 271 

      recursive subroutine loop_acr12_271_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 276 

      recursive subroutine loop_acr12_276_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i + 1) + a(i + 2)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 276 

      recursive subroutine loop_acr12_276_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i + 1) + a(i + 2)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 276 

      recursive subroutine loop_acr12_276_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_276_host_1
      external loop_acr12_276_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_276_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_276_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 281 

      recursive subroutine loop_acr12_281_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 325 

      recursive subroutine loop_acr12_325_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 330 

      recursive subroutine loop_acr12_330_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i - 2)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 330 

      recursive subroutine loop_acr12_330_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i - 2)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 330 

      recursive subroutine loop_acr12_330_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_330_host_1
      external loop_acr12_330_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_330_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_330_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 335 

      recursive subroutine loop_acr12_335_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 379 

      recursive subroutine loop_acr12_379_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 384 

      recursive subroutine loop_acr12_384_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i + 1) + a(i + 2) + a(i - 2) + a(i - 3) 
     &+ a(i + 3)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 384 

      recursive subroutine loop_acr12_384_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i + 1) + a(i + 2) + a(i - 2) + a(i - 3) 
     &+ a(i + 3)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 384 

      recursive subroutine loop_acr12_384_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_384_host_1
      external loop_acr12_384_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_384_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_384_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 389 

      recursive subroutine loop_acr12_389_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 433 

      recursive subroutine loop_acr12_433_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 438 

      recursive subroutine loop_acr12_438_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i + 1) + a(i + 2) + a(i + 3)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 438 

      recursive subroutine loop_acr12_438_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i + 1) + a(i + 2) + a(i + 3)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 438 

      recursive subroutine loop_acr12_438_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_438_host_1
      external loop_acr12_438_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_438_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_438_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 443 

      recursive subroutine loop_acr12_443_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 488 

      recursive subroutine loop_acr12_488_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 493 

      recursive subroutine loop_acr12_493_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i - 2) + a(i - 3)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 493 

      recursive subroutine loop_acr12_493_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 1) + a(i - 2) + a(i - 3)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 493 

      recursive subroutine loop_acr12_493_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_493_host_1
      external loop_acr12_493_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_493_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_493_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 498 

      recursive subroutine loop_acr12_498_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine



!     Host handler for loop on line 544 

      recursive subroutine loop_acr12_544_host (loop_ref,a_head,a,nl)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  nl
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = nl + i
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 549 

      recursive subroutine loop_acr12_549_host_1 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 9) + a(i + 9) + a(i + 10) + a(i - 10) + a(i - 1
     &1) + a(i + 11)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 549 

      recursive subroutine loop_acr12_549_host_0 (loop_ref,a_head,a)
      implicit none
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (i)
         do  i = boundsLow(1),boundsHigh(1)
            a(i) = a(i - 9) + a(i + 9) + a(i + 10) + a(i - 10) + a(i - 1
     &1) + a(i + 11)
         enddo  
!$OMP END PARALLEL 
      end subroutine



!     Host handler for loop on line 549 

      recursive subroutine loop_acr12_549_host (loop_ref,a_head,a)
      implicit none
      integer*8  which_run
      integer*8  loop_ref,a_head(4)
      integer  a(0:*)
      external loop_acr12_549_host_1
      external loop_acr12_549_host_0
      integer*8  loop_get_dependency_mask
      which_run = not (loop_get_dependency_mask (loop_ref))
      if (btest (which_run,0)) then
         call loop_acr12_549_host_1(loop_ref,a_head,a)
      else  
         call loop_acr12_549_host_0(loop_ref,a_head,a)
      endif  
      end subroutine



!     Host handler for loop on line 554 

      recursive subroutine loop_acr12_554_host (loop_ref,c_head,a_head,c
     &,a)
      implicit none
      integer*8  loop_ref,c_head(4),a_head(4)
      integer  a(0:*)
      integer  c(0:*)
      integer  i
      integer*8  boundsLow(1),boundsHigh(1),loopSteps(1)
      integer  nloop
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),loopStep
     &s(1))
      call loop_red_init(loop_ref,1_8,nloop,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (min:nloop),PRIVATE (i)
!$OMP    DO  SCHEDULE (runtime)
         do  i = boundsLow(1),boundsHigh(1)
            if (a(i) .ne. c(i)) then
               nloop = min (nloop,i)
            endif  
         enddo  
!$OMP    END DO  NOWAIT
!$OMP END PARALLEL 
      call loop_red_post(loop_ref,1_8,nloop,0_8)
      end subroutine


