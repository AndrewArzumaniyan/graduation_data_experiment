
      program INTRINSICS
      print *, '=== START OF F2C_MATH intrinsic test ==========='

c     TESTING abs GENERIC INTRINSIC
c     integer*4 abs(integer*4)
      call abs1
c     real*4 abs(real*4)
      call abs2
c     real*8 abs(real*8)
      call abs3
c     real*4 abs(complex*8)
      call abs4
c     real*8 abs(complex*16)
      call abs5
c     real*4 cabs(complex*8)
      call abs6
c     real*8 dabs(real*8)
      call abs7
c     integer*4 iabs(integer*4)
      call abs8
c     real*8 cdabs(complex*16)
      call abs14
c     real*8 zabs(complex*16)
      call abs15

c     TESTING acos GENERIC INTRINSIC
c     real*4 acos(real*4)
      call acos1
c     real*8 acos(real*8)
      call acos2
c     real*8 dacos(real*8)
      call acos3

c     TESTING acosh GENERIC INTRINSIC
c     real*4 acosh(real*4)
      call acosh1
c     real*8 acosh(real*8)
      call acosh2
c     real*8 dacosh(real*8)
      call acosh3

c     TESTING aimag GENERIC INTRINSIC
c     real*4 aimag(complex*8)
      call aimag1
c     real*8 aimag(complex*16)
      call aimag2
c     real*4 imag(complex*8)
      call aimag3
c     real*8 imag(complex*16)
      call aimag4
c     real*8 dimag(complex*16)
      call aimag5

c     TESTING asin GENERIC INTRINSIC
c     real*4 asin(real*4)
      call asin1
c     real*8 asin(real*8)
      call asin2
c     real*8 dasin(real*8)
      call asin3

c     TESTING asinh GENERIC INTRINSIC
c     real*4 asinh(real*4)
      call asinh1
c     real*8 asinh(real*8)
      call asinh2
c     real*8 dasinh(real*8)
      call asinh3

c     TESTING atan GENERIC INTRINSIC
c     real*4 atan(real*4)
      call atan1
c     real*8 atan(real*8)
      call atan2_
c     real*8 datan(real*8)
      call atan3

c     TESTING atan2 GENERIC INTRINSIC
c     real*4 atan2(real*4, real*4)
      call atan21
c     real*8 atan2(real*8, real*8)
      call atan22
c     real*8 datan2(real*8, real*8)
      call atan23

c     TESTING atanh GENERIC INTRINSIC
c     real*4 atanh(real*4)
      call atanh1
c     real*8 atanh(real*8)
      call atanh2
c     real*8 datanh(real*8)
      call atanh3

c     TESTING bessel_j0 GENERIC INTRINSIC
c     real*4 bessel_j0(real*4)
      call bessel_j01
c     real*8 bessel_j0(real*8)
      call bessel_j02

c     TESTING bessel_j1 GENERIC INTRINSIC
c     real*4 bessel_j1(real*4)
      call bessel_j11
c     real*8 bessel_j1(real*8)
      call bessel_j12

c     TESTING bessel_jn GENERIC INTRINSIC
c     real*4 bessel_jn(integer*4, real*4)
      call bessel_jn1
c     real*8 bessel_jn(integer*4, real*8)
      call bessel_jn2

c     TESTING bessel_y0 GENERIC INTRINSIC
c     real*4 bessel_y0(real*4)
      call bessel_y01
c     real*8 bessel_y0(real*8)
      call bessel_y02

c     TESTING bessel_y1 GENERIC INTRINSIC
c     real*4 bessel_y1(real*4)
      call bessel_y11
c     real*8 bessel_y1(real*8)
      call bessel_y12
c     TESTING bessel_yn GENERIC INTRINSIC
c     real*4 bessel_yn(integer*4, real*4)
      call bessel_yn1
c     real*8 bessel_yn(integer*4, real*8)
      call bessel_yn2
c     TESTING btest GENERIC INTRINSIC
c     logical*1 btest(integer*1)
      call btest1
c     logical*2 btest(integer*2)
      call btest2
c     logical*4 btest(integer*4)
      call btest3
c     logical*8 btest(integer*8)
      call btest4

c     TESTING cmplx GENERIC INTRINSIC
c     complex*8 cmplx(integer*4)
      call cmplx1
c     complex*8 cmplx(real*4)
      call cmplx2
c     complex*8 cmplx(real*8)
      call cmplx3
c     complex*8 cmplx(complex*8)
      call cmplx4
c     complex*8 cmplx(complex*16)
      call cmplx5
c     complex*8 cmplx(integer*4, integer*4)
      call cmplx6
c     complex*8 cmplx(real*4, real*4)
      call cmplx7
c     complex*8 cmplx(real*8, real*8)
      call cmplx8
c     complex*8 cmplx(integer*4, integer*4, 4)
      call cmplx9
c     complex*8 cmplx(real*4, real*4, 4)
      call cmplx10
c     complex*8 cmplx(real*8, real*8, 4)
      call cmplx11
c     complex*16 cmplx(integer*4, integer*4, 8)
      call cmplx12
c     complex*16 cmplx(real*4, real*4, 8)
      call cmplx13
c     complex*16 cmplx(real*8, real*8, 8)
      call cmplx14
c     complex*16 dcmplx(integer*4)
      call cmplx15
c     complex*16 dcmplx(real*4)
      call cmplx16
c     complex*16 dcmplx(real*8)
      call cmplx17
c     complex*16 dcmplx(complex*8)
      call cmplx18
c     complex*16 dcmplx(complex*16)
      call cmplx19
c     complex*16 dcmplx(integer*4, integer*4)
      call cmplx20
c     complex*16 dcmplx(real*4, real*4)
      call cmplx21
c     complex*16 dcmplx(real*8, real*8)
      call cmplx22

c     TESTING conjg GENERIC INTRINSIC
c     complex*8 conjg(complex*8)
      call conjg1
c     complex*16 conjg(complex*16)
      call conjg2
c     complex*16 dconjg(complex*16)
      call conjg3

c     TESTING cos GENERIC INTRINSIC
c     real*4 cos(real*4)
      call cos1
c     real*8 cos(real*8)
      call cos2
c     complex*8 cos(complex*8)
      call cos3
c     complex*16 cos(complex*16)
      call cos4
c     real*8 dcos(real*8)
      call cos5
c     complex*8 ccos(complex*8)
      call cos6
c     complex*16 cdcos(complex*16)
      call cos7
c     complex*16 zcos(complex*16)
      call cos8

c     TESTING cosh GENERIC INTRINSIC
c     real*4 cosh(real*4)
      call cosh1
c     real*8 cosh(real*8)
      call cosh2
c     real*8 dcosh(real*8)
      call cosh3

c     TESTING dble GENERIC INTRINSIC
c     real*8 dble(integer*1)
      call dble1
c     real*8 dble(integer*2)
      call dble2
c     real*8 dble(integer*4)
      call dble3
c     real*8 dble(integer*8)
      call dble4
c     real*8 dble(real*4)
      call dble5
c     real*8 dble(real*8)
      call dble6
c     real*8 dble(complex*8)
      call dble7
c     real*8 dble(complex*16)
      call dble8

c     TESTING dfloat GENERIC INTRINSIC
c     real*8 dfloat(integer*1)
      call dfloat1
c     real*8 dfloat(integer*2)
      call dfloat2
c     real*8 dfloat(integer*4)
      call dfloat3
c     real*8 dfloat(integer*8)
      call dfloat4

c     TESTING dim GENERIC INTRINSIC
c     integer*1 dim(integer*1)
      call dim1
c     integer*2 dim(integer*2)
      call dim2
c     integer*4 dim(integer*4)
      call dim3
c     integer*8 dim(integer*8)
      call dim4
c     real*4 dim(real*4)
      call dim5
c     real*8 dim(real*8)
      call dim6
c     real*8 ddim(real*8)
      call dim13

c     TESTING dprod SPECIFIC INTRINSIC
c     real*8 dprod(real*4)
      call dprod1

c     TESTING dreal SPECIFIC INTRINSIC
c     real*8 dreal(complex*16)
      call dreal1

c     TESTING dshiftl SPECIFIC INTRINSIC
c     integer*8 dshiftl(integer*8)
      call dshiftl1

c     TESTING dshiftr SPECIFIC INTRINSIC
c     integer*8 dshiftr(integer*8)
      call dshiftr1

c     TESTING erf GENERIC INTRINSIC
c     real*4 erf(real*4)
      call erf1
c     real*8 erf(real*8)
      call erf2
c     real*8 derf(real*8)
      call erf3

c     TESTING erfc GENERIC INTRINSIC
c     real*4 erfc(real*4)
      call erfc1
c     real*8 erfc(real*8)
      call erfc2
c     real*8 derfc(real*8)
      call erfc3

c     TESTING erfc_scaled GENERIC INTRINSIC
c     real*4 erfc_scaled(real*4)
      call erfc_scaled1
c     real*8 erfc_scaled(real*8)
      call erfc_scaled2

c     TESTING exp GENERIC INTRINSIC
c     real*4 exp(real*4)
      call exp1
c     real*8 exp(real*8)
      call exp2
c     complex*8 exp(complex*8)
      call exp3
c     complex*16 exp(complex*16)
      call exp4
c     real*8 dexp(real*8)
      call exp5
c     complex*8 cexp(complex*8)
      call exp6
c     complex*16 cdexp(complex*16)
      call exp7
c     complex*16 zexp(complex*16)
      call exp8

c     TESTING gamma GENERIC INTRINSIC
c     real*4 gamma(real*4)
      call gamma1
c     real*8 gamma(real*8)
      call gamma2

c     TESTING hypot GENERIC INTRINSIC
c     real*4 hypot(real*4)
      call hypot1
c     real*8 hypot(real*8)
      call hypot2

c     TESTING iand GENERIC INTRINSIC
c     integer*1 iand(integer*1)
      call iand1
c     integer*2 iand(integer*2)
      call iand2
c     integer*4 iand(integer*4)
      call iand3
c     integer*8 iand(integer*8)
      call iand4
c     integer*1 and(integer*1)
      call iand5
c     integer*2 and(integer*2)
      call iand6
c     integer*4 and(integer*4)
      call iand7
c     integer*8 and(integer*8)
      call iand8

c     TESTING ibclr GENERIC INTRINSIC
c     integer*1 ibclr(integer*1)
      call ibclr1
c     integer*2 ibclr(integer*2)
      call ibclr2
c     integer*4 ibclr(integer*4)
      call ibclr3
c     integer*8 ibclr(integer*8)
      call ibclr4

c     TESTING ibits GENERIC INTRINSIC
c     integer*1 ibits(integer*1)
      call ibits1
c     integer*2 ibits(integer*2)
      call ibits2
c     integer*4 ibits(integer*4)
      call ibits3
c     integer*8 ibits(integer*8)
      call ibits4

c     TESTING ibset GENERIC INTRINSIC
c     integer*1 ibset(integer*1)
      call ibset1
c     integer*2 ibset(integer*2)
      call ibset2
c     integer*4 ibset(integer*4)
      call ibset3
c     integer*8 ibset(integer*8)
      call ibset4

c     TESTING ieor GENERIC INTRINSIC
c     integer*1 ieor(integer*1)
      call ieor1
c     integer*2 ieor(integer*2)
      call ieor2
c     integer*4 ieor(integer*4)
      call ieor3
c     integer*8 ieor(integer*8)
      call ieor4
c     integer*1 xor(integer*1)
      call ieor9
c     integer*2 xor(integer*2)
      call ieor10
c     integer*4 xor(integer*4)
      call ieor11
c     integer*8 xor(integer*8)
      call ieor12

c     TESTING ior GENERIC INTRINSIC
c     integer*1 ior(integer*1)
      call ior1
c     integer*2 ior(integer*2)
      call ior2
c     integer*4 ior(integer*4)
      call ior3
c     integer*8 ior(integer*8)
      call ior4
c     integer*1 or(integer*1)
      call ior5
c     integer*2 or(integer*2)
      call ior6
c     integer*4 or(integer*4)
      call ior7
c     integer*8 or(integer*8)
      call ior8

c     TESTING ishft GENERIC INTRINSIC
c     integer*1 ishft(integer*1)
      call ishft1
c     integer*2 ishft(integer*2)
      call ishft2
c     integer*4 ishft(integer*4)
      call ishft3
c     integer*8 ishft(integer*8)
      call ishft4

c     TESTING lshift GENERIC INTRINSIC
c     integer*1 lshift(integer*1)
      call lshift1
c     integer*2 lshift(integer*2)
      call lshift2
c     integer*4 lshift(integer*4)
      call lshift3
c     integer*8 lshift(integer*8)
      call lshift4

c     TESTING rshift GENERIC INTRINSIC
c     integer*1 rshift(integer*1)
      call rshift1
c     integer*2 rshift(integer*2)
      call rshift2
c     integer*4 rshift(integer*4)
      call rshift3
c     integer*8 rshift(integer*8)
      call rshift4

c     TESTING ishftc GENERIC INTRINSIC
c     integer*1 ishftc(integer*1)
      call ishftc1
c     integer*2 ishftc(integer*2)
      call ishftc2
c     integer*4 ishftc(integer*4)
      call ishftc3
c     integer*8 ishftc(integer*8)
      call ishftc4
c     integer*1 ishftc(integer*1)
      call ishftc5
c     integer*2 ishftc(integer*2)
      call ishftc6
c     integer*4 ishftc(integer*4)
      call ishftc7
c     integer*8 ishftc(integer*8)
      call ishftc8

c     TESTING log GENERIC INTRINSIC
c     real*4 log(real*4)
      call log1
c     real*8 log(real*8)
      call log2
c     complex*8 log(complex*8)
      call log3
c     complex*16 log(complex*16)
      call log4
c     real*4 alog(real*4)
      call log5
c     real*8 dlog(real*8)
      call log6
c     complex*8 clog(complex*8)
      call log7
c     complex*16 cdlog(complex*16)
      call log8
c     complex*16 zlog(complex*16)
      call log9

c     TESTING log10 GENERIC INTRINSIC
c     real*4 log10(real*4)
      call log101
c     real*8 log10(real*8)
      call log102

c     real*4 alog10(real*4)
      call log105
c     real*8 dlog10(real*8)
      call log106

c     TESTING log_gamma GENERIC INTRINSIC
c     real*4 log_gamma(real*4)
      call log_gamma1
c     real*8 log_gamma(real*8)
      call log_gamma2

c     TESTING max GENERIC INTRINSIC
c     integer*1 max(integer*1)
      call max1_
c     integer*2 max(integer*2)
      call max2_
c     integer*4 max(integer*4)
      call max3_
c     integer*8 max(integer*8)
      call max4_
c     real*4 max(real*4)
      call max5_
c     real*8 max(real*8)
      call max6_
c     integer*4 max0(integer*4)
      call max7_
c     real*4 amax1(real*4)
      call max8_
c     real*8 dmax1(real*8)
      call max9_

c     integer*4 max1(real*4)
      call max13_

c     real*4 amax0(integer*4)
      call max17_

c     TESTING merge_bits GENERIC INTRINSIC
c     integer*1 merge_bits(integer*1)
      call merge_bits1
c     integer*2 merge_bits(integer*2)
      call merge_bits2
c     integer*4 merge_bits(integer*4)
      call merge_bits3
c     integer*8 merge_bits(integer*8)
      call merge_bits4

c     TESTING min GENERIC INTRINSIC
c     integer*1 min(integer*1)
      call min1_
c     integer*2 min(integer*2)
      call min2_
c     integer*4 min(integer*4)
      call min3_
c     integer*8 min(integer*8)
      call min4_
c     real*4 min(real*4)
      call min5_
c     real*8 min(real*8)
      call min6_
c     integer*4 min0(integer*4)
      call min7_
c     real*4 amin1(real*4)
      call min8_
c     real*8 dmin1(real*8)
      call min9_
c     integer*4 min1(real*4)
      call min13_
c     real*4 amin0(integer*4)
      call min17_

c     TESTING mod GENERIC INTRINSIC
c     integer*1 mod(integer*1)
      call mod1
c     integer*2 mod(integer*2)
      call mod2
c     integer*4 mod(integer*4)
      call mod3
c     integer*8 mod(integer*8)
      call mod4
c     real*4 amod(real*4)
      call mod10
c     real*8 dmod(real*8)
      call mod11

c     TESTING modulo GENERIC INTRINSIC
c     integer*1 modulo(integer*1)
      call modulo1
c     integer*2 modulo(integer*2)
      call modulo2
c     integer*4 modulo(integer*4)
      call modulo3
c     integer*8 modulo(integer*8)
      call modulo4
c     real*4 modulo(real*4)
      call modulo5
c     real*8 modulo(real*8)
      call modulo6

c     TESTING not GENERIC INTRINSIC
c     integer*1 not(integer*1)
      call not1
c     integer*2 not(integer*2)
      call not2
c     integer*4 not(integer*4)
      call not3
c     integer*8 not(integer*8)
      call not4

c     TESTING popcnt GENERIC INTRINSIC
c     integer*1 popcnt(integer*1)
      call popcnt1
c     integer*2 popcnt(integer*2)
      call popcnt2
c     integer*4 popcnt(integer*4)
      call popcnt3
c     integer*8 popcnt(integer*8)
      call popcnt4

c     TESTING poppar GENERIC INTRINSIC
c     integer*1 poppar(integer*1)
      call poppar1
c     integer*2 poppar(integer*2)
      call poppar2
c     integer*4 poppar(integer*4)
      call poppar3
c     integer*8 poppar(integer*8)
      call poppar4

c     TESTING real GENERIC INTRINSIC
c     real*4 real(integer*1)
      call real1
c     real*4 real(integer*2)
      call real2
c     real*4 real(integer*4)
      call real3
c     real*4 real(integer*8)
      call real4
c     real*4 real(real*4)
      call real5
c     real*4 real(real*8)
      call real6
c     real*4 real(complex*8)
      call real7
c     real*4 real(complex*16)
      call real8
c     real*4 float(integer*4)
      call real10
c     real*4 sngl(real*4)
      call real13
c     real*4 sngl(real*8)
      call real14

c     TESTING shifta GENERIC INTRINSIC
c     integer*1 shifta(integer*1)
      call shifta1
c     integer*2 shifta(integer*2)
      call shifta2
c     integer*4 shifta(integer*4)
      call shifta3
c     integer*8 shifta(integer*8)
      call shifta4

c     TESTING shiftl GENERIC INTRINSIC
c     integer*1 shiftl(integer*1)
      call shiftl1
c     integer*2 shiftl(integer*2)
      call shiftl2
c     integer*4 shiftl(integer*4)
      call shiftl3
c     integer*8 shiftl(integer*8)
      call shiftl4

c     TESTING shiftr GENERIC INTRINSIC
c     integer*1 shiftr(integer*1)
      call shiftr1
c     integer*2 shiftr(integer*2)
      call shiftr2
c     integer*4 shiftr(integer*4)
      call shiftr3
c     integer*8 shiftr(integer*8)
      call shiftr4

c     TESTING sign GENERIC INTRINSIC
c     integer*1 sign(integer*1)
      call sign1
c     integer*2 sign(integer*2)
      call sign2
c     integer*4 sign(integer*4)
      call sign3
c     integer*8 sign(integer*8)
      call sign4
c     real*4 sign(real*4)
      call sign5
c     real*8 sign(real*8)
      call sign6
c     integer*4 isign(integer*4)
      call sign9
c     real*8 dsign(real*8)
      call sign16

c     TESTING sin GENERIC INTRINSIC
c     real*4 sin(real*4)
      call sin1
c     real*8 sin(real*8)
      call sin2
c     complex*8 sin(complex*8)
      call sin3
c     complex*16 sin(complex*16)
      call sin4
c     real*8 dsin(real*8)
      call sin5
c     complex*8 csin(complex*8)
      call sin6
c     complex*16 cdsin(complex*16)
      call sin7
c     complex*16 zsin(complex*16)
      call sin8

c     TESTING sinh GENERIC INTRINSIC
c     real*4 sinh(real*4)
      call sinh1
c     real*8 sinh(real*8)
      call sinh2
c     real*8 dsinh(real*8)
      call sinh3

c     TESTING sqrt GENERIC INTRINSIC
c     real*4 sqrt(real*4)
      call sqrt1
c     real*8 sqrt(real*8)
      call sqrt2
c     complex*8 sqrt(complex*8)
      call sqrt3
c     complex*16 sqrt(complex*16)
      call sqrt4
c     real*8 dsqrt(real*8)
      call sqrt5
c     complex*8 csqrt(complex*8)
      call sqrt6
c     complex*16 cdsqrt(complex*16)
      call sqrt7
c     complex*16 zsqrt(complex*16)
      call sqrt8

c     TESTING tan GENERIC INTRINSIC
c     real*4 tan(real*4)
      call tan1
c     real*8 tan(real*8)
      call tan2
c     complex*8 tan(complex*8)
      call tan3
c     complex*16 tan(complex*16)
      call tan4
c     real*8 dtan(real*8)
      call tan5

c     TESTING tanh GENERIC INTRINSIC
c     real*4 tanh(real*4)
      call tanh1
c     real*8 tanh(real*8)
      call tanh2
c     real*8 dtanh(real*8)
      call tanh3

c     TESTING trailz SPECIFIC INTRINSIC
c     integer*1 trailz(integer*1)
      call trailz1
c     integer*2 trailz(integer*2)
      call trailz2
c     integer*4 trailz(integer*4)
      call trailz3
c     integer*8 trailz(integer*8)
      call trailz4

      print *, '=== END OF F2C_MATH intrinsic test ============='
      end

C -------------------------------------------------

      subroutine abs1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'abs_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = abs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'abs_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = abs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'abs_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = abs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N)
      complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y, tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'abs_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = abs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = abs(B(i))
          if (abs(tmp - A(i))/tmp .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y, tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'abs_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = abs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = abs(B(i))
          if (abs(tmp - A(i)) / tmp .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N)
      complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y, tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cabs_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cabs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cabs(B(i))
          if (abs(tmp - A(i))/tmp .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dabs_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dabs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dabs(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'iabs_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = iabs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (iabs(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs14
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y, tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cdabs_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cdabs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cdabs(B(i))
          if (abs(tmp - A(i))/tmp .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine abs15
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y, tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'zabs_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = zabs(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = zabs(B(i))
          if (abs(tmp - A(i))/tmp .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine acos1
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'acos_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = acos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = acos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine acos2
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'acos_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = acos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = acos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine acos3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dacos_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dacos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dacos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine acosh1
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'acosh_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = acosh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = acosh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine acosh2
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'acosh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = acosh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = acosh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine acosh3
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dacosh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dacosh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dacosh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine aimag1
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      character*24 tname
      real A(N)
      complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'aimag_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = aimag(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (aimag(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine aimag2
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'aimag_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = aimag(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (aimag(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine aimag3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      character*24 tname
      real A(N)
      complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'imag_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = imag(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (imag(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine aimag4
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'imag_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = imag(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (imag(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine aimag5
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dimag_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dimag(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dimag(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine asin1
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'asin_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = asin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = asin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine asin2
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'asin_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = asin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = asin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine asin3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dasin_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dasin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dasin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine asinh1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'asinh_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = asinh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = asinh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine asinh2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'asinh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = asinh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = asinh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine asinh3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dasinh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dasinh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dasinh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine atan1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'atan_float'
      
      
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = atan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = atan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine atan2_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'atan_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = atan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = atan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine atan3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'datan_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = datan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = datan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine atan21
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'atan2_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W1 + S1
          C(i) = C(i) * W2 + S2
      enddo

!dvm$ actual(B, C)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = atan2(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = atan2(B(i), C(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine atan22
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'atan2_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W1 + S1
          C(i) = C(i) * W2 + S2
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = atan2(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = atan2(B(i), C(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine atan23
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'datan2_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W1 + S1
          C(i) = C(i) * W2 + S2
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = datan2(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = datan2(B(i), C(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif
      end

C -------------------------------------------------

      subroutine atanh1
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'atanh_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = atanh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = atanh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine atanh2
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'atanh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = atanh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = atanh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine atanh3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'datanh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = datanh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = datanh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_j01
      integer, parameter :: N = 256, ER = N + 1, W = 20, S = -10
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_j0_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_j0(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_j0(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_j02
      integer, parameter :: N = 256, ER = N + 1, W = 20, S = -10
      real*8, parameter :: EPS = 1d-12
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_j0_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_j0(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_j0(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_j11
      integer, parameter :: N = 256, ER = N + 1, W = 20, S = -10
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_j1_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_j1(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_j1(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_j12
      integer, parameter :: N = 256, ER = N + 1, W = 20, S = -10
      real*8, parameter :: EPS = 1d-12
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_j1_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_j1(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_j1(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_jn1
      integer, parameter :: N = 256, ER = N + 1, W1 = 19, S1 = 1, W2 = 
     &20, S2 = -10
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), C(N)
      integer B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_jn_float'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          call random_number(C(i))
          B(i) = int(tmp * W1 + S1)
          C(i) = C(i) * W2 + S2
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_jn(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_jn(B(i), C(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_jn2
      integer, parameter :: N = 256, ER = N + 1, W1 = 19, S1 = 1, W2 = 
     &20, S2 = -10
      real*8, parameter :: EPS = 1d-12
      character*24 tname
      real*8 A(N), C(N)
      integer B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_jn_double'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          call random_number(C(i))
          B(i) = int(tmp * W1 + S1)
          C(i) = C(i) * W2 + S2
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_jn(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_jn(B(i), C(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_y01
      integer, parameter :: N = 256, ER = N + 1, W = 10, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_y0_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_y0(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_y0(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_y02
      integer, parameter :: N = 256, ER = N + 1, W = 10, S = 1
      real*8, parameter :: EPS = 1d-12
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_y0_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_y0(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_y0(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_y11
      integer, parameter :: N = 256, ER = N + 1, W = 10, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_y1_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_y1(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_y1(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_y12
      integer, parameter :: N = 256, ER = N + 1, W = 10, S = 1
      real*8, parameter :: EPS = 1d-12
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_y1_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_y1(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_y1(B(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_yn1
      integer, parameter :: N = 256, ER = N + 1, W1 = 10, S1 = 0, W2 = 
     &19, S2 = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), C(N)
      integer B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_yn_float'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          call random_number(C(i))
          B(i) = int(tmp * W1 + S1)
          C(i) = C(i) * W2 + S2 + B(i)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_yn(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_yn(B(i), C(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine bessel_yn2
      integer, parameter :: N = 256, ER = N + 1, W1 = 10, S1 = 0, W2 = 
     &19, S2 = 1
      real*8, parameter :: EPS = 1d-12
      character*24 tname
      real*8 A(N), C(N)
      integer B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'bessel_yn_double'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          call random_number(C(i))
          B(i) = int(tmp * W1 + S1)
          C(i) = C(i) * W2 + S2 + B(i)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = bessel_yn(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(bessel_yn(B(i), C(i)) - A(i)) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine btest1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      logical*1 A(N)
      integer*1 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'btest_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = btest(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (btest(B(i), C(i)) .neqv. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine btest2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      logical*2 A(N)
      integer*2 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'btest_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = btest(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (btest(B(i), C(i)) .neqv. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine btest3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      logical*4 A(N)
      integer*4 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'btest_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = btest(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (btest(B(i), C(i)) .neqv. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine btest4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      logical*8 A(N)
      integer*8 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'btest_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = btest(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (btest(B(i), C(i)) .neqv. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      integer B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      real B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      real*8 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      integer B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_long_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      real B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_float_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      real*8 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_double_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx9
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      integer B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_long_long_4'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i), 4)
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i), 4) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx10
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      real B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_float_float_4'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i), 4)
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i), 4) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx11
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N)
      real*8 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_double_double_4'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i), 4)
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i), 4) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx12
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      integer B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_long_long_8'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i), 8)
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i), 8) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx13
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      real B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_float_float_8'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i), 8)
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i), 8) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx14
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      real*8 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cmplx_double_double_8'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cmplx(B(i), C(i), 8)
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (cmplx(B(i), C(i), 8) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx15
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      integer B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx16
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      real B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx17
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      real*8 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx18
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx19
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx20
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      integer B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_long_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx21
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      real B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_float_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cmplx22
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N)
      real*8 B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcmplx_double_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          call random_number(C(i))
          B(i) = B(i) * W + S
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcmplx(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dcmplx(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine conjg1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'conjg_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = conjg(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (conjg(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine conjg2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'conjg_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = conjg(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (conjg(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine conjg3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      double complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dconjg_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dconjg(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dconjg(B(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cos_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cos_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      complex tmp
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cos_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos4
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cos_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcos_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dcos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos6
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ccos_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ccos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = ccos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos7
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cdcos_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cdcos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cdcos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cos8
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'zcos_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = zcos(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = zcos(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cosh1
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cosh_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cosh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cosh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cosh2
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cosh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cosh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cosh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine cosh3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dcosh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dcosh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dcosh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*1 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*2 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*4 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*8 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      real B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dble8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dble_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dble(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dble(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dfloat1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*1 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dfloat_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dfloat(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dfloat(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dfloat2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*2 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dfloat_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dfloat(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dfloat(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dfloat3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*4 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dfloat_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dfloat(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dfloat(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dfloat4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      integer*8 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dfloat_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dfloat(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dfloat(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dim1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dim_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dim(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dim(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dim2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dim_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dim(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dim(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dim3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dim_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dim(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dim(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dim4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dim_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dim(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dim(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dim5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dim_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dim(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dim(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dim6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dim_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dim(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dim(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dim13
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ddim_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ddim(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ddim(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dprod1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N)
      real B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dprod_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dprod(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dprod(B(i), C(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine dreal1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dreal_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dreal(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dreal(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------
!! it does not work with D(i) == 0 && 64 with Intel 2015
      subroutine dshiftl1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dshiftl_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = 1 + int(tmp * 62)  !!! HERE
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dshiftl(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dshiftl(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------
!! it does not work with D(i) == 0 && 64 with Intel 2015
      subroutine dshiftr1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dshiftr_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = 1 + int(tmp * 62)  !!! HERE
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dshiftr(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dshiftr(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erf1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'erf_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = erf(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = erf(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erf2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'erf_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = erf(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = erf(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erf3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'derf_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = derf(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = derf(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erfc1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'erfc_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = erfc(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = erfc(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erfc2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'erfc_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = erfc(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = erfc(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erfc3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'derfc_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = derfc(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = derfc(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erfc_scaled1
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'erfc_scaled_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = erfc_scaled(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = erfc_scaled(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine erfc_scaled2
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'erfc_scaled_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = erfc_scaled(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = erfc_scaled(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp1
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'exp_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = exp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = exp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp2
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'exp_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = exp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = exp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp3
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'exp_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = exp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = exp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp4
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'exp_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = exp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = exp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp5
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dexp_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dexp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dexp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp6
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cexp_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cexp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cexp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp7
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cdexp_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cdexp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cdexp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine exp8
      integer, parameter :: N = 256, ER = N + 1, W = 4, S = -2
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 x, y
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'zexp_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = zexp(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = zexp(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine gamma1
      integer, parameter :: N = 256, ER = N + 1, W = 3, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'gamma_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = gamma(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = gamma(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine gamma2
      integer, parameter :: N = 256, ER = N + 1, W = 3, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'gamma_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = gamma(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = gamma(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine hypot1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'hypot_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = hypot(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = hypot(B(i), C(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine hypot2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'hypot_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = hypot(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = hypot(B(i), C(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'iand_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = iand(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (iand(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'iand_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = iand(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (iand(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'iand_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = iand(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (iand(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'iand_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = iand(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (iand(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'and_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = and(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (and(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'and_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = and(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (and(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'and_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = and(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (and(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine iand8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'and_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = and(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (and(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibclr1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibclr_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibclr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibclr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibclr2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibclr_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibclr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibclr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibclr3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibclr_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibclr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibclr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibclr4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibclr_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibclr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibclr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibits1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibits_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * (bit_size(D(i)) + 1))
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1 - D(i)))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibits2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibits_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * (bit_size(D(i)) + 1))
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1 - D(i)))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibits3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibits_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * (bit_size(D(i)) + 1))
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1 - D(i)))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibits4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibits_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * (bit_size(D(i)) + 1))
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1 - D(i)))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibset1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibset_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibset(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibset(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibset2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibset_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibset(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibset(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibset3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibset_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibset(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibset(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ibset4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ibset_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ibset(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ibset(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ieor_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ieor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ieor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ieor_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ieor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ieor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ieor_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ieor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ieor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ieor_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ieor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ieor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor9
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'xor_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = xor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (xor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor10
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'xor_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = xor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (xor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor11
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'xor_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = xor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (xor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ieor12
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'xor_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = xor(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (xor(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
      intrinsic ior

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ior_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ior(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ior(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
      intrinsic ior

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ior_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ior(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ior(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
      intrinsic ior
	  
!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ior_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ior(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ior(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
      intrinsic ior
	  
!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ior_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ior(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ior(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'or_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = or(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (or(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
	  
!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'or_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = or(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (or(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
	  
!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'or_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = or(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (or(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ior8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp
	  
!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'or_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = or(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (or(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishft1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishft_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (2 * bit_size(C(i)) + 1) - bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishft(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishft(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishft2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishft_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (2 * bit_size(C(i)) + 1) - bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishft(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishft(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishft3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishft_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (2 * bit_size(C(i)) + 1) - bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishft(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishft(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishft4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishft_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (2 * bit_size(C(i)) + 1) - bit_size(C(i)))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishft(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishft(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine lshift1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'lshift_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = lshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (lshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine lshift2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'lshift_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = lshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (lshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine lshift3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'lshift_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = lshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (lshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine lshift4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'lshift_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = lshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (lshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine rshift1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'rshift_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = rshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (rshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine rshift2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'rshift_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = rshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (rshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine rshift3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'rshift_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = rshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (rshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine rshift4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'rshift_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = rshift(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (rshift(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * bit_size(D(i)) + 1)
          call random_number(tmp)
          C(i) = int(tmp * D(i))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * bit_size(D(i)) + 1)
          call random_number(tmp)
          C(i) = int(tmp * D(i))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * bit_size(D(i)) + 1)
          call random_number(tmp)
          C(i) = int(tmp * D(i))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ishftc8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'ishftc_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * bit_size(D(i)) + 1)
          call random_number(tmp)
          C(i) = int(tmp * D(i))
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = ishftc(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (ishftc(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log1
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log2
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log3
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log4
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log5
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'alog_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = alog(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = alog(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log6
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dlog_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dlog(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dlog(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log7
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'clog_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = clog(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = clog(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log8
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cdlog_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cdlog(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cdlog(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log9
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'zlog_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = zlog(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = zlog(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log101
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log10_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log10(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log10(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log102
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log10_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log10(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log10(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log105
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'alog10_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = alog10(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = alog10(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log106
      integer, parameter :: N = 256, ER = N + 1, W = 99, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dlog10_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dlog10(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dlog10(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log_gamma1
      integer, parameter :: N = 256, ER = N + 1, W = 3, S = 1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log_gamma_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log_gamma(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log_gamma(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine log_gamma2
      integer, parameter :: N = 256, ER = N + 1, W = 3, S = 1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'log_gamma_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = log_gamma(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = log_gamma(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max1_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max2_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max3_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max4_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max5_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max6_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max7_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max0_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max0(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max0(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max8_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'amax1_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = amax1(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (amax1(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max9_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dmax1_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dmax1(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dmax1(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max13_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N)
      real B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'max1_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = max1(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (max1(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine max17_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      integer*4 B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'amax0_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = amax0(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (amax0(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end


C -------------------------------------------------

      subroutine merge_bits1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'merge_bits_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = merge_bits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (merge_bits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine merge_bits2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'merge_bits_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = merge_bits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (merge_bits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine merge_bits3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'merge_bits_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = merge_bits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (merge_bits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine merge_bits4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'merge_bits_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = merge_bits(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (merge_bits(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min1_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min2_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min3_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min(B(i), C(i), D(i)) .ne. A(i)) then
              if (i < erri) then
                   erri = i
              endif
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min4_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min5_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min6_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min7_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min0_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min0(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min0(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min8_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'amin1_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = amin1(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (amin1(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min9_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dmin1_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dmin1(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dmin1(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min13_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N)
      real B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'min1_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
          call random_number(C(i))
          C(i) = C(i) * W + S
          call random_number(D(i))
          D(i) = D(i) * W + S
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = min1(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (min1(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine min17_
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      integer*4 B(N), C(N), D(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'amin0_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
          call random_number(tmp)
          D(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C, D)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = amin0(B(i), C(i), D(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (amin0(B(i), C(i), D(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine mod1
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'mod_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = mod(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (mod(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine mod2
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'mod_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = mod(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (mod(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine mod3
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'mod_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = mod(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (mod(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine mod4
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'mod_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = mod(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (mod(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine mod10
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      real A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'amod_float'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = amod(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (amod(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine mod11
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dmod_double'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dmod(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dmod(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine modulo1
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'modulo_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = modulo(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (modulo(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine modulo2
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'modulo_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = modulo(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (modulo(B(i), C(i)) .ne. A(i)) then
               erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine modulo3
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'modulo_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = modulo(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (modulo(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine modulo4
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'modulo_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W1 + S1)
          call random_number(tmp)
          C(i) = int(tmp * W2 + S2)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = modulo(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (modulo(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine modulo5
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      real, parameter :: EPS = 1e-5
      character*24 tname
      real A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'modulo_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W1 + S1
          call random_number(C(i))
          C(i) = C(i) * W2 + S2
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = modulo(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(modulo(B(i), C(i)) - A(i)) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine modulo6
      integer, parameter :: N = 256, ER = N + 1, W1 = 200, S1 = -100, W2
     & = 99, S2 = 1
      real*8, parameter :: EPS = 1d-12
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'modulo_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W1 + S1
          call random_number(C(i))
          C(i) = C(i) * W2 + S2
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = modulo(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (abs(modulo(B(i), C(i)) - A(i)) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine not1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'not_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = not(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (not(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine not2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'not_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = not(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (not(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine not3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'not_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = not(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (not(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine not4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'not_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = not(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (not(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine popcnt1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'popcnt_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = popcnt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (popcnt(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine popcnt2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'popcnt_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = popcnt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (popcnt(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine popcnt3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'popcnt_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = popcnt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (popcnt(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine popcnt4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'popcnt_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = popcnt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (popcnt(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine poppar1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'poppar_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = poppar(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (poppar(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine poppar2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'poppar_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = poppar(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (poppar(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine poppar3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'poppar_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = poppar(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (poppar(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine poppar4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'poppar_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = poppar(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (poppar(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      integer*1 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      integer*2 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      integer*4 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      integer*8 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      real B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      double complex B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real x, y

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'real_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = real(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (real(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real10
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      integer*4 B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'float_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = float(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (float(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real13
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N)
      real B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sngl_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sngl(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sngl(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine real14
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sngl_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sngl(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sngl(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shifta1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shifta_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shifta(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shifta(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shifta2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shifta_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shifta(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shifta(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shifta3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shifta_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shifta(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shifta(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shifta4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shifta_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shifta(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shifta(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftl1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftl_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftl(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftl(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftl2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftl_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftl(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftl(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftl3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftl_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftl(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftl(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftl4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftl_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftl(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftl(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftr1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftr_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftr2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftr_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftr3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftr_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine shiftr4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'shiftr_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * (bit_size(C(i)) + 1))
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = shiftr(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (shiftr(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*1 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sign_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*2 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sign_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sign_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sign_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sign_float'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sign_double'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (sign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign9
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      integer*4 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'isign_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = isign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (isign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sign16
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      character*24 tname
      real*8 A(N), B(N), C(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dsign_double'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = int(tmp * W + S)
          call random_number(tmp)
          C(i) = int(tmp * W + S)
      enddo

!dvm$ actual(B, C)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dsign(B(i), C(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (dsign(B(i), C(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sin_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sin_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sin_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin4
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sin_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dsin_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dsin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dsin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin6
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'csin_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = csin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = csin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin7
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cdsin_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cdsin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cdsin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sin8
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'zsin_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = zsin(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = zsin(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sinh1
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sinh_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sinh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sinh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sinh2
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sinh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sinh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sinh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sinh3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dsinh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dsinh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dsinh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt1
      integer, parameter :: N = 256, ER = N + 1, W = 100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sqrt_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt2
      integer, parameter :: N = 256, ER = N + 1, W = 100, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sqrt_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sqrt_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt4
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'sqrt_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = sqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = sqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt5
      integer, parameter :: N = 256, ER = N + 1, W = 100, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dsqrt_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dsqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dsqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt6
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'csqrt_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = csqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = csqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt7
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'cdsqrt_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = cdsqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = cdsqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine sqrt8
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'zsqrt_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = zsqrt(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = zsqrt(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tan1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'tan_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = tan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = tan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tan2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'tan_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = tan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = tan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tan3
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1e-6
      character*24 tname
      complex A(N), B(N)
      real x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'tan_complexf'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = cmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = tan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = tan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tan4
      integer, parameter :: N = 256, ER = N + 1, W = 2, S = -1
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      double complex A(N), B(N)
      real*8 x, y
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      double complex tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'tan_complexd'
      erri = ER

      do i = 1, N
          call random_number(x)
          call random_number(y)
          B(i) = dcmplx(x * W + S, y * W + S)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = tan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = tan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tan5
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dtan_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dtan(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dtan(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tanh1
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real, parameter :: EPS = 1e-6
      character*24 tname
      real A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'tanh_float'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)	  
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = tanh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = tanh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tanh2
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'tanh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = tanh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = tanh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine tanh3
      integer, parameter :: N = 256, ER = N + 1, W = 200, S = -100
      real*8, parameter :: EPS = 1d-15
      character*24 tname
      real*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real*8 tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'dtanh_double'
      erri = ER

      do i = 1, N
          call random_number(B(i))
          B(i) = B(i) * W + S
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = dtanh(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri)), private(tmp)
      do i = 1, N
          tmp = dtanh(B(i))
          if (abs(tmp - A(i))/abs(tmp) .gt. EPS) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine trailz1
      integer, parameter :: N = 256, ER = N + 1, W = 8
      character*24 tname
      integer*1 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'trailz_char'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = 2 ** int(tmp * W)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = trailz(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (trailz(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine trailz2
      integer, parameter :: N = 256, ER = N + 1, W = 16
      character*24 tname
      integer*2 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)


      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'trailz_short'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = 2 ** int(tmp * W)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = trailz(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (trailz(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine trailz3
      integer, parameter :: N = 256, ER = N + 1, W = 32
      character*24 tname
      integer*4 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'trailz_long'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = 2 ** int(tmp * W)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = trailz(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (trailz(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine trailz4
      integer, parameter :: N = 256, ER = N + 1, W = 64
      character*24 tname
      integer*8 A(N), B(N)
      integer erri, i, asize, clock
      integer, allocatable :: seed(:)
      real tmp

!dvm$ distribute A(BLOCK)

      call random_seed(size = asize)
      allocate(seed(asize))
      call system_clock(count = clock)
      seed = clock + 37 * (/(i - 1, i = 1, asize)/)
      call random_seed(put = seed)
      deallocate(seed)

      tname = 'trailz_longlong'
      erri = ER

      do i = 1, N
          call random_number(tmp)
          B(i) = 2 ** int(tmp * W)
      enddo

!dvm$ actual(B)
!dvm$ region
!dvm$ parallel (i) on A(i)
      do i = 1, N
          A(i) = trailz(B(i))
      enddo
!dvm$ end region
!dvm$ get_actual(A)

!dvm$ parallel (i) on A(i), reduction(min(erri))
      do i = 1, N
          if (trailz(B(i)) .ne. A(i)) then
              erri = min(erri, i)
          endif
      enddo

      if (erri .eq. ER) then     
          call ansyes(tname)
      else
          call ansno(tname)
      endif 
      
      end

C -------------------------------------------------

      subroutine ansyes(name)
      character*24 name
      print *, name, '  -  complete'
      end

      subroutine ansno(name)
      character*24 name
      print *, name, '  -  ***error'
      end
