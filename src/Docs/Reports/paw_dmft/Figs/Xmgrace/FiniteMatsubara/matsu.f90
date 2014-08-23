       program main
       integer(4),parameter :: nomega=2**12
       real(8)   ,parameter :: kbt=1.d0
       real(8)   ,parameter :: mumin=0.d0,mumax=200
       integer(4),parameter :: nmu=1000
       complex(8),parameter :: ci=(0.d0,1.d0)
       real(8)              :: pi
       real(8)              :: mu
       real(8)              :: rn
       real(8)              :: omega(nomega)
       complex(8)           :: g(nomega)
       integer(4)           :: nu,imu
!      ************************************************************************
       pi=4.d0*atan(1.d0)
       do nu=1,nomega
         omega(nu)=real(2*nu-1,kind=8)*pi*kbt
       enddo
       do imu=1,nmu
         mu=mumin+(mumax-mumin)*real(imu-1,kind=8)/real(nmu-1,kind=8)
         do nu=1,nomega
           g(nu)=1.d0/(ci*omega(nu)+mu)
         enddo
!        == matsubara sam
         rn=kbt*2.d0*real(sum(g)) !(factor two from negative and positive freq.)
         write(*,*)mu,(kbt*2.d0*real(sum(g(:2**j))),j=0,12)
       enddo
       stop
       end
