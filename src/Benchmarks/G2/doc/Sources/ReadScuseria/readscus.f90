      Program main
      implicit none
      character(30) :: name
      real(8)     :: val1,val2,val3,val4
      do 
        read(5,*,err=10,end=10)name,val1,val2,val3,val4
        val2=val2+val1        
        val3=val3+val1
        val4=val4+val1
!print*,'===',name,val1,val2,val3,val4
!cycle
        write(6,fmt='(t7,"I=I+1; ID(i)= ",a30,"; DATA(:,I)=(/",f7.1,",",f7.1,",",f7.1,",",f7.1,"/)")') &
     &           name,val1,val2,val3,val4              
      enddo
10    continue
      stop
      end
