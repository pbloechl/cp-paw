!
!      ..1.........2.........3.........4.........5.........6.........7.........8
       Program main
!      *************************************************************************
!      ** reads the datafile data.dat, which has been retrieved from          **
!      ** https://wiki.fysik.dtu.dk/gpaw/setups/molecule_tests.html           **
!      ** and extracts the molecular names used in bin/g2_analyze.f90         **
!      *************************************************************************
       implicit none
       character(80) :: line
       character(30) :: name
       character(10) :: molid
       real(8)       :: val(3)
       integer(4)    :: nfilin=10
       integer(4)    :: nfilf90=11
       integer(4)    :: nfilout=12
       integer(4)    :: i
!      *************************************************************************
       open(nfilin,file='gpawdata.in')
       open(nfilf90,file='dataout.f90')
       open(nfilout,file='dataout.dat')
       i=0
       do
         read(nfilin,fmt='(a)',end=1000)line
         line=adjustl(line)
         if(line(1:1).eq.'#') then
           write(nfilout,fmt='(a)')trim(line)
           line(1:1)=' '
           line=adjustl(line)
           write(nfilf90,fmt='("!",t7,"** ",a,t78,"**")')trim(line)
           cycle
         end if
         read(line,*)name,val
         call extractmolid(name,molid)
         name="'"//trim(adjustl(name))//"'"
         molid="'"//trim(molid)//"'"
         write(nfilout,fmt='(a30,a10,3f10.4)')name,molid,val
         i=i+1
         write(nfilf90,fmt='(t7,"name (",i3,")=",a)')i,trim(name)
         write(nfilf90,fmt='(t7,"id   (",i3,")=",a)')i,trim(molid)
         write(nfilf90,fmt='(t7,"egpaw(",i3,")=",f10.5,"d0*ev")')i,val(1)
         write(nfilf90,fmt='(t7,"evasp(",i3,")=",f10.5,"d0*ev")')i,val(2)
       enddo
1000   continue
       stop
       end
!
!      ..1.........2.........3.........4.........5.........6.........7.........8
       subroutine extractmolid(string1,string2)
       implicit none
       character(*),intent(in) :: string1
       character(*),intent(out):: string2
       character(80)           :: str
       integer(4)              :: n
       integer(4)              :: i,i2
!      *************************************************************************
       n=min(len(string1),len(string2))
       n=min(n,len(str))
!
!      == remove undesired letters =============================================
       i=0
       i2=0
       str=string1
       string2=' '
       do while (i.le.n-1)
         i=i+1
         if(str(i:i+3).eq.'\rm{') then
           i=i+3
           cycle
         end if
         if(str(i:i).eq.' ') cycle
         if(str(i:i).eq.'}') cycle
         if(str(i:i).eq.'{') cycle
         if(str(i:i).eq.'_') cycle
         if(str(i:i).eq.'^') cycle
         if(str(i:i).eq.')') cycle
         if(str(i:i).eq.'(') then
           i2=i2+1
           string2(i2:i2)='_'
           cycle
         end if
         i2=i2+1
         string2(i2:i2)=str(i:i)
       enddo
!
!      == clean up =============================================================
       if(string2(1:3).eq.'H3C') string2(1:3)='CH3'
       if(string2(1:4).eq.'HOOH') string2(1:4)='H2O2'
!
!      == make uppercase =======================================================
       do i=1,len(string2)
         i2=iachar(string2(i:i))
         if(i2.ge.97.and.i2.le.122) then
           string2(i:i)=achar(i2-32)
         end if
       enddo
       return
       end
