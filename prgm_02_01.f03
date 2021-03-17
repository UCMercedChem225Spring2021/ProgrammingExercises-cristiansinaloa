      Program prgm_02_01

      implicit none
      real:: m,l
      integer:: n1,n2
      integer:: ierror=1
      real,external:: PIB_1D_T_Element

      write(*,*)' What is the mass of the particle M? (must be an integer)'
      read(*,*) m
      if (m <= 0) then
        write(*,*)'The mass cannot be less than or equal to zero.'
        stop
      end if

      write(*,*)'What is the value of the lenght of the box L? (must be &
                  & an integer)'
      read(*,*) l

      if (l <= 0.) then
        write(*,*)'The box length cannot be less than or equal to zero.'
        stop
      end if

      write(*,*)'What is the value of the quantum number of the first & 
                  & eigenstate n1? (must be an integer)'
      read(*,*) n1

      write(*,*)'What is the value of the quantum number of the second &
                  & eigenstate n2? (must be an integer)'
      read(*,*) n2

 900 Format(1x, 'Kinetic Energy Matrix element', i5,',',i5,' is',f12.5,'.')
!
!     Do the printing
!     
      If (n1 == n2) then
        write(*,900) n1, n2, PIB_1D_T_Element(m,l,n1)
      else
        write(*,'(1x, a, i2, a, i2, a, a)') 'Kinetic Energy Matrix&
                  & element ', n1, ',', n2, ' is ', '0.'
      end if
!
      End Program prgm_02_01
!
!     (m,l,n1) = (a,b,c)
!
      real function PIB_1D_T_Element(a,b,c)
        implicit none
        real:: a,b
        real,parameter:: pi=4.*atan(1.0)
        integer:: c

        PIB_1D_T_Element =((c**2)*(pi**2))/(2*a*(b**2))
        return
!
      end function PIB_1D_T_Element
