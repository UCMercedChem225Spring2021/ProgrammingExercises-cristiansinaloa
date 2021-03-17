      Program prgm_02_02

      implicit none
      real:: m,l,b
      integer:: n1,n2
      integer:: ierror=1
      real,external:: PIB_1D_T_Element
      real,external:: PIB_1D_Modified_V_Element

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
      n2=n1

      write(*,*)'What is the value of b for the potential V(x)=bx? &
                  &(must be a real)'
      read(*,*) b

 900 Format(1x, 'Kinetic Energy Matrix element', i5,',',i5,' is',f12.5,'.')
 1000 Format(1x, 'Potential Energy Matrix element', i5,',',i5,' is',f12.5,'.')
!
!     Do the printing
!     
      If (n1 == n2) then
        write(*,900) n1, n2, PIB_1D_T_Element(m,l,n1)
        write(*,1000) n1, n2, PIB_1D_Modified_V_Element(b,l)
      else
        write(*,'(1x, a, i2, a, i2, a, a)') 'Kinetic Energy Matrix&
                  & element ', n1, ',', n2, ' is ', '0.'
        write(*,'(1x, a, i2, a, i2, a, a)') 'Potential Energy Matrix&
                  & element ', n1, ',', n2, ' is ', '0.'
      end if
!
      End Program prgm_02_02
!
!     (m,l,n1) = (a,b,c)
!
      real function PIB_1D_T_Element(a,b,c)
        implicit none
        real:: a,b
        real,parameter:: pi=4.0*atan(1.0)
        integer:: c

        PIB_1D_T_Element =((c**2)*(pi**2))/(2*a*(b**2))
        return
!
      end function PIB_1D_T_Element
!
      real function PIB_1D_Modified_V_Element(a,b)
        implicit none
        real:: a,b

        PIB_1D_Modified_V_Element = (a*b)/2.0 
        return
!
      end function PIB_1D_Modified_V_Element
