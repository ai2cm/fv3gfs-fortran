!***************************************************************************
!
! Name: neuralphys
!
! Language: FORTRAN                           Type - MODULE
!
! Version: 1.0          Date: 03-25-20         
!     
!
! **************************************************************
!
! Module contains all subroutines require to initialize and 
! calculate ensemble of NNs for GFS model physics.
!
! **************************************************************
!
       module neuralphys

        use module_iounitdef, only : nonetf
        use machine,          only : kind_phys

        use omp_lib
                 
        implicit none 



        private
 
        public :: init_phys_nn_emulator, phys_nn_emulation 
                  
! Number of members in the NN ensemble

        integer, parameter     :: nn_num_of_members = 1
!
        real, parameter     :: pi = 3.1415927
! Files containing NN weights and biases

        character(*), parameter::nn_file_name(nn_num_of_members)= (/& 
             '/scratch1/NCEPDEV/global/Alexei.A.Belochitski/data/Fphys-506-250-304-2.asc' /)


 ! Internal types and variables

        type nndata_1d
           real, allocatable :: a(:)
        end type nndata_1d

        type nndata_2d
           real, allocatable :: a(:,:)
        end type nndata_2d

! NN Hidden and output weights
        type(nndata_2d) :: nn_w1(nn_num_of_members),nn_w2(nn_num_of_members)
! NN Hidden and output biases
        type(nndata_1d) :: nn_b1(nn_num_of_members),nn_b2(nn_num_of_members)
! NN Number of inputs, hidden neurons and outputs
        integer      :: nn_in(nn_num_of_members),nn_hid(nn_num_of_members),  & 
                        nn_out(nn_num_of_members)

      contains
!
! Initialize TMP NNs


        subroutine read_namelist(path, online)
            use fms_mod, only: file_exist, open_namelist_file, check_nml_error, close_file
            character(len=*), intent(out) :: path
            logical, intent(inout) :: online

            integer status
            integer :: unit, io, ierr

            namelist /neuralphys/ path, online

            if ( file_exist('input.nml')) then
                unit = open_namelist_file( )
                ierr=1
                do while (ierr /= 0)
                    read  (unit, nml=neuralphys, iostat=io, end=10)
                    ierr = check_nml_error(io,'neuralphys')
                enddo
            10 call close_file (unit)           ! Open and read Namelist file.
            else
                stop -1
            endif
        end subroutine
        
        subroutine init_phys_nn_emulator(me, online) 
!
! --- This subroutine initializes NN ensemble, i.e. reads NNs coefficients
!
!   
          integer, intent(in) ::  me
          logical, intent(inout) :: online

          integer iin,ihid,iout,member
          character(len=128) :: nn_file_name(1), path

          call read_namelist(path, online)
          nn_file_name(1) = trim(path)

!
          if (me == 0) print*,'Module NEURALPHYS: Number of NN ensemble members:', &
                               nn_num_of_members

! Load NNs weights and biases

          do member=1,nn_num_of_members

             open(unit=nonetf,err=10,file=trim(nn_file_name(member)),status='old')

             read(nonetf,'(3i5)') iin,ihid,iout
             nn_in(member)=iin; nn_out(member)=iout; nn_hid(member)=ihid 
             
             allocate(nn_w1(member)%a(iin,ihid),nn_w2(member)%a(ihid,iout))
             allocate(nn_b1(member)%a(ihid),nn_b2(member)%a(iout)) 

             read(nonetf,*,err=11,end=12) nn_w1(member)%a, nn_w2(member)%a, &
                                          nn_b1(member)%a, nn_b2(member)%a  
             close(nonetf) 
 
             if (me == 0)  print*,'Module NEURALPHYS: NN File Loaded: ', & 
                                   nn_file_name(member)
  
          end do

! All NNs in the ensemble must have the same number inputs and outputs 

          if (.not.all(nn_in == nn_in(1)).or..not.all(nn_out == nn_out(1))) then
             if (me == 0) print *,  "Module NEURALPHYS: NN ensemble members have different number of inputs and/or outputs. Exiting."
             stop
          endif

          return        

! Catch file opening/reading errors

10        if (me == 0) print *, "Module NEURALPHYS: Error opening file ",    & 
                                nn_file_name(member), ". Exiting."
          stop

11        if (me == 0) print *, "Module NEURALPHYS: Error reading file ",    &
                                nn_file_name(member), ". Exiting."
          stop

12        if (me == 0) print *, "Module NEURALPHYS: Reached EOF too early ", &
                                nn_file_name(member), ". Exiting."
          stop 
        
        end subroutine init_phys_nn_emulator

! TMP emulator

        subroutine  phys_nn_emulation(                                     & 
!  Inputs:
! Prognostic variables:
     &     pgr,phil,prsl,ugrs,vgrs,vvl,tgrs,shm,cwmr,ozmr, 	&
! Surface variables:
     &     smc,slc,stc,tskin,canopy,hice,weasd,  &
! Metavariables:
     &     ftime,doy,mon,glon,glat,cosz,solcon,  &
! Outputs:
! Prognostic variables:
     &     gu0,gv0,gt0,oshm,ocwmr,oozmr, &
! Surface variables:
     &     osmc,oslc,ostc,otskin,ocanopy,ohice,oweasd)


! Inputs
!          integer,  intent(in) :: jday

          real, intent(in) :: tskin,canopy,hice,weasd,ftime,doy,mon,glat,glon,cosz,pgr,solcon
          real, intent(in):: phil(:),prsl(:),ugrs(:),vgrs(:),vvl(:),tgrs(:), &
     &    shm(:),cwmr(:),ozmr(:),smc(:),slc(:),stc(:)
! Outputs     
          real, intent(out):: gu0(:),gv0(:),gt0(:),oshm(:),ocwmr(:),oozmr(:),  &
     &    osmc(:),oslc(:),ostc(:)
          real, intent(out):: otskin,ocanopy,ohice,oweasd  
!
! Local variables
          real  nn_input_vector(nn_in(1)),  nn_output_vector(nn_out(1)) 

!             
! Create NN input vector:
!        


          nn_input_vector(1)  =           cos(2.* pi * doy/366.)  
          nn_input_vector(2) =            sin(2.* pi * doy/366.)
          nn_input_vector(3) =            cos(2.* pi * mon/12.) 
          nn_input_vector(4) =            sin(2.* pi * mon/12.)
          nn_input_vector(5) =            glat
          nn_input_vector(6) =            cos(glon)
          nn_input_vector(7) =            sin(glon)
          nn_input_vector(8) =            cosz 
          nn_input_vector(9:72) =        phil         ! layer geopotential height
          nn_input_vector(73:136) =       prsl         ! layer pressure
          nn_input_vector(137) =          pgr          ! surface pressure 
          nn_input_vector(138:201) =      ugrs         ! u component of layer wind
          nn_input_vector(202:265) =      vgrs         ! v component of layer wind
          nn_input_vector(266:329) =      vvl          ! layer mean vertical velocity
          nn_input_vector(330:393) =      tgrs         ! layer mean temperature
          nn_input_vector(394:430) =      shm(1:37)    ! specific humidity
          nn_input_vector(431:473) =      cwmr(1:43)   ! cloud water mixing ratio
          nn_input_vector(474:505) =      ozmr(33:64)  ! ozone mixing ratio
          nn_input_vector(506) =          solcon       ! solar constant

!             
! Call NN computation                     
          call compute_nn(nn_input_vector,nn_output_vector,nn_num_of_members,& 
                  nn_w1,nn_w2,nn_b1,nn_b2,nn_hid)
!
! Unpack NN output vector
          gu0 	       = nn_output_vector(1:64) + ugrs   ! u component of layer wind
          gv0          = nn_output_vector(65:128) + vgrs ! v component of layer wind
          gt0          = nn_output_vector(129:192) + tgrs! layer mean temperature 
!          where (nn_output_vector(193:304) < 0.) nn_output_vector(193:304) = 0.
          oshm(1:37)   = nn_output_vector(193:229)
          oshm(38:64)  = 0.
          oshm         = oshm + shm                      ! specific humidity
          ocwmr(1:43)  = nn_output_vector(230:272) 
          ocwmr(44:64) = 0. 
          ocwmr        = ocwmr + cwmr                    ! cloud water mixing ratio
          oozmr(33:64) = nn_output_vector(273:304)    
          oozmr(1:32)  = 0.
          oozmr        = oozmr + ozmr                    ! ozone mixing ratio
!
        end subroutine phys_nn_emulation 
        
        subroutine  compute_nn(X,Y,num_of_members,w1,w2,b1,b2,nhid)

      
 !  Input:
 !            X(IN) NN input vector 
          integer, intent(in) :: num_of_members,nhid(num_of_members)
          real, intent(in)::X(:)
          type(nndata_2d), intent(in) :: w1(num_of_members),w2(num_of_members)
          type(nndata_1d), intent(in) :: b1(num_of_members),b2(num_of_members)
         

 !   Ouput:
 !            Y(OUT) NN output vector (composition coefficients for SNN)

          real, intent(out):: Y(:)

! Local variables 
          integer i, nout
          real, allocatable :: x2(:),x3(:)
          integer member

          nout=size(Y)

          Y = 0.

          allocate(x3(nout)) 

          do member = 1,num_of_members
  
             allocate(x2(nhid(member))) 

! Calculate neurons in the hidden layer

!             forall(i = 1:nhid(member)) x2(i)= tanh(sum(X*w1(member)%a(:,i))+  & 
!                                               b1(member)%a(i))

!$OMP PARALLEL default (shared) private (i)                                                                           
!$OMP DO                                                                                                              
             do i = 1,nhid(member)

                x2(i)= tanh(sum(X*w1(member)%a(:,i)) + b1(member)%a(i))

             enddo
!$OMP END DO                                                                                                          
!$OMP END PARALLEL 

! Calculate NN output 

!             forall(i=1:nout) x3(i)= sum(w2(member)%a(:,i)*x2) + b2(member)%a(i)

!$OMP PARALLEL default (shared) private (i)                                                                           
!$OMP DO                                                                                                              
             do i=1,nout

                x3(i)= sum(w2(member)%a(:,i)*x2) + b2(member)%a(i)

             enddo
!$OMP END DO                                                                                                          
!$OMP END PARALLEL 

             Y = Y + x3 
             
             deallocate(x2)
         
          end do                    ! member

          deallocate(x3)
      
          Y = Y / num_of_members
         
    end  subroutine  compute_nn                 

  end module neuralphys




