# 1 "four_to_grid_stochy.F"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/nix/store/9r0a3dipi8saq2zasp668zsk6qhqp5jb-glibc-2.32-48-dev/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "four_to_grid_stochy.F"
      module four_to_grid_mod

      use spectral_layout_mod, only: num_parthds_stochy => ompthreads

      implicit none

      contains

      subroutine four_to_grid(syn_gr_a_1,syn_gr_a_2,
     & lon_dim_coef,lon_dim_grid,lons_lat,lot)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine
      implicit none
!!
      real(kind=kind_dbl_prec) syn_gr_a_1(lon_dim_coef,lot)
      real(kind=kind_dbl_prec) syn_gr_a_2(lon_dim_grid,lot)
      integer lon_dim_coef
      integer lon_dim_grid
      integer lons_lat
      integer lot
!________________________________________________________



      real(kind=kind_dbl_prec) aux1crs(42002)
      real(kind=kind_dbl_prec) scale_ibm
      integer ibmsign
      integer init

      integer lot_thread
      integer num_threads
      integer nvar_thread_max
      integer nvar_1
      integer nvar_2
      integer thread




      external dcrft
      external scrft

!________________________________________________________
      num_threads = min(num_parthds_stochy,lot)

      nvar_thread_max = (lot+num_threads-1)/num_threads

      if ( kind_dbl_prec == 8 ) then !------------------------------------






!$omp parallel do shared(syn_gr_a_1,syn_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)

         do thread=1,num_threads ! start of thread loop ..............
           nvar_1=(thread-1)*nvar_thread_max + 1
           nvar_2=min(nvar_1+nvar_thread_max-1,lot)

           lot_thread=nvar_2 - nvar_1 + 1

           if (nvar_2 >= nvar_1) then
# 82 "four_to_grid_stochy.F"
             init = 1
             ibmsign = -1
             scale_ibm = 1.0d0

             call dcrft(init,
     & syn_gr_a_1(1,nvar_1) ,lon_dim_coef/2,
     & syn_gr_a_2(1,nvar_1) ,lon_dim_grid,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000)

             init = 0
             call dcrft(init,
     & syn_gr_a_1(1,nvar_1) ,lon_dim_coef/2,
     & syn_gr_a_2(1,nvar_1) ,lon_dim_grid,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000)

           endif

         enddo ! fin thread loop ......................................
      else !------------------------------------------------------------






!$omp parallel do shared(syn_gr_a_1,syn_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)

         do thread=1,num_threads ! start of thread loop ..............
            nvar_1 = (thread-1)*nvar_thread_max + 1
            nvar_2 = min(nvar_1+nvar_thread_max-1,lot)

              lot_thread = nvar_2 - nvar_1 + 1

            if (nvar_2 >= nvar_1) then
# 138 "four_to_grid_stochy.F"
              init = 1
              ibmsign = -1
              scale_ibm = 1.0d0
              call scrft(init,
     & syn_gr_a_1(1,nvar_1) ,lon_dim_coef/2,
     & syn_gr_a_2(1,nvar_1) ,lon_dim_grid,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000,
     & aux1crs(22001),0)
              init = 0
              call scrft(init,
     & syn_gr_a_1(1,nvar_1) ,lon_dim_coef/2,
     & syn_gr_a_2(1,nvar_1) ,lon_dim_grid,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000,
     & aux1crs(22001),0)

           endif
         enddo ! fin thread loop ......................................
      endif !-----------------------------------------------------------
!!
      return
      end
      subroutine grid_to_four(anl_gr_a_2,anl_gr_a_1,
     & lon_dim_grid,lon_dim_coef,lons_lat,lot)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine
      implicit none
!!
      real(kind=kind_dbl_prec) anl_gr_a_2(lon_dim_grid,lot)
      real(kind=kind_dbl_prec) anl_gr_a_1(lon_dim_coef,lot)
      integer lon_dim_grid
      integer lon_dim_coef
      integer lons_lat
      integer lot
!________________________________________________________
      real(kind=kind_dbl_prec) aux1crs(42002)
      real(kind=kind_dbl_prec) scale_ibm,rone
      integer ibmsign
      integer init
      integer lot_thread
      integer num_threads
      integer nvar_thread_max
      integer nvar_1,nvar_2
      integer thread
!________________________________________________________






      num_threads=min(num_parthds_stochy,lot)

      nvar_thread_max=(lot+num_threads-1)/num_threads

      if ( kind_dbl_prec == 8 ) then !------------------------------------
!$omp parallel do shared(anl_gr_a_1,anl_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm,rone)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)

         do thread=1,num_threads ! start of thread loop ..............
            nvar_1 = (thread-1)*nvar_thread_max + 1
            nvar_2 = min(nvar_1+nvar_thread_max-1,lot)

            if (nvar_2 >= nvar_1) then
              lot_thread = nvar_2 - nvar_1 + 1

              init = 1
              ibmsign = 1
              rone = 1.0d0
              scale_ibm = rone/lons_lat
              call drcft(init,
     & anl_gr_a_2(1,nvar_1), lon_dim_grid,
     & anl_gr_a_1(1,nvar_1), lon_dim_coef/2,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000)
              init = 0
              call drcft(init,
     & anl_gr_a_2(1,nvar_1), lon_dim_grid,
     & anl_gr_a_1(1,nvar_1), lon_dim_coef/2,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000)

            endif
         enddo ! fin thread loop ......................................
      else !------------------------------------------------------------
!$omp parallel do shared(anl_gr_a_1,anl_gr_a_2,lons_lat)
!$omp+shared(lon_dim_coef,lon_dim_grid)
!$omp+shared(lot,num_threads,nvar_thread_max)
!$omp+shared(ibmsign,scale_ibm,rone)
!$omp+private(thread,nvar_1,nvar_2,lot_thread,init,aux1crs)

         do thread=1,num_threads ! start of thread loop ..............
            nvar_1 = (thread-1)*nvar_thread_max + 1
            nvar_2 = min(nvar_1+nvar_thread_max-1,lot)

            if (nvar_2 >= nvar_1) then
              lot_thread=nvar_2 - nvar_1 + 1

              init = 1
              ibmsign = 1
              rone = 1.0d0
              scale_ibm = rone/lons_lat
              call srcft(init,
     & anl_gr_a_2(1,nvar_1), lon_dim_grid,
     & anl_gr_a_1(1,nvar_1), lon_dim_coef/2,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000,
     & aux1crs(22001),0)
              init = 0
              call srcft(init,
     & anl_gr_a_2(1,nvar_1), lon_dim_grid,
     & anl_gr_a_1(1,nvar_1), lon_dim_coef/2,
     & lons_lat,lot_thread,ibmsign,scale_ibm,
     & aux1crs,22000,
     & aux1crs(22001),20000,
     & aux1crs(22001),0)

            endif
         enddo ! fin thread loop ......................................
      endif !-----------------------------------------------------------
!!
      return
      end

      end module four_to_grid_mod
