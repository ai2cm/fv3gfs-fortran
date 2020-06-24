program test
    use m_countdown, only : cdstart, cdshow, cdstop
    use machine, only : kind_phys
    use m_satmedmfvdif, only : satmedmfvdif
    implicit none

    integer :: ix, im, km, ntrac, ntcw, ntiw, ntke
    integer, dimension(:), allocatable :: kpbl, kinver, kpbl_ref

    real (kind=kind_phys) :: delt, xkzm_m, xkzm_h, xkzm_s, xkzm_m_ref, xkzm_h_ref
    real (kind=kind_phys), dimension(:), allocatable :: xmu, garea, &
        psk, rbsoil, zorl, tsea, u10m, v10m, fm, fh, evap, heat, &
        stress, spd1, dusfc, dvsfc, dtsfc, dqsfc, hpbl, &
        dusfc_ref, dvsfc_ref, dtsfc_ref, dqsfc_ref, hpbl_ref
    real (kind=kind_phys), dimension(:, :), allocatable :: dv, du, tdt, &
        u1, v1, t1, swh, hlw, prsi, del, prsl, prslk, phii, phil, &
        dv_ref, du_ref, tdt_ref
    real (kind=kind_phys), dimension(:, :, :), allocatable :: rtg, q1, rtg_ref

    logical :: dspheat

    integer :: nunit, tile, num_tiles
    integer :: ser_count, ser_count_max
    character(len=6) :: ser_count_str

    nunit = 42
    ser_count_max = 10
    num_tiles = 6

    call cdstart(num_tiles * ser_count_max)

    do tile = 0, num_tiles - 1
    
    !$ser init directory='./data' prefix='Serialized' prefix_ref='Generator' mpi_rank=tile

    do ser_count = 0, ser_count_max

        call cdshow(tile * ser_count_max + ser_count)
        
        write(ser_count_str, '(i6.6)') ser_count

        !$ser mode read
        !$ser savepoint "satmedmfvdif-in-"//trim(ser_count_str)
        !$ser data ix=ix im=im km=km ntrac=ntrac ntcw=ntcw ntiw=ntiw
        !$ser data ntke=ntke

        allocate(kpbl(im), kinver(im), xmu(im), garea(im), psk(ix), &
            rbsoil(im), zorl(im), tsea(im), u10m(im), v10m(im), fm(im), &
            fh(im), evap(im), heat(im), stress(im), spd1(im), dusfc(im), &
            dvsfc(im), dtsfc(im), dqsfc(im), hpbl(im), dv(im, km), &
            du(im, km), tdt(im, km), u1(ix, km), v1(ix, km), t1(ix, km), &
            swh(ix, km), hlw(ix, km), prsi(ix, km+1), del(ix, km), prsl(ix, km), &
            prslk(ix, km), phii(ix, km+1), phil(ix, km), rtg(im, km, ntrac), &
            q1(ix, km, ntrac), &
            dv_ref(im, km), du_ref(im, km), tdt_ref(im, km), rtg_ref(im, km, ntrac), &
            kpbl_ref(im), dusfc_ref(im), dvsfc_ref(im), dtsfc_ref(im), dqsfc_ref(im), &
            hpbl_ref(im))

        ! input
        !$ser data u1=u1 v1=v1
        !$ser data t1=t1 q1=q1 swh=swh hlw=hlw xmu=xmu garea=garea psk=psk
        !$ser data rbsoil=rbsoil zorl=zorl u10m=u10m v10m=v10m fm=fm fh=fh
        !$ser data tsea=tsea heat=heat evap=evap stress=stress spd1=spd1
        !$ser data prsi=prsi del=del prsl=prsl prslk=prslk
        !$ser data phii=phii phil=phil delt=delt dspheat=dspheat
        !$ser data kinver=kinver xkzm_s=xkzm_s 
        ! inout
        !$ser data dv=dv du=du tdt=tdt rtg=rtg kpbl=kpbl 
        !$ser data dusfc=dusfc dvsfc=dvsfc dtsfc=dtsfc dqsfc=dqsfc
        !$ser data xkzm_m=xkzm_m xkzm_h=xkzm_h hpbl=hpbl

        call satmedmfvdif(ix, im, km, ntrac, ntcw, ntiw, ntke, &
            dv, du, tdt, rtg, u1, v1, t1, q1, swh, hlw, xmu, garea, &
            psk, rbsoil, zorl, u10m, v10m, fm, fh, &
            tsea, heat, evap, stress, spd1, kpbl, &
            prsi, del, prsl, prslk, phii, phil, delt,  &
            dspheat, dusfc, dvsfc, dtsfc, dqsfc, hpbl, &
            kinver, xkzm_m, xkzm_h, xkzm_s)

        !$ser mode read
        !$ser savepoint "satmedmfvdif-out-"//trim(ser_count_str)
        ! inout
        !$ser data dv=dv_ref du=du_ref tdt=tdt_ref rtg=rtg_ref kpbl=kpbl_ref
        !$ser data dusfc=dusfc_ref dvsfc=dvsfc_ref dtsfc=dtsfc_ref dqsfc=dqsfc_ref
        !$ser data xkzm_m=xkzm_m_ref xkzm_h=xkzm_h_ref hpbl=hpbl_ref

        if (any(dv /= dv_ref)) write(*,*) 'ERROR: dv does not validate', tile, ser_count
        if (any(du /= du_ref)) write(*,*) 'ERROR: du does not validate', tile, ser_count
        if (any(tdt /= tdt_ref)) write(*,*) 'ERROR: tdt does not validate', tile, ser_count
        if (any(rtg /= rtg_ref)) write(*,*) 'ERROR: rtg does not validate', tile, ser_count
        if (any(kpbl /= kpbl_ref)) write(*,*) 'ERROR: kpbl does not validate', tile, ser_count
        if (any(dusfc /= dusfc_ref)) write(*,*) 'ERROR: dusfc does not validate', tile, ser_count
        if (any(dvsfc /= dvsfc_ref)) write(*,*) 'ERROR: dvsfc does not validate', tile, ser_count
        if (any(dtsfc /= dtsfc_ref)) write(*,*) 'ERROR: dtsfc does not validate', tile, ser_count
        if (any(dqsfc /= dqsfc_ref)) write(*,*) 'ERROR: dqsfc does not validate', tile, ser_count
        if ((xkzm_m /= xkzm_m_ref)) write(*,*) 'ERROR: xkzm_m does not validate', tile, ser_count
        if ((xkzm_h /= xkzm_h_ref)) write(*,*) 'ERROR: xkzm_h does not validate', tile, ser_count
        if (any(hpbl /= hpbl_ref)) write(*,*) 'ERROR: hpbl does not validate', tile, ser_count

        deallocate(kpbl, kinver, xmu, garea, psk, &
            rbsoil, zorl, tsea, u10m, v10m, fm, &
            fh, evap, heat, stress, spd1, dusfc, &
            dvsfc, dtsfc, dqsfc, hpbl, dv, &
            du, tdt, u1, v1, t1, &
            swh, hlw, prsi, del, prsl, &
            prslk, phii, phil, rtg, &
            q1, &
            dv_ref, du_ref, tdt_ref, rtg_ref, &
            kpbl_ref, dusfc_ref, dvsfc_ref, dtsfc_ref, dqsfc_ref, &
            hpbl_ref)

    end do

    !ser cleanup

    end do

    call cdstop()

    write(*,*) 'FINISHED!'

end program test
