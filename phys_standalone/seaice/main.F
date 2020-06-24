program test
    use m_countdown, only : cdstart, cdshow, cdstop
    use machine, only : kind_phys
    use module_sfc_sice, only : sfc_sice
    implicit none

    integer :: im, km, ipr
    integer, dimension(:), allocatable :: islimsk

    real (kind=kind_phys) :: delt, cimin
    real (kind=kind_phys), dimension(:), allocatable :: ps, t1, q1, &
        sfcemis, dlwflx, sfcnsw, sfcdsw, srflag, cm, ch, prsl1, prslki, &
        wind, hice, fice, tice, weasd, tskin, tprcp, ep, snwdph, &
        qsurf, snowmt, gflux, cmm, chh, evap, hflx, &
        hice_ref, fice_ref, tice_ref, weasd_ref, tskin_ref, tprcp_ref, &
        ep_ref, snwdph_ref, qsurf_ref, snowmt_ref, gflux_ref, cmm_ref, &
        chh_ref, evap_ref, hflx_ref
    real (kind=kind_phys), dimension(:, :), allocatable :: stc, stc_ref

    logical :: lprnt
    logical, dimension(:), allocatable :: flag_iter

    integer :: iter
    integer :: tile, num_tiles
    integer :: ser_count, ser_count_max
    character(len=6) :: ser_count_str

    ser_count_max = 10
    num_tiles = 6

    call cdstart(num_tiles * ser_count_max * 2)

    do tile = 0, num_tiles - 1

    !$ser init directory='./data' prefix='Serialized' prefix_ref='Generator' mpi_rank=tile

    do ser_count = 0, ser_count_max
    do iter = 1, 2

        call cdshow(tile * ser_count_max + ser_count * 2 + iter - 1)
        
        write(ser_count_str, '(i6.6)') ser_count

        !$ser mode read
        !$ser verbatim if (iter == 1) then
        !$ser savepoint "sfc_sice-in-iter1-"//trim(ser_count_str)
        !$ser verbatim else
        !$ser savepoint "sfc_sice-in-iter2-"//trim(ser_count_str)
        !$ser verbatim end if
        !$ser data im=im km=km ipr=ipr lprnt=lprnt

        allocate(ps(im), t1(im), q1(im), sfcemis(im), dlwflx(im), sfcnsw(im), &
            sfcdsw(im), srflag(im), cm(im), ch(im), prsl1(im), prslki(im), &
            wind(im), hice(im), fice(im), tice(im), weasd(im), tskin(im), &
            tprcp(im), ep(im), snwdph(im), qsurf(im), snowmt(im), gflux(im), &
            cmm(im), chh(im), evap(im), hflx(im), stc(im, km), flag_iter(im), &
            islimsk(im), &
            hice_ref(im), fice_ref(im), tice_ref(im), weasd_ref(im), tskin_ref(im), &
            tprcp_ref(im), stc_ref(im, km), ep_ref(im), snwdph_ref(im), &
            qsurf_ref(im), snowmt_ref(im), gflux_ref(im), cmm_ref(im), &
            chh_ref(im), evap_ref(im), hflx_ref(im))

        ! input
        !$ser data ps=ps t1=t1 q1=q1 delt=delt sfcemis=sfcemis dlwflx=dlwflx
        !$ser data sfcnsw=sfcnsw sfcdsw=sfcdsw srflag=srflag cm=cm ch=ch
        !$ser data prsl1=prsl1 prslki=prslki islimsk=islimsk wind=wind
        !$ser data flag_iter=flag_iter cimin=cimin
        ! inout
        !$ser data hice=hice fice=fice tice=tice weasd=weasd
        !$ser data tskin=tskin tprcp=tprcp stc=stc ep=ep
        !$ser data snwdph=snwdph qsurf=qsurf snowmt=snowmt
        !$ser data gflux=gflux cmm=cmm chh=chh evap=evap hflx=hflx

        call sfc_sice                                                  &
            !  ---  inputs:
            ( im, km, ps, t1, q1, delt,                                &
            sfcemis, dlwflx, sfcnsw, sfcdsw, srflag,                   &
            cm, ch, prsl1, prslki, islimsk, wind,                      &
            flag_iter, lprnt, ipr, cimin,                              &
            !  ---  input/outputs:
            hice, fice, tice, weasd, tskin, tprcp, stc, ep,            &
            snwdph, qsurf, snowmt, gflux, cmm, chh, evap, hflx )
                
        !$ser mode read
        !$ser verbatim if (iter == 1) then
        !$ser savepoint "sfc_sice-out-iter1-"//trim(ser_count_str)
        !$ser verbatim else
        !$ser savepoint "sfc_sice-out-iter2-"//trim(ser_count_str)
        !$ser verbatim end if
        ! inout
        !$ser data hice=hice_ref fice=fice_ref tice=tice_ref weasd=weasd_ref
        !$ser data tskin=tskin_ref tprcp=tprcp_ref stc=stc_ref ep=ep_ref
        !$ser data snwdph=snwdph_ref qsurf=qsurf_ref snowmt=snowmt_ref
        !$ser data gflux=gflux_ref cmm=cmm_ref chh=chh_ref evap=evap_ref hflx=hflx_ref

        if (any(hice /= hice_ref)) write(*,*) 'ERROR: hice does not validate', tile, ser_count, iter
        if (any(fice /= fice_ref)) write(*,*) 'ERROR: fice does not validate', tile, ser_count, iter
        if (any(tice /= tice_ref)) write(*,*) 'ERROR: tice does not validate', tile, ser_count, iter
        if (any(weasd /= weasd_ref)) write(*,*) 'ERROR: weasd does not validate', tile, ser_count, iter
        if (any(tskin /= tskin_ref)) write(*,*) 'ERROR: tskin does not validate', tile, ser_count, iter
        if (any(tprcp /= tprcp_ref)) write(*,*) 'ERROR: tprcp does not validate', tile, ser_count, iter
        if (any(stc /= stc_ref)) write(*,*) 'ERROR: stc does not validate', tile, ser_count, iter
        if (any(ep /= ep_ref)) write(*,*) 'ERROR: ep does not validate', tile, ser_count, iter
        if (any(snwdph /= snwdph_ref)) write(*,*) 'ERROR: snwdph does not validate', tile, ser_count, iter
        if (any(qsurf /= qsurf_ref)) write(*,*) 'ERROR: qsurf does not validate', tile, ser_count, iter
        if (any(snowmt /= snowmt_ref)) write(*,*) 'ERROR: snowmt does not validate', tile, ser_count, iter
        if (any(gflux /= gflux_ref)) write(*,*) 'ERROR: gflux does not validate', tile, ser_count, iter
        if (any(cmm /= cmm_ref)) write(*,*) 'ERROR: cmm does not validate', tile, ser_count, iter
        if (any(chh /= chh_ref)) write(*,*) 'ERROR: chh does not validate', tile, ser_count, iter
        if (any(evap /= evap_ref)) write(*,*) 'ERROR: evap does not validate', tile, ser_count, iter
        if (any(hflx /= hflx_ref)) write(*,*) 'ERROR: hflx does not validate', tile, ser_count, iter
        
        deallocate(ps, t1, q1, sfcemis, dlwflx, sfcnsw, &
            sfcdsw, srflag, cm, ch, prsl1, prslki, &
            wind, hice, fice, tice, weasd, tskin, &
            tprcp, ep, snwdph, qsurf, snowmt, gflux, &
            cmm, chh, evap, hflx, stc, flag_iter, &
            islimsk, &
            hice_ref, fice_ref, tice_ref, weasd_ref, tskin_ref, &
            tprcp_ref, stc_ref, ep_ref, snwdph_ref, &
            qsurf_ref, snowmt_ref, gflux_ref, cmm_ref, &
            chh_ref, evap_ref, hflx_ref)

    end do
    end do

    !ser cleanup

    end do

    call cdstop()

    write(*,*) 'VALIDATION SUCCESSFULL!'

end program test
