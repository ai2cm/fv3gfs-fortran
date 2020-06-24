program test
    use m_countdown, only : cdstart, cdshow, cdstop
    use machine, only : kind_phys
    use gfdl_cloud_microphys_mod, only : gfdl_cloud_microphys_init, &
        gfdl_cloud_microphys_driver
    implicit none

    integer :: iie, kke, kbot, seconds

    real (kind=kind_phys) :: dt_in
    real (kind=kind_phys), dimension(:, :), allocatable :: area, land, &
        rain, snow, ice, graupel, rain_ref, snow_ref, ice_ref, graupel_ref
    real (kind=kind_phys), dimension(:, :, :), allocatable :: delp, dz, &
        uin, vin, p, pt, qv, ql, qr, qg, qa, qn, qi, qs, pt_dt, qa_dt, &
        udt, vdt, w, qv_dt, ql_dt, qr_dt, qi_dt, qs_dt, qg_dt, refl_10cm, &
        qi_ref, qs_ref, qv_dt_ref, ql_dt_ref, qr_dt_ref, qi_dt_ref, &
        qs_dt_ref, qg_dt_ref, qa_dt_ref, pt_dt_ref, w_ref, udt_ref, vdt_ref, &
        refl_10cm_ref

    logical :: lradar, reset

    integer :: nunit, tile, num_tiles
    integer :: ser_count, ser_count_max
    character(len=100) :: nml_file
    character(len=6) :: ser_count_str

    nunit = 42
    nml_file = 'input.nml'
    ser_count_max = 10
    num_tiles = 6

    call cdstart(num_tiles * ser_count_max)

    call gfdl_cloud_microphys_init(0, 0, nunit, (/''/), 6, nml_file)

    do tile = 0, num_tiles - 1
    
    !$ser init directory='./data' prefix='Serialized' prefix_ref='Generator' mpi_rank=tile

    do ser_count = 0, ser_count_max

        call cdshow(tile * ser_count_max + ser_count)
        
        write(ser_count_str, '(i6.6)') ser_count

        !$ser mode read
        !$ser savepoint "cloud_mp-in-"//trim(ser_count_str)
        !$ser data iie=iie kke=kke kbot=kbot

        allocate(area(iie, 1), land(iie, 1), rain(iie, 1), snow(iie, 1), &
            ice(iie, 1), graupel(iie, 1), delp(iie, 1, kke), dz(iie, 1, kke), &
            uin(iie, 1, kke), vin(iie, 1, kke), p(iie, 1, kke), pt(iie, 1, kke), &
            qv(iie, 1, kke), ql(iie, 1, kke), qr(iie, 1, kke), qg(iie, 1, kke), &
            qa(iie, 1, kke), qn(iie, 1, kke), qi(iie, 1, kke), qs(iie, 1, kke), &
            pt_dt(iie, 1, kke), qa_dt(iie, 1, kke), udt(iie, 1, kke), vdt(iie, 1, kke), &
            w(iie, 1, kke), qv_dt(iie, 1, kke), ql_dt(iie, 1, kke), qr_dt(iie, 1, kke), &
            qi_dt(iie, 1, kke), qs_dt(iie, 1, kke), qg_dt(iie, 1, kke), refl_10cm(iie, 1, kke), &
            qi_ref(iie, 1, kke), qs_ref(iie, 1, kke), qv_dt_ref(iie, 1, kke), &
            ql_dt_ref(iie, 1, kke), qr_dt_ref(iie, 1, kke), qi_dt_ref(iie, 1, kke), &
            qs_dt_ref(iie, 1, kke), qg_dt_ref(iie, 1, kke), qa_dt_ref(iie, 1, kke), &
            pt_dt_ref(iie, 1, kke), w_ref(iie, 1, kke), udt_ref(iie, 1, kke), vdt_ref(iie, 1, kke), &
            rain_ref(iie, 1), snow_ref(iie, 1), ice_ref(iie, 1), graupel_ref(iie, 1), &
            refl_10cm_ref(iie, 1, kke))

        ! input
        !$ser data qv=qv ql=ql qr=qr qg=qg qa=qa qn=qn
        !$ser data pt=pt uin=uin vin=vin dz=dz delp=delp
        !$ser data area=area dt_in=dt_in land=land
        !$ser data seconds=seconds p=p lradar=lradar
        !$ser data reset=reset
        ! inout
        !$ser data qi=qi qs=qs qv_dt=qv_dt
        !$ser data ql_dt=ql_dt qr_dt=qr_dt qi_dt=qi_dt qs_dt=qs_dt qg_dt=qg_dt qa_dt=qa_dt
        !$ser data pt_dt=pt_dt w=w udt=udt vdt=vdt
        !$ser data rain=rain snow=snow ice=ice graupel=graupel
        !$ser data refl_10cm=refl_10cm

        call gfdl_cloud_microphys_driver (qv, ql, qr, qi, qs, qg, qa, qn,     &
            qv_dt, ql_dt, qr_dt, qi_dt, qs_dt, qg_dt, qa_dt, pt_dt, pt, w,    &
            uin, vin, udt, vdt, dz, delp, area, dt_in, land, rain, snow, ice, &
            graupel, .false., .true., 1, iie, 1, 1, 1,                        &
            kke, 1, kbot, seconds, p, lradar, refl_10cm, reset)

        !$ser mode read
        !$ser savepoint "cloud_mp-out-"//trim(ser_count_str)
        ! inout
        !$ser data qi=qi_ref qs=qs_ref qv_dt=qv_dt_ref
        !$ser data ql_dt=ql_dt_ref qr_dt=qr_dt_ref qi_dt=qi_dt_ref qs_dt=qs_dt_ref qg_dt=qg_dt_ref qa_dt=qa_dt_ref
        !$ser data pt_dt=pt_dt_ref w=w_ref udt=udt_ref vdt=vdt_ref
        !$ser data rain=rain_ref snow=snow_ref ice=ice_ref graupel=graupel_ref
        !$ser data refl_10cm=refl_10cm_ref

        if (any(qi /= qi_ref)) write(*,*) 'ERROR: qi does not validate', tile, ser_count
        if (any(qs /= qs_ref)) write(*,*) 'ERROR: qs does not validate', tile, ser_count
        if (any(qv_dt /= qv_dt_ref)) write(*,*) 'ERROR: qv_dt does not validate', tile, ser_count
        if (any(ql_dt /= ql_dt_ref)) write(*,*) 'ERROR: ql_dt does not validate', tile, ser_count
        if (any(qr_dt /= qr_dt_ref)) write(*,*) 'ERROR: qr_dt does not validate', tile, ser_count
        if (any(qi_dt /= qi_dt_ref)) write(*,*) 'ERROR: qi_dt does not validate', tile, ser_count
        if (any(qs_dt /= qs_dt_ref)) write(*,*) 'ERROR: qs_dt does not validate', tile, ser_count
        if (any(qg_dt /= qg_dt_ref)) write(*,*) 'ERROR: qg_dt does not validate', tile, ser_count
        if (any(qa_dt /= qa_dt_ref)) write(*,*) 'ERROR: qa_dt does not validate', tile, ser_count
        if (any(pt_dt /= pt_dt_ref)) write(*,*) 'ERROR: pt_dt does not validate', tile, ser_count
        if (any(w /= w_ref)) write(*,*) 'ERROR: w does not validate', tile, ser_count
        if (any(udt /= udt_ref)) write(*,*) 'ERROR: udt does not validate', tile, ser_count
        if (any(vdt /= vdt_ref)) write(*,*) 'ERROR: vdt does not validate', tile, ser_count
        if (any(rain /= rain_ref)) write(*,*) 'ERROR: rain does not validate', tile, ser_count
        if (any(snow /= snow_ref)) write(*,*) 'ERROR: snow does not validate', tile, ser_count
        if (any(ice /= ice_ref)) write(*,*) 'ERROR: ice does not validate', tile, ser_count
        if (any(graupel /= graupel_ref)) write(*,*) 'ERROR: graupel does not validate', tile, ser_count
        if (any(refl_10cm /= refl_10cm_ref)) write(*,*) 'ERROR: refl_10cm does not validate', tile, ser_count

        deallocate(area, land, rain, snow, ice, graupel, delp, dz, uin, vin, &
            p, pt, qv, ql, qr, qg, qa, qn, qi, qs, pt_dt, qa_dt, udt, vdt, w, &
            qv_dt, ql_dt, qr_dt, qi_dt, qs_dt, qg_dt, refl_10cm, qi_ref, &
            qs_ref, qv_dt_ref, ql_dt_ref, qr_dt_ref, qi_dt_ref, &
            qs_dt_ref, qg_dt_ref, qa_dt_ref, pt_dt_ref, w_ref, udt_ref, vdt_ref, &
            rain_ref, snow_ref, ice_ref, graupel_ref, &
            refl_10cm_ref)

    end do

    !ser cleanup

    end do

    call cdstop()

    write(*,*) 'VALIDATION SUCCESSFULL!'

end program test
