import unittest
import os
import numpy as np
import fv3gfs.wrapper
from mpi4py import MPI
from util import (
    get_default_config,
    main,
)


test_dir = os.path.dirname(os.path.abspath(__file__))


class WindTransformationTests(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super(WindTransformationTests, self).__init__(*args, **kwargs)

    def setUp(self):
        pass

    def tearDown(self):
        MPI.COMM_WORLD.barrier()

    def test_transform_dgrid_winds_to_agrid_winds(self):
        fv3gfs.wrapper.step()
        state = fv3gfs.wrapper.get_state(
            ["x_wind", "y_wind", "eastward_wind", "northward_wind"]
        )
        x_wind_fortran = state["x_wind"]
        y_wind_fortran = state["y_wind"]
        u_fortran = state["eastward_wind"]
        v_fortran = state["northward_wind"]

        # We expect exact reproduction of the fortran here, since the wrapper wraps the same routine
        # that is used to convert the D-grid winds to the A-grid winds within the fortran model.
        u_wrapper, v_wrapper = fv3gfs.wrapper.transform_dgrid_winds_to_agrid_winds(
            x_wind_fortran, y_wind_fortran
        )
        np.testing.assert_equal(u_wrapper.view[:], u_fortran.view[:])
        np.testing.assert_equal(v_wrapper.view[:], v_fortran.view[:])

    def test_transform_agrid_winds_to_dgrid_winds(self):
        fv3gfs.wrapper.step()
        state = fv3gfs.wrapper.get_state(
            ["x_wind", "y_wind", "eastward_wind", "northward_wind"]
        )
        x_wind_fortran = state["x_wind"]
        y_wind_fortran = state["y_wind"]
        u_fortran = state["eastward_wind"]
        v_fortran = state["northward_wind"]

        # Here we do not expect an exact match, because the cubed_a2d subroutine is not an exact
        # inverse of the cubed_to_latlon subroutine.  Errors can actually be non-trivial, so the
        # tolerance must be large (30 m/s).  In general the cubed_a2d subroutine tends to produce
        # a smoother wind field than the native winds.
        #
        # TODO: to my eye the plotted transformed wind field looks reasonable, but the tolerance
        # required is surprisingly large.  We should check more carefully to make sure things
        # are operating as we expect here.
        (
            x_wind_wrapper,
            y_wind_wrapper,
        ) = fv3gfs.wrapper.transform_agrid_winds_to_dgrid_winds(u_fortran, v_fortran)
        np.testing.assert_allclose(
            x_wind_wrapper.view[:], x_wind_fortran.view[:], atol=30
        )
        np.testing.assert_allclose(
            y_wind_wrapper.view[:], y_wind_fortran.view[:], atol=30
        )


if __name__ == "__main__":
    config = get_default_config()
    main(test_dir, config)
