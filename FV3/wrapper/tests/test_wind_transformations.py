import unittest
import os
import numpy as np
import pytest
import fv3gfs.wrapper
from copy import deepcopy
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

        # We expect exact reproduction of the fortran here, since the wrapper
        # wraps the same routine that is used to convert the D-grid winds to the
        # A-grid winds within the fortran model.
        u_wrapper, v_wrapper = fv3gfs.wrapper.transform_dgrid_winds_to_agrid_winds(
            x_wind_fortran, y_wind_fortran
        )
        np.testing.assert_equal(u_wrapper.view[:], u_fortran.view[:])
        np.testing.assert_equal(v_wrapper.view[:], v_fortran.view[:])

        assert u_wrapper.units == "m/s"
        assert v_wrapper.units == "m/s"

    def test_transform_agrid_winds_to_dgrid_winds(self):
        # This test mimics the wind updating procedure from the
        # atmosphere_state_update subroutine in the atmosphere.F90 module, but
        # uses the Python wrapped cubed_a2d subroutine to convert the physics
        # wind increments from the A to the D grid.

        fv3gfs.wrapper.step_dynamics()
        fv3gfs.wrapper.compute_physics()
        pre_applied_physics_state = fv3gfs.wrapper.get_state(
            [
                "x_wind",
                "y_wind",
                "eastward_wind_before_physics",
                "northward_wind_before_physics",
                "eastward_wind_after_physics",
                "northward_wind_after_physics",
            ]
        )
        x_wind_before_physics = pre_applied_physics_state["x_wind"]
        y_wind_before_physics = pre_applied_physics_state["y_wind"]

        u_before_physics = pre_applied_physics_state["eastward_wind_before_physics"]
        u_after_physics = pre_applied_physics_state["eastward_wind_after_physics"]

        v_before_physics = pre_applied_physics_state["northward_wind_before_physics"]
        v_after_physics = pre_applied_physics_state["northward_wind_after_physics"]

        # Note that 3D variables from the physics component of the model have
        # their vertical dimension flipped with respect to 3D variables from the
        # dynamical core.  Therefore we need to flip the vertical dimension of
        # these A-grid wind increments so that when we convert them to D-grid
        # wind increments, they align with the D-grid winds in the dynamical
        # core.
        #
        # deepcopy calls here are used out of convenience to construct Quantity
        # objects of the same shape and metadata as others.  Their data is
        # overwritten immediately.
        u_increment = deepcopy(u_after_physics)
        u_increment.view[:] = u_after_physics.view[:] - u_before_physics.view[:]
        u_increment.view[:] = u_increment.view[:][::-1, :, :]

        v_increment = deepcopy(v_after_physics)
        v_increment.view[:] = v_after_physics.view[:] - v_before_physics.view[:]
        v_increment.view[:] = v_increment.view[:][::-1, :, :]

        (
            x_wind_physics_increment,
            y_wind_physics_increment,
        ) = fv3gfs.wrapper.transform_agrid_winds_to_dgrid_winds(
            u_increment, v_increment
        )

        assert x_wind_physics_increment.units == "m/s"
        assert y_wind_physics_increment.units == "m/s"

        fv3gfs.wrapper.apply_physics()
        updated_dynamical_core_state = fv3gfs.wrapper.get_state(["x_wind", "y_wind"])
        x_wind_after_physics = updated_dynamical_core_state["x_wind"]
        y_wind_after_physics = updated_dynamical_core_state["y_wind"]

        np.testing.assert_allclose(
            x_wind_before_physics.view[:] + x_wind_physics_increment.view[:],
            x_wind_after_physics.view[:],
        )
        np.testing.assert_allclose(
            y_wind_before_physics.view[:] + y_wind_physics_increment.view[:],
            y_wind_after_physics.view[:],
        )

    def test_transform_dgrid_winds_to_agrid_winds_invalid_units(self):
        state = fv3gfs.wrapper.get_state(["x_wind", "y_wind"])
        x_wind = state["x_wind"]
        y_wind = state["y_wind"]
        x_wind.metadata.units = "m/s/s"

        with pytest.raises(ValueError, match="Input wind components"):
            fv3gfs.wrapper.transform_dgrid_winds_to_agrid_winds(x_wind, y_wind)

    def test_transform_agrid_winds_to_dgrid_winds_invalid_units(self):
        state = fv3gfs.wrapper.get_state(["eastward_wind", "northward_wind"])
        eastward_wind = state["eastward_wind"]
        northward_wind = state["northward_wind"]
        eastward_wind.metadata.units = "m/s/s"

        with pytest.raises(ValueError, match="Input wind components"):
            fv3gfs.wrapper.transform_agrid_winds_to_dgrid_winds(
                eastward_wind, northward_wind
            )


if __name__ == "__main__":
    config = get_default_config()
    # Deactivate fv_subgrid_z for these tests.  Due to how the
    # atmosphere_state_update subroutine is implemented in the atmosphere.F90
    # module, the A-grid wind tendencies from the fv_subgrid_z subroutine are
    # lumped into the A-grid wind tendencies from the physics.  We do not
    # currently have easy access to the fv_subgrid_z tendency from the wrapper,
    # and rather than implement that for the sake of the A-grid to D-grid
    # transformation test, it is easier to simply turn it off. This way the
    # A-grid wind tendency that is applied in the atmosphere_state_update
    # subroutine comes only from the difference in the A-grid winds at the start
    # of the physics and at the end of the physics, which is something that we
    # can easily compute using existing wrapper infrastructure.
    config["namelist"]["fv_core_nml"]["fv_sg_adj"] = -1
    main(test_dir, config)
