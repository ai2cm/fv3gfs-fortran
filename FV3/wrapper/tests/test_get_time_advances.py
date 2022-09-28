"""Example usage:

$ mpirun -n 6  \
     python3 -m mpi4py test_get_time.py noleap

Note the argument specifying the calendar type at the end of the command
is required.  Valid calendars are:
- julian
- noleap
- thirty_day
 """
import unittest
import os
import cftime
from datetime import timedelta
import fv3gfs.wrapper
from mpi4py import MPI
from util import get_default_config, main

test_dir = os.path.dirname(os.path.abspath(__file__))


class GetTimeAdvancesTests(unittest.TestCase):
    DATE_TYPE = None
    INIT_TIME = None
    TIMESTEP_SECONDS = None

    def __init__(self, *args, **kwargs):
        super(GetTimeAdvancesTests, self).__init__(*args, **kwargs)
        self.mpi_comm = MPI.COMM_WORLD

    def setUp(self):
        pass

    def tearDown(self):
        self.mpi_comm.barrier()
           
    def test_get_time_advances(self):
        fv3gfs.wrapper.step_dynamics()
        state = fv3gfs.wrapper.get_state(names=["time"])
        assert state["time"] == self.INIT_TIME
        fv3gfs.wrapper.compute_physics()
        state = fv3gfs.wrapper.get_state(names=["time"])
        assert state["time"] == self.INIT_TIME
        fv3gfs.wrapper.apply_physics()
        state = fv3gfs.wrapper.get_state(names=["time"])
        one_timestep_in = self.INIT_TIME + timedelta(seconds=self.TIMESTEP_SECONDS)
        assert state["time"] == one_timestep_in
        

def get_init_time(config):
    time_array = config['namelist']["coupler_nml"]['current_date']
    return cftime.DatetimeJulian(*time_array)


if __name__ == "__main__":
    config = get_default_config()
    GetTimeAdvancesTests.INIT_TIME = get_init_time(config)
    GetTimeAdvancesTests.TIMESTEP_SECONDS = config["namelist"]["coupler_nml"]["dt_atmos"]
    main(test_dir, config)
