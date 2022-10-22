import unittest
import os
from mpi4py import MPI
import cftime
import fv3gfs.wrapper
from util import get_from_restarts_config, main

test_dir = os.path.dirname(os.path.abspath(__file__))


class GetInitializationTimeTests(unittest.TestCase):

    # these are the values in the restart file coupler.res used to initialize
    RUN_INITIALIZATION_TIME = cftime.DatetimeJulian(2016, 8, 1, 0, 0, 0, 0)
    SEGMENT_START_TIME = cftime.DatetimeJulian(2016, 8, 1, 0, 30, 0, 0)

    def __init__(self, *args, **kwargs):
        super(GetInitializationTimeTests, self).__init__(*args, **kwargs)
        self.mpi_comm = MPI.COMM_WORLD

    def setUp(self):
        pass

    def tearDown(self):
        self.mpi_comm.barrier()

    def test_get_initialization_time(self):
        initialization_time = (
            fv3gfs.wrapper.get_state(names=["initialization_time"])
            ["initialization_time"]
        )
        current_time = fv3gfs.wrapper.get_state(names=["time"])["time"]
        assert initialization_time == self.RUN_INITIALIZATION_TIME
        assert current_time == self.SEGMENT_START_TIME
        
        
if __name__ == "__main__":
    config = get_from_restarts_config()
    main(test_dir, config)
