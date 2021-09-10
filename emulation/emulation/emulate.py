import logging
import os
import mpi4py

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

def microphysics(state):

    rank = mpi4py.COMM_WORLD.Get_rank()
    with open(f"microphysics_success_{rank}.txt", "w") as f:
        f.write("SUCCESS")

    logger.info("Successful call to microphysics in emulate.py")