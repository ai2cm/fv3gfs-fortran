import logging
from mpi4py import MPI
from dataclasses import dataclass
import numpy as np
import sys

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

rank = MPI.COMM_WORLD.rank


@dataclass
class Key:
    prefix: str
    block: int
    rank: int
    field: str

    @staticmethod
    def from_string(s) -> "Key":
        prefix, block, rank, field = s.split(":")
        return Key(prefix, int(block), int(rank), field)


def read_from(state, prefix):
    out = {}
    for key in state:
        key_t = Key.from_string(key)
        if key_t.prefix == prefix:
            out[key_t.field] = state[key]
    return out


def compute_error(state):
    statein = read_from(state, "statein")
    stateout_emulator = read_from(state, "stateout_emulator")
    stateout_gfs = read_from(state, "stateout_gfs")
    grid = read_from(state, "grid")
    area = grid["area"][None, :]

    def sum_(x):
        o = np.sum(x, axis=1)
        return MPI.COMM_WORLD.allreduce(o)

    for key in stateout_gfs:
        x = statein[key]
        y = stateout_gfs[key]
        prediction = stateout_emulator[key]
        total_area = sum_(area)
        pred_error = sum_((prediction - y) ** 2 * area) / total_area
        persistence_error = sum_((x - y) ** 2 * area) / total_area

        ratio = 1 - pred_error / persistence_error

        if rank == 0:
            print(key, ratio, file=sys.stderr)


def microphysics(state):
    if rank == 0:
        with open(f"microphysics_success.txt", "w") as f:
            f.write("SUCCESS")
        logger.info("Successful call to microphysics in emulate.py")


def start_physics(state):
    if rank == 0:
        with open(f"physics_success.txt", "w") as f:
            f.write("SUCCESS")
        logger.info("Inside start_physics.")


def end_physics(state):
    compute_error(state)
    if rank == 0:
        logger.info("Inside of end_physics")
        logger.info(list(state))
    state.clear()
