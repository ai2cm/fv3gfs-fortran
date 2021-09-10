import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

def store(state):

    rank = mpi4py.COMM_WORLD.Get_rank()
    with open(f"store_success_{rank}.txt", "w") as f:
        f.write("SUCCESS")

    logging.INFO("Successful call to store in monitor.py")