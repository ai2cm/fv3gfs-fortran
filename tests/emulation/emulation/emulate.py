import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

def microphysics(state):

    try:
        with open(f"microphysics_success.txt", "w") as f:
            f.write("SUCCESS")
    except Exception as e:
        logger.error(e)
        raise e

    logger.info("Successful call to microphysics in emulate.py")


def start_physics(state):
    try:
        with open(f"physics_success.txt", "w") as f:
            f.write("SUCCESS")
    except Exception as e:
        logger.error(e)
        raise e

    logger.info("Inside start_physics.")


def end_physics(state):
    logger.info("Inside of end_physics")
    logger.info(list(state))
