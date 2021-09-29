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