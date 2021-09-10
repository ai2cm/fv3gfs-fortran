import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

def store(state):

    try:
        with open(f"store_success.txt", "w") as f:
            f.write("SUCCESS")
    except Exception as e:
        logger.error(e)
        raise e

    logging.info("Successful call to store in monitor.py")