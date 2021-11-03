import logging
import datetime

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

def store(state):

    rank = state["rank"].strip()

    yr, mon, day, t_zone, hr, min, sec, mil_sec = [int(s) for s in state["model_time"]]
    current_time = datetime.datetime(yr, mon, day, hr, min, sec, mil_sec * 1000)


    with open(f"log.{current_time.isoformat()}.{rank}.txt", "a") as f:
        for key in state:
            try:
                print(key, state[key].mean(), file=f)
            except:
                print(key, state[key], file=f)

    try:
        with open(f"store_success.txt", "w") as f:
            f.write("SUCCESS")
    except Exception as e:
        logger.error(e)
        raise e

    logging.info("Successful call to store in monitor.py")