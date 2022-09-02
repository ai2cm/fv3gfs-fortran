import logging
import numpy as np

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

t_in = "air_temperature_input"
t_g = "air_temperature_after_gscond"
t_p = "air_temperature_after_precpd"

qv_g = "specific_humidity_after_gscond"
qv_lg = "specific_humidity_after_last_gscond"
qv_p = "specific_humidity_after_precpd"
qv_in = "specific_humidity_input"

qc_p = "cloud_water_mixing_ratio_after_precpd"
qc_in = "cloud_water_mixing_ratio_input"


def assert_expected_variables_present(state):

    expected_fields = {
        "air_pressure",
        "air_temperature_after_gscond",
        t_g,
        t_p,
        t_in,
        qv_g,
        qv_lg,
        qv_p,
        qv_in,
        qc_p,
        qc_in,
        "air_temperature_after_last_gscond",
        "cloud_water_mixing_ratio_after_gscond",
        "latitude",
        "longitude",
        "model_time",
        "pressure_thickness_of_atmospheric_layer",
        "ratio_of_snowfall_to_rainfall",
        "specific_humidity_after_gscond",
        "specific_humidity_after_last_gscond",
        "specific_humidity_after_precpd",
        "specific_humidity_input",
        "surface_air_pressure_after_last_gscond",
        "surface_air_pressure",
        "tendency_of_rain_water_mixing_ratio_due_to_microphysics",
        "total_precipitation",
        "rhc",
        "rank"
    }

    assert expected_fields == set(state), set(state)


def state_dependent_update(state):
    """A routine that injects a simple state dependent prediction.

    This is useful for integration testing.
    """
    eps = 1e-7
    state[t_g] += state[t_in] * eps
    state[t_p] += state[t_in] * eps
    state[qc_p] += state[qc_in] * eps
    state[qv_p] += state[qv_in] * eps
    state[qv_g] += state[qv_in] * eps


def microphysics(state):
    assert_expected_variables_present(state)
    state_dependent_update(state)
    try:
        with open(f"microphysics_success.txt", "w") as f:
            f.write("SUCCESS")
    except Exception as e:
        logger.error(e)
        raise e
    logger.info("Successful call to microphysics in emulate.py")


def gscond(state):
    logger = logging.getLogger("gscond.state")
    # used for regression testing later on
    logger.info(sorted(state))
