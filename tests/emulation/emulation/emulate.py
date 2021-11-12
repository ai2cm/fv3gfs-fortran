import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


def assert_expected_variables_present(state):

    expected_fields = {
        "air_pressure",
        "air_temperature_after_gscond",
        "air_temperature_after_last_gscond",
        "air_temperature_after_precpd",
        "air_temperature_input",
        "cloud_water_mixing_ratio_after_precpd",
        "cloud_water_mixing_ratio_input",
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
    }

    assert expected_fields == set(state), set(state)


def microphysics(state):
    assert_expected_variables_present(state)
    try:
        with open(f"microphysics_success.txt", "w") as f:
            f.write("SUCCESS")
    except Exception as e:
        logger.error(e)
        raise e
    logger.info("Successful call to microphysics in emulate.py")
