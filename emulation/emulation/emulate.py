import f90nml
import logging
import os
import tensorflow as tf

from .debug import print_errors
from ._filesystem import get_dir
from .monitor import store_zarr

logger = logging.getLogger(__name__)

TF_MODEL_PATH = None  # local or remote path to tensorflow model
STORE_EMU_DATA = None
NML_PATH = None
DT_SEC = None

@print_errors
def _load_environment_vars_into_global():

    global TF_MODEL_PATH
    global STORE_EMU_DATA
    global NML_PATH

    cwd = os.getcwd()
    TF_MODEL_PATH = os.environ["TF_MODEL_PATH"]
    STORE_EMU_DATA = bool(os.environ.get("STORE_EMU_DATA", False))
    NML_PATH = os.path.join(cwd, "input.nml")


@print_errors
def _load_nml():
    namelist = f90nml.read(NML_PATH)
    logger.info(f"Loaded namelist for emulation from {NML_PATH}")
    
    return namelist

@print_errors
def _get_timestep(namelist):
    return int(namelist["coupler_nml"]["dt_atmos"])


@print_errors
def _load_tf_model() -> tf.keras.Model:
    logger.debug(f"Loading keras model: {TF_MODEL_PATH}")
    with get_dir(TF_MODEL_PATH) as local_model_path:
        model = tf.keras.models.load_model(local_model_path)

    return model


_load_environment_vars_into_global()
NML = _load_nml()
DT_SEC = _get_timestep(NML)
MODEL = _load_tf_model()


@print_errors
def microphysics(state):

    inputs = [state[name].T for name in MODEL.input_names]
    predictions = MODEL.predict(inputs)
    model_outputs = {
        name: output.T # transposed adjust
        for name, output in zip(MODEL.output_names, predictions)
    }

    overwrites = set(state).intersection(model_outputs)
    logger.info(f"Overwritting existing state fields: {overwrites}")
    microphysics_diag = {
        f"{orig_updated}_physics_diag": state[orig_updated]
        for orig_updated in overwrites
    }
    state.update(model_outputs)
    state.update(microphysics_diag)

    if STORE_EMU_DATA:
        store_zarr(state)