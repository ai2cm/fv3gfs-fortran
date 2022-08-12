import pace.util
from ._wrapper import (
    get_state,
    set_state,
    get_n_ghost_cells,
    get_tracer_metadata,
    _get_diagnostic_info,
    _get_diagnostic_data,
    flags,
    DiagnosticInfo,
)
from ._control import (
    apply_physics,
    cleanup,
    compute_physics,
    get_step_count,
    initialize,
    save_fortran_restart,
    save_intermediate_restart_if_enabled,
    step,
    step_dynamics,
    step_physics,
    step_pre_radiation,
    step_radiation,
    step_post_radiation_physics
)
from ._restart import get_restart_names, open_restart
from . import examples

from .thermodynamics import set_state_mass_conserving


def get_diagnostic_by_name(
    name: str, module_name: str = "gfs_phys"
) -> pace.util.Quantity:
    """Get a diagnostic field as a Quantity

    Currently, only supports diagnostics defined in the GFS_diagnostics.F90
    """
    info = _get_diagnostic_info()
    for idx, meta in info.items():
        if meta.module_name == module_name and meta.name == name:
            return _get_diagnostic_data(idx)
    raise ValueError(f"There is no diagnostic {name} in module {module_name}.")


def get_diagnostic_metadata_by_name(
    name: str, module_name: str = "gfs_phys"
) -> DiagnosticInfo:
    """Get diagnostic metadata by name

    Currently, only supports diagnostics defined in the GFS_diagnostics.F90
    """
    info = _get_diagnostic_info()
    for idx, meta in info.items():
        if meta.module_name == module_name and meta.name == name:
            return meta
    raise ValueError(f"There is no diagnostic {name} in module {module_name}.")


__version__ = "0.6.0"

__all__ = list(key for key in locals().keys() if not key.startswith("_"))
