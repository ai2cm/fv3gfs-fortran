# -*- coding: utf-8 -*-
# cython: language_level=3, always_allow_keywords=True
# cython: c_string_type=unicode, c_string_encoding=utf8
"""Non-templated code
"""
from mpi4py import MPI


cdef extern:
    void initialize_subroutine(int *comm)
    void do_step_subroutine()
    void cleanup_subroutine()
    void do_dynamics()
    void compute_physics_subroutine()
    void apply_physics_subroutine()
    void save_intermediate_restart_if_enabled_subroutine()
    void save_intermediate_restart_subroutine()
    void get_num_cpld_calls(int *num_cpld_calls_out)
    void do_pre_radiation()
    void do_radiation()
    void do_physics()


def get_step_count():
    """Return the number of physics steps the Fortran model would like to complete
    before exiting, based on its configuration."""
    cdef int return_value
    get_num_cpld_calls(&return_value)
    return return_value


def initialize():
    """Call initialization routines for the Fortran model."""
    cdef int comm
    comm = MPI.COMM_WORLD.py2f()
    initialize_subroutine(&comm)


def step():
    """Perform one dynamics-physics cycle of the Fortran model."""
    step_dynamics()
    step_physics()
    save_intermediate_restart_if_enabled_subroutine()


def step_dynamics():
    """Perform one physics step worth of dynamics in the Fortran model.

    Physics quantities are not updated by this routine."""
    do_dynamics()


def step_physics():
    """Perform a physics step in the Fortran model.

    Equivalent to calling compute_physics() and apply_physics() in that order."""
    compute_physics_subroutine()
    apply_physics_subroutine()


def compute_physics():
    """Call physics routines in the Fortran model and update physics prognostic state.

    It is necessary to call apply_physics() after this to update the dynamical
    prognostic state with the output from the routines called by this function."""
    compute_physics_subroutine()


def apply_physics():
    """Update dynamical prognostic state with output from physics routines."""
    apply_physics_subroutine()


def step_pre_radiation():
    """Do pre-radiation computations (e.g. time varying logic)"""
    do_pre_radiation()


def step_radiation():
    """Compute Radiative transfer scheme"""
    do_radiation()


def step_post_radiation_physics():
    """Compute Post-radiation physics (e.g. moist physics turbulence)"""
    # TODO ensure that IPD_control.first_step is set in this routine
    do_physics()


def save_intermediate_restart_if_enabled():
    """If the Fortran model wants to do so on this timestep, write intermediate restart files.

    This function is used at the end of the Fortran main loop to replicate the
    intermediate restart behavior of the Fortran model.
    """
    save_intermediate_restart_if_enabled_subroutine()


def save_fortran_restart():
    """Trigger the Fortran model to write restart files."""
    save_intermediate_restart_subroutine()


def cleanup():
    """Call the Fortran cleanup routines, which clear memory and write final restart files."""
    cleanup_subroutine()

