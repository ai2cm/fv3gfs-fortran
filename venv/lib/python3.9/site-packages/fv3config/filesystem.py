import os
import fsspec
import backoff
from ._exceptions import DelayedImportError
from . import caching
from concurrent.futures import ThreadPoolExecutor, Executor


try:
    import gcsfs

    FSSPEC_ERRORS = (RuntimeError, gcsfs.utils.HttpError)
except ImportError as err:
    gcsfs = DelayedImportError(err)
    FSSPEC_ERRORS = RuntimeError
try:
    import google.auth
except ImportError as err:
    google = DelayedImportError(err)


fsspec_backoff = backoff.on_exception(backoff.expo, FSSPEC_ERRORS, max_time=60)


def get_fs(path: str) -> fsspec.AbstractFileSystem:
    """Return the fsspec filesystem required to handle a given path."""
    return _get_fs(path)


def _get_fs(path: str) -> fsspec.AbstractFileSystem:
    """Private function implementing public get_fs function, used so that if we
    mock this implementation, it is still used in modules which import using
    `from filesystem import get_fs`.
    """
    if path.startswith("gs://"):
        return fsspec.filesystem("gs", requester_pays=True)
    else:
        return fsspec.filesystem("file")


def isabs(path: str) -> bool:
    """Return whether the path is a local or remote absolute path"""
    if len(_get_protocol_prefix(path)) > 0:
        return True
    else:  # local file
        return os.path.isabs(path)


def is_existing_absolute_path(path: str) -> bool:
    """Return whether the path is an existing absolute path"""
    return isabs(path) and get_fs(path).exists(path)


def _get_protocol_prefix(location):
    """If a string starts with "<protocol>://"", return that part of the string.
    Otherwise, return an empty string.
    """
    separator = "://"
    if separator in location:
        return location[: location.index(separator) + len(separator)]
    else:
        return ""


def _get_path(location):
    """If a string starts with "<protocol>://"", return the rest of the string.
    Otherwise, return the entire string.
    """
    separator = "://"
    if separator in location:
        return location[location.index(separator) + len(separator) :]
    else:
        return location


def is_local_path(location):
    """returns True if the location is local, False otherwise"""
    return _get_protocol_prefix(location) == ""


def put_directory(
    local_source_dir: str,
    dest_dir: str,
    fs: fsspec.AbstractFileSystem = None,
    executor: Executor = None,
):
    """Copy the contents of a local directory to a local or remote directory.
    """
    if fs is None:
        fs = get_fs(dest_dir)
    if executor is None:
        executor = ThreadPoolExecutor()
        manage_threads = True
    else:
        manage_threads = False
    for token in os.listdir(local_source_dir):
        source = os.path.join(os.path.abspath(local_source_dir), token)
        dest = os.path.join(dest_dir, token)
        if os.path.isdir(source):
            fsspec_backoff(fs.makedirs)(dest, exist_ok=True)  # must be blocking call
            put_directory(source, dest, fs=fs, executor=executor)
        else:
            executor.submit(fsspec_backoff(fs.put), source, dest)
    if manage_threads:
        executor.shutdown(wait=True)


def get_file(source_filename: str, dest_filename: str, cache: bool = None):
    """Copy a file from a local or remote location to a local location.

    Optionally cache remote files in the local fv3config cache.
    
    Args:
        source_filename: the local or remote location to copy
        dest_filename: the local target location
        cache (optional): if True and source is remote, copy the file from the
            fv3config cache if it has been previously downloaded, and cache the file
            if not. Does nothing if source_filename is local.
            Default ``fv3config.caching.CACHE_REMOTE_FILES``, set by
            ``fv3config.enable_remote_caching(True/False)``.
    """
    if cache is None:
        cache = caching.CACHE_REMOTE_FILES
    if not cache or is_local_path(source_filename):
        _get_file_uncached(source_filename, dest_filename)
    else:
        _get_file_cached(source_filename, dest_filename)


@fsspec_backoff
def _get_file_uncached(source_filename, dest_filename):
    fs = get_fs(source_filename)
    fs.get(source_filename, dest_filename)


def _get_file_cached(source_filename, dest_filename):
    if is_local_path(source_filename):
        raise ValueError(f"will not cache a local path, was given {source_filename}")
    else:
        cache_location = _get_cache_filename(source_filename)
        if not os.path.isfile(cache_location):
            os.makedirs(os.path.dirname(cache_location), exist_ok=True)
            _get_file_uncached(source_filename, cache_location)
        _get_file_uncached(cache_location, dest_filename)


@fsspec_backoff
def put_file(source_filename, dest_filename):
    """Copy a file from a local location to a local or remote location.
    
    Args:
        source_filename (str): the local location to copy
        dest_filename (str): the local or remote target location
    """
    fs = get_fs(dest_filename)
    fs.put(source_filename, dest_filename)


def _get_cache_filename(source_filename):
    prefix = _get_protocol_prefix(source_filename).strip("://")
    path = _get_path(source_filename)
    if len(path) == 0:
        raise ValueError(f"no file path given in source filename {source_filename}")
    cache_dir = caching.get_internal_cache_dir()
    return os.path.join(cache_dir, prefix, path)
