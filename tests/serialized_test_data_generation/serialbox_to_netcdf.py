import sys
import argparse
import os
import shutil
import xarray as xr
import f90nml
import numpy as np

sys.path.append("/serialbox/python")  # noqa: E402
import serialbox  # noqa: E402


def get_parser():
    parser = argparse.ArgumentParser("converts serialbox data to netcdf")
    parser.add_argument(
        "data_path", type=str, help="path of serialbox data to convert",
    )
    parser.add_argument(
        "output_path", type=str, help="output directory where netcdf data will be saved"
    )
    return parser


def read_serialized_data(serializer, savepoint, variable):
    data = serializer.read(variable, savepoint)
    if len(data.flatten()) == 1:
        return data[0]
    return data


def get_all_savepoint_names(data_path):
    savepoint_names = set()
    serializer = get_serializer(data_path, rank=0)
    for savepoint in serializer.savepoint_list():
        savepoint_names.add(savepoint.name)
    return savepoint_names


def get_serializer(data_path, rank):
    return serialbox.Serializer(
        serialbox.OpenModeKind.Read, data_path, "Generator_rank" + str(rank)
    )


def main(data_path: str, output_path: str):
    os.makedirs(output_path, exist_ok=True)
    namelist_filename_in = os.path.join(data_path, "input.nml")
    namelist_filename_out = os.path.join(output_path, "input.nml")
    if namelist_filename_out != namelist_filename_in:
        shutil.copyfile(os.path.join(data_path, "input.nml"), namelist_filename_out)
    namelist = f90nml.read(namelist_filename_out)
    total_ranks = (
        6 * namelist["fv_core_nml"]["layout"][0] * namelist["fv_core_nml"]["layout"][1]
    )

    savepoint_names = get_all_savepoint_names(data_path)
    for savepoint_name in sorted(list(savepoint_names)):
        rank_list = []
        # all ranks have the same names, just look at first one
        serializer = get_serializer(data_path, rank=0)
        names_list = list(
            serializer.fields_at_savepoint(serializer.get_savepoint(savepoint_name)[0])
        )
        serializer_list = []
        for rank in range(total_ranks):
            serializer = get_serializer(data_path, rank)
            serializer_list.append(serializer)
            savepoints = serializer.get_savepoint(savepoint_name)
            rank_data = {}
            for name in set(names_list):
                rank_data[name] = []
                for savepoint in savepoints:
                    rank_data[name].append(
                        read_serialized_data(serializer, savepoint, name)
                    )
            rank_list.append(rank_data)
        n_savepoints = len(savepoints)  # checking from last rank is fine
        data_vars = {}
        if n_savepoints > 0:
            encoding = {}
            for varname in set(names_list).difference(["rank"]):
                data_shape = list(rank_list[0][varname][0].shape)
                data_vars[varname] = get_data(
                    data_shape, total_ranks, n_savepoints, rank_list, varname
                )
                if len(data_shape) > 2:
                    encoding[varname] = {"zlib": True, "complevel": 1}
            dataset = xr.Dataset(data_vars=data_vars)
            dataset.to_netcdf(
                os.path.join(output_path, f"{savepoint_name}.nc"),
                encoding=encoding
            )


def get_data(data_shape, total_ranks, n_savepoints, output_list, varname):
    array = np.full([n_savepoints, total_ranks] + data_shape, fill_value=np.nan)
    dims = ["savepoint", "rank"] + [
        f"dim_{varname}_{i}" for i in range(len(data_shape))
    ]
    data = xr.DataArray(array, dims=dims)
    for rank in range(total_ranks):
        for i_savepoint in range(n_savepoints):
            if len(data_shape) > 0:
                data[i_savepoint, rank, :] = output_list[rank][varname][i_savepoint]
            else:
                data[i_savepoint, rank] = output_list[rank][varname][i_savepoint]
    return data


if __name__ == "__main__":
    parser = get_parser()
    args = parser.parse_args()
    main(data_path=args.data_path, output_path=args.output_path)
