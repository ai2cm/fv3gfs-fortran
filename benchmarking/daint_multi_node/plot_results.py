#!/usr/bin/env python3

import os
import click
import numpy as np
import matplotlib.pyplot as plt
import json


def read_data(results_directory):
    raw_data = []
    for subdir, dirs, files in os.walk(results_directory):
        for fname in files:
            fullpath = os.path.join(subdir, fname)
            if fullpath.endswith(".json"):
                with open(fullpath) as f:
                    results = json.load(f)
                    raw_data.append(results)
    assert raw_data, "Did not find any *.json files in provided directory"
    return raw_data


def process_raw_data(raw_data, subdomain_size):
    filter = "c" + str(subdomain_size) + "_"
    data = {}
    modules = ["Dycore", "Tracers", "Remapping", "Total"]
    for dataset in raw_data:
        if filter in dataset["setup"]["dirname"]:
            size_per_domain, _, nodes_on_edge, *_ = dataset["setup"]["dirname"].split("_")
            size_per_domain = int(size_per_domain[1:])
            nodes_on_edge = int(nodes_on_edge)
            total_nodes = nodes_on_edge**2 * 6
            total_size = size_per_domain * nodes_on_edge
            timesteps = dataset["times"]["FVDynamics"]["hits"]
            t_dyncore = dataset["times"]["DynCore"]["mean"] / timesteps
            t_traceradv = dataset["times"]["TracerAdvection"]["mean"] / timesteps
            t_remapping = dataset["times"]["Remapping"]["mean"] / timesteps
            t_total = dataset["times"]["mainloop"]["mean"] / timesteps - t_dyncore - t_traceradv - t_remapping
            data[total_nodes] = [t_dyncore, t_remapping, t_traceradv, t_total]
    assert data, "Did not find any matching data sets for provided filter"

    nodes = list(data.keys())
    values = list(data.values())
    nodes, values = zip(*sorted(zip(nodes, values)))
    nodes = np.array(nodes)
    grid_size = 200 * 48 / subdomain_size / np.sqrt(nodes / 6)
    values = np.array(values)
    dt_atmos = 1.5 * grid_size * 7 * 8
    t_total = np.sum(values, axis=1)
    sypd = dt_atmos / t_total / 365.0

    return values, nodes, grid_size, modules, sypd


def stacked_bar_chart(values, nodes, grid_size, modules, sypd, title):
    labels = [str(x) for x in nodes]
    values_cum = values.cumsum(axis=1)
    category_colors = plt.get_cmap("RdYlGn")(
        np.linspace(0.15, 0.85, values.shape[1]))

    fig, ax = plt.subplots(figsize=(9.2, 5))
    ax.invert_yaxis()
    ax.set_xlabel("Time per timestep [s]")
    ax.set_ylabel("# nodes")
    ax.title.set_text(title)

    for i, (colname, color) in enumerate(zip(modules, category_colors)):
        widths = values[:, i]
        starts = values_cum[:, i] - widths
        ax.barh(labels, widths, left=starts, height=0.5,
                label=colname, color=color)

        if not i:
            xcenters = starts
            r, g, b, _ = color
            text_color = 'white' if r * g * b < 0.5 else 'darkgrey'
            for y, (x, c, d) in enumerate(zip(xcenters, grid_size, sypd)):
                ax.text(x, y, f"  {c:.2f} km  /  {d:.2f} SYPD", ha='left', va='center',
                        color=text_color, fontsize='x-small')

        ax.legend(ncol=len(modules), bbox_to_anchor=(0.5, 1.0),
              loc="upper center", fontsize="small")

    return fig, ax


@click.command()
@click.argument("results_directory", type=str, nargs=1)
@click.argument("subdomain_size", type=int, nargs=1)
def plot_results(results_directory, subdomain_size):
    raw_data = read_data(results_directory)
    values, nodes, grid_size, modules, sypd = process_raw_data(raw_data, subdomain_size=subdomain_size)
    title = f"Weak scaling with {subdomain_size} x {subdomain_size} x 79 gridpoints per node"
    fig, ax = stacked_bar_chart(values, nodes, grid_size, modules, sypd, title)
    plt.savefig(f"results_c{subdomain_size}.png")


if __name__ == "__main__":
    plot_results()
