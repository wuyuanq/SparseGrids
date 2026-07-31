
# SparseGrids

> **Adaptive Sparse Grid Algorithms for Accelerating Compositional Flash Calculations**

[![Language](https://img.shields.io/badge/Language-Fortran-blue.svg)]()
[![Platform](https://img.shields.io/badge/Platform-Linux-lightgrey.svg)]()
[![Research](https://img.shields.io/badge/Purpose-Scientific%20Computing-green.svg)]()

---

## Overview

SparseGrids is a research-oriented collection of adaptive sparse-grid implementations for accelerating compositional flash calculations. The repository contains multiple algorithmic versions developed throughout a series of research projects, ranging from the original sparse-grid algorithm to high-performance implementations based on optimized data structures.

The primary objective of this project is to reduce the computational cost of repeated flash calculations by replacing expensive nonlinear iterations with efficient sparse-grid interpolation.

---

## Research Evolution

```text
High-dimensional Sparse Grid
        (FPE 2018)
              │
              ▼
 Parallel Sparse Grid Construction
       (CAMWA 2019)
              │
              ▼
 High-performance Sparse Grids
 (Tree / Array / Hole-skipping)
              │
              ▼
 Physics-informed Neural Networks
          (PoF 2023)
```

This repository mainly contains the sparse-grid implementations corresponding to the first three stages.

---

# Repository Structure

```text
SparseGrids
├── sg/
├── sg_general/
├── sg_hole/
├── sg_hpc_array_hole/
├── sg_hpc_tree_hole/
├── sg_hpc_tree_analytical_hole/
└── README.md
```

| Directory | Description |
|-----------|-------------|
| **sg** | Original adaptive sparse-grid implementation |
| **sg_general** | Generalized implementation for arbitrary dimensions |
| **sg_hole** | Sparse grid with hole-skipping optimization |
| **sg_hpc_array_hole** | High-performance array-based implementation |
| **sg_hpc_tree_hole** | High-performance tree-based implementation |
| **sg_hpc_tree_analytical_hole** | Tree implementation with analytical initialization |

---

# Features

- Adaptive sparse-grid construction
- High-dimensional interpolation
- Fast flash calculation
- Multiple algorithm implementations
- Shared-memory optimization
- MATLAB visualization and error analysis

---

# Compilation

Example:

```bash
cd sg_hpc_tree_hole
make -f Makefile.mac
```

Recommended compilers

- Intel Fortran
- GNU Fortran

---

# Running

Typical input files include

```text
infile_sg.F90
infile_sg_2D.F90
infile_sg_3D.F90
infile_sg_5D.F90
infile_sg_10D.F90
```

---

# MATLAB Utilities

| Script | Purpose |
|--------|---------|
| RST_plot_2D.m | Sparse-grid visualization |
| RST_plot_error.m | Error analysis |
| interpolation.m | Interpolation verification |
| sl.m | Supporting utilities |

---

# Algorithm Evolution

| Version | Main Improvement |
|----------|------------------|
| sg | Original implementation |
| sg_general | Dimension-independent implementation |
| sg_hole | Hole-skipping optimization |
| sg_hpc_array_hole | Array-based HPC implementation |
| sg_hpc_tree_hole | Tree-based indexing |
| sg_hpc_tree_analytical_hole | Analytical initialization |

---

# Related Publications

The algorithms implemented in this repository are described in the following publications.

1. **Wu, Y.**, Chen, Z.
   *The application of high-dimensional sparse grids in flash calculations: From theory to realisation*.
   **Fluid Phase Equilibria**, 464, 22–31 (2018).

2. **Wu, Y.**, Ye, M.
   *A Parallel Sparse Grid Construction Algorithm Based on the Shared Memory Architecture and Its Application to Flash Calculations*.
   **Computers & Mathematics with Applications**, 77, 2114–2129 (2019).

3. **Wu, Y.**, Sun, S.
   *Removing the Performance Bottleneck of Pressure–Temperature Flash Calculations during Both the Online and Offline Stages by Using Physics-informed Neural Networks*.
   **Physics of Fluids** (2023).

---

# Citation

If you use this repository in academic research, please cite the corresponding publication(s).

```bibtex
@article{wu2018fpe,
  author  = {Wu, Yuanqing and Chen, Zhangxin},
  title   = {The application of high-dimensional sparse grids in flash calculations: From theory to realisation},
  journal = {Fluid Phase Equilibria},
  volume  = {464},
  pages   = {22--31},
  year    = {2018}
}

@article{wu2019camwa,
  author  = {Wu, Yuanqing and Ye, Ming},
  title   = {A Parallel Sparse Grid Construction Algorithm Based on the Shared Memory Architecture and Its Application to Flash Calculations},
  journal = {Computers & Mathematics with Applications},
  volume  = {77},
  pages   = {2114--2129},
  year    = {2019}
}
```

---

# License

MIT license.

---

# Author

**Dr. Yuanqing Wu**

King Abdullah University of Science and Technology (KAUST)
