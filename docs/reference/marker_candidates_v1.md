# Marker candidates (v1)

Data from The Genome Aggregation Database (gnomAD) for binary SNP loci
with `rs` names, i.e., where the `rs` column starts with `rs`, there is
one character in the `ref` column and one character in the `alt` column.

## Usage

``` r
marker_candidates_v1
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
4128 rows and 23 columns.

## Source

<https://gnomad.broadinstitute.org/>

## Details

The loci are filtered by MAF values for AFR, EAS, and NFE populations.
The MAF values must be within 0.1, 0.15, 0.2 from 0.5.
