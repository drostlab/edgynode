# edgynode
## Statistical Assessment and Visualization of Gene Regulatory Networks

### Motivation

The expression of genes is a fundamental molecular mechanism allowing organisms to produce proteins that govern all functional aspects of living cells. In the past years, complex regulatory mechanisms have been described that place transcription factors (TFs), transposable elements (TEs), and regulatory RNA as main actors governing gene expression. Deciphering these interactions between genes and their regulators in various biological contexts such as development or stress response are of immense interest to life scientists. Given the recent advancements in next- generation sequencing technologies, the inference of these regulatory interactions from (single-cell) RNAseq, ChIPseq, and ATACseq data become feasible and are able to paint a genome-wide picture of gene regulation.

### Short package description

The `edgynode` package imports inferred gene regulatory networks and performs network statistics and network simulation procedures to investigate the topology and structure of the GRN at hand.

## Install `edgynode`

```r
# install edgynode from GitHub
devtools::install_github("drostlab/edgynode")
```

## Example

### Small example with internal dataset
```r
# library(edgynode)

# Benchmark GENIE3 inferred networks with raw, no_noise, and quantile_norm combinations
genie3_49_raw <- as.matrix(read.csv(
system.file("data/network_raw_49_placenta_development.csv",
 package = "edgynode"), row.names = 1))

genie3_49_noNoiseCM_raw <- as.matrix(read.csv(
system.file("data/network_noNoiseCM_raw_49_placenta_development.csv",
 package = "edgynode"), row.names = 1))

genie3_49_qnorm_no_noise_removed <- as.matrix(read.csv(
system.file("data/network_qnorm_49_placenta_development.csv",
 package = "edgynode"), row.names = 1))

genie3_49_noNoiseCM_qnorm <- as.matrix(read.csv(
system.file("data/network_noNoiseCM_qnorm_49_placenta_development.csv",
 package = "edgynode"), row.names = 1))

# Run Benchmark using Hamming distance
benchmark_hamming <- edgynode::network_benchmark_noise_filtering(
genie3_49_raw,
genie3_49_noNoiseCM_raw,
genie3_49_qnorm_no_noise_removed,
genie3_49_noNoiseCM_qnorm,
dist_type = "hamming",
grn_tool = "GENIE3")

# visualize at results
edgynode::plot_network_benchmark_noise_filtering(benchmark_hamming,
            dist_type = "hamming", 
            title = "Network Inference Tool: GENIE3")
```
![](img/Example_Fig1.png)