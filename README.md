# edgynode
## Statistical Assessment and Visualization of Gene Regulatory Networks

### Motivation

The expression of genes is a fundamental molecular mechanism allowing organisms to produce proteins that govern all functional aspects of living cells. In the past years, complex regulatory mechanisms have been described that place transcription factors (TFs), transposable elements (TEs), and regulatory RNA as main actors governing gene expression. Deciphering these interactions between genes and their regulators in various biological contexts such as development or stress response are of immense interest to life scientists. Given the recent advancements in next- generation sequencing technologies, the inference of these regulatory interactions from (single-cell) RNAseq, ChIPseq, and ATACseq data become feasible and are able to paint a genome-wide picture of gene regulation.

### Short package description

The `edgynode` package imports inferred gene regulatory networks and performs network statistics and network simulation procedures to investigate the topology and structure of the GRN at hand.

## Install `edgynode`

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install()
# Install package dependencies
BiocManager::install("monocle")

# install edgynode from GitHub
devtools::install_github("drostlab/edgynode")
```

## Example

### Small example with internal dataset
```r
##### Compare networks inferred by PPCOR and PIDC
## Import and rescale PPCOR network
# path to PPCOR output file
ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
# import PPCOR specific output
ppcor_parsed <- edgynode::ppcor(ppcor_output)
# rescaling PPCOR output
ppcor_rescaled <- edgynode::network_rescale(ppcor_parsed)

## Import and rescale PIDC network
# path to PIDC output file
pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'edgynode')
# import PIDC specific output
pidc_parsed <- edgynode::pidc(pidc_output)
# rescaling PIDC output
pidc_rescaled <- edgynode::network_rescale(pidc_parsed)


### compare both networks
compared <- edgynode::network_compare(ppcor_rescaled, pidc_rescaled)
# Visualize output
compared
```