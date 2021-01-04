#' @title Compare two (weighted) adjacency matrices using various comparison methods
#' @description This function uses the \code{\link[igraph]{compare}}
#' function to compare two adjacency matrices.
#' @param adj_mat_x a square adjacency matrix. Also supports a sparse matrix from the \code{Matrix} package.
#' @param adj_mat_y a square adjacency matrix. Also supports a sparse matrix from the \code{Matrix} package.
#' @param comparison_method the network graph comparison method that shall be employed. Options are:
#' \itemize{
#'  \item \code{comparison_method = "vi"}: variation of information (VI) metric of Meila (2003) (default). Find details at \code{\link[igraph]{compare}}.
#'  \item \code{comparison_method = "nmi"}: normalized mutual information measure proposed by Danon et al. (2005). Find details at \code{\link[igraph]{compare}}.
#'  \item \code{comparison_method = "split.join"}: split-join distance of can Dongen (2000). Find details at \code{\link[igraph]{compare}}.
#'  \item \code{comparison_method = "rand"}: the Rand index of Rand (1971). Find details at \code{\link[igraph]{compare}}.
#'  \item \code{comparison_method = "adjusted.rand"}: the adjusted Rand index by Hubert and Arabie (1985). Find details at \code{\link[igraph]{compare}}.
#' }
#' @param weighted This argument specifies whether to create a weighted graph from an adjacency matrix.
#' If it is NULL then an unweighted graph is created and the elements of the adjacency matrix gives the number
#' of edges between the vertices. If it is a character constant then for every non-zero matrix entry
#' an edge is created and the value of the entry is added as an edge attribute named by the weighted argument. If it is TRUE then a weighted graph is created and the name of the edge attribute will be weight. See also details below.
#' @param mode a character value specifying how \code{igraph} should interpret the input matrices.
#' Options are:
#' \itemize{
#' \item If \code{weighted = NULL}:
#' \itemize{
#' \item \code{mode = "directed"}: The graph will be directed and a matrix element gives the number of edges between two vertices.
#' \item \code{mode = "undirected"}: This is exactly the same as max, for convenience. Note that it is not checked whether the matrix is symmetric (default).
#' \item \code{mode = "upper"}: An undirected graph will be created, only the upper right triangle (including the diagonal) is used for the number of edges.
#' \item \code{mode = "lower"}: An undirected graph will be created, only the lower left triangle (including the diagonal) is used for creating the edges.
#' \item \code{mode = "max"}: An undirected graph will be created and max(A(i,j), A(j,i)) gives the number of edges.
#' \item \code{mode = "min"}: undirected graph will be created with min(A(i,j), A(j,i)) edges between vertex i and j.
#' \item \code{mode = "plus"}: undirected graph will be created with A(i,j)+A(j,i) edges between vertex i and j.
#' }
#'\item If the \code{weighted} argument is not \code{NULL} then the elements of the matrix give the weights of the edges (if they are not zero). The details depend on the value of the mode argument:
#'itemize{
#' \item \code{mode = "directed"}: The graph will be directed and a matrix element gives the edge weights.
#' \item \code{mode = "undirected"}: First we check that the matrix is symmetric. It is an error if not. Then only the upper triangle is used to create a weighted undirected graph (default).
#' \item \code{mode = "upper"}: An undirected graph will be created, only the upper right triangle (including the diagonal) is used (for the edge weights).
#' \item \code{mode = "lower"}: An undirected graph will be created, only the lower left triangle (including the diagonal) is used for creating the edges.
#' \item \code{mode = "max"}: An undirected graph will be created and max(A(i,j), A(j,i)) gives the edge weights.
#' \item \code{mode = "min"}: An undirected graph will be created, min(A(i,j), A(j,i)) gives the edge weights.
#' \item \code{mode = "plus"}: An undirected graph will be created, A(i,j)+A(j,i) gives the edge weights.
#' }
#' }
#'
#' @param diag a logical value specifying whether to include the diagonal of the matrix in the calculation. If \code{diag = FALSE} then the diagonal first set to zero and then passed along the downstream functions.
#' @param add_colnames a character value specifying whether column names shall be added as vertex attributes. Options are:
#' \itemize{
#' \item \code{add_colnames = NULL} (default):  if present, column names are added as vertex attribute ‘name’.
#' \item \code{add_colnames = NA}: column names will not be added.
#' \item \code{add_colnames = ""}: If a character constant is specified then it gives the name of the vertex attribute to add.
#' }
#' @param add_rownames a character value specifying whether to add the row names as vertex attributes. Possible values the same as the previous argument. By default row names are not added. If ‘add.rownames’ and ‘add.colnames’ specify the same vertex attribute, then the former is ignored.
#' @param \dots additional arguments passed on to \code{\link[igraph]{graph_from_adjacency_matrix}}.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @export
#' @return A real number returned from the respective \code{comparison_method} method.
#' @examples
#' ##### Compare networks inferred by PPCOR and PIDC
#' ## Import and rescale GENIE3 network
#' # path to GENIE3 output file
#' genie_output <- system.file('beeline_examples/GENIE3/outFile.csv', package = 'edgynode')
#' # import GENIE3 specific output
#' genie_parsed <- genie(genie_output)
#' # rescaling GENIE3 output
#' genie_rescaled <- network_rescale(genie_parsed)
#'
#' ## Import and rescale PIDC network
#' # path to PIDC output file
#' pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'edgynode')
#' # import PIDC specific output
#' pidc_parsed <- pidc(pidc_output)
#' #Set diagonal values
#' diag(pidc_parsed) <- 1
#' # rescaling PIDC output
#' pidc_rescaled <- network_rescale(pidc_parsed)
#' 
#' ### compare both networks
#' # network_compare(genie_rescaled, pidc_rescaled)


network_compare <-
  function (adj_mat_x,
            adj_mat_y,
            comparison_method = "vi",
            weighted = TRUE,
            mode = "undirected",
            diag = TRUE,
            add_colnames = NULL,
            add_rownames = NA,
            ...) {
    if (!is.matrix(adj_mat_x))
      warning("Please provide a numeric matrix as adj_mat_x.", call. = FALSE)

    if (!is.matrix(adj_mat_y))
      warning("Please provide a numeric matrix as adj_mat_y.", call. = FALSE)

    #Creating the igraph objects
    g1 <-
      igraph::graph.adjacency(
        adj_mat_x,
        weighted = weighted,
        mode = mode,
        diag = diag,
        add.colnames = add_colnames,
        add.rownames = add_rownames,
        ...
      )
    g2 <-
      igraph::graph.adjacency(adj_mat_y, weighted = weighted,
                              mode = mode,
                              diag = diag,
                              add.colnames = add_colnames,
                              add.rownames = add_rownames,
                              ...)

    #Creating the community objects
    comm_g1 <- igraph::spinglass.community(g1)
    comm_g2 <- igraph::spinglass.community(g2)

    #Calculating comparisons
    res <- igraph::compare(comm_g1, comm_g2, method = comparison_method)

    return(res)
  }
