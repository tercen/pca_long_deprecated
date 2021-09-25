# pca operator

#### Description

`pca` operator performs principle component analysis.

##### Usage

Input projection (`input.convention` = `observations.in.columns`)|.
---|---
`row`   | represents the variables (e.g. genes, channels, markers)
`col`   | represents the observations (e.g. cells, samples, individuals) 
`y-axis`| measurement value

Input projection  (`input.convention` = `observations.in.rows`)|.
---|---
`col`   | represents the variables (e.g. genes, channels, markers)
`row`   | represents the observations (e.g. cells, samples, individuals) 
`y-axis`| measurement value

Input parameters|.
---|---
`scale`   | logical, indicating whether the variables should be scaled to have unit variance before the analysis takes place (dafault = FALSE)
`center`   | logical, indicating whether the variables should be shifted to be zero centered before the analysis takes place (default = TRUE)
`input.convention`| `observations.in.columns` or `observations.in.rows`
`na.action`| A function which indicates what should happen when the data contain NAs
`tol`| numeric, indicating the magnitude below which components should be omitted. Components are omitted if their standard deviations are less than or equal to tol times the standard deviation of the first component
`maxComp`| numeric, maximum number of components to return, default 5

Output relations|.
---|---
`pca1.scores, pca2.scores, ..., `| Scores on the principal components 1..maxComp, i.e. the data projected on the principal components.
`pca1.loadings, pca2.loadings, ..., `| Loadings (eigenvectors) of the principal components 1..maxComp, i.e. the weight of the original variables in the principal components

##### Details

The operator performs principal component analysis. It reduces the amount of variables (e.g. indicated by rows) to a lower number (default 5) while retaining an optimal amount of information.

##### See Also

[tsne](https://github.com/tercen/tsne_operator)

