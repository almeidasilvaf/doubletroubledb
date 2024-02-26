
#' Plot tree with tips colored by taxon
#'
#' @param tree A phylo object with a phylogenetic tree.
#' @param metadata A data frame of metadata.
#' @param taxon Character indicating the taxon to use for coloring.
#' @param min_n Numeric indicating the minimum number of species per taxon
#' to use for coloring. Default: 2.
#'
plot_tree <- function(tree, metadata, taxon, min_n = 2) {
    
    # Filter metadata and clean taxon variable
    tokeep <- count(metadata, .data[[taxon]]) |> 
        filter(n >= min_n) |> 
        pull(.data[[taxon]])
    
    
    meta <- metadata |>
        filter(species %in% tree$tip.label) |> 
        mutate(
            Taxon = ifelse(.data[[taxon]] %in% tokeep, .data[[taxon]], "Other")
        ) |>
        select(all_of(c("species", "Taxon", "ncbi_species")))
    
    ## ## Plot species tree tips colored by taxon
    p_tree <- ggtree::ggtree(tree, branch.length = "none")
    tree_meta <- p_tree$data |>
        inner_join(meta, by = c("label" = "species")) |>
        mutate(
            tooltip = paste0(
                "Taxon: ", Taxon, "\n", 
                "NCBI species: ", ncbi_species, "\n",
                "Ensembl species: ", label
                
            )
        )
    
    p_final <- p_tree +
        geom_point(
            data = tree_meta, 
            aes(x = x, y = y, color = Taxon, label = tooltip)
        ) +
        ggsci::scale_color_d3("category20") +
        theme(legend.position = "left") +
        labs(color = str_to_title(taxon))
    
    return(p_final)
}

#' Create a named vector with a color palette for duplication modes
#' 
#' @return A named character vector with colors for each duplication mode.
#' @noRd
#' 
dup_palette <- function() {
    
    pal <- c(
        All = "gray20",
        SD = "#EFC000FF",
        TD = "#CD534CFF",
        PD = "#79AF97FF",
        TRD = "#7AA6DCFF",
        rTRD = "#7AA6DCFF",
        dTRD = "#003C67FF",
        DD = "#6A6599FF",
        SSD = "#7AA6DCFF"
    )
    
    return(pal)
}


#' Plot frequency of duplicates per mode for each species
#'
#' @param dup_counts A data frame in long format with the number of
#' duplicates per mode for each species, as returned by 
#' the function \code{duplicates2counts}.
#' @param plot_type Character indicating how to plot frequencies. One of
#' 'facet' (facets for each level of the variable \strong{type}),
#' 'stack' (levels of the variable \strong{type} as stacked bars), or
#' 'stack_percent' (levels of the variable \strong{type} as stacked bars,
#' with x-axis representing relative frequencies). Default: 'facet'.
#' @param remove_zero Logical indicating whether or not to remove rows
#' with zero values. Default: TRUE.
#' 
#' @return A ggplot object.
#' 
#' @importFrom ggplot2 ggplot aes geom_bar facet_wrap theme_bw theme labs
#' scale_fill_manual element_blank
#' @importFrom rlang .data
#' @export
#' @rdname plot_duplicate_freqs
#' @examples
#' data(dup_counts)
#' 
#' # Plot counts
#' plot_duplicate_freqs(dup_counts[[1]], plot_type = "stack_percent")
plot_duplicate_freqs <- function(
        dup_counts, plot_type = "facet", remove_zero = TRUE
) {
    
    # Define palette
    pal <- dup_palette()
    
    # Remove zeros
    if(remove_zero) { dup_counts <- dup_counts[dup_counts$n != 0, ] }
    
    if(plot_type == "facet") {
        p <- ggplot(dup_counts, aes(x = .data$n, y = .data$species)) +
            geom_bar(
                aes(fill = .data$type), stat = "identity", color = "grey20",
                show.legend = FALSE
            ) +
            facet_wrap("type", nrow = 1, scales = "free_x") +
            labs(y = "", x = "Absolute frequency")
        
    } else if(plot_type == "stack") {
        p <- ggplot(
            dup_counts, aes(x = .data$n, y = .data$species, fill = .data$type)
        ) +
            geom_bar(color = "gray20", position = "stack", stat = "identity") +
            labs(fill = "Type", y = "", x = "Absolute frequency")
        
    } else if(plot_type == "stack_percent") {
        p <- ggplot(
            dup_counts, 
            aes(x = .data$n, y = .data$species, fill = .data$type, label = .data$text)
        ) +
            geom_bar(position = "fill", stat = "identity", color = NA) +
            labs(fill = "Type", y = "", x = "Relative frequency")
        
    } else {
        stop("Input to argument 'plot_type' must be one of 'facet', 'stack', or 'stack_percent'.")
    }
    
    p <- p + 
        scale_fill_manual(values = pal) +
        theme_bw() +
        theme(panel.grid = element_blank())
    
    return(p)
}




