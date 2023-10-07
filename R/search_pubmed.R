# 
# search_pubmed <- function(query){
#   # query <- "'Brian Schilder'[aut] OR 'Brian M Schilder'[aut]"
#   requireNamespace("pubmedR")
#   requireNamespace("bibliometrix")
#   requireNamespace("ggplot2")
#   requireNamespace("dplyr")
#   requireNamespace("tibble")
#   
#   res <- pubmedR::pmQueryTotalCount(query = query)
#   D <- pubmedR::pmApiRequest(query = query,
#                              limit = res$total_count,
#                              api_key = NULL)
#   # M <- pubmedR::pmApi2df(D)
#   M2 <- bibliometrix::convert2df(file = D,
#                                  dbsource = "pubmed",
#                                  format = "api")
#   M2$ID_ <- M2$ID
#   M2$ID <- paste(M2$TI, M2$AB, M2$ID, M2$DE)
#   cs <- bibliometrix::conceptualStructure(M2,
#                                           # minDegree = 1,
#                                           # clust = 3,
#                                           field = "ID")
#   
#   plot_df <- cs$docCoord |> tibble::rownames_to_column("id") |>
#     merge(M2 |> dplyr::mutate(id=tolower(SR)), by="id") |>
#     dplyr::mutate(
#       label = paste(
#         stringr::str_to_title(id),
#         shQuote(
#           stringr::str_trunc(
#             width = 50,
#             stringr::str_to_sentence(TI)
#           )
#         ),
#         sep="\n"
#       ),
#       Cluster=factor(Cluster)
#     )
#   
#   ggplot(plot_df, aes(x=dim1, y=dim2,
#                       color=Cluster,
#                       size=contrib,
#                       label=label)) +
#     geom_point(alpha=.8) +
#     scale_color_viridis_d(end = .8, option = "plasma") +
#     # geom_label() +
#     ggrepel::geom_label_repel(size=3, alpha=.8) +
#     theme_bw()
#   
#   results <- bibliometrix::biblioAnalysis(M2)
#   net <- bibliometrix::biblioNetwork(M2)
#   # summary(results)
#   return(list(query=query,
#               raw=D,
#               df=M2,
#               analysis=results,
#               network=net))
# }
# 
# search_pubmed_fulltext <- function(query,
#                                    target_terms=NULL){
#   
#   # query <- "(AUTHOR:(Brian Schilder) OR AUTHOR:(Brian M Schilder))"
#   # mesh <- data.table::fread("~/Desktop/Geneshot_ontologies/data/ontologies/BioPortal/MESH.csv.gz")
#   # efo <- ontoProc::getEFOOnto()
#   # target_terms <- c(mesh$`Preferred Label`,
#   #                   unname(efo$name))|>
#   #   tolower()
#   # library(pluralize)
#   requireNamespace("pluralize")
#   requireNamespace("europepmc")
#   requireNamespace("tidypmc")
#   requireNamespace("tm")
#   requireNamespace("tidytext")
#   requireNamespace("dplyr")
#   requireNamespace("tidygraph")
#   requireNamespace("igraph")
#   requireNamespace("visNetwork")
#   
#   res <- europepmc::epmc_search(paste(query,"OPEN_ACCESS:Y"))
#   docs <- Map(europepmc::epmc_ftxt,na.omit(res$pmcid))
#   txt <- Map(tidypmc::pmc_text, docs)
#   # metadata <- Map(tidypmc::pmc_metadata, docs) |>
#   #   data.table::rbindlist(use.names = TRUE,
#   #                         idcol = "doc_id",
#   #                         fill = TRUE)
#   # table <- Map(tidypmc::pmc_table, docs)
#   #### Collapse text ####
#   dt <- data.table::rbindlist(
#     txt,
#     use.names = TRUE,
#     idcol = "doc_id")[,list(
#       text = txt |> pluralize::singularize() |>
#         tm::removePunctuation() |>
#         tm::removeNumbers() |>
#         paste(collapse = " ")
#     ),
#     by="doc_id"]
#   stp <- tidytext::get_stopwords()$word
#   #### TermDocumentMatrix ####
#   tdm <-
#     tm::DataframeSource(dt) |>
#     tm::Corpus() |>
#     tm::TermDocumentMatrix()
#   #### TF-IDF ####
#   counts <- data.table::rbindlist(
#     txt,
#     use.names = TRUE,
#     idcol = "doc_id") |>
#     tidytext::unnest_tokens(word, text) |>
#     subset(!word %in% stp) |>
#     dplyr::count(doc_id, word, sort = TRUE)
#   if(is.null(target_terms)){
#     target_terms <- counts$word
#   }
#   tfidf_target <- tidytext::bind_tf_idf(counts,
#                                         term = "word",
#                                         document = "doc_id",
#                                         n = "n") |>
#     dplyr::mutate(len=nchar(word)) |>
#     dplyr::mutate(word=pluralize:::singularize(word)) |>
#     subset(word %in% target_terms) |>
#     subset(len > 2) |>
#     dplyr::group_by(word) |>
#     dplyr::summarise(n=sum(n),
#                      tf_idf=mean(tf_idf)) |>
#     dplyr::arrange(dplyr::desc(tf_idf))
#   #### Compute term-term co-occurence ####
#   X <- as.matrix(tdm[rownames(tdm) %in% head(tfidf_target$word,10000),])
#   X_cor <- cor(t(X))
#   X_cor <- X_cor + abs(min(X_cor, na.rm = TRUE))
#   diag(X_cor) <- NA
#   g <- tidygraph::as_tbl_graph(X_cor) |>
#     tidygraph::activate(what = "edges")
#   g <- g |>
#     tidygraph::filter(weight>rev(
#       quantile(data.frame(g)$weight,
#                na.rm = TRUE,
#                seq(0, 1, 0.1)
#       )
#     )[2]) |>
#     tidygraph::filter(!is.na(weight))
#   g <- g |> tidygraph::activate(what = "nodes")
#   pal <- colorRamp2::colorRamp2(breaks = quantile(igraph::harmonic_centrality(g),
#                                                   probs = seq(0,1,length.out=1000)),
#                                 colors = pals::viridis(1000),
#                                 transparency = .75)
#   igraph::V(g)$color <- pal(igraph::harmonic_centrality(g))
#   igraph::V(g)$value <- igraph::harmonic_centrality(g)^3
#   # plot(g)
#   vn <- visNetwork::visIgraph(g,
#                               randomSeed = 2023,
#                               # layout = "layout_with_kk"
#   ) |>
#     visNetwork::visEdges(arrows = list(enable=FALSE))
#   methods::show(vn)
#   #### Return ####
#   return(
#     list(documents=dt,
#          tfidf_target=tfidf_target,
#          graph=g,
#          plot=vn
#     )
#   )
#   
#   #### UMAP of documents ####
#   # X_tfidf <- tidytext::cast_sparse(tfidf,
#   #                                  row = "doc_id",
#   #                                  column = "word",
#   #                                  value = "tf_idf")
#   # obj <- scKirby::process_seurat(obj = X_tfidf)
#   # mod <- uwot::umap(as.matrix(X_tfidf),
#   #                   n_neighbors = nrow(X_tfidf)-1)
#   # umap_df <- merge(
#   #  metadata,
#   #  data.table::as.data.table(
#   #    mod |> `colnames<-`(paste0("UMAP",seq_len(ncol(mod)))),
#   #    keep.rownames = "doc_id"
#   #  ))
#   #
#   # ggplot(umap_df, aes(x=UMAP1, y=UMAP2,
#   #                     # color=Cluster,
#   #                     # size=contrib,
#   #                     label=stringr::str_wrap(Title,50))) +
#   #   geom_point(alpha=.8) +
#   #   scale_color_viridis_d(end = .8, option = "plasma") +
#   #   # geom_label() +
#   #   ggrepel::geom_label_repel(
#   #     size=3, alpha=.8, max.overlaps = 30) +
#   #   theme_bw()
# }
# 
# 
# 
