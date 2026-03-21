library(shinyWidgets)
library(FactoMineR)
library(factoextra)

vars_x <- reactive({
  all_vars <- names(base_ve_log)[sapply(base_ve_log, is.numeric)]
  all_vars <- all_vars[!grepl("unnamed|^Index$", all_vars, ignore.case = TRUE)]
  emission_keywords <- c("emission", "concentration", "diametre", "nb/cm3", "nb/s", "nb/km", "mm3/s", "mm3/km", "µm", "nb\\*/cm3")
  all_vars[!grepl(paste(emission_keywords, collapse = "|"), all_vars, ignore.case = TRUE)]
})

vars_y <- reactive({
  all_vars <- names(base_ve_log)[sapply(base_ve_log, is.numeric)]
  emission_keywords <- c("emission", "concentration", "diametre", "nb/cm3", "nb/s", "nb/km", "mm3/s", "mm3/km", "µm", "nb\\*/cm3")
  all_vars[grepl(paste(emission_keywords, collapse = "|"), all_vars, ignore.case = TRUE)]
})

observe({
  updatePickerInput(session, "acp_vars_x",
                    choices  = vars_x(),
                    selected = vars_x()[1:min(10, length(vars_x()))])
  updatePickerInput(session, "acp_vars_y",
                    choices  = vars_y(),
                    selected = vars_y())
})

output$acp_status_message <- renderUI({
  n_x <- length(input$acp_vars_x)
  n_y <- length(input$acp_vars_y)
  n   <- n_x + n_y
  if (n == 0) {
    div(style = "background: #FFF3CD; padding: 15px; border-left: 4px solid #FFC107;",
        p(strong("Aucune variable selectionnee."), " Selectionnez des variables ci-dessus."))
  } else if (n < 2) {
    div(style = "background: #FFEBEE; padding: 15px; border-left: 4px solid #D32F2F;",
        p(strong("Selectionnez au moins 2 variables"), " pour lancer l'ACP."))
  } else {
    div(style = "background: #D4EDDA; padding: 15px; border-left: 4px solid #28A745;",
        p(strong(n, "variables selectionnees"),
          paste0("(", n_x, " vehicule + ", n_y, " emissions)"),
          " Cliquez sur 'Lancer l'ACP'."))
  }
})

output$kmeans_status_message <- renderUI({
  if (!isTruthy(acp_result())) {
    div(style = "background: #FFF3CD; padding: 15px; border-left: 4px solid #FFC107;",
        p(strong("Lancez d'abord l'ACP"), " dans l'onglet 'VUE D'ENSEMBLE'."))
  } else {
    div(style = "background: #D4EDDA; padding: 15px; border-left: 4px solid #28A745;",
        p("ACP disponible. Choisissez K et cliquez sur 'Lancer le K-means'."))
  }
})

output$cah_status_message <- renderUI({
  if (!isTruthy(acp_result())) {
    div(style = "background: #FFF3CD; padding: 15px; border-left: 4px solid #FFC107;",
        p(strong("Lancez d'abord l'ACP"), " dans l'onglet 'VUE D'ENSEMBLE'."))
  } else {
    div(style = "background: #D4EDDA; padding: 15px; border-left: 4px solid #28A745;",
        p("ACP disponible. Choisissez K et cliquez sur 'Lancer la CAH'."))
  }
})

acp_result <- eventReactive(input$run_acp, {
  all_vars <- c(input$acp_vars_x, input$acp_vars_y)
  df <- base_ve_log %>%
    select(all_of(all_vars)) %>%
    select(where(is.numeric)) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(across(everything(), ~ replace(., is.infinite(.) | is.nan(.), NA)))
  seuil_na <- 0.5
  df <- df[, colMeans(is.na(df)) <= seuil_na, drop = FALSE]
  df <- df %>%
    mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  ncp_use <- min(input$acp_ncp, ncol(df) - 1, nrow(df) - 1)
  PCA(df, scale.unit = input$acp_scale, ncp = ncp_use, graph = FALSE)
}, ignoreNULL = TRUE)

get_top_vars_contrib <- function(res, dim_idx, n = 10) {
  contrib <- as.data.frame(res$var$contrib)
  if (dim_idx > ncol(contrib)) return(rownames(contrib))
  names(sort(contrib[, dim_idx], decreasing = TRUE))[1:min(n, nrow(contrib))]
}

make_interp <- function(res, dim1, dim2, n = 3) {
  eig     <- res$eig
  contrib <- as.data.frame(res$var$contrib)
  var1    <- round(eig[min(dim1, nrow(eig)), 2], 1)
  var2    <- round(eig[min(dim2, nrow(eig)), 2], 1)
  get_top <- function(d) {
    if (d > ncol(contrib)) return("N/A")
    top <- sort(contrib[, d], decreasing = TRUE)
    paste(names(top)[1:min(n, length(top))], collapse = ", ")
  }
  tagList(tags$ul(
    tags$li(strong(paste0("Dim", dim1, " (", var1, "%) : ")), get_top(dim1)),
    tags$li(strong(paste0("Dim", dim2, " (", var2, "%) : ")), get_top(dim2)),
    tags$li("Variance cumulee : ", strong(paste0(var1 + var2, "%")))
  ))
}

cluster_colors <- c("#BBDEFB","#C8E6C9","#FFECB3","#FFCDD2",
                    "#E1BEE7","#B2EBF2","#DCEDC8","#FFE0B2","#F8BBD0","#CFD8DC")

output$table_eigenvalues <- renderDT({
  req(acp_result())
  eig <- as.data.frame(round(acp_result()$eig, 3))
  eig$Composante <- rownames(eig)
  eig <- eig[, c("Composante", "eigenvalue", "percentage of variance", "cumulative percentage of variance")]
  colnames(eig) <- c("Composante", "Valeur propre", "Variance (%)", "Variance cumulee (%)")
  datatable(eig, rownames = FALSE, options = list(pageLength = 15, dom = "t")) %>%
    formatStyle("Valeur propre", backgroundColor = styleInterval(1, c("#FFCDD2", "#C8E6C9"))) %>%
    formatStyle("Variance cumulee (%)", backgroundColor = styleInterval(c(50, 70, 80), c("#FFEBEE", "#FFF9C4", "#E8F5E9", "#C8E6C9")))
})

output$scree_plot <- renderPlotly({
  req(acp_result())
  p <- fviz_eig(acp_result(), addlabels = TRUE, ylim = c(0, 100),
                barfill = "#1976D2", barcolor = "#1976D2", linecolor = "#D32F2F",
                ggtheme = theme_minimal()) +
    labs(title = "Eboulis des valeurs propres", x = "Composantes", y = "% variance") +
    geom_hline(yintercept = 100 / ncol(acp_result()$var$coord),
               linetype = "dashed", color = "#FFC107", linewidth = 1) +
    theme(plot.title = element_text(face = "bold", color = "#1976D2"))
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
})

output$kaiser_interpretation <- renderUI({
  req(acp_result())
  eig      <- acp_result()$eig
  n_kaiser <- sum(eig[, 1] >= 1)
  var_cum  <- eig[min(n_kaiser, nrow(eig)), 3]
  div(style = "background: #E8F5E9; padding: 20px; border-left: 4px solid #4CAF50;",
      h5(strong("INTERPRETATION - Regle de Kaiser")),
      tags$ul(
        tags$li(strong(n_kaiser, " composantes"), " ont une valeur propre >= 1"),
        tags$li("Variance cumulee : ", strong(paste0(round(var_cum, 1), "%"))),
        if (var_cum >= 70) span(style = "color:#28a745;font-weight:bold;", "Excellent ! >= 70%")
        else if (var_cum >= 50) span(style = "color:#ffc107;font-weight:bold;", "Acceptable.")
        else span(style = "color:#dc3545;font-weight:bold;", "Variance faible.")
      ))
})

output$plot_contrib <- renderPlotly({
  req(acp_result())
  dim_num <- as.integer(gsub("Dim\\.", "", input$acp_dim_select))
  p <- fviz_contrib(acp_result(), choice = "var", axes = dim_num,
                    top = input$acp_top_contrib, fill = "#1976D2", color = "#1976D2",
                    ggtheme = theme_minimal()) +
    labs(title = paste0("Contributions - Axe ", dim_num)) +
    theme(plot.title = element_text(face = "bold", color = "#1976D2"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
})

output$plot_cos2 <- renderPlotly({
  req(acp_result())
  dim_num <- as.integer(gsub("Dim\\.", "", input$acp_dim_select))
  p <- fviz_cos2(acp_result(), choice = "var", axes = dim_num,
                 top = input$acp_top_contrib, fill = "#D32F2F", color = "#D32F2F",
                 ggtheme = theme_minimal()) +
    labs(title = paste0("Cos2 - Axe ", dim_num)) +
    theme(plot.title = element_text(face = "bold", color = "#D32F2F"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
})

output$table_contrib <- renderDT({
  req(acp_result())
  contrib <- as.data.frame(acp_result()$var$contrib)
  cos2    <- as.data.frame(acp_result()$var$cos2)
  coord   <- as.data.frame(acp_result()$var$coord)
  dims    <- paste0("Dim.", seq_len(ncol(contrib)))
  df_out  <- data.frame(Variable = rownames(contrib))
  for (d in dims) {
    if (d %in% colnames(coord))   df_out[[paste0("Coord_",   d)]] <- round(coord[[d]],   3)
    if (d %in% colnames(contrib)) df_out[[paste0("Contrib_", d)]] <- round(contrib[[d]], 2)
    if (d %in% colnames(cos2))    df_out[[paste0("Cos2_",    d)]] <- round(cos2[[d]],    3)
  }
  datatable(df_out, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
})

make_plan_plot <- function(res, dim1, dim2, top_n = 10) {
  top_vars <- get_top_vars_contrib(res, dim1, top_n)
  p <- fviz_pca_var(res, axes = c(dim1, dim2),
                    select.var = list(name = top_vars),
                    col.var = "cos2",
                    gradient.cols = c("#D32F2F", "#FFC107", "#1976D2"),
                    repel = TRUE, ggtheme = theme_minimal()) +
    labs(title    = paste0("Plan (", dim1, ", ", dim2, ")"),
         subtitle = paste0("Top ", top_n, " variables par contribution")) +
    theme(plot.title    = element_text(face = "bold", color = "#1976D2"),
          plot.subtitle = element_text(color = "#666"))
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
}

output$plot_var_1_3 <- renderPlotly({ req(acp_result()); make_plan_plot(acp_result(), 1, 3) })
output$plot_var_1_4 <- renderPlotly({ req(acp_result()); make_plan_plot(acp_result(), 1, 4) })
output$plot_var_1_6 <- renderPlotly({ req(acp_result()); make_plan_plot(acp_result(), 1, 6) })
output$plot_var_1_7 <- renderPlotly({ req(acp_result()); make_plan_plot(acp_result(), 1, 7) })

output$interp_1_3 <- renderUI({ req(acp_result()); make_interp(acp_result(), 1, 3) })
output$interp_1_4 <- renderUI({ req(acp_result()); make_interp(acp_result(), 1, 4) })
output$interp_1_6 <- renderUI({ req(acp_result()); make_interp(acp_result(), 1, 6) })
output$interp_1_7 <- renderUI({ req(acp_result()); make_interp(acp_result(), 1, 7) })

output$plot_acp_custom <- renderPlotly({
  req(acp_result())
  dim1     <- as.integer(gsub("Dim\\.", "", input$acp_dim1))
  dim2     <- as.integer(gsub("Dim\\.", "", input$acp_dim2))
  top_vars <- get_top_vars_contrib(acp_result(), dim1, input$acp_top_n_custom)
  p <- switch(input$acp_plot_type,
              "var" = fviz_pca_var(acp_result(), axes = c(dim1, dim2),
                                   select.var = list(name = top_vars), col.var = "cos2",
                                   gradient.cols = c("#D32F2F", "#FFC107", "#1976D2"),
                                   repel = TRUE, ggtheme = theme_minimal()) +
                labs(title = paste0("Plan (", dim1, ", ", dim2, ")")) +
                theme(plot.title = element_text(face = "bold", color = "#1976D2")),
              "ind" = fviz_pca_ind(acp_result(), axes = c(dim1, dim2),
                                   col.ind = "#1976D2", alpha.ind = 0.5, ggtheme = theme_minimal()) +
                labs(title = paste0("Individus Plan (", dim1, ", ", dim2, ")")) +
                theme(plot.title = element_text(face = "bold", color = "#1976D2")),
              "biplot" = fviz_pca_biplot(acp_result(), axes = c(dim1, dim2),
                                         select.var = list(name = top_vars),
                                         col.var = "#D32F2F", col.ind = "#1976D2",
                                         alpha.ind = 0.4, repel = TRUE, ggtheme = theme_minimal()) +
                labs(title = paste0("Biplot Plan (", dim1, ", ", dim2, ")")) +
                theme(plot.title = element_text(face = "bold", color = "#1976D2"))
  )
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
})

output$acp_interp_axes <- renderUI({
  req(acp_result())
  dim1 <- as.integer(gsub("Dim\\.", "", input$acp_dim1))
  dim2 <- as.integer(gsub("Dim\\.", "", input$acp_dim2))
  make_interp(acp_result(), dim1, dim2)
})

kmeans_data <- reactive({
  req(acp_result())
  n_comp <- min(input$kmeans_ncp_use, ncol(acp_result()$ind$coord))
  as.data.frame(acp_result()$ind$coord[, 1:n_comp])
})

output$plot_elbow <- renderPlotly({
  input$run_kmeans
  req(acp_result())
  isolate(tryCatch({
    df  <- kmeans_data()
    wss <- sapply(1:10, function(k) { set.seed(42); kmeans(df, centers = k, nstart = 10, iter.max = 300, algorithm = "MacQueen")$tot.withinss })
    df_plot <- data.frame(K = 1:10, WSS = wss)
    p <- ggplot(df_plot, aes(x = K, y = WSS)) +
      geom_line(color = "#7B1FA2", linewidth = 1) + geom_point(color = "#7B1FA2", size = 3) +
      scale_x_continuous(breaks = 1:10) +
      labs(title = "Methode du coude", x = "Nombre de clusters K", y = "Inertie totale (WSS)") +
      theme_minimal() + theme(plot.title = element_text(face = "bold", color = "#7B1FA2"))
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
  }, error = function(e) { message("[ERREUR] plot_elbow : ", conditionMessage(e)); NULL }))
})

output$plot_silhouette <- renderPlotly({
  input$run_kmeans
  req(acp_result())
  isolate(tryCatch({
    df  <- kmeans_data()
    sil <- sapply(2:10, function(k) { set.seed(42); km <- kmeans(df, centers = k, nstart = 10, iter.max = 300, algorithm = "MacQueen"); mean(cluster::silhouette(km$cluster, dist(df))[, 3]) })
    df_plot <- data.frame(K = 2:10, Silhouette = sil)
    p <- ggplot(df_plot, aes(x = K, y = Silhouette)) +
      geom_line(color = "#1976D2", linewidth = 1) + geom_point(color = "#1976D2", size = 3) +
      scale_x_continuous(breaks = 2:10) +
      labs(title = "Indice de silhouette", x = "Nombre de clusters K", y = "Silhouette moyenne") +
      theme_minimal() + theme(plot.title = element_text(face = "bold", color = "#1976D2"))
    ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
  }, error = function(e) { message("[ERREUR] plot_silhouette : ", conditionMessage(e)); NULL }))
})

kmeans_result <- eventReactive(input$run_kmeans, {
  req(kmeans_data())
  tryCatch({
    set.seed(42)
    kmeans(kmeans_data(), centers = input$kmeans_k, nstart = 25, iter.max = 300, algorithm = "MacQueen")
  }, error = function(e) { message("[ERREUR] kmeans_result : ", conditionMessage(e)); NULL })
}, ignoreNULL = TRUE)

output$plot_kmeans_clusters <- renderPlotly({
  req(acp_result(), kmeans_result())
  p <- fviz_cluster(kmeans_result(), data = kmeans_data(), geom = "point",
                    ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal()) +
    labs(title    = paste0("K-means (K = ", input$kmeans_k, ")"),
         subtitle = paste0(input$kmeans_ncp_use, " composantes")) +
    theme(plot.title = element_text(face = "bold", color = "#7B1FA2"))
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
})

output$kmeans_interpretation <- renderUI({
  req(kmeans_result())
  km    <- kmeans_result()
  sizes <- table(km$cluster)
  tagList(
    h5(strong("Taille des groupes :")),
    tags$ul(lapply(seq_along(sizes), function(i) {
      tags$li(strong(paste0("Groupe ", i, " : ")),
              sizes[i], " obs (", round(sizes[i] / sum(sizes) * 100, 1), "%)")
    })),
    p(style = "color:#666;font-size:12px;",
      "Ratio inter/total : ", round(km$betweenss / km$totss * 100, 1), "%")
  )
})

output$table_kmeans_stats <- renderDT({
  req(acp_result(), kmeans_result())
  df_scores <- as.data.frame(acp_result()$ind$coord)
  df_scores$Cluster <- factor(kmeans_result()$cluster)
  n_comp <- min(input$kmeans_ncp_use, ncol(acp_result()$ind$coord))
  stats <- df_scores %>%
    group_by(Cluster) %>%
    summarise(across(paste0("Dim.", 1:n_comp), ~ round(mean(.), 3), .names = "Moy_{.col}"), N = n())
  datatable(stats, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
})

output$kmeans_auto_interp <- renderUI({
  req(kmeans_result(), acp_result())
  km    <- kmeans_result()
  ratio <- round(km$betweenss / km$totss * 100, 1)
  var1  <- round(acp_result()$eig[1, 2], 1)
  tagList(tags$ul(
    tags$li(strong("Qualite : "), paste0(ratio, "% de variance inter-groupe"),
            if (ratio >= 60) span(style = "color:#28a745;", " Bonne separation")
            else if (ratio >= 40) span(style = "color:#ffc107;", " Separation moyenne")
            else span(style = "color:#dc3545;", " Groupes peu distincts")),
    tags$li(strong(paste0("Composante 1 (", var1, "% variance) : ")),
            "Structure principale separant les groupes"),
    tags$li(strong("Conseil :"),
            " Comparez les groupes sur les variables d'emissions pour identifier les phases a fort impact environnemental")
  ))
})

cah_data <- reactive({
  req(acp_result())
  n_comp <- min(input$cah_ncp_use, ncol(acp_result()$ind$coord))
  as.data.frame(acp_result()$ind$coord[, 1:n_comp])
})

cah_result <- eventReactive(input$run_cah, {
  req(acp_result())
  tryCatch({
    n_comp   <- isolate(min(input$cah_ncp_use, ncol(acp_result()$ind$coord)))
    methode  <- isolate(input$cah_method)
    df       <- as.data.frame(acp_result()$ind$coord[, 1:n_comp, drop = FALSE])
    dist_mat <- dist(df, method = "euclidean")
    hclust(dist_mat, method = methode)
  }, error = function(e) { message("[ERREUR] cah_result : ", conditionMessage(e)); NULL })
}, ignoreNULL = TRUE)

cah_clusters <- reactive({
  req(cah_result())
  cutree(cah_result(), k = input$cah_k)
})

output$plot_dendro <- renderPlotly({
  req(cah_result(), acp_result())
  MAX_OBS <- 300
  n_total <- nrow(acp_result()$ind$coord)
  n_comp  <- min(isolate(input$cah_ncp_use), ncol(acp_result()$ind$coord))
  df_full <- as.data.frame(acp_result()$ind$coord[, 1:n_comp, drop = FALSE])
  if (n_total > MAX_OBS) {
    set.seed(42)
    df_sub <- df_full[sample(n_total, MAX_OBS), ]
  } else {
    df_sub <- df_full
  }
  hc_sub   <- hclust(dist(df_sub, method = "euclidean"), method = isolate(input$cah_method))
  note_sub <- if (n_total > MAX_OBS) paste0(" echantillon ", MAX_OBS, "/", n_total, " obs") else ""
  p <- fviz_dend(hc_sub,
                 k           = input$cah_k,
                 k_colors    = "jco",
                 rect        = TRUE,
                 rect_fill   = TRUE,
                 rect_border = "jco",
                 show_labels = FALSE,
                 ggtheme     = theme_minimal()) +
    labs(title    = paste0("Dendrogramme Methode ", isolate(input$cah_method)),
         subtitle = paste0("K = ", input$cah_k, " groupes", note_sub)) +
    theme(plot.title    = element_text(face = "bold", color = "#2E7D32"),
          plot.subtitle = element_text(color = "#666"))
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
})

output$plot_cah_clusters <- renderPlotly({
  req(acp_result(), cah_clusters())
  p <- fviz_cluster(list(data = cah_data(), cluster = cah_clusters()),
                    geom = "point", ellipse.type = "convex",
                    palette = "jco", ggtheme = theme_minimal()) +
    labs(title    = paste0("Groupes CAH (K = ", input$cah_k, ")"),
         subtitle = paste0("Methode ", input$cah_method, " ", input$cah_ncp_use, " composantes")) +
    theme(plot.title    = element_text(face = "bold", color = "#2E7D32"),
          plot.subtitle = element_text(color = "#666"))
  ggplotly(p) %>% layout(hoverlabel = list(bgcolor = "white"))
})

output$cah_interpretation <- renderUI({
  req(cah_clusters())
  sizes <- table(cah_clusters())
  tagList(tags$ul(lapply(seq_along(sizes), function(i) {
    tags$li(strong(paste0("Groupe ", i, " : ")),
            sizes[i], " obs (", round(sizes[i] / sum(sizes) * 100, 1), "%)")
  })))
})

output$table_cah_stats <- renderDT({
  req(acp_result(), cah_clusters())
  df_scores <- as.data.frame(acp_result()$ind$coord)
  df_scores$Groupe_CAH <- factor(cah_clusters())
  n_comp <- min(input$cah_ncp_use, ncol(acp_result()$ind$coord))
  stats <- df_scores %>%
    group_by(Groupe_CAH) %>%
    summarise(across(paste0("Dim.", 1:n_comp), ~ round(mean(.), 3), .names = "Moy_{.col}"), N = n())
  datatable(stats, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
})

output$cah_auto_interp <- renderUI({
  req(cah_clusters())
  sizes      <- table(cah_clusters())
  n_total    <- sum(sizes)
  plus_gros  <- names(which.max(sizes))
  plus_petit <- names(which.min(sizes))
  tagList(tags$ul(
    tags$li(strong("Groupe le plus grand : Groupe "), plus_gros,
            " (", sizes[plus_gros], " obs ",
            round(sizes[plus_gros] / n_total * 100, 1), "%)"),
    tags$li(strong("Groupe le plus petit : Groupe "), plus_petit,
            " (", sizes[plus_petit], " obs ",
            round(sizes[plus_petit] / n_total * 100, 1), "%)"),
    tags$li(strong("Conseil :"), " Comparez les groupes CAH avec K-means via le tableau de concordance."),
    tags$li(strong("Methode Ward.D2 :"), " Produit des groupes equilibres recommandee pour composantes ACP.")
  ))
})

output$table_eigenvalues_brut <- renderDT({
  req(acp_result())
  eig <- as.data.frame(round(acp_result()$eig, 4))
  eig$Composante <- rownames(eig)
  eig <- eig[, c("Composante", "eigenvalue", "percentage of variance", "cumulative percentage of variance")]
  colnames(eig) <- c("Composante", "Valeur propre", "Variance (%)", "Variance cumulee (%)")
  datatable(eig, rownames = FALSE, options = list(pageLength = 20, dom = "t")) %>%
    formatStyle("Valeur propre", backgroundColor = styleInterval(1, c("#FFCDD2", "#C8E6C9")))
})

output$table_var_coord <- renderDT({
  req(acp_result())
  coord <- as.data.frame(round(acp_result()$var$coord, 4))
  coord$Variable <- rownames(coord)
  coord <- coord[, c("Variable", setdiff(names(coord), "Variable"))]
  datatable(coord, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
})

output$table_var_contrib <- renderDT({
  req(acp_result())
  contrib <- as.data.frame(round(acp_result()$var$contrib, 3))
  contrib$Variable <- rownames(contrib)
  contrib <- contrib[, c("Variable", setdiff(names(contrib), "Variable"))]
  datatable(contrib, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
})

output$table_var_cos2 <- renderDT({
  req(acp_result())
  cos2 <- as.data.frame(round(acp_result()$var$cos2, 4))
  cos2$Variable <- rownames(cos2)
  cos2 <- cos2[, c("Variable", setdiff(names(cos2), "Variable"))]
  datatable(cos2, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
})

output$table_ind_coord <- renderDT({
  req(acp_result())
  ind <- as.data.frame(round(head(acp_result()$ind$coord, 50), 4))
  datatable(ind, rownames = TRUE, caption = "50 premieres observations",
            options = list(pageLength = 15, scrollX = TRUE))
})

output$table_kmeans_centers <- renderDT({
  req(kmeans_result())
  centers <- as.data.frame(round(kmeans_result()$centers, 4))
  centers$Cluster <- paste0("Cluster ", rownames(centers))
  centers <- centers[, c("Cluster", setdiff(names(centers), "Cluster"))]
  datatable(centers, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
})

output$table_kmeans_inertie <- renderDT({
  req(kmeans_result())
  km <- kmeans_result()
  df_in <- data.frame(
    Cluster       = c(paste0("Cluster ", seq_along(km$withinss)), "TOTAL"),
    Inertie_intra = c(round(km$withinss, 2), round(km$tot.withinss, 2)),
    Pct           = c(round(km$withinss / km$totss * 100, 2), round(km$tot.withinss / km$totss * 100, 2))
  )
  datatable(df_in, rownames = FALSE, options = list(pageLength = 15, dom = "t"))
})

output$table_kmeans_assign <- renderDT({
  req(acp_result(), kmeans_result())
  df <- as.data.frame(round(head(acp_result()$ind$coord, 50), 4))
  df$Cluster_Kmeans <- head(kmeans_result()$cluster, 50)
  df <- df[, c("Cluster_Kmeans", setdiff(names(df), "Cluster_Kmeans"))]
  datatable(df, rownames = TRUE, options = list(pageLength = 15, scrollX = TRUE)) %>%
    formatStyle("Cluster_Kmeans", backgroundColor = styleEqual(1:10, cluster_colors))
})

output$table_cah_sizes <- renderDT({
  req(cah_clusters())
  sizes <- as.data.frame(table(Groupe = cah_clusters()))
  sizes$Pct <- round(sizes$Freq / sum(sizes$Freq) * 100, 2)
  colnames(sizes) <- c("Groupe CAH", "N observations", "% du total")
  datatable(sizes, rownames = FALSE, options = list(pageLength = 10, dom = "t"))
})

output$table_concordance <- renderDT({
  req(kmeans_result(), cah_clusters())
  km_cl  <- kmeans_result()$cluster
  cah_cl <- cah_clusters()
  n_min  <- min(length(km_cl), length(cah_cl))
  conc   <- as.data.frame.matrix(table(KMeans = km_cl[1:n_min], CAH = cah_cl[1:n_min]))
  conc$Cluster_KMeans <- paste0("KM-", rownames(conc))
  conc <- conc[, c("Cluster_KMeans", setdiff(names(conc), "Cluster_KMeans"))]
  datatable(conc, rownames = FALSE,
            caption = "Lignes = K-means | Colonnes = CAH | Valeurs = nb obs en commun",
            options = list(pageLength = 10, dom = "t"))
})

output$table_cah_assign <- renderDT({
  req(acp_result(), cah_clusters())
  df <- as.data.frame(round(head(acp_result()$ind$coord, 50), 4))
  df$Groupe_CAH <- head(cah_clusters(), 50)
  df <- df[, c("Groupe_CAH", setdiff(names(df), "Groupe_CAH"))]
  datatable(df, rownames = TRUE, options = list(pageLength = 15, scrollX = TRUE)) %>%
    formatStyle("Groupe_CAH", backgroundColor = styleEqual(1:10, cluster_colors))
})

output$acp_summary <- renderPrint({
  req(acp_result())
  res <- acp_result()
  cat("=== Valeurs propres ===\n");        print(round(res$eig, 4))
  cat("\n=== Coordonnees variables ===\n"); print(round(res$var$coord, 4))
  cat("\n=== Contributions (%) ===\n");     print(round(res$var$contrib, 3))
  cat("\n=== Cos2 ===\n");                  print(round(res$var$cos2, 4))
})