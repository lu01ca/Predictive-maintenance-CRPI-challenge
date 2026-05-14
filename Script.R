# ==============================================================================
# LAB DATA CHALLENGE 2026 
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP E CARICAMENTO LIBRERIE
# ------------------------------------------------------------------------------
library(GGally)
library(tidyverse)
library(zoo)
library(gridExtra)
library(randomForest)
library(caret)
library(pROC)
library(smotefamily)

# Caricamento Dati
data <- read_csv('/Users/li/Desktop/Lab data challenge 2026/data.csv')

# Rinominare colonne per brevità e pulizia
colnames(data) <- c("rot", "cop", "lav", "fasc",
                    "tempe", "tempp", "mal0", "mal1",
                    "mal2", "mal3", "mal4", "mal5")

# Aggiungiamo l'Indice sequenziale
data$Indice <- 1:nrow(data)
data$fasc <- as.factor(data$fasc)


##############################
########### EDA ##############
##############################
{
  # ------------------------------------------------------------------------------
  # 2. ESPLORAZIONE DELLE VARIABILI (Univariata)
  # ------------------------------------------------------------------------------
  # Input numerici
  print(summary(data %>% select(rot, cop, lav, tempe, tempp)))
  
  # --- GRAFICI UNIVARIATI ---
  # Istogrammi variabili continue
  h1 <- ggplot(data, aes(x=rot)) + geom_histogram(fill="gray70", color="white") + theme_minimal()
  h2 <- ggplot(data, aes(x=cop)) + geom_histogram(fill="gray70", color="white") + theme_minimal()
  h3 <- ggplot(data, aes(x=lav)) + geom_histogram(fill="gray70", color="white") + theme_minimal()
  h4 <- ggplot(data, aes(x=tempe)) + geom_histogram(fill="gray70", color="white") + theme_minimal()
  h5 <- ggplot(data, aes(x=tempp)) + geom_histogram(fill="gray70", color="white") + theme_minimal()
  grid.arrange(h1, h2, h3, h4, h5, ncol = 2, top="Istogrammi Input Variables")
  
  # Boxplot input numerici (per outlier)
  b1 <- ggplot(data, aes(y=rot)) + geom_boxplot() + theme_minimal()
  b2 <- ggplot(data, aes(y=cop)) + geom_boxplot() + theme_minimal()
  b3 <- ggplot(data, aes(y=lav)) + geom_boxplot() + theme_minimal()
  b4 <- ggplot(data, aes(y=tempe)) + geom_boxplot() + theme_minimal()
  b5 <- ggplot(data, aes(y=tempp)) + geom_boxplot() + theme_minimal()
  grid.arrange(b1, b2, b3, b4, b5, ncol = 5, top="Boxplot Input Variables")
  
  # Barplot variabile categorica (fasc)
  b_fasc <- ggplot(data, aes(x=fasc, fill=fasc)) + geom_bar() + theme_minimal() + labs(title="Frequenza Fascia Prodotto")
  print(b_fasc)
  
  
  # ------------------------------------------------------------------------------
  # 3. SUMMARY STRATIFICATA PER FASCIA PRODOTTO
  # ------------------------------------------------------------------------------
  # Calcolo Median e IQR per Fascia
  quantili_fascia <- data %>%
    group_by(fasc) %>%
    summarise(across(c(rot, cop, lav, tempe, tempp), 
                     list(Q1 = ~quantile(., 0.25), Med = median, Q3 = ~quantile(., 0.75)))) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
  print(as.data.frame(quantili_fascia))
  
  
  # ------------------------------------------------------------------------------
  # 4. DATA CLEANING & ANALISI MULTI-ETICHETTA 
  # ------------------------------------------------------------------------------
  # Correzione delle 7 anomalie (mal0=1 ma tutti i tipi specifici=0)
  data <- data %>%
    mutate(
      mal0 = ifelse(mal1 == 0 & mal2 == 0 & mal3 == 0 & mal4 == 0 & mal5 == 0, 0, mal0)
    )
  
  # Riepilogo stati Malfunzionamento
  print(table(data$mal0))
  
  # Analisi Sovrapposizione: conteggio casi con più di un guasto contemporaneo
  guasti_multipli <- data %>%
    filter(rowSums(across(mal1:mal5)) > 1) %>%
    rowwise() %>%
    mutate(Combinazione = paste(which(c_across(mal1:mal5) == 1), collapse = "+")) %>%
    select(Indice, Combinazione, rot:mal5)
  # Esistono 20 casi Multi-Label
  print(as.data.frame(guasti_multipli))
  
  
  # ------------------------------------------------------------------------------
  # 5. SCATTERPLOT DELLE VARIE COPPIE DI VARIABILI
  # ------------------------------------------------------------------------------
  # PAIRPLOT (Matrice Scatterplot + Correlazioni grafiche)
  pairs_plot <- ggpairs(data, columns = 1:6,
                        mapping = aes(color = as.factor(mal0), alpha = 0.5),
                        upper = list(continuous = wrap("cor", size = 2)),
                        diag = list(continuous = "densityDiag"),
                        lower = list(continuous = wrap("points", size = 0.4))) +
    theme_bw() + labs(title = "Pairplot Variabili Input vs Stato mal0")
  print(pairs_plot)
  
  # ------------------------------------------------------------------------------
  # 6. FEATURE ENGINEERING GLOBALE
  # ------------------------------------------------------------------------------
  data <- data %>%
    # Varianza Mobile
    mutate(
      Varianza_Mobile_Coppia = replace_na(
        rollapplyr(cop, width = 5, FUN = var, fill = 0, partial = TRUE), 0
      )
    ) %>%
    # Variabili Fisiche e Interazioni
    mutate(
      Indice_Resistenza = cop / rot,
      Delta_Termico     = tempp - tempe,
      Sforzo_Usura      = lav * cop,
      Potenza = cop * rot,
      Num_rot = rot * lav
    )
  
  # ------------------------------------------------------------------------------
  # 7. CORRELOGRAMMA
  # ------------------------------------------------------------------------------
  dati_cor <- data %>%
    select(rot, cop, lav, tempp, Varianza_Mobile_Coppia, 
           Indice_Resistenza, Delta_Termico, Sforzo_Usura)
  
  matrice_cor <- cor(dati_cor, use = "complete.obs")
  
  # Heatmap delle Correlazioni
  h_cor <- as.data.frame(as.table(matrice_cor)) %>%
    ggplot(aes(x=Var1, y=Var2, fill=Freq)) + geom_tile() +
    geom_text(aes(label=round(Freq, 2)), size=2) +
    scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1)) +
    theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1))
  print(h_cor)
  
  
  # ------------------------------------------------------------------------------
  # 8. INFLUENZA DELLE VARIABILI (Time-to-Failure)
  # ------------------------------------------------------------------------------
  
  # Funzione per Firma del Guasto GLOBALE
  plot_ttf_avanzate <- function(target, finestra = 15) {
    indici <- which(data[[target]] == 1)
    if(length(indici) == 0) return(NULL)
    
    dati_ts <- map_dfr(indici, function(idx) {
      inizio <- max(1, idx - finestra)
      data[inizio:idx, ] %>%
        select(where(is.numeric), 
               -any_of(c("Indice", "mal0", "mal1", "mal2", "mal3", "mal4", "mal5"))) %>%
        mutate(across(everything(), ~ as.numeric(scale(.)))) %>%
        mutate(TTF = row_number() - n(), ID = as.character(idx))
    })
    
    dati_long <- dati_ts %>%
      pivot_longer(cols = -c(TTF, ID), names_to = "Var", values_to = "Val")
    
    # Media globale della traiettoria
    d_media <- dati_long %>% 
      group_by(TTF, Var) %>% 
      summarise(MedVal = mean(Val, na.rm=TRUE), .groups="drop")
    
    ggplot() +
      geom_line(data=dati_long, aes(x=TTF, y=Val, group=ID), color="gray80", alpha=0.4) +
      geom_line(data=d_media, aes(x=TTF, y=MedVal), color="dodgerblue", linewidth=1.2) +
      geom_point(data=filter(d_media, TTF==0), aes(x=TTF, y=MedVal), color="black", fill="dodgerblue", shape=21) +
      facet_wrap(~ Var, nrow = 5, scales="free_y") + 
      theme_minimal() +
      theme(strip.text = element_text(face="bold", size=10)) +
      labs(title=paste("Firma del", target, "(Proxy Avanzati)"), 
           x="Time-to-Failure (Ultime 15 lavorazioni)", y="Valore Standardizzato")
  }
  
  print(plot_ttf_avanzate("mal1")) #man mano farlo per tutti i malfunzionamenti
  
}

#############################################
########### MALFUNZIONAMENTO 1 ##############
#############################################
{
  # Definiamo le colonne da escludere (gli altri target mal)
  cols_to_drop <- c("mal0", "mal2", "mal3", "mal4", "mal5", "Indice")
  dataatures <- data[, !(names(data) %in% cols_to_drop)]
  dataatures <- as.data.frame(dataatures)
  
  # Split temporale — val_size=2000 mirror del test set
  n_total   <- nrow(dataatures)
  val_size  <- 2000
  train_idx <- 1:(n_total - val_size)
  val_idx   <- (n_total - val_size + 1):n_total
  
  train_set <- dataatures[train_idx, ]
  val_set   <- dataatures[val_idx,   ]
  
  cat("Positivi in train ORIGINALE:", sum(train_set$mal1 == 1), "\n")
  cat("Positivi in validation:",      sum(val_set$mal1   == 1), "\n")
  
  # ==============================================================================
  # 2. ONE-HOT ENCODING E SMOTE
  # ==============================================================================
  train_X <- train_set[, names(train_set) != "mal1"]
  train_Y <- train_set$mal1
  
  dummy_model_1 <- dummyVars(" ~ .", data = train_X)
  train_X_num   <- data.frame(predict(dummy_model_1, newdata = train_X))
  
  train_smote_obj_1 <- SMOTE(X = train_X_num, target = train_Y,
                             K = 5, dup_size = 5)
  train_smote_1 <- train_smote_obj_1$data
  names(train_smote_1)[ncol(train_smote_1)] <- "mal1"
  train_smote_1$mal1 <- as.factor(train_smote_1$mal1)
  
  cat("Positivi in train DOPO SMOTE:", sum(train_smote_1$mal1 == "1"), "\n")
  
  # Validation set: dummy encoding e target separato
  val_X       <- val_set[, names(val_set) != "mal1"]
  val_X_num_1 <- data.frame(predict(dummy_model_1, newdata = val_X))
  actual_mal1 <- as.factor(val_set$mal1)
  
  # ==============================================================================
  # 3. FOLD TEMPORALI su train_smote (growing window, 3 fold)
  # ==============================================================================
  n_train_1  <- nrow(train_set)
  init_win_1 <- floor(n_train_1 * 0.50)
  horizon_1  <- floor(n_train_1 * 0.167)
  
  get_cv_folds <- function(n, init_win, horizon) {
    folds <- list()
    start_val <- init_win + 1
    while ((start_val + horizon - 1) <= n) {
      end_val <- min(start_val + horizon - 1, n)
      folds[[length(folds) + 1]] <- list(
        train = 1:(start_val - 1),
        val   = start_val:end_val
      )
      start_val <- start_val + horizon
    }
    return(folds)
  }
  
  folds_1 <- get_cv_folds(n_train_1, init_win_1, horizon_1)
  cat("\nFold temporali:", length(folds_1), "\n")
  for (i in seq_along(folds_1)) {
    n_pos <- sum(train_set$mal1[folds_1[[i]]$val] == 1)
    cat(sprintf("  Fold %d: train=%d | val=%d | positivi_val=%d\n",
                i, length(folds_1[[i]]$train),
                length(folds_1[[i]]$val), n_pos))
  }
  
  # ==============================================================================
  # 4. RANDOM SEARCH — 100 combinazioni
  # ==============================================================================
  set.seed(123)
  n_iter_1 <- 100
  
  param_grid_1 <- data.frame(
    ntree    = sample(200:1000, n_iter_1, replace = TRUE),
    mtry     = sample(2:8,      n_iter_1, replace = TRUE),
    nodesize = sample(3:20,     n_iter_1, replace = TRUE),
    maxnodes = sample(20:300,   n_iter_1, replace = TRUE)
  )
  
  cv_results_1           <- param_grid_1
  cv_results_1$f2_cv    <- NA
  
  for (i in 1:n_iter_1) {
    fold_scores <- numeric(length(folds_1))
    
    for (j in seq_along(folds_1)) {
      fold_train_X <- train_X_num[folds_1[[j]]$train, ]
      fold_train_Y <- train_Y[folds_1[[j]]$train]
      fold_val_X   <- train_X_num[folds_1[[j]]$val, ]
      fold_val_Y   <- train_set$mal1[folds_1[[j]]$val]
      
      # SMOTE dentro il fold
      smote_fold <- SMOTE(X = fold_train_X, target = fold_train_Y,
                          K = 5, dup_size = 15)$data
      names(smote_fold)[ncol(smote_fold)] <- "mal1"
      smote_fold$mal1 <- as.factor(smote_fold$mal1)
      
      set.seed(42)
      mod <- randomForest(
        mal1 ~ .,
        data     = smote_fold,
        ntree    = param_grid_1$ntree[i],
        mtry     = param_grid_1$mtry[i],
        nodesize = param_grid_1$nodesize[i],
        maxnodes = param_grid_1$maxnodes[i]
      )
      
      probs_fold <- predict(mod, newdata = fold_val_X, type = "prob")[, "1"]
      pred_fold <- ifelse(probs_fold > 0.1, 1, 0)
      tp <- sum(pred_fold == 1 & fold_val_Y == 1)
      fp <- sum(pred_fold == 1 & fold_val_Y == 0)
      fn <- sum(pred_fold == 0 & fold_val_Y == 1)
      
      precision <- tp / (tp + fp + 1e-9)
      recall    <- tp / (tp + fn + 1e-9)
      f2        <- (1 + 4) * precision * recall / (4 * precision + recall + 1e-9)
      
      fold_scores[j] <- f2
    }
    
    cv_results_1$f2_cv[i] <- mean(fold_scores)
    
    cat(sprintf("[%02d/%d] ntree=%d mtry=%d nodesize=%d maxnodes=%d | f2=%.4f\n",
                i, n_iter_1,
                param_grid_1$ntree[i], param_grid_1$mtry[i],
                param_grid_1$nodesize[i], param_grid_1$maxnodes[i],
                cv_results_1$f2_cv[i]))
  }
  
  # ==============================================================================
  # 5. RISULTATI
  # ==============================================================================
  cv_results_1 <- cv_results_1[order(-cv_results_1$f2_cv), ]
  cat("\nTOP 5 COMBINAZIONI MAL1:\n")
  print(head(cv_results_1, 5))
  
  best_1 <- cv_results_1[1, ]
  ntree_star_1    <- best_1$ntree
  mtry_star_1     <- best_1$mtry
  nodesize_star_1 <- best_1$nodesize
  maxnodes_star_1 <- best_1$maxnodes
  
  cat("\nBEST RF HYPERPARAMETERS FOR MAL1:\n")
  cat("ntree:",    ntree_star_1,    "\n")
  cat("mtry:",     mtry_star_1,     "\n")
  cat("nodesize:", nodesize_star_1, "\n")
  cat("maxnodes:", maxnodes_star_1, "\n")
  cat("f2 CV:",   round(best_1$f2_cv, 4), "\n")
  
  # ==============================================================================
  # 6. MODELLO FINALE SU TUTTO IL TRAIN_SMOTE
  # ==============================================================================
  set.seed(123)
  mod.rf.final_1 <- randomForest(
    mal1 ~ .,
    data     = train_smote_1,
    ntree    = ntree_star_1,
    mtry     = mtry_star_1,
    nodesize = nodesize_star_1,
    maxnodes = maxnodes_star_1,
    importance = TRUE
  )
  
  # Previsione e ROC sul validation set
  probs_1 <- predict(mod.rf.final_1, newdata = val_X_num_1, type = "prob")[, "1"]
  roc_obj_1 <- pROC::roc(actual_mal1, probs_1, plot = TRUE)
  
  coords_1    <- pROC::coords(roc_obj_1, "best", best.method = "youden",
                              ret = c("threshold", "specificity", "sensitivity"))
  best_thresh_1 <- coords_1$threshold[1]
  cat("Soglia Ottimale (Youden):", best_thresh_1, "\n")
  
  pred_class_1  <- factor(ifelse(probs_1 > best_thresh_1, "1", "0"), levels = c("0","1"))
  print(caret::confusionMatrix(pred_class_1, actual_mal1, positive = "1"))
  
  # Salvataggio
  #saveRDS(mod.rf.final_1, "mod_rf_smote_mal1.2.rds")
  #save(best_thresh_1, best_1, file = "params_rf_smote_mal1.2.RData")
  
  # ==============================================================================
  # 7. PLOT IMPORTANCE
  # ==============================================================================
  imp_1 <- mod.rf.final_1$importance
  imp.df_1 <- data.frame(
    Variable         = rownames(imp_1),
    MeanDecreaseGini = imp_1[, "MeanDecreaseGini"],
    row.names        = NULL
  )
  imp.df_1$Percentage <- (imp.df_1$MeanDecreaseGini / sum(imp.df_1$MeanDecreaseGini)) * 100
  
  p_imp_1 <- ggplot(imp.df_1,
                    aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              hjust = -0.2, size = 3.5, fontface = "bold") +
    coord_flip() +
    labs(title = "RF Variable Importance (Mal1)", x = NULL, y = "Mean Decrease Gini") +
    theme_minimal()
  print(p_imp_1)
  
  # ==============================================================================
  # 8. PLOT OOB
  # ==============================================================================
  oob.error_1 <- mod.rf.final_1$err.rate[, "OOB"]
  oob_df_1    <- data.frame(ntrees = 1:ntree_star_1, oob.error = oob.error_1)
  start_idx_1 <- floor(ntree_star_1 * 0.66)
  
  print(
    ggplot(oob_df_1, aes(x = ntrees, y = oob.error)) +
      geom_line(color = "#3498DB", linewidth = 1.2, alpha = 0.9) +
      geom_hline(yintercept = mean(oob.error_1[start_idx_1:ntree_star_1]),
                 linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
      annotate("text",
               x     = ntree_star_1 * 0.55,
               y     = mean(oob.error_1[start_idx_1:ntree_star_1]),
               label = sprintf("Avg OOB (last 33%%): %.4f",
                               mean(oob.error_1[start_idx_1:ntree_star_1])),
               color = "#E74C3C", size = 4, fontface = "bold", vjust = -0.8) +
      labs(title    = "OOB Error Convergence (Mal1)",
           subtitle = sprintf("Final OOB: %.4f", oob.error_1[ntree_star_1]),
           x = "Number of trees", y = "OOB error rate") +
      theme_minimal(base_size = 13)
  )
}


#############################################
########### MALFUNZIONAMENTO 2 ##############
#############################################
{
  # Definiamo le colonne da escludere (gli altri target mal)
  cols_to_drop <- c("mal0", "mal1", "mal3", "mal4", "mal5", "Indice")
  dataatures <- data[, !(names(data) %in% cols_to_drop)]
  dataatures <- as.data.frame(dataatures)
  
  # Split temporale — val_size=2000 mirror del test set
  n_total   <- nrow(dataatures)
  val_size  <- 2000
  train_idx <- 1:(n_total - val_size)
  val_idx   <- (n_total - val_size + 1):n_total
  
  train_set <- dataatures[train_idx, ]
  val_set   <- dataatures[val_idx,   ]
  
  cat("Positivi in train ORIGINALE:", sum(train_set$mal2 == 1), "\n")
  cat("Positivi in validation:",      sum(val_set$mal2   == 1), "\n")
  
  # ==============================================================================
  # 2. ONE-HOT ENCODING E SMOTE
  # ==============================================================================
  train_X <- train_set[, names(train_set) != "mal2"]
  train_Y <- train_set$mal2
  
  dummy_model_2 <- dummyVars(" ~ .", data = train_X)
  train_X_num   <- data.frame(predict(dummy_model_2, newdata = train_X))
  
  train_smote_obj_2 <- SMOTE(X = train_X_num, target = train_Y,
                             K = 5, dup_size = 5)
  train_smote_2 <- train_smote_obj_2$data
  names(train_smote_2)[ncol(train_smote_2)] <- "mal2"
  train_smote_2$mal2 <- as.factor(train_smote_2$mal2)
  
  cat("Positivi in train DOPO SMOTE:", sum(train_smote_2$mal2 == "1"), "\n")
  
  # Validation set: dummy encoding e target separato
  val_X       <- val_set[, names(val_set) != "mal2"]
  val_X_num_2 <- data.frame(predict(dummy_model_2, newdata = val_X))
  actual_mal2 <- as.factor(val_set$mal2)
  
  # ==============================================================================
  # 3. FOLD TEMPORALI su train_smote (growing window, 3 fold)
  # ==============================================================================
  n_train_2  <- nrow(train_set)
  init_win_2 <- floor(n_train_2 * 0.50)
  horizon_2  <- floor(n_train_2 * 0.167)
  
  get_cv_folds <- function(n, init_win, horizon) {
    folds <- list()
    start_val <- init_win + 1
    while ((start_val + horizon - 1) <= n) {
      end_val <- min(start_val + horizon - 1, n)
      folds[[length(folds) + 1]] <- list(
        train = 1:(start_val - 1),
        val   = start_val:end_val
      )
      start_val <- start_val + horizon
    }
    return(folds)
  }
  
  folds_2 <- get_cv_folds(n_train_2, init_win_2, horizon_2)
  cat("\nFold temporali:", length(folds_2), "\n")
  for (i in seq_along(folds_2)) {
    n_pos <- sum(train_set$mal2[folds_2[[i]]$val] == 1)
    cat(sprintf("  Fold %d: train=%d | val=%d | positivi_val=%d\n",
                i, length(folds_2[[i]]$train),
                length(folds_2[[i]]$val), n_pos))
  }
  
  # ==============================================================================
  # 4. RANDOM SEARCH — 30 combinazioni
  # ==============================================================================
  set.seed(123)
  n_iter_2 <- 30
  
  param_grid_2 <- data.frame(
    ntree    = sample(200:1000, n_iter_2, replace = TRUE),
    mtry     = sample(2:8,      n_iter_2, replace = TRUE),
    nodesize = sample(3:20,     n_iter_2, replace = TRUE),
    maxnodes = sample(20:300,   n_iter_2, replace = TRUE)
  )
  
  cv_results_2           <- param_grid_2
  cv_results_2$f2_cv    <- NA
  
  for (i in 1:n_iter_2) {
    fold_scores <- numeric(length(folds_2))
    
    for (j in seq_along(folds_2)) {
      fold_train_X <- train_X_num[folds_2[[j]]$train, ]
      fold_train_Y <- train_Y[folds_2[[j]]$train]
      fold_val_X   <- train_X_num[folds_2[[j]]$val, ]
      fold_val_Y   <- train_set$mal2[folds_2[[j]]$val]
      
      # SMOTE dentro il fold
      smote_fold <- SMOTE(X = fold_train_X, target = fold_train_Y,
                          K = 5, dup_size = 15)$data
      names(smote_fold)[ncol(smote_fold)] <- "mal2"
      smote_fold$mal2 <- as.factor(smote_fold$mal2)
      
      set.seed(42)
      mod <- randomForest(
        mal2 ~ .,
        data     = smote_fold,
        ntree    = param_grid_2$ntree[i],
        mtry     = param_grid_2$mtry[i],
        nodesize = param_grid_2$nodesize[i],
        maxnodes = param_grid_2$maxnodes[i]
      )
      
      probs_fold <- predict(mod, newdata = fold_val_X, type = "prob")[, "1"]
      pred_fold <- ifelse(probs_fold > 0.1, 1, 0)
      tp <- sum(pred_fold == 1 & fold_val_Y == 1)
      fp <- sum(pred_fold == 1 & fold_val_Y == 0)
      fn <- sum(pred_fold == 0 & fold_val_Y == 1)
      
      precision <- tp / (tp + fp + 1e-9)
      recall    <- tp / (tp + fn + 1e-9)
      f2        <- (1 + 4) * precision * recall / (4 * precision + recall + 1e-9)
      
      fold_scores[j] <- f2
    }
    
    cv_results_2$f2_cv[i] <- mean(fold_scores)
    
    cat(sprintf("[%02d/%d] ntree=%d mtry=%d nodesize=%d maxnodes=%d | f2=%.4f\n",
                i, n_iter_2,
                param_grid_2$ntree[i], param_grid_2$mtry[i],
                param_grid_2$nodesize[i], param_grid_2$maxnodes[i],
                cv_results_2$f2_cv[i]))
  }
  
  # ==============================================================================
  # 5. RISULTATI
  # ==============================================================================
  cv_results_2 <- cv_results_2[order(-cv_results_2$f2_cv), ]
  cat("\nTOP 5 COMBINAZIONI mal2:\n")
  print(head(cv_results_2, 5))
  
  best_2 <- cv_results_2[1, ]
  ntree_star_2    <- best_2$ntree
  mtry_star_2     <- best_2$mtry
  nodesize_star_2 <- best_2$nodesize
  maxnodes_star_2 <- best_2$maxnodes
  
  cat("\nBEST RF HYPERPARAMETERS FOR mal2:\n")
  cat("ntree:",    ntree_star_2,    "\n")
  cat("mtry:",     mtry_star_2,     "\n")
  cat("nodesize:", nodesize_star_2, "\n")
  cat("maxnodes:", maxnodes_star_2, "\n")
  cat("f2 CV:",   round(best_2$f2_cv, 4), "\n")
  
  # ==============================================================================
  # 6. MODELLO FINALE SU TUTTO IL TRAIN_SMOTE
  # ==============================================================================
  set.seed(123)
  mod.rf.final_2 <- randomForest(
    mal2 ~ .,
    data     = train_smote_2,
    ntree    = ntree_star_2,
    mtry     = mtry_star_2,
    nodesize = nodesize_star_2,
    maxnodes = maxnodes_star_2,
    importance = TRUE
  )
  
  # Previsione e ROC sul validation set
  probs_2 <- predict(mod.rf.final_2, newdata = val_X_num_2, type = "prob")[, "1"]
  roc_obj_2 <- pROC::roc(actual_mal2, probs_2, plot = TRUE)
  
  coords_2    <- pROC::coords(roc_obj_2, "best", best.method = "youden",
                              ret = c("threshold", "specificity", "sensitivity"))
  best_thresh_2 <- coords_2$threshold[1]
  cat("Soglia Ottimale (Youden):", best_thresh_2, "\n")
  
  pred_class_2  <- factor(ifelse(probs_2 > best_thresh_2, "1", "0"), levels = c("0","1"))
  print(caret::confusionMatrix(pred_class_2, actual_mal2, positive = "1"))
  
  # Salvataggio
  #saveRDS(mod.rf.final_2, "mod_rf_smote_mal2.rds")
  #save(best_thresh_2, best_2, file = "params_rf_smote_mal2.RData")
  
  
  # ==============================================================================
  # 7. PLOT IMPORTANCE
  # ==============================================================================
  imp_2 <- mod.rf.final_2$importance
  imp.df_2 <- data.frame(
    Variable         = rownames(imp_2),
    MeanDecreaseGini = imp_2[, "MeanDecreaseGini"],
    row.names        = NULL
  )
  imp.df_2$Percentage <- (imp.df_2$MeanDecreaseGini / sum(imp.df_2$MeanDecreaseGini)) * 100
  
  p_imp_2 <- ggplot(imp.df_2,
                    aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              hjust = -0.2, size = 3.5, fontface = "bold") +
    coord_flip() +
    labs(title = "RF Variable Importance (mal2)", x = NULL, y = "Mean Decrease Gini") +
    theme_minimal()
  print(p_imp_2)
  
  # ==============================================================================
  # 8. PLOT OOB
  # ==============================================================================
  oob.error_2 <- mod.rf.final_2$err.rate[, "OOB"]
  oob_df_2    <- data.frame(ntrees = 1:ntree_star_2, oob.error = oob.error_2)
  start_idx_2 <- floor(ntree_star_2 * 0.66)
  
  print(
    ggplot(oob_df_2, aes(x = ntrees, y = oob.error)) +
      geom_line(color = "#3498DB", linewidth = 1.2, alpha = 0.9) +
      geom_hline(yintercept = mean(oob.error_2[start_idx_2:ntree_star_2]),
                 linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
      annotate("text",
               x     = ntree_star_2 * 0.55,
               y     = mean(oob.error_2[start_idx_2:ntree_star_2]),
               label = sprintf("Avg OOB (last 33%%): %.4f",
                               mean(oob.error_2[start_idx_2:ntree_star_2])),
               color = "#E74C3C", size = 4, fontface = "bold", vjust = -0.8) +
      labs(title    = "OOB Error Convergence (mal2)",
           subtitle = sprintf("Final OOB: %.4f", oob.error_2[ntree_star_2]),
           x = "Number of trees", y = "OOB error rate") +
      theme_minimal(base_size = 13)
  )
}


#############################################
########### MALFUNZIONAMENTO 3 ##############
#############################################
{
  # Definiamo le colonne da escludere (gli altri target mal)
  cols_to_drop <- c("mal0", "mal1", "mal2", "mal4", "mal5", "Indice")
  dataatures <- data[, !(names(data) %in% cols_to_drop)]
  dataatures <- as.data.frame(dataatures)
  
  # Split temporale — val_size=2000 mirror del test set
  n_total   <- nrow(dataatures)
  val_size  <- 2000
  train_idx <- 1:(n_total - val_size)
  val_idx   <- (n_total - val_size + 1):n_total
  
  train_set <- dataatures[train_idx, ]
  val_set   <- dataatures[val_idx,   ]
  
  cat("Positivi in train ORIGINALE:", sum(train_set$mal3 == 1), "\n")
  cat("Positivi in validation:",      sum(val_set$mal3   == 1), "\n")
  
  # ==============================================================================
  # 2. ONE-HOT ENCODING E SMOTE
  # ==============================================================================
  train_X <- train_set[, names(train_set) != "mal3"]
  train_Y <- train_set$mal3
  
  dummy_model_3 <- dummyVars(" ~ .", data = train_X)
  train_X_num   <- data.frame(predict(dummy_model_3, newdata = train_X))
  
  train_smote_obj_3 <- SMOTE(X = train_X_num, target = train_Y,
                             K = 5, dup_size = 5)
  train_smote_3 <- train_smote_obj_3$data
  names(train_smote_3)[ncol(train_smote_3)] <- "mal3"
  train_smote_3$mal3 <- as.factor(train_smote_3$mal3)
  
  cat("Positivi in train DOPO SMOTE:", sum(train_smote_3$mal3 == "1"), "\n")
  
  # Validation set: dummy encoding e target separato
  val_X       <- val_set[, names(val_set) != "mal3"]
  val_X_num_3 <- data.frame(predict(dummy_model_3, newdata = val_X))
  actual_mal3 <- as.factor(val_set$mal3)
  
  # ==============================================================================
  # 3. FOLD TEMPORALI su train_smote (growing window, 3 fold)
  # Nota: SMOTE non preserva l'ordine temporale — i fold si costruiscono
  # sul train originale e si applica SMOTE dentro ogni fold
  # ==============================================================================
  n_train_3  <- nrow(train_set)
  init_win_3 <- floor(n_train_3 * 0.50)
  horizon_3  <- floor(n_train_3 * 0.167)
  
  get_cv_folds <- function(n, init_win, horizon) {
    folds <- list()
    start_val <- init_win + 1
    while ((start_val + horizon - 1) <= n) {
      end_val <- min(start_val + horizon - 1, n)
      folds[[length(folds) + 1]] <- list(
        train = 1:(start_val - 1),
        val   = start_val:end_val
      )
      start_val <- start_val + horizon
    }
    return(folds)
  }
  
  folds_3 <- get_cv_folds(n_train_3, init_win_3, horizon_3)
  cat("\nFold temporali:", length(folds_3), "\n")
  for (i in seq_along(folds_3)) {
    n_pos <- sum(train_set$mal3[folds_3[[i]]$val] == 1)
    cat(sprintf("  Fold %d: train=%d | val=%d | positivi_val=%d\n",
                i, length(folds_3[[i]]$train),
                length(folds_3[[i]]$val), n_pos))
  }
  
  # ==============================================================================
  # 4. RANDOM SEARCH — 30 combinazioni
  # ==============================================================================
  set.seed(123)
  n_iter_3 <- 30
  
  param_grid_3 <- data.frame(
    ntree    = sample(200:1000, n_iter_3, replace = TRUE),
    mtry     = sample(2:8,      n_iter_3, replace = TRUE),
    nodesize = sample(3:20,     n_iter_3, replace = TRUE),
    maxnodes = sample(20:300,   n_iter_3, replace = TRUE)
  )
  
  cv_results_3           <- param_grid_3
  cv_results_3$f2_cv    <- NA
  
  for (i in 1:n_iter_3) {
    fold_scores <- numeric(length(folds_3))
    
    for (j in seq_along(folds_3)) {
      fold_train_X <- train_X_num[folds_3[[j]]$train, ]
      fold_train_Y <- train_Y[folds_3[[j]]$train]
      fold_val_X   <- train_X_num[folds_3[[j]]$val, ]
      fold_val_Y   <- train_set$mal3[folds_3[[j]]$val]
      
      # SMOTE dentro il fold
      smote_fold <- SMOTE(X = fold_train_X, target = fold_train_Y,
                          K = 5, dup_size = 15)$data
      names(smote_fold)[ncol(smote_fold)] <- "mal3"
      smote_fold$mal3 <- as.factor(smote_fold$mal3)
      
      set.seed(42)
      mod <- randomForest(
        mal3 ~ .,
        data     = smote_fold,
        ntree    = param_grid_3$ntree[i],
        mtry     = param_grid_3$mtry[i],
        nodesize = param_grid_3$nodesize[i],
        maxnodes = param_grid_3$maxnodes[i]
      )
      
      probs_fold <- predict(mod, newdata = fold_val_X, type = "prob")[, "1"]
      pred_fold <- ifelse(probs_fold > 0.1, 1, 0)
      tp <- sum(pred_fold == 1 & fold_val_Y == 1)
      fp <- sum(pred_fold == 1 & fold_val_Y == 0)
      fn <- sum(pred_fold == 0 & fold_val_Y == 1)
      
      precision <- tp / (tp + fp + 1e-9)
      recall    <- tp / (tp + fn + 1e-9)
      f2        <- (1 + 4) * precision * recall / (4 * precision + recall + 1e-9)
      
      fold_scores[j] <- f2
    }
    
    cv_results_3$f2_cv[i] <- mean(fold_scores)
    
    cat(sprintf("[%02d/%d] ntree=%d mtry=%d nodesize=%d maxnodes=%d | f2=%.4f\n",
                i, n_iter_3,
                param_grid_3$ntree[i], param_grid_3$mtry[i],
                param_grid_3$nodesize[i], param_grid_3$maxnodes[i],
                cv_results_3$f2_cv[i]))
  }
  
  # ==============================================================================
  # 5. RISULTATI
  # ==============================================================================
  cv_results_3 <- cv_results_3[order(-cv_results_3$f2_cv), ]
  cat("\nTOP 5 COMBINAZIONI mal3:\n")
  print(head(cv_results_3, 5))
  
  best_3 <- cv_results_3[1, ]
  ntree_star_3    <- best_3$ntree
  mtry_star_3     <- best_3$mtry
  nodesize_star_3 <- best_3$nodesize
  maxnodes_star_3 <- best_3$maxnodes
  
  cat("\nBEST RF HYPERPARAMETERS FOR mal3:\n")
  cat("ntree:",    ntree_star_3,    "\n")
  cat("mtry:",     mtry_star_3,     "\n")
  cat("nodesize:", nodesize_star_3, "\n")
  cat("maxnodes:", maxnodes_star_3, "\n")
  cat("f2 CV:",   round(best_3$f2_cv, 4), "\n")
  
  # ==============================================================================
  # 6. MODELLO FINALE SU TUTTO IL TRAIN_SMOTE
  # ==============================================================================
  set.seed(123)
  mod.rf.final_3 <- randomForest(
    mal3 ~ .,
    data     = train_smote_3,
    ntree    = ntree_star_3,
    mtry     = mtry_star_3,
    nodesize = nodesize_star_3,
    maxnodes = maxnodes_star_3,
    importance = TRUE
  )
  
  # Previsione e ROC sul validation set
  probs_3 <- predict(mod.rf.final_3, newdata = val_X_num_3, type = "prob")[, "1"]
  roc_obj_3 <- pROC::roc(actual_mal3, probs_3, plot = TRUE)
  
  coords_3    <- pROC::coords(roc_obj_3, "best", best.method = "youden",
                              ret = c("threshold", "specificity", "sensitivity"))
  best_thresh_3 <- coords_3$threshold[1]
  cat("Soglia Ottimale (Youden):", best_thresh_3, "\n")
  
  pred_class_3  <- factor(ifelse(probs_3 > best_thresh_3, "1", "0"), levels = c("0","1"))
  print(caret::confusionMatrix(pred_class_3, actual_mal3, positive = "1"))
  
  # Salvataggio
  #saveRDS(mod.rf.final_3, "mod_rf_smote_mal3.rds")
  #save(best_thresh_3, best_3, file = "params_rf_smote_mal3.RData")
  
  # ==============================================================================
  # 7. PLOT IMPORTANCE
  # ==============================================================================
  imp_3 <- mod.rf.final_3$importance
  imp.df_3 <- data.frame(
    Variable         = rownames(imp_3),
    MeanDecreaseGini = imp_3[, "MeanDecreaseGini"],
    row.names        = NULL
  )
  imp.df_3$Percentage <- (imp.df_3$MeanDecreaseGini / sum(imp.df_3$MeanDecreaseGini)) * 100
  
  p_imp_3 <- ggplot(imp.df_3,
                    aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              hjust = -0.2, size = 3.5, fontface = "bold") +
    coord_flip() +
    labs(title = "RF Variable Importance (mal3)", x = NULL, y = "Mean Decrease Gini") +
    theme_minimal()
  print(p_imp_3)
  
  # ==============================================================================
  # 8. PLOT OOB
  # ==============================================================================
  oob.error_3 <- mod.rf.final_3$err.rate[, "OOB"]
  oob_df_3    <- data.frame(ntrees = 1:ntree_star_3, oob.error = oob.error_3)
  start_idx_3 <- floor(ntree_star_3 * 0.66)
  
  print(
    ggplot(oob_df_3, aes(x = ntrees, y = oob.error)) +
      geom_line(color = "#3498DB", linewidth = 1.2, alpha = 0.9) +
      geom_hline(yintercept = mean(oob.error_3[start_idx_3:ntree_star_3]),
                 linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
      annotate("text",
               x     = ntree_star_3 * 0.55,
               y     = mean(oob.error_3[start_idx_3:ntree_star_3]),
               label = sprintf("Avg OOB (last 33%%): %.4f",
                               mean(oob.error_3[start_idx_3:ntree_star_3])),
               color = "#E74C3C", size = 4, fontface = "bold", vjust = -0.8) +
      labs(title    = "OOB Error Convergence (mal3)",
           subtitle = sprintf("Final OOB: %.4f", oob.error_3[ntree_star_3]),
           x = "Number of trees", y = "OOB error rate") +
      theme_minimal(base_size = 13)
  )
}

#############################################
########### MALFUNZIONAMENTO 4 ##############
#############################################
{
  # Definiamo le colonne da escludere (gli altri target mal)
  cols_to_drop <- c("mal0", "mal1", "mal2", "mal3", "mal5", "Indice")
  dataatures <- data[, !(names(data) %in% cols_to_drop)]
  dataatures <- as.data.frame(dataatures)
  
  # Split temporale — val_size=2000 mirror del test set
  n_total   <- nrow(dataatures)
  val_size  <- 2000
  train_idx <- 1:(n_total - val_size)
  val_idx   <- (n_total - val_size + 1):n_total
  
  train_set <- dataatures[train_idx, ]
  val_set   <- dataatures[val_idx,   ]
  
  cat("Positivi in train ORIGINALE:", sum(train_set$mal4 == 1), "\n")
  cat("Positivi in validation:",      sum(val_set$mal4   == 1), "\n")
  
  # ==============================================================================
  # 2. ONE-HOT ENCODING E SMOTE
  # ==============================================================================
  train_X <- train_set[, names(train_set) != "mal4"]
  train_Y <- train_set$mal4
  
  dummy_model_4 <- dummyVars(" ~ .", data = train_X)
  train_X_num   <- data.frame(predict(dummy_model_4, newdata = train_X))
  
  train_smote_obj_4 <- SMOTE(X = train_X_num, target = train_Y,
                             K = 5, dup_size = 5)
  train_smote_4 <- train_smote_obj_4$data
  names(train_smote_4)[ncol(train_smote_4)] <- "mal4"
  train_smote_4$mal4 <- as.factor(train_smote_4$mal4)
  
  cat("Positivi in train DOPO SMOTE:", sum(train_smote_4$mal4 == "1"), "\n")
  
  # Validation set: dummy encoding e target separato
  val_X       <- val_set[, names(val_set) != "mal4"]
  val_X_num_4 <- data.frame(predict(dummy_model_4, newdata = val_X))
  actual_mal4 <- as.factor(val_set$mal4)
  
  # ==============================================================================
  # 3. FOLD TEMPORALI su train_smote (growing window, 3 fold)
  # Nota: SMOTE non preserva l'ordine temporale — i fold si costruiscono
  # sul train originale e si applica SMOTE dentro ogni fold
  # ==============================================================================
  n_train_4  <- nrow(train_set)
  init_win_4 <- floor(n_train_4 * 0.50)
  horizon_4  <- floor(n_train_4 * 0.167)
  
  get_cv_folds <- function(n, init_win, horizon) {
    folds <- list()
    start_val <- init_win + 1
    while ((start_val + horizon - 1) <= n) {
      end_val <- min(start_val + horizon - 1, n)
      folds[[length(folds) + 1]] <- list(
        train = 1:(start_val - 1),
        val   = start_val:end_val
      )
      start_val <- start_val + horizon
    }
    return(folds)
  }
  
  folds_4 <- get_cv_folds(n_train_4, init_win_4, horizon_4)
  cat("\nFold temporali:", length(folds_4), "\n")
  for (i in seq_along(folds_4)) {
    n_pos <- sum(train_set$mal4[folds_4[[i]]$val] == 1)
    cat(sprintf("  Fold %d: train=%d | val=%d | positivi_val=%d\n",
                i, length(folds_4[[i]]$train),
                length(folds_4[[i]]$val), n_pos))
  }
  
  # ==============================================================================
  # 4. RANDOM SEARCH — 30 combinazioni
  # ==============================================================================
  set.seed(123)
  n_iter_4 <- 30
  
  param_grid_4 <- data.frame(
    ntree    = sample(200:1000, n_iter_4, replace = TRUE),
    mtry     = sample(2:8,      n_iter_4, replace = TRUE),
    nodesize = sample(3:20,     n_iter_4, replace = TRUE),
    maxnodes = sample(20:300,   n_iter_4, replace = TRUE)
  )
  
  cv_results_4           <- param_grid_4
  cv_results_4$f2_cv    <- NA
  
  for (i in 1:n_iter_4) {
    fold_scores <- numeric(length(folds_4))
    
    for (j in seq_along(folds_4)) {
      fold_train_X <- train_X_num[folds_4[[j]]$train, ]
      fold_train_Y <- train_Y[folds_4[[j]]$train]
      fold_val_X   <- train_X_num[folds_4[[j]]$val, ]
      fold_val_Y   <- train_set$mal4[folds_4[[j]]$val]
      
      # SMOTE dentro il fold
      smote_fold <- SMOTE(X = fold_train_X, target = fold_train_Y,
                          K = 5, dup_size = 15)$data
      names(smote_fold)[ncol(smote_fold)] <- "mal4"
      smote_fold$mal4 <- as.factor(smote_fold$mal4)
      
      set.seed(42)
      mod <- randomForest(
        mal4 ~ .,
        data     = smote_fold,
        ntree    = param_grid_4$ntree[i],
        mtry     = param_grid_4$mtry[i],
        nodesize = param_grid_4$nodesize[i],
        maxnodes = param_grid_4$maxnodes[i]
      )
      
      probs_fold <- predict(mod, newdata = fold_val_X, type = "prob")[, "1"]
      pred_fold <- ifelse(probs_fold > 0.1, 1, 0)
      tp <- sum(pred_fold == 1 & fold_val_Y == 1)
      fp <- sum(pred_fold == 1 & fold_val_Y == 0)
      fn <- sum(pred_fold == 0 & fold_val_Y == 1)
      
      precision <- tp / (tp + fp + 1e-9)
      recall    <- tp / (tp + fn + 1e-9)
      f2        <- (1 + 4) * precision * recall / (4 * precision + recall + 1e-9)
      
      fold_scores[j] <- f2
    }
    
    cv_results_4$f2_cv[i] <- mean(fold_scores)
    
    cat(sprintf("[%02d/%d] ntree=%d mtry=%d nodesize=%d maxnodes=%d | f2=%.4f\n",
                i, n_iter_4,
                param_grid_4$ntree[i], param_grid_4$mtry[i],
                param_grid_4$nodesize[i], param_grid_4$maxnodes[i],
                cv_results_4$f2_cv[i]))
  }
  
  # ==============================================================================
  # 5. RISULTATI
  # ==============================================================================
  cv_results_4 <- cv_results_4[order(-cv_results_4$f2_cv), ]
  cat("\nTOP 5 COMBINAZIONI mal4:\n")
  print(head(cv_results_4, 5))
  
  best_4 <- cv_results_4[1, ]
  ntree_star_4    <- best_4$ntree
  mtry_star_4     <- best_4$mtry
  nodesize_star_4 <- best_4$nodesize
  maxnodes_star_4 <- best_4$maxnodes
  
  cat("\nBEST RF HYPERPARAMETERS FOR mal4:\n")
  cat("ntree:",    ntree_star_4,    "\n")
  cat("mtry:",     mtry_star_4,     "\n")
  cat("nodesize:", nodesize_star_4, "\n")
  cat("maxnodes:", maxnodes_star_4, "\n")
  cat("f2 CV:",   round(best_4$f2_cv, 4), "\n")
  
  # ==============================================================================
  # 6. MODELLO FINALE SU TUTTO IL TRAIN_SMOTE
  # ==============================================================================
  set.seed(123)
  mod.rf.final_4 <- randomForest(
    mal4 ~ .,
    data     = train_smote_4,
    ntree    = ntree_star_4,
    mtry     = mtry_star_4,
    nodesize = nodesize_star_4,
    maxnodes = maxnodes_star_4,
    importance = TRUE
  )
  
  # Previsione e ROC sul validation set
  probs_4 <- predict(mod.rf.final_4, newdata = val_X_num_4, type = "prob")[, "1"]
  roc_obj_4 <- pROC::roc(actual_mal4, probs_4, plot = TRUE)
  
  coords_4    <- pROC::coords(roc_obj_4, "best", best.method = "youden",
                              ret = c("threshold", "specificity", "sensitivity"))
  best_thresh_4 <- coords_4$threshold[1]
  cat("Soglia Ottimale (Youden):", best_thresh_4, "\n")
  
  pred_class_4  <- factor(ifelse(probs_4 > best_thresh_4, "1", "0"), levels = c("0","1"))
  print(caret::confusionMatrix(pred_class_4, actual_mal4, positive = "1"))
  
  # Salvataggio
  #saveRDS(mod.rf.final_4, "mod_rf_smote_mal4.rds")
  #save(best_thresh_4, best_4, file = "params_rf_smote_mal4.RData")
  
  # ==============================================================================
  # 7. PLOT IMPORTANCE
  # ==============================================================================
  imp_4 <- mod.rf.final_4$importance
  imp.df_4 <- data.frame(
    Variable         = rownames(imp_4),
    MeanDecreaseGini = imp_4[, "MeanDecreaseGini"],
    row.names        = NULL
  )
  imp.df_4$Percentage <- (imp.df_4$MeanDecreaseGini / sum(imp.df_4$MeanDecreaseGini)) * 100
  
  p_imp_4 <- ggplot(imp.df_4,
                    aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              hjust = -0.2, size = 3.5, fontface = "bold") +
    coord_flip() +
    labs(title = "RF Variable Importance (mal4)", x = NULL, y = "Mean Decrease Gini") +
    theme_minimal()
  print(p_imp_4)
  
  # ==============================================================================
  # 8. PLOT OOB
  # ==============================================================================
  oob.error_4 <- mod.rf.final_4$err.rate[, "OOB"]
  oob_df_4    <- data.frame(ntrees = 1:ntree_star_4, oob.error = oob.error_4)
  start_idx_4 <- floor(ntree_star_4 * 0.66)
  
  print(
    ggplot(oob_df_4, aes(x = ntrees, y = oob.error)) +
      geom_line(color = "#3498DB", linewidth = 1.2, alpha = 0.9) +
      geom_hline(yintercept = mean(oob.error_4[start_idx_4:ntree_star_4]),
                 linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
      annotate("text",
               x     = ntree_star_4 * 0.55,
               y     = mean(oob.error_4[start_idx_4:ntree_star_4]),
               label = sprintf("Avg OOB (last 33%%): %.4f",
                               mean(oob.error_4[start_idx_4:ntree_star_4])),
               color = "#E74C3C", size = 4, fontface = "bold", vjust = -0.8) +
      labs(title    = "OOB Error Convergence (mal4)",
           subtitle = sprintf("Final OOB: %.4f", oob.error_4[ntree_star_4]),
           x = "Number of trees", y = "OOB error rate") +
      theme_minimal(base_size = 13)
  )
}

#####################################
########### PREVISIONE ##############
#####################################
{
  # carica i modelli
  mod.rf.final_1 <- readRDS("mod_rf_smote_mal1.rds")
  load("params_rf_smote_mal1.RData")
  mod.rf.final_2 <- readRDS("mod_rf_smote_mal2.rds")
  load("params_rf_smote_mal2.RData")
  mod.rf.final_3 <- readRDS("mod_rf_smote_mal3.rds")
  load("params_rf_smote_mal3.RData")
  mod.rf.final_4 <- readRDS("mod_rf_smote_mal4.rds")
  load("params_rf_smote_mal4.RData")
  
  
  test_set_raw <- read.csv("/Users/li/Desktop/Lab data challenge 2026/test.csv")
  test_set <- test_set_raw[, 1:6]
  colnames(test_set) <- c("rot", "cop", "lav", "fasc", "tempe", "tempp")
  
  test_set$fasc <- gsub("^M$", "MEDIA", test_set$fasc)
  # 1. Fix Fascia_prodotto
  test_set$fasc <- gsub("^M$", "MEDIA", test_set$fasc)
  
  # 4. Feature engineering su test con stato iniziale
  test_fe <- test_set %>%
    mutate(fasc = as.factor(fasc)) %>%
    mutate(
      Varianza_Mobile_Coppia = replace_na(
        rollapplyr(cop, width = 5, FUN = var, fill = 0, partial = TRUE), 0)
    ) %>%
    mutate(
      Indice_Resistenza = cop / rot,
      Delta_Termico     = tempp - tempe,
      Sforzo_Usura      = lav * cop,
      Potenza = cop * rot,
      Num_rot = rot * lav
    )
  
  
  feature_cols_test <- c("rot", "cop", "lav", "fasc",
                         "tempe", "tempp", "Varianza_Mobile_Coppia",
                         "Indice_Resistenza", "Delta_Termico", "Sforzo_Usura",
                         "Num_rot")
  
  test_fe_test <- test_fe[, feature_cols_test]
  
  # Prepara test set con dummy encoding per RF
  test_X_rf     <- test_fe[, names(test_fe) %in% names(train_X)]
  test_X_num <- data.frame(predict(dummy_model_1, newdata = test_X_rf))
  
  prob_1_test <- predict(mod.rf.final_1, newdata = test_X_num, type = "prob")[, "1"]
  prob_2_test <- predict(mod.rf.final_2, newdata = test_X_num, type = "prob")[, "1"]
  prob_3_test <- predict(mod.rf.final_3, newdata = test_X_num, type = "prob")[, "1"]
  prob_4_test <- predict(mod.rf.final_4, newdata = test_X_num, type = "prob")[, "1"]
  
  pred_mal1 <- ifelse(prob_1_test > best_thresh_1,  1, 0) #best_thresh_1 = 0.004930966
  pred_mal2 <- ifelse(prob_2_test > best_thresh_2, 1, 0) #best_thresh_2 = 0.7160121
  pred_mal3 <- ifelse(prob_3_test > best_thresh_3, 1, 0) #best_thresh_3 = 0.4062069
  pred_mal4 <- ifelse(prob_4_test > best_thresh_4, 1, 0) #best_thresh_4 = 0.4918851
  pred_mal5 <- rep(0, nrow(test_fe))
  
  pred_mal0 <- ifelse(pred_mal1==1 | pred_mal2==1 | pred_mal3==1 | pred_mal4==1, 1, 0)
  
  cat("mal0:", sum(pred_mal0), "\n")
  cat("mal1:", sum(pred_mal1), "\n")
  cat("mal2:", sum(pred_mal2), "\n")
  cat("mal3:", sum(pred_mal3), "\n")
  cat("mal4:", sum(pred_mal4), "\n")
  
  # ------------------------------------------------------------------------------
  # 6. SALVATAGGIO DEI RISULTATI
  # ------------------------------------------------------------------------------
  risultati_finali <- data.frame(
    Malfunzionamento = pred_mal0,
    Malfunzionamento_tipo1 = pred_mal1,
    Malfunzionamento_tipo2 = pred_mal2,
    Malfunzionamento_tipo3 = pred_mal3,
    Malfunzionamento_tipo4 = pred_mal4,
    Malfunzionamento_tipo5 = pred_mal5
  )
  
  # Esporta in CSV
  #write.csv(risultati_finali, "Definitivo.csv", row.names = FALSE)
}
