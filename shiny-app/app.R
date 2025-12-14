# ==============================================================================
# DASHBOARD SHINY — FILTROS (AND) COM LISTAS SINCRONIZADAS (CASCATA)
# Correção: ao filtrar por Macrocategoria (ou qualquer filtro), os elementos derivados
# (Filmes, Caregoria/Categorias, Macrocategorias, etc.) passam a obedecer estritamente
# ao recorte AND, e as listas dos filtros são atualizadas para refletir a interseção.
# ==============================================================================
# OBS: UI permanece igual (apenas comportamento server-side das choices/selected).
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(dplyr)
  library(DT)
  library(stringr)
  library(scales)
  library(htmlwidgets)
  library(readxl)

})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------------------------------------
# 1) DADOS
# ------------------------------------------------------------------------------
xlsx_path <- file.path("data", "x3-NOVO-DadosIntegradosAgentesTodosDetalhado.xlsx")

if (!file.exists(xlsx_path)) {
  xlsx_path <- file.path("shiny-app", "data", "x3-NOVO-DadosIntegradosAgentesTodosDetalhado.xlsx")
}

if (!file.exists(xlsx_path)) {
  stop(
    "Não encontrei o Excel em:\n",
    " - data/x3-NOVO-DadosIntegradosAgentesTodosDetalhado.xlsx\n",
    " - shiny-app/data/x3-NOVO-DadosIntegradosAgentesTodosDetalhado.xlsx\n",
    "Coloque o arquivo em shiny-app/data/ ou rode o app dentro de shiny-app/."
  )
}

dados_raw <- readxl::read_excel(xlsx_path) %>% as.data.frame()



req_cols <- c("Agente", "Agente Padronizado", "Tema Nível", "Nome Filme")
miss <- setdiff(req_cols, names(dados_raw))
if (length(miss) > 0) stop("Colunas ausentes no dataset: ", paste(miss, collapse = ", "))

snip256 <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""
  s <- str_squish(s)
  str_trunc(s, width = 256, side = "right", ellipsis = "…")
}
safe_chr <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""
  s
}

col_2009 <- "2009_Como você resumiria este filme? (em poucas palavras)"
col_2013 <- "2013a_Em uma ou duas palavras é um filme sobre...:"
col_caregoria <- "Caregoria"
col_dna <- "DNA-Mnem"
col_score_tema <- "Score Nível Tema"

# ------------------------------------------------------------------------------
# 2) PALETA / FORMAS
# ------------------------------------------------------------------------------
PALETA_CORES <- c(
  "Autor" = "#D32F2F",
  "Autor_N1" = "#EF9A9A",
  "Autor_N3" = "#E53935",
  "Autor_N5" = "#B71C1C",
  "Par" = "#FDD835",
  "Analista" = "#26C6DA",
  "Analista_SV" = "#1FA7C5",
  "Analista_VS" = "#0097A7",
  "IA" = "#135CAD",
  "ChatGPT" = "#4C9FEA",
  "Gemini" = "#1E88E5",
  "Outro" = "#9E9E9E"
)

PLOTLY_SYMBOLS <- c(
  "Autor" = "square",
  "Autor_N1" = "square",
  "Autor_N3" = "square",
  "Autor_N5" = "square",
  "Par" = "square-open",
  "Analista" = "diamond",
  "Analista_SV" = "triangle-up",
  "Analista_VS" = "triangle-down",
  "IA" = "x",
  "ChatGPT" = "asterisk",
  "Gemini" = "hash",
  "Outro" = "circle"
)

FILME_COR <- "#39D353"
FILME_SYMBOL <- "star"
FILME_SIZE <- 22

MACRO_COR <- "#7A7A7A"
MACRO_SYMBOL <- "circle-open"
MACRO_SIZE <- 26

CAT_COR <- "#4A4A4A"
CAT_SYMBOL <- "circle"
CAT_SIZE <- round(MACRO_SIZE * 0.8)

LINHA_COR_FILME_AGENTE <- "#606060"
LINHA_COR_CAT_MACRO <- "#9A9A9A"
LINHA_LARGURA <- 1

# ------------------------------------------------------------------------------
# 3) CATEGORIZAÇÃO + NORMALIZAÇÕES
# ------------------------------------------------------------------------------
categorizar_agente <- function(agente, agente_pad, analista) {
  txt <- paste(agente %||% "", agente_pad %||% "", analista %||% "", sep = " | ")
  txt <- tolower(txt)
  
  if (grepl("autor", txt)) {
    if (grepl("\\bn1\\b", txt)) return("Autor_N1")
    if (grepl("\\bn3\\b", txt)) return("Autor_N3")
    if (grepl("\\bn5\\b", txt)) return("Autor_N5")
    return("Autor")
  }
  if (grepl("par", txt)) return("Par")
  if (grepl("analista", txt)) {
    if (grepl("\\bsv\\b", txt)) return("Analista_SV")
    if (grepl("\\bvs\\b", txt)) return("Analista_VS")
    return("Analista")
  }
  if (grepl("chatgpt", txt)) return("ChatGPT")
  if (grepl("gemini", txt)) return("Gemini")
  if (grepl("\\bia\\b|intelig", txt)) return("IA")
  "Outro"
}

dados <- dados_raw %>%
  mutate(
    AgentePad = as.character(`Agente Padronizado`),
    TemaNivel = as.character(`Tema Nível`),
    FilmeNome = as.character(`Nome Filme`),
    AnalistaTxt = if ("Analista" %in% names(.)) as.character(.data[["Analista"]]) else NA_character_,
    # Importante: padroniza espaços para evitar “parece não filtrar” por mismatch de strings
    MacrocategoriaTxt = if ("Macrocategoria" %in% names(.)) str_squish(as.character(.data[["Macrocategoria"]])) else NA_character_,
    CaregoriaTxt = if (col_caregoria %in% names(.)) str_squish(as.character(.data[[col_caregoria]])) else NA_character_,
    DNAMnemTxt = if (col_dna %in% names(.)) str_squish(as.character(.data[[col_dna]])) else NA_character_,
    ScoreNivelTemaTxt = if (col_score_tema %in% names(.)) suppressWarnings(as.numeric(.data[[col_score_tema]])) else NA_real_,
    Categoria_Agente = mapply(categorizar_agente, .data[["Agente"]], AgentePad, AnalistaTxt),
    Categoria_Agente = factor(Categoria_Agente, levels = names(PALETA_CORES))
  ) %>%
  group_by(AgentePad) %>%
  mutate(FreqAgente_Global = n()) %>%
  ungroup() %>%
  mutate(
    MedidaPonderada_FreqAgente_x_ScoreTema = ifelse(
      is.na(ScoreNivelTemaTxt), NA_real_,
      FreqAgente_Global * ScoreNivelTemaTxt
    )
  )

colunas_numericas <- names(dados)[vapply(dados, is.numeric, logical(1))]
if (length(colunas_numericas) < 2) stop("Precisa de pelo menos 2 colunas numéricas para X e Y.")

pick_default <- function(cands, fallback) {
  hit <- intersect(cands, colunas_numericas)
  if (length(hit) > 0) hit[1] else fallback
}
default_x <- pick_default(
  c("Score de Participação autores ou pares de -2 a 5", "Score Nível Tema", "ScoreNivelTemaTxt"),
  colunas_numericas[1]
)
default_y <- pick_default(
  c("ValorConcordMesoCatLog10/2", "ValorConcordMesoCat"),
  colunas_numericas[min(2, length(colunas_numericas))]
)

# ------------------------------------------------------------------------------
# 4) OUTPUTS versionado
# ------------------------------------------------------------------------------
outputs_dir <- file.path(getwd(), "outputs")
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)

next_version_path <- function(prefix = "dispersao_agentes", ext = ".html") {
  files <- list.files(outputs_dir, pattern = paste0("^", prefix, "_v\\d{3}\\", ext, "$"), full.names = FALSE)
  v <- 0L
  if (length(files) > 0) {
    nums <- suppressWarnings(as.integer(stringr::str_extract(files, "(?<=_v)\\d{3}")))
    nums <- nums[!is.na(nums)]
    if (length(nums) > 0) v <- max(nums)
  }
  file.path(outputs_dir, sprintf("%s_v%03d%s", prefix, v + 1L, ext))
}

# ------------------------------------------------------------------------------
# 5) MEDIDAS CALCULADAS (no recorte filtrado)
# ------------------------------------------------------------------------------
add_calculated_measures <- function(d) {
  d <- d %>%
    group_by(AgentePad) %>%
    mutate(FreqAgente_Filtrado = n()) %>%
    ungroup() %>%
    mutate(
      MedidaPonderada_FreqAgente_x_ScoreTema = ifelse(
        is.na(ScoreNivelTemaTxt), NA_real_,
        FreqAgente_Filtrado * ScoreNivelTemaTxt
      )
    )
  d
}

# ------------------------------------------------------------------------------
# 6) PREP PLOT DATA (agentes)
# ------------------------------------------------------------------------------
prep_plot_data <- function(d, xcol, ycol, agregar = TRUE, tipo = "ftc") {
  d <- d %>% filter(!is.na(.data[[xcol]]), !is.na(.data[[ycol]]))
  if (!agregar) return(d)
  
  g <- switch(
    tipo,
    "ftc" = d %>% group_by(FilmeNome, TemaNivel, Categoria_Agente),
    "fc"  = d %>% group_by(FilmeNome, Categoria_Agente),
    "tc"  = d %>% group_by(TemaNivel, Categoria_Agente),
    d %>% group_by(FilmeNome, TemaNivel, Categoria_Agente)
  )
  
  d2 <- g %>%
    summarise(
      n = n(),
      AgentePad = dplyr::first(AgentePad),
      Macrocategoria = dplyr::first(MacrocategoriaTxt),
      Caregoria = dplyr::first(CaregoriaTxt),
      `DNA-Mnem` = dplyr::first(DNAMnemTxt),
      ScoreNivelTema = dplyr::first(ScoreNivelTemaTxt),
      FreqAgente_Filtrado = dplyr::first(FreqAgente_Filtrado),
      MedidaPonderada_FreqAgente_x_ScoreTema = dplyr::first(MedidaPonderada_FreqAgente_x_ScoreTema),
      `2009_Como você resumiria este filme? (em poucas palavras)` =
        if (col_2009 %in% names(d)) dplyr::first(.data[[col_2009]]) else NA_character_,
      `2013a_Em uma ou duas palavras é um filme sobre...:` =
        if (col_2013 %in% names(d)) dplyr::first(.data[[col_2013]]) else NA_character_,
      x = mean(.data[[xcol]], na.rm = TRUE),
      y = mean(.data[[ycol]], na.rm = TRUE),
      .groups = "drop"
    )
  
  d2[[xcol]] <- d2$x
  d2[[ycol]] <- d2$y
  d2
}

# ------------------------------------------------------------------------------
# 7) PLOTLY — dispersão (filmes, macro, categoria, linhas)
# ------------------------------------------------------------------------------
make_plotly_scatter <- function(d_points, d_base, xcol, ycol, var_tamanho = "none",
                                transparencia = 0.7, mostrar_legenda = TRUE) {
  
  d_base_xy <- d_base %>% filter(!is.na(.data[[xcol]]), !is.na(.data[[ycol]]))
  if (nrow(d_points) == 0) {
    return(plot_ly() %>% layout(title = list(text = "Sem dados após filtros", x = 0.5)))
  }
  
  # Filmes
  filmes_cent <- d_base_xy %>%
    group_by(FilmeNome) %>%
    summarise(
      x_filme = mean(.data[[xcol]], na.rm = TRUE),
      y_filme = mean(.data[[ycol]], na.rm = TRUE),
      n_filme = n(),
      Macrocategoria = dplyr::first(MacrocategoriaTxt),
      Caregoria = dplyr::first(CaregoriaTxt),
      `DNA-Mnem` = dplyr::first(DNAMnemTxt),
      MedidaPonderada_FreqAgente_x_ScoreTema = mean(MedidaPonderada_FreqAgente_x_ScoreTema, na.rm = TRUE),
      .groups = "drop"
    )
  
  filmes_cent$hover_filme <- paste0(
    "<b>Filme:</b> ", safe_chr(filmes_cent$FilmeNome),
    "<br><b>n:</b> ", filmes_cent$n_filme,
    ifelse(is.na(filmes_cent$Macrocategoria) | filmes_cent$Macrocategoria == "", "", paste0("<br><b>Macrocategoria:</b> ", safe_chr(filmes_cent$Macrocategoria))),
    ifelse(is.na(filmes_cent$Caregoria) | filmes_cent$Caregoria == "", "", paste0("<br><b>Caregoria:</b> ", safe_chr(filmes_cent$Caregoria))),
    ifelse(is.na(filmes_cent$`DNA-Mnem`) | filmes_cent$`DNA-Mnem` == "", "", paste0("<br><b>DNA-Mnem:</b> ", safe_chr(filmes_cent$`DNA-Mnem`))),
    "<hr>",
    "<b>Centróide X:</b> ", format(filmes_cent$x_filme, digits = 6),
    "<br><b>Centróide Y:</b> ", format(filmes_cent$y_filme, digits = 6)
  )
  
  # Macrocategorias
  macro_cent <- d_base_xy %>%
    filter(!is.na(MacrocategoriaTxt), MacrocategoriaTxt != "") %>%
    group_by(MacrocategoriaTxt) %>%
    summarise(
      x_macro = mean(.data[[xcol]], na.rm = TRUE),
      y_macro = mean(.data[[ycol]], na.rm = TRUE),
      n_macro = n(),
      n_filmes = n_distinct(FilmeNome),
      .groups = "drop"
    ) %>% rename(Macrocategoria = MacrocategoriaTxt)
  
  macro_cent$hover_macro <- paste0(
    "<b>Macrocategoria:</b> ", safe_chr(macro_cent$Macrocategoria),
    "<br><b>n:</b> ", macro_cent$n_macro,
    "<br><b>n filmes:</b> ", macro_cent$n_filmes,
    "<hr>",
    "<b>Centróide X:</b> ", format(macro_cent$x_macro, digits = 6),
    "<br><b>Centróide Y:</b> ", format(macro_cent$y_macro, digits = 6)
  )
  
  # Categorias (Caregoria) vinculadas a macro
  cat_cent <- d_base_xy %>%
    filter(!is.na(CaregoriaTxt), CaregoriaTxt != "", !is.na(MacrocategoriaTxt), MacrocategoriaTxt != "") %>%
    group_by(MacrocategoriaTxt, CaregoriaTxt) %>%
    summarise(
      x_cat = mean(.data[[xcol]], na.rm = TRUE),
      y_cat = mean(.data[[ycol]], na.rm = TRUE),
      n_cat = n(),
      n_filmes = n_distinct(FilmeNome),
      top_dna = paste0(head(names(sort(table(DNAMnemTxt), decreasing = TRUE)), 3), collapse = "; "),
      .groups = "drop"
    ) %>%
    rename(Macrocategoria = MacrocategoriaTxt, Caregoria = CaregoriaTxt)
  
  cat_cent$hover_cat <- paste0(
    "<b>Caregoria:</b> ", safe_chr(cat_cent$Caregoria),
    "<br><b>Macrocategoria:</b> ", safe_chr(cat_cent$Macrocategoria),
    "<br><b>n:</b> ", cat_cent$n_cat,
    "<br><b>n filmes:</b> ", cat_cent$n_filmes,
    ifelse(is.na(cat_cent$top_dna) | cat_cent$top_dna == "", "", paste0("<br><b>Top DNA-Mnem:</b> ", safe_chr(cat_cent$top_dna))),
    "<hr>",
    "<b>Centróide X:</b> ", format(cat_cent$x_cat, digits = 6),
    "<br><b>Centróide Y:</b> ", format(cat_cent$y_cat, digits = 6)
  )
  
  # Linhas filme → agentes exibidos
  linhas_filme_agente <- d_points %>%
    select(FilmeNome, all_of(xcol), all_of(ycol)) %>%
    left_join(filmes_cent %>% select(FilmeNome, x_filme, y_filme), by = "FilmeNome") %>%
    filter(!is.na(x_filme), !is.na(y_filme)) %>%
    mutate(x0 = x_filme, y0 = y_filme, x1 = .data[[xcol]], y1 = .data[[ycol]])
  
  # Linhas categoria → macro
  linhas_cat_macro <- cat_cent %>%
    left_join(macro_cent %>% select(Macrocategoria, x_macro, y_macro), by = "Macrocategoria") %>%
    filter(!is.na(x_macro), !is.na(y_macro)) %>%
    mutate(x0 = x_cat, y0 = y_cat, x1 = x_macro, y1 = y_macro)
  
  linha_op_1 <- min(0.60, max(0.05, transparencia * 0.35))
  linha_op_2 <- min(0.60, max(0.05, transparencia * 0.45))
  
  # Tamanho agentes
  if (!is.null(var_tamanho) && var_tamanho != "none" && var_tamanho %in% names(d_points)) {
    v <- d_points[[var_tamanho]]
    if (all(is.na(v))) d_points$TamanhoPonto <- 10
    else d_points$TamanhoPonto <- ifelse(is.na(v), 10, scales::rescale(v, to = c(8, 22)))
  } else {
    if ("n" %in% names(d_points)) {
      cap <- suppressWarnings(quantile(d_points$n, 0.95, na.rm = TRUE))
      if (is.na(cap) || cap <= 0) cap <- max(d_points$n, na.rm = TRUE)
      d_points$TamanhoPonto <- scales::rescale(pmin(d_points$n, cap), to = c(9, 26))
    } else d_points$TamanhoPonto <- 10
  }
  

# Hover agentes (inclui macro/caregoria/dna) — corrigido linha-a-linha
mac <- if ("Macrocategoria" %in% names(d_points)) safe_chr(d_points$Macrocategoria) else rep("", nrow(d_points))
careg <- if ("Caregoria" %in% names(d_points)) safe_chr(d_points$Caregoria) else rep("", nrow(d_points))
dna <- if ("DNA-Mnem" %in% names(d_points)) safe_chr(d_points$`DNA-Mnem`) else rep("", nrow(d_points))

d_points$hover <- paste0(
  "<b>Filme:</b> ", safe_chr(d_points$FilmeNome),
  "<br><b>Categoria_Agente:</b> ", safe_chr(d_points$Categoria_Agente),
  "<br><b>Agente Padronizado:</b> ", safe_chr(d_points$AgentePad),
  "<br><b>Tema:</b> ", safe_chr(d_points$TemaNivel),
  ifelse(mac != "", paste0("<br><b>Macrocategoria:</b> ", mac), ""),
  ifelse(careg != "", paste0("<br><b>Caregoria:</b> ", careg), ""),
  ifelse(dna != "", paste0("<br><b>DNA-Mnem:</b> ", dna), ""),
  if ("n" %in% names(d_points)) paste0("<br><b>n (agregado):</b> ", d_points$n) else "",
  "<hr>",
  "<b>X:</b> ", format(d_points[[xcol]], digits = 6),
  "<br><b>Y:</b> ", format(d_points[[ycol]], digits = 6)
)

  
  p <- plot_ly()
  
  if (nrow(linhas_cat_macro) > 0) {
    p <- p %>% add_segments(
      data = linhas_cat_macro,
      x = ~x0, y = ~y0, xend = ~x1, yend = ~y1,
      inherit = FALSE, showlegend = FALSE, hoverinfo = "skip",
      line = list(color = LINHA_COR_CAT_MACRO, width = LINHA_LARGURA),
      opacity = linha_op_2
    )
  }
  
  if (nrow(linhas_filme_agente) > 0) {
    p <- p %>% add_segments(
      data = linhas_filme_agente,
      x = ~x0, y = ~y0, xend = ~x1, yend = ~y1,
      inherit = FALSE, showlegend = FALSE, hoverinfo = "skip",
      line = list(color = LINHA_COR_FILME_AGENTE, width = LINHA_LARGURA),
      opacity = linha_op_1
    )
  }
  
  if (nrow(macro_cent) > 0) {
    p <- p %>% add_trace(
      data = macro_cent,
      x = ~x_macro, y = ~y_macro,
      type = "scatter", mode = "markers",
      name = "Macrocategoria",
      text = ~hover_macro, hoverinfo = "text",
      marker = list(size = MACRO_SIZE, color = MACRO_COR, symbol = MACRO_SYMBOL,
                    opacity = transparencia, line = list(width = 1.2, color = "black"))
    )
  }
  
  if (nrow(cat_cent) > 0) {
    p <- p %>% add_trace(
      data = cat_cent,
      x = ~x_cat, y = ~y_cat,
      type = "scatter", mode = "markers",
      name = "Caregoria",
      text = ~hover_cat, hoverinfo = "text",
      marker = list(size = CAT_SIZE, color = CAT_COR, symbol = CAT_SYMBOL,
                    opacity = transparencia, line = list(width = 1.0, color = "black"))
    )
  }
  
  cats <- sort(unique(as.character(d_points$Categoria_Agente)))
  for (ct in cats) {
    dd <- d_points %>% filter(as.character(Categoria_Agente) == ct)
    p <- p %>% add_trace(
      data = dd,
      x = ~.data[[xcol]], y = ~.data[[ycol]],
      type = "scatter", mode = "markers",
      name = ct,
      text = ~hover, hoverinfo = "text",
      marker = list(size = ~TamanhoPonto, opacity = transparencia,
                    color = PALETA_CORES[[ct]] %||% "#9E9E9E",
                    symbol = PLOTLY_SYMBOLS[[ct]] %||% "circle",
                    line = list(width = 1, color = "black"))
    )
  }
  
  if (nrow(filmes_cent) > 0) {
    p <- p %>% add_trace(
      data = filmes_cent,
      x = ~x_filme, y = ~y_filme,
      type = "scatter", mode = "markers",
      name = "Filme",
      text = ~hover_filme, hoverinfo = "text",
      marker = list(size = FILME_SIZE, color = FILME_COR, symbol = FILME_SYMBOL,
                    opacity = transparencia, line = list(width = 1.2, color = "black"))
    )
  }
  
  p %>%
    layout(
      title = list(text = paste0("<b>", xcol, " × ", ycol, "</b>"), x = 0.5),
      xaxis = list(title = xcol, gridcolor = "#ecf0f1", zerolinecolor = "#bdc3c7"),
      yaxis = list(title = ycol, gridcolor = "#ecf0f1", zerolinecolor = "#bdc3c7"),
      plot_bgcolor = "#ffffff",
      paper_bgcolor = "#ffffff",
      showlegend = mostrar_legenda,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18),
      hovermode = "closest",
      margin = list(l = 70, r = 30, t = 70, b = 110)
    ) %>%
    config(displaylogo = FALSE, scrollZoom = TRUE)
}

# ------------------------------------------------------------------------------
# 8) UI (igual ao seu último)
# ------------------------------------------------------------------------------
grupo_choices <- c("Categoria_Agente","AgentePad","TemaNivel","MacrocategoriaTxt","CaregoriaTxt","FilmeNome")
grupo_labels <- c(
  "Categoria de Agente","Agente Padronizado","Tema (Nível)","Macrocategoria","Caregoria","Filme"
)
names(grupo_labels) <- grupo_choices

make_plotly_violin <- function(d, medida, grupo, transparencia = 0.7) {
  d2 <- d %>% filter(!is.na(.data[[medida]])) %>% mutate(Grupo = as.character(.data[[grupo]]))
  if (nrow(d2) == 0) return(plot_ly() %>% layout(title = list(text = "Sem dados para violino", x = 0.5)))
  top_grupos <- d2 %>% count(Grupo, sort = TRUE) %>% slice_head(n = 30) %>% pull(Grupo)
  d2 <- d2 %>% filter(Grupo %in% top_grupos)
  
  plot_ly(d2, x = ~Grupo, y = ~.data[[medida]], type = "violin",
          box = list(visible = TRUE), meanline = list(visible = TRUE),
          points = "outliers", opacity = transparencia) %>%
    layout(
      title = list(text = paste0("<b>Violino — ", medida, " por ", grupo, "</b>"), x = 0.5),
      xaxis = list(title = grupo, tickangle = -30),
      yaxis = list(title = medida),
      plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
      margin = list(l = 70, r = 30, t = 70, b = 110)
    ) %>% config(displaylogo = FALSE, scrollZoom = TRUE)
}

make_plotly_bar <- function(d, medida, grupo, fun = "mean", transparencia = 0.9) {
  d2 <- d %>% mutate(Grupo = as.character(.data[[grupo]]))
  
  if (fun == "count") {
    agg <- d2 %>% count(Grupo, name = "valor") %>% arrange(desc(valor))
    ytitle <- "Contagem"
  } else {
    d2 <- d2 %>% filter(!is.na(.data[[medida]]))
    if (nrow(d2) == 0) return(plot_ly() %>% layout(title = list(text = "Sem dados para barras", x = 0.5)))
    agg <- d2 %>%
      group_by(Grupo) %>%
      summarise(
        valor = dplyr::case_when(
          fun == "mean" ~ mean(.data[[medida]], na.rm = TRUE),
          fun == "median" ~ median(.data[[medida]], na.rm = TRUE),
          fun == "sum" ~ sum(.data[[medida]], na.rm = TRUE),
          TRUE ~ mean(.data[[medida]], na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>% arrange(desc(valor))
    ytitle <- paste0(fun, "(", medida, ")")
  }
  
  agg <- agg %>% slice_head(n = 30)
  
  plot_ly(agg, x = ~reorder(Grupo, valor), y = ~valor, type = "bar", opacity = transparencia) %>%
    layout(
      title = list(text = paste0("<b>Barras — ", ytitle, " por ", grupo, "</b>"), x = 0.5),
      xaxis = list(title = grupo, tickangle = -30),
      yaxis = list(title = ytitle),
      plot_bgcolor = "#ffffff", paper_bgcolor = "#ffffff",
      margin = list(l = 70, r = 30, t = 70, b = 110)
    ) %>% config(displaylogo = FALSE, scrollZoom = TRUE)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Tahoma, sans-serif; background-color: #f5f7fa; }
      .title-panel {
        background: linear-gradient(135deg, #2c3e50, #3498db);
        color: white; padding: 18px; border-radius: 10px; margin-bottom: 16px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.10);
      }
      .well {
        background-color: white; border-radius: 10px; border: 1px solid #e1e8ed;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .btn-primary { background: linear-gradient(135deg, #2c3e50, #3498db); border: none; font-weight: bold; }
      .stat-card {
        background: white; border-radius: 10px; padding: 12px; margin: 8px 0;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
        border-left: 4px solid #3498db;
      }
      .small-note { color: #7f8c8d; font-style: italic; text-align: center; }
      .sub-controls { background:#ffffff; border:1px solid #e1e8ed; border-radius:10px; padding:12px; margin-bottom:12px; }
    "))
  ),
  div(class = "title-panel",
      h2("ANÁLISE DE AGENTES — FCT IA 2024", style = "margin:0;"),
      h4("Dispersão, violino e barras com filtros múltiplos (AND) e listas sincronizadas", style = "margin:6px 0 0 0; opacity:0.9;")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "well",
          h4("FILTROS (multi)"),
          selectizeInput(
            "filtro_categoria_agente", "Categoria de Agente:",
            choices = levels(dados$Categoria_Agente),
            selected = levels(dados$Categoria_Agente),
            multiple = TRUE,
            options = list(plugins = list("remove_button", "drag_drop"), placeholder = "Selecione uma ou mais…")
          ),
          selectizeInput(
            "filtro_agente_pad", "Agente Padronizado:",
            choices = sort(unique(na.omit(dados$AgentePad))),
            selected = sort(unique(na.omit(dados$AgentePad))),
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Selecione…")
          ),
          selectizeInput(
            "filtro_tema", "Tema (Nível):",
            choices = sort(unique(na.omit(dados$TemaNivel))),
            selected = sort(unique(na.omit(dados$TemaNivel))),
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Selecione…")
          ),
          selectizeInput(
            "filtro_macrocategoria", "Macrocategoria:",
            choices = sort(unique(na.omit(dados$MacrocategoriaTxt))),
            selected = sort(unique(na.omit(dados$MacrocategoriaTxt))),
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Selecione…")
          ),
          selectizeInput(
            "filtro_caregoria", "Caregoria (busca):",
            choices = sort(unique(na.omit(dados$CaregoriaTxt))),
            selected = sort(unique(na.omit(dados$CaregoriaTxt))),
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Digite para buscar…")
          ),
          selectizeInput(
            "filtro_filme", "Filme (Nome) — opcional:",
            choices = sort(unique(na.omit(dados$FilmeNome))),
            selected = character(0),
            multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Se vazio = todos")
          )
      ),
      div(class = "well",
          h4("CONTROLES DO GRÁFICO (Dispersão)"),
          selectInput("eixo_x", "Eixo X:", choices = colunas_numericas, selected = default_x),
          selectInput("eixo_y", "Eixo Y:", choices = colunas_numericas, selected = default_y),
          selectInput("var_tamanho", "Tamanho por:", choices = c("Nenhum" = "none", colunas_numericas), selected = "none"),
          sliderInput("transparencia", "Transparência:", 0.1, 1, 0.7, 0.1),
          checkboxInput("mostrar_legenda", "Mostrar legenda", TRUE),
          checkboxInput("agregar", "Agregação (reduzir pontos)", TRUE),
          selectInput("agregacao_tipo", "Agregação por:",
                      choices = c("Filme × Tema × Categoria" = "ftc", "Filme × Categoria" = "fc", "Tema × Categoria" = "tc"),
                      selected = "ftc")
      ),
      div(class = "well",
          h4("AÇÕES"),
          actionButton("btn_aplicar", "APLICAR FILTROS", class = "btn-primary", width = "100%"),
          tags$div(style = "height:8px;"),
          actionButton("btn_reset", "RESTAURAR PADRÕES", width = "100%",
                       style = "background-color:#95a5a6; color:white; border:none;"),
          tags$div(style = "height:8px;"),
          actionButton("btn_salvar_html", "SALVAR HTML (outputs)", width = "100%")
      ),
      div(class = "well",
          h4("ESTATÍSTICAS"),
          div(class = "stat-card",
              div(style="font-size:12px; color:#7f8c8d;", "TOTAL DE REGISTROS"),
              div(style="font-size:22px; font-weight:bold;", textOutput("total_registros", inline = TRUE))
          ),
          div(class = "stat-card",
              div(style="font-size:12px; color:#7f8c8d;", "REGISTROS FILTRADOS"),
              div(style="font-size:22px; font-weight:bold; color:#e74c3c;", textOutput("dados_filtrados", inline = TRUE))
          ),
          div(class = "stat-card",
              div(style="font-size:12px; color:#7f8c8d;", "CATEGORIAS ATIVAS"),
              div(style="font-size:22px; font-weight:bold; color:#27ae60;", textOutput("categorias_ativas", inline = TRUE))
          )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Dispersão",
                 div(style="background:white; border-radius:10px; padding:18px;",
                     h3("DISPERSÃO", style="margin-top:0;"),
                     plotlyOutput("grafico", height = "650px"),
                     tags$p("Passe o mouse para detalhes. Use zoom/pan do Plotly.", class="small-note")
                 )
        ),
        tabPanel("Violino",
                 div(style="background:white; border-radius:10px; padding:18px;",
                     h3("GRÁFICO DE VIOLINO", style="margin-top:0;"),
                     div(class="sub-controls",
                         fluidRow(
                           column(6, selectInput("violin_medida", "Medida (numérica):",
                                                 choices = colunas_numericas, selected = default_y)),
                           column(6, selectInput("violin_grupo", "Agrupar por:",
                                                 choices = setNames(grupo_choices, grupo_labels[grupo_choices]),
                                                 selected = "Categoria_Agente"))
                         )
                     ),
                     plotlyOutput("grafico_violin", height = "600px")
                 )
        ),
        tabPanel("Barras",
                 div(style="background:white; border-radius:10px; padding:18px;",
                     h3("GRÁFICO DE BARRAS", style="margin-top:0;"),
                     div(class="sub-controls",
                         fluidRow(
                           column(4, selectInput("bar_medida", "Medida (numérica):",
                                                 choices = colunas_numericas, selected = default_y)),
                           column(4, selectInput("bar_grupo", "Agrupar por:",
                                                 choices = setNames(grupo_choices, grupo_labels[grupo_choices]),
                                                 selected = "Categoria_Agente")),
                           column(4, selectInput("bar_fun", "Agregação:",
                                                 choices = c("Média"="mean","Mediana"="median","Soma"="sum","Contagem"="count"),
                                                 selected = "mean"))
                         )
                     ),
                     plotlyOutput("grafico_bar", height = "600px")
                 )
        ),
        tabPanel("Tabela",
                 div(style="background:white; border-radius:10px; padding:18px;",
                     h3("DADOS FILTRADOS", style="margin-top:0;"),
                     DTOutput("tabela")
                 )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 9) SERVER — CORREÇÃO PRINCIPAL: sincronizar choices/selected em cascata (AND)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$total_registros <- renderText({
    format(nrow(dados), big.mark=".", decimal.mark=",")
  })
  
  # Helper: atualiza selectize respeitando AND (interseção)
  sync_selectize <- function(id, choices_vec, selected_current, allow_empty = FALSE, prefer_all_when_empty = TRUE) {
    choices_vec <- sort(unique(na.omit(choices_vec)))
    selected_current <- selected_current %||% character(0)
    selected_new <- intersect(selected_current, choices_vec)
    
    if (!allow_empty) {
      # Para filtros "não opcionais": se ficar vazio, seleciona tudo disponível
      if (length(selected_new) == 0 && prefer_all_when_empty) selected_new <- choices_vec
    } else {
      # Para filtros opcionais (Filme): pode ficar vazio
      # Se havia seleção mas saiu do universo, zera
      if (length(selected_current) > 0 && length(selected_new) == 0) selected_new <- character(0)
    }
    
    updateSelectizeInput(session, id, choices = choices_vec, selected = selected_new, server = TRUE)
  }
  
  observeEvent(input$btn_reset, {
    updateSelectizeInput(session, "filtro_categoria_agente", choices = levels(dados$Categoria_Agente), selected = levels(dados$Categoria_Agente), server = TRUE)
    updateSelectizeInput(session, "filtro_agente_pad", choices = sort(unique(na.omit(dados$AgentePad))), selected = sort(unique(na.omit(dados$AgentePad))), server = TRUE)
    updateSelectizeInput(session, "filtro_tema", choices = sort(unique(na.omit(dados$TemaNivel))), selected = sort(unique(na.omit(dados$TemaNivel))), server = TRUE)
    updateSelectizeInput(session, "filtro_macrocategoria", choices = sort(unique(na.omit(dados$MacrocategoriaTxt))), selected = sort(unique(na.omit(dados$MacrocategoriaTxt))), server = TRUE)
    updateSelectizeInput(session, "filtro_caregoria", choices = sort(unique(na.omit(dados$CaregoriaTxt))), selected = sort(unique(na.omit(dados$CaregoriaTxt))), server = TRUE)
    updateSelectizeInput(session, "filtro_filme", choices = sort(unique(na.omit(dados$FilmeNome))), selected = character(0), server = TRUE)
    
    updateSelectInput(session, "eixo_x", selected = default_x)
    updateSelectInput(session, "eixo_y", selected = default_y)
    updateSelectInput(session, "var_tamanho", selected = "none")
    updateSliderInput(session, "transparencia", value = 0.7)
    updateCheckboxInput(session, "mostrar_legenda", value = TRUE)
    updateCheckboxInput(session, "agregar", value = TRUE)
    updateSelectInput(session, "agregacao_tipo", selected = "ftc")
    
    updateSelectInput(session, "violin_medida", selected = default_y)
    updateSelectInput(session, "violin_grupo", selected = "Categoria_Agente")
    updateSelectInput(session, "bar_medida", selected = default_y)
    updateSelectInput(session, "bar_grupo", selected = "Categoria_Agente")
    updateSelectInput(session, "bar_fun", selected = "mean")
    
    showNotification("Padrões restaurados.", type = "message", duration = 2)
  })
  
  # Aplica filtros (AND) e, em seguida, sincroniza as listas (choices) com o recorte final
  dados_filtrados <- eventReactive(input$btn_aplicar, {
    d <- dados
    
    if (!is.null(input$filtro_categoria_agente) && length(input$filtro_categoria_agente) > 0) {
      d <- d %>% filter(Categoria_Agente %in% input$filtro_categoria_agente)
    }
    if (!is.null(input$filtro_agente_pad) && length(input$filtro_agente_pad) > 0) {
      d <- d %>% filter(AgentePad %in% input$filtro_agente_pad)
    }
    if (!is.null(input$filtro_tema) && length(input$filtro_tema) > 0) {
      d <- d %>% filter(TemaNivel %in% input$filtro_tema)
    }
    if (!is.null(input$filtro_macrocategoria) && length(input$filtro_macrocategoria) > 0) {
      d <- d %>% filter(MacrocategoriaTxt %in% input$filtro_macrocategoria)
    }
    if (!is.null(input$filtro_caregoria) && length(input$filtro_caregoria) > 0) {
      d <- d %>% filter(CaregoriaTxt %in% input$filtro_caregoria)
    }
    if (!is.null(input$filtro_filme) && length(input$filtro_filme) > 0) {
      d <- d %>% filter(FilmeNome %in% input$filtro_filme)
    }
    
    d <- add_calculated_measures(d)
    
    # Sincroniza choices/selected com o universo filtrado (AND real)
    # (Isto evita a sensação de “não filtrou filmes/categorias”, pois tudo passa a refletir o recorte)
    isolate({
      sync_selectize("filtro_categoria_agente", levels(d$Categoria_Agente), input$filtro_categoria_agente, allow_empty = FALSE)
      sync_selectize("filtro_agente_pad", d$AgentePad, input$filtro_agente_pad, allow_empty = FALSE)
      sync_selectize("filtro_tema", d$TemaNivel, input$filtro_tema, allow_empty = FALSE)
      sync_selectize("filtro_macrocategoria", d$MacrocategoriaTxt, input$filtro_macrocategoria, allow_empty = FALSE)
      sync_selectize("filtro_caregoria", d$CaregoriaTxt, input$filtro_caregoria, allow_empty = FALSE)
      sync_selectize("filtro_filme", d$FilmeNome, input$filtro_filme, allow_empty = TRUE, prefer_all_when_empty = FALSE)
    })
    
    d
  }, ignoreNULL = FALSE)
  
  output$dados_filtrados <- renderText({
    d <- dados_filtrados()
    pct <- round(nrow(d) / nrow(dados) * 100, 1)
    paste0(format(nrow(d), big.mark=".", decimal.mark=","), " (", pct, "%)")
  })
  
  output$categorias_ativas <- renderText({
    d <- dados_filtrados()
    length(unique(d$Categoria_Agente))
  })
  
  plot_data <- reactive({
    d <- dados_filtrados()
    req(nrow(d) > 0)
    prep_plot_data(
      d = d,
      xcol = input$eixo_x,
      ycol = input$eixo_y,
      agregar = isTRUE(input$agregar),
      tipo = input$agregacao_tipo %||% "ftc"
    )
  })
  
  output$grafico <- renderPlotly({
    d_base <- dados_filtrados()
    d_pts <- plot_data()
    make_plotly_scatter(
      d_points = d_pts,
      d_base = d_base,
      xcol = input$eixo_x,
      ycol = input$eixo_y,
      var_tamanho = input$var_tamanho %||% "none",
      transparencia = input$transparencia %||% 0.7,
      mostrar_legenda = isTRUE(input$mostrar_legenda)
    )
  })
  
  output$grafico_violin <- renderPlotly({
    d <- dados_filtrados()
    req(nrow(d) > 0)
    make_plotly_violin(
      d = d,
      medida = input$violin_medida %||% default_y,
      grupo = input$violin_grupo %||% "Categoria_Agente",
      transparencia = input$transparencia %||% 0.7
    )
  })
  
  output$grafico_bar <- renderPlotly({
    d <- dados_filtrados()
    req(nrow(d) > 0)
    make_plotly_bar(
      d = d,
      medida = input$bar_medida %||% default_y,
      grupo = input$bar_grupo %||% "Categoria_Agente",
      fun = input$bar_fun %||% "mean",
      transparencia = min(1, max(0.15, (input$transparencia %||% 0.7) + 0.2))
    )
  })
  
  output$tabela <- renderDT({
    d <- dados_filtrados()
    keep_cols <- intersect(
      c(
        "FilmeNome", "AgentePad", "TemaNivel", "Categoria_Agente",
        "MacrocategoriaTxt", "CaregoriaTxt", "DNAMnemTxt",
        "FreqAgente_Filtrado", "ScoreNivelTemaTxt", "MedidaPonderada_FreqAgente_x_ScoreTema",
        input$eixo_x, input$eixo_y
      ),
      names(d)
    )
    
    d_view <- d %>% select(all_of(keep_cols))
    nm <- names(d_view)
    nm[nm == "MacrocategoriaTxt"] <- "Macrocategoria"
    nm[nm == "CaregoriaTxt"] <- "Caregoria"
    nm[nm == "DNAMnemTxt"] <- "DNA-Mnem"
    nm[nm == "ScoreNivelTemaTxt"] <- "Score Nível Tema"
    names(d_view) <- nm
    
    datatable(
      d_view,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$btn_salvar_html, {
    d_base <- dados_filtrados()
    d_pts <- plot_data()
    
    if (is.null(d_pts) || nrow(d_pts) == 0) {
      showNotification("Nada para salvar (sem dados).", type = "error", duration = 3)
      return()
    }
    
    p <- make_plotly_scatter(
      d_points = d_pts,
      d_base = d_base,
      xcol = input$eixo_x,
      ycol = input$eixo_y,
      var_tamanho = input$var_tamanho %||% "none",
      transparencia = input$transparencia %||% 0.7,
      mostrar_legenda = isTRUE(input$mostrar_legenda)
    )
    
    path <- next_version_path(prefix = "dispersao_agentes", ext = ".html")
    htmlwidgets::saveWidget(p, file = path, selfcontained = TRUE)
    showNotification(paste0("Salvo em: ", path), type = "message", duration = 4)
  })
  
  observeEvent(input$btn_aplicar, {
    d <- dados_filtrados()
    showNotification(paste0("Filtros aplicados: ", nrow(d), " registros."), type = "message", duration = 2)
  })
}

shinyApp(ui = ui, server = server)

