# Trabalho de Econometria I - FGV EESP, 2026
# Corresidência com idosos e as decisões de estudo e trabalho dos jovens brasileiros
# Artur Deboni Ferezin e Marco Antonio Ravaglia
#
# Base: PNAD Contínua 2025, 1ª visita (IBGE)
# https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/
# Se base_tratada_pnadc2025.rds estiver na pasta, o script usa essa base;
# caso contrário, monta a base a partir de PNADC_2025_visita1_20260923.zip

# 0. Pacotes e pastas
pacotes <- c("readr", "data.table", "sandwich", "lmtest", "car",
             "marginaleffects", "ggplot2", "flextable", "officer")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando) > 0) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))

pasta <- "."
setwd(pasta)
arq_zip <- "PNADC_2025_visita1_20260923.zip"
arq_csv <- "pnad2025_completa.csv"
dir.create("resultados", showWarnings = FALSE)

arq_rds <- c("base_tratada_pnadc2025.rds", "resultados/base_tratada_pnadc2025.rds")
arq_rds <- arq_rds[file.exists(arq_rds)][1]

if (!is.na(arq_rds)) {
  base <- readRDS(arq_rds)
  setDT(base)
  cat("Base tratada lida de", arq_rds, "| jovens na amostra final:", nrow(base), "\n")
} else {

# 1. Leitura dos microdados
layout <- data.frame(
  var = c("Ano","UF","UPA","V1008","V1014","V1022","V1032",
          "V2003","V2005","V2007","V2009","V2010","V3002",
          "V5004A","VD3005","VD4001","VD4002"),
  ini = c(1,   6,  12,  28,  30,  32,  58,
          90,  92,  94, 103, 106, 108,
          415, 530, 533, 534),
  tam = c(4,   2,   9,   2,   2,   1,  15,
          2,   2,   1,   3,   1,   1,
          1,   2,   1,   1)
)
layout$fim <- layout$ini + layout$tam - 1

if (file.exists(arq_zip)) {
  pnad <- read_fwf(
    unz(arq_zip, "PNADC_2025_visita1.txt"),
    col_positions = fwf_positions(layout$ini, layout$fim, layout$var),
    col_types = cols(.default = col_character()),
    progress = FALSE
  )
  setDT(pnad)
} else if (file.exists(arq_csv)) {
  pnad <- fread(arq_csv, select = layout$var, colClasses = "character")
  pad <- function(x, n) ifelse(is.na(x) | x == "", NA_character_,
                               formatC(as.integer(x), width = n, flag = "0"))
  for (v in c("UF","V1008","V1014","V2005","VD3005")) {
    n <- layout$tam[layout$var == v]
    set(pnad, j = v, value = pad(pnad[[v]], n))
  }
  for (v in names(pnad)) set(pnad, i = which(pnad[[v]] == ""), j = v, value = NA_character_)
} else stop("Coloque na pasta o .rds, o .zip do IBGE ou o pnad2025_completa.csv")
stopifnot(all(pnad$Ano == "2025"))
cat("Pessoas na base:", nrow(pnad), "\n")

# 2. Variáveis de pessoa
pnad[, `:=`(
  dom_id   = paste0(UPA, V1008, V1014),
  idade    = as.integer(V2009),
  anos_est = suppressWarnings(as.integer(VD3005)),
  peso     = as.numeric(V1032)
)]

# 3. Variáveis de domicílio
dom <- pnad[, .(
  n_idoso    = sum(idade >= 60),
  n_criancas = sum(idade <= 14),
  educ_resp  = anos_est[V2005 == "01"][1],
  idade_resp = idade[V2005 == "01"][1],
  n_apos     = sum(V5004A == "1" & idade >= 15, na.rm = TRUE)
), by = dom_id]

# 4. Amostra: jovens de 15 a 24 anos
excluir_extra <- FALSE
codigos_fora  <- c("01","02","03","17","18","19", if (excluir_extra) c("07","15","16"))
jov <- pnad[idade >= 15 & idade <= 24 & !(V2005 %in% codigos_fora)]
jov <- merge(jov, dom, by = "dom_id")

jov[, `:=`(
  idoso    = as.integer(n_idoso > 0),
  aposent  = as.integer((n_apos - (V5004A %in% "1")) > 0),
  freq     = as.integer(V3002 == "1"),
  ocupado  = as.integer(VD4002 %in% "1"),
  mulher   = as.integer(V2007 == "2"),
  idade2   = idade^2,
  urbano   = as.integer(V1022 == "1"),
  resp_idoso = as.integer(idade_resp >= 60),
  raca     = factor(fcase(V2010 == "1", "Branca",
                          V2010 == "2", "Preta",
                          V2010 == "4", "Parda",
                          V2010 %in% c("3","5"), "Amarela/Indígena",
                          default = NA_character_),
                    levels = c("Branca","Preta","Parda","Amarela/Indígena"))
)]
jov[is.na(VD4001), ocupado := NA]
jov[, `:=`(
  so_estuda   = as.integer(freq == 1 & ocupado == 0),
  estuda_trab = as.integer(freq == 1 & ocupado == 1),
  so_trab     = as.integer(freq == 0 & ocupado == 1),
  nemnem      = as.integer(freq == 0 & ocupado == 0)
)]

siglas <- c("11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
            "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
            "28"="SE","29"="BA","31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR",
            "42"="SC","43"="RS","50"="MS","51"="MT","52"="GO","53"="DF")
jov[, uf := relevel(factor(siglas[UF]), ref = "SP")]

vars_modelo <- c("so_estuda","estuda_trab","so_trab","nemnem","freq","ocupado","idoso","mulher","idade","idade2",
                 "raca","urbano","n_criancas","educ_resp","uf")
base <- na.omit(jov[, c(vars_modelo, "aposent", "resp_idoso", "peso", "dom_id"), with = FALSE])
cat("Jovens na amostra final:", nrow(base),
    " | descartados por NA:", nrow(jov) - nrow(base), "\n")

saveRDS(base, "resultados/base_tratada_pnadc2025.rds")
fwrite(base, "resultados/base_tratada_pnadc2025.csv")
}

# 5. Estatísticas descritivas
base[, `:=`(preta = as.integer(raca == "Preta"), parda = as.integer(raca == "Parda"),
            amar_ind = as.integer(raca == "Amarela/Indígena"))]
desc_vars <- c("so_estuda","estuda_trab","so_trab","nemnem","freq","ocupado","idoso","aposent","mulher","idade",
               "preta","parda","amar_ind","urbano","n_criancas","educ_resp")
rotulos <- c(so_estuda="Só estuda", estuda_trab="Estuda e trabalha", so_trab="Só trabalha",
             freq="Frequenta escola", ocupado="Ocupado", nemnem="Nem-nem",
             idoso="Mora com idoso (60+)", aposent="Mora com aposentado/pensionista",
             mulher="Mulher", idade="Idade", urbano="Urbano",
             preta="Preta", parda="Parda", amar_ind="Amarela ou indígena",
             n_criancas="Nº de crianças (0-14)",
             educ_resp="Anos de estudo do responsável")

descr <- rbindlist(lapply(desc_vars, function(v) {
  x <- base[[v]]; g <- base$idoso
  tt <- if (v == "idoso") list(p.value = NA) else t.test(x[g == 1], x[g == 0])
  data.table(Variavel = rotulos[v],
             Media = mean(x), DP = sd(x), Min = min(x), Max = max(x),
             Media_com_idoso = mean(x[g == 1]),
             Media_sem_idoso = mean(x[g == 0]),
             Diferenca = mean(x[g == 1]) - mean(x[g == 0]),
             p_valor = tt$p.value)
}))
raca_tab <- base[, .(Proporcao = .N / nrow(base)), by = raca][order(raca)]
print(descr, digits = 3); print(raca_tab)
fwrite(descr, "resultados/tab1_descritivas.csv")

proxy <- base[, .N, by = .(idoso, aposent)][, prop := N / sum(N)][order(idoso, aposent)]
print(proxy)
fwrite(proxy, "resultados/tab_proxy_idoso_aposentado.csv")

coorte <- base[, .(N = .N, prop_resp_idoso = mean(resp_idoso),
                   educ_resp_media = mean(educ_resp)), by = .(idoso, resp_idoso)]
print(coorte[order(idoso, resp_idoso)])
cat("Jovens com idoso cujo responsável tem 60+:",
    round(100 * base[idoso == 1, mean(resp_idoso)], 1), "%\n")
fwrite(coorte, "resultados/tab_coorte_responsavel.csv")

fig1 <- base[, .(so_estuda = mean(so_estuda), estuda_trab = mean(estuda_trab),
                 so_trab = mean(so_trab), nemnem = mean(nemnem)), by = .(idade, idoso)]
fig1 <- melt(fig1, id.vars = c("idade","idoso"))
fig1[, idoso := factor(idoso, 0:1, c("Sem idoso","Com idoso"))]
fig1[, variable := factor(variable, c("so_estuda","estuda_trab","so_trab","nemnem"),
                          c("Só estuda","Estuda e trabalha","Só trabalha","Nem-nem"))]
g1 <- ggplot(fig1, aes(idade, value, linetype = idoso)) +
  geom_line() + geom_point(size = 1.5) + facet_wrap(~variable, nrow = 2) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                     limits = c(0, NA)) +
  scale_x_continuous(breaks = 15:24) +
  labs(x = "Idade", y = "Proporção de jovens", linetype = NULL) +
  theme_bw(base_family = "serif") + theme(legend.position = "bottom")
ggsave("resultados/fig1_proporcoes_por_idade.png", g1, width = 7, height = 5.5, dpi = 300)

vars_cor <- c(so_estuda="Só estuda", estuda_trab="Estuda e trabalha", so_trab="Só trabalha",
              nemnem="Nem-nem", idoso="IDOSO", mulher="Mulher", idade="Idade",
              preta="Preta", parda="Parda", urbano="Urbano",
              n_criancas="Nº de crianças", educ_resp="Educ. do responsável")
cm <- cor(base[, names(vars_cor), with = FALSE])
dimnames(cm) <- list(vars_cor, vars_cor)
cm_l <- as.data.table(as.table(cm))
setnames(cm_l, c("v1","v2","r"))
cm_l[, `:=`(v1 = factor(v1, vars_cor), v2 = factor(v2, rev(vars_cor)))]
cm_l <- cm_l[as.integer(v1) < (length(vars_cor) + 1 - as.integer(v2))]
g3 <- ggplot(cm_l, aes(v1, v2, fill = r)) + geom_tile(colour = "white") +
  geom_text(aes(label = sub(".", ",", sprintf("%.2f", r), fixed = TRUE)), size = 2.4) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       limits = c(-1, 1), name = "r") +
  labs(x = NULL, y = NULL) + theme_minimal(base_family = "serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
ggsave("resultados/fig3_matriz_correlacao.png", g3, width = 6, height = 5, dpi = 300, bg = "white")
fwrite(as.data.table(cm, keep.rownames = "var"), "resultados/tab_correlacoes.csv")

# 6. Especificações
ys <- c(so_estuda = "Só estuda", estuda_trab = "Estuda e trabalha",
        so_trab = "Só trabalha", nemnem = "Nem-nem")
controles <- "mulher + idade + idade2 + raca + urbano + n_criancas + educ_resp + uf"
specs <- list(
  "(1)" = "idoso",
  "(2)" = paste("idoso +", controles),
  "(3)" = paste("idoso + idoso:mulher +", controles)
)

estimar <- function(y, rhs) {
  f <- as.formula(paste(y, "~", rhs))
  list(
    MPL    = lm(f, data = base),
    Probit = glm(f, data = base, family = binomial(link = "probit")),
    Logit  = glm(f, data = base, family = binomial(link = "logit"))
  )
}
modelos <- lapply(names(ys), function(y) lapply(specs, function(s) estimar(y, s)))
names(modelos) <- names(ys)

# 7. Efeito de IDOSO (coeficientes, EMM e EMNM)
vc <- function(m) sandwich::vcovHC(m, type = "HC1")

emm_idoso <- function(m, por_sexo = FALSE) {
  if (por_sexo) {
    r <- avg_comparisons(m, variables = list(idoso = c(0, 1)), by = "mulher", vcov = vc(m))
    d <- avg_comparisons(m, variables = list(idoso = c(0, 1)), by = "mulher",
                         vcov = vc(m), hypothesis = "b2 = b1")
    out <- rbind(as.data.frame(r)[, c("estimate","std.error","p.value")],
                 as.data.frame(d)[, c("estimate","std.error","p.value")])
    out$termo <- c("Homens","Mulheres","Diferença (M − H)")
  } else {
    r <- avg_comparisons(m, variables = list(idoso = c(0, 1)), vcov = vc(m))
    out <- as.data.frame(r)[, c("estimate","std.error","p.value")]
    out$termo <- "IDOSO"
  }
  out
}

emnm_idoso <- function(m, por_sexo = FALSE) {
  X <- model.matrix(m); b <- coef(m); V <- vc(m)
  cdf <- if (inherits(m, "glm")) m$family$linkinv else identity
  pdf <- if (inherits(m, "glm")) m$family$mu.eta  else function(z) rep(1, length(z))
  efeito <- function(xbar) {
    x1 <- x0 <- xbar
    x1["idoso"] <- 1; x0["idoso"] <- 0
    if ("idoso:mulher" %in% names(xbar)) {
      x1["idoso:mulher"] <- xbar["mulher"]; x0["idoso:mulher"] <- 0
    }
    est <- cdf(sum(x1 * b)) - cdf(sum(x0 * b))
    g   <- pdf(sum(x1 * b)) * x1 - pdf(sum(x0 * b)) * x0
    c(est = est, g)
  }
  xbar <- colMeans(X)
  if (por_sexo) {
    xh <- xbar; xh["mulher"] <- 0
    xm <- xbar; xm["mulher"] <- 1
    eh <- efeito(xh); em <- efeito(xm)
    ests <- c(eh[1], em[1], em[1] - eh[1])
    G <- rbind(eh[-1], em[-1], em[-1] - eh[-1])
    termos <- c("Homens","Mulheres","Diferença (M − H)")
  } else {
    e <- efeito(xbar); ests <- e[1]; G <- matrix(e[-1], nrow = 1); termos <- "IDOSO"
  }
  se <- sqrt(diag(G %*% V %*% t(G)))
  data.frame(estimate = unname(ests), std.error = se,
             p.value = 2 * pnorm(-abs(unname(ests) / se)), termo = termos)
}

resultados <- rbindlist(lapply(names(ys), function(y) {
  rbindlist(lapply(names(specs), function(s) {
    rbindlist(lapply(c("MPL","Probit","Logit"), function(mod) {
      m <- modelos[[y]][[s]][[mod]]
      ct <- coeftest(m, vcov = vc(m))
      coef_id <- data.table(tipo = "Coeficiente", termo = "IDOSO",
                            estimate = ct["idoso", 1], std.error = ct["idoso", 2],
                            p.value = ct["idoso", 4])
      if (s == "(3)") {
        coef_int <- data.table(tipo = "Coeficiente", termo = "IDOSO × MULHER",
                               estimate = ct["idoso:mulher", 1],
                               std.error = ct["idoso:mulher", 2],
                               p.value = ct["idoso:mulher", 4])
        coef_id <- rbind(coef_id, coef_int)
      }
      emm  <- data.table(tipo = "EMM",  emm_idoso(m,  por_sexo = (s == "(3)")))
      emnm <- data.table(tipo = "EMNM", emnm_idoso(m, por_sexo = (s == "(3)")))
      cbind(Y = ys[y], spec = s, modelo = mod,
            rbind(coef_id, emm[, .(tipo, termo, estimate, std.error, p.value)],
                  emnm[, .(tipo, termo, estimate, std.error, p.value)]),
            N = nobs(m))
    }))
  }))
}))

estrelas <- function(p) fcase(p < 0.01, "***", p < 0.05, "**", p < 0.10, "*", default = "")
resultados[, celula := sprintf("%.4f%s (%.4f)", estimate, estrelas(p.value), std.error)]
fwrite(resultados, "resultados/resultados_idoso_completo.csv")

tab2 <- dcast(resultados[modelo == "Probit" & tipo %in% c("EMM","EMNM")],
              spec + termo ~ Y + tipo, value.var = "celula")
print(tab2)

tab3 <- dcast(resultados[spec == "(2)"],
              Y ~ modelo + tipo, value.var = "celula")
setcolorder(tab3, c("Y","MPL_Coeficiente",
                    "Probit_Coeficiente","Probit_EMM","Probit_EMNM",
                    "Logit_Coeficiente","Logit_EMM","Logit_EMNM"))
tab3[, c("MPL_EMM","MPL_EMNM") := NULL]
print(tab3)

soma4 <- resultados[spec == "(2)" & termo == "IDOSO" & tipo != "Coeficiente",
                    .(soma = sum(estimate)), by = .(modelo, tipo)]
print(soma4)
fwrite(soma4, "resultados/tab4_soma_efeitos.csv")

# 8. EMM dos controles (Probit)
emm_todas <- rbindlist(lapply(names(ys), function(y) {
  m <- modelos[[y]][["(2)"]][["Probit"]]
  r <- as.data.frame(avg_slopes(m, variables = c("idoso","mulher","urbano",
                                                "n_criancas","educ_resp","raca"),
                               vcov = vc(m)))
  data.table(Y = ys[y], r[, intersect(names(r), c("term","contrast","estimate",
                                                 "std.error","p.value"))])
}))
print(emm_todas)
fwrite(emm_todas, "resultados/tab_ap_emm_probit_todas.csv")

emm_idade <- rbindlist(lapply(names(ys), function(y) {
  m <- modelos[[y]][["(2)"]][["Probit"]]
  b <- coef(m); xb <- predict(m, type = "link")
  data.table(Y = ys[y],
             EMM_idade = mean(dnorm(xb) * (b["idade"] + 2 * b["idade2"] * base$idade)),
             idade_vertice = -b["idade"] / (2 * b["idade2"]))
}))
print(emm_idade)
fwrite(emm_idade, "resultados/tab_emm_idade.csv")

# 8b. Viés de variável omitida
specs_ovb <- c("Só IDOSO"             = "idoso",
               "+ idade, idade²"      = "idoso + idade + idade2",
               "+ educ. responsável"  = "idoso + educ_resp",
               "+ nº de crianças"     = "idoso + n_criancas",
               "Completa (2)"         = paste("idoso +", controles))
ovb <- rbindlist(lapply(names(ys), function(y) rbindlist(lapply(names(specs_ovb), function(s) {
  m <- lm(as.formula(paste(y, "~", specs_ovb[s])), data = base)
  ct <- coeftest(m, vcov = vc(m))
  data.table(Y = ys[y], spec = s, idoso = ct["idoso", 1], ep = ct["idoso", 2])
}))))
ovb[, spec := factor(spec, names(specs_ovb))]
print(dcast(ovb, spec ~ Y, value.var = "idoso"), digits = 3)
fwrite(ovb, "resultados/tab_vies_variavel_omitida.csv")

# 8c. H2 nos três modelos
h2 <- resultados[spec == "(3)" & ((tipo == "Coeficiente" & termo == "IDOSO × MULHER") |
                                  (tipo == "EMM" & termo == "Diferença (M − H)")),
                 .(Y, modelo, tipo, estimate, std.error, p.value)]
print(h2[order(Y, modelo)], digits = 3)
fwrite(h2, "resultados/tab_h2_tres_modelos.csv")

# 8d. Jovem-tipo
perfil <- data.table(mulher = 0, idade = 18, idade2 = 18^2,
                   raca = factor("Parda", levels(base$raca)), urbano = 1,
                   n_criancas = 0, educ_resp = 9,
                   uf = factor("SP", levels(base$uf)), idoso = c(0, 1))
jovem_tipo <- rbindlist(lapply(names(ys), function(y) {
  m <- modelos[[y]][["(2)"]][["Probit"]]
  p <- predict(m, newdata = perfil, type = "response")
  data.table(Y = ys[y], sem_idoso = p[1], com_idoso = p[2], dif_pp = 100 * (p[2] - p[1]))
}))
print(jovem_tipo, digits = 3)
fwrite(jovem_tipo, "resultados/tab_jovem_tipo.csv")

# 8e. Probabilidade prevista por idade
fig4 <- rbindlist(lapply(c("so_estuda","so_trab"), function(y) {
  m <- modelos[[y]][["(2)"]][["Probit"]]
  rbindlist(lapply(15:24, function(a) rbindlist(lapply(0:1, function(d) {
    nd <- copy(base); nd[, `:=`(idade = a, idade2 = a^2, idoso = d)]
    data.table(Y = ys[y], idade = a, idoso = d, p = mean(predict(m, nd, type = "response")))
  }))))
}))
fig4[, idoso := factor(idoso, 0:1, c("Sem idoso","Com idoso"))]
g4 <- ggplot(fig4, aes(idade, p, linetype = idoso)) + geom_line() + geom_point(size = 1.5) +
  facet_wrap(~Y) + scale_x_continuous(breaks = 15:24) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
  labs(x = "Idade", y = "Probabilidade prevista", linetype = NULL) +
  theme_bw(base_family = "serif") + theme(legend.position = "bottom")
ggsave("resultados/fig4_prob_prevista_probit.png", g4, width = 7, height = 3.6, dpi = 300)

# 9. Diagnósticos
diagn <- rbindlist(lapply(names(ys), function(y) {
  mpl <- modelos[[y]][["(2)"]][["MPL"]]
  prb <- modelos[[y]][["(2)"]][["Probit"]]
  lgt <- modelos[[y]][["(2)"]][["Logit"]]
  p0  <- glm(as.formula(paste(y, "~ 1")), data = base, family = binomial("probit"))
  white <- bptest(mpl, ~ fitted(mpl) + I(fitted(mpl)^2), data = base)
  uf_terms <- grep("^uf", names(coef(mpl)), value = TRUE)
  f_uf <- linearHypothesis(mpl, paste(uf_terms, "= 0"), vcov. = vc(mpl), test = "F")
  data.table(
    Y = ys[y],
    MPL_R2 = summary(mpl)$r.squared,
    MPL_prev_fora_0_1 = mean(fitted(mpl) < 0 | fitted(mpl) > 1),
    White_LM = unname(white$statistic), White_p = white$p.value,
    F_UF_robusto = f_uf$F[2], F_UF_p = f_uf$`Pr(>F)`[2],
    Probit_pseudoR2_McFadden = 1 - as.numeric(logLik(prb)) / as.numeric(logLik(p0)),
    Probit_acerto = mean((fitted(prb) > 0.5) == base[[y]]),
    Acerto_ingenuo = max(mean(base[[y]]), 1 - mean(base[[y]])),
    Probit_prev_positivas = mean(fitted(prb) > 0.5),
    Probit_AIC = AIC(prb), Logit_AIC = AIC(lgt)
  )
}))
print(diagn, digits = 4)
fwrite(diagn, "resultados/tab_diagnosticos.csv")

vif_tab <- as.data.frame(car::vif(modelos[[1]][["(2)"]][["MPL"]]))
print(vif_tab)
fwrite(vif_tab, "resultados/tab_vif.csv", row.names = TRUE)

# 9b. Erro-padrão agrupado por domicílio
ep_cl <- rbindlist(lapply(names(ys), function(y) {
  m <- modelos[[y]][["(3)"]][["MPL"]]
  data.table(Y = ys[y],
             ep_HC1 = sqrt(vc(m)["idoso:mulher", "idoso:mulher"]),
             ep_dom = sqrt(sandwich::vcovCL(m, cluster = base$dom_id, type = "HC1")["idoso:mulher", "idoso:mulher"]))
}))
print(ep_cl, digits = 3)

# 10. Gráfico do EMM de IDOSO
fig2 <- resultados[spec == "(2)" & (tipo == "EMM")]
fig2[, `:=`(li = estimate - 1.96 * std.error, ls = estimate + 1.96 * std.error)]
g2 <- ggplot(fig2, aes(x = 100 * estimate, y = modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = 100 * li, xmax = 100 * ls), width = 0.2, orientation = "y") +
  geom_point(size = 2) + facet_wrap(~Y, ncol = 1) +
  labs(x = "Efeito de morar com idoso (pontos percentuais)", y = NULL) +
  theme_bw(base_family = "serif")
ggsave("resultados/fig2_emm_idoso.png", g2, width = 6, height = 6, dpi = 300)

# 11. Tabelas em Word
doc <- read_docx()
add_tab <- function(doc, df, titulo, nota) {
  doc <- body_add_par(doc, titulo, style = "heading 2")
  ft <- flextable(as.data.frame(df))
  ft <- fontsize(ft, size = 9, part = "all"); ft <- autofit(ft)
  doc <- body_add_flextable(doc, ft)
  body_add_par(doc, nota, style = "Normal")
}
nota_ep <- paste("Erros-padrão robustos à heterocedasticidade (HC1; WHITE, 1980) entre",
                 "parênteses. *** p<0,01; ** p<0,05; * p<0,10.")
doc <- add_tab(doc, descr, "Tabela 1 – Estatísticas descritivas",
               "Médias de dummies lidas como proporções. p-valor: teste t de Welch.")
doc <- add_tab(doc, tab2, "Tabela 2 – Probit: efeitos marginais de IDOSO",
               paste(nota_ep, "EMM = efeito marginal médio; EMNM = efeito marginal na média.",
                     "Especificação (3) inclui IDOSO × MULHER."))
doc <- add_tab(doc, tab3, "Tabela 3 – Comparação MPL, Probit e Logit (especificação 2)",
               paste(nota_ep, "EMM = efeito marginal médio; EMNM = efeito marginal na média de IDOSO."))
doc <- add_tab(doc, diagn, "Tabela A1 – Diagnósticos", "Especificação (2).")
print(doc, target = "resultados/tabelas_trabalho.docx")

cat("\nPronto. Resultados em ./resultados\n")
