## ============================================================================
## Dave in the Cave -- All Israel came to him
## An interactive Voronoi TREEMAP (ggvmap, interactive = TRUE) of the tribal
## contingents that came to David at Hebron (1 Chronicles 12:23-37).
##   * cell AREA   = headcount
##   * cell COLOUR = tribe, from an LTC palette
##   * each cell carries its tribe's FLAT icon, drawn in a lighter / darker
##     shade of that cell's own colour so it stands out
##
## Files needed in the working directory:
##   Dave in the Cave.xlsx   (your spreadsheet)
##   tribe_icons.csv         (icon outlines, supplied with this script)
##
## Icons: game-icons.net -- Lorc, Delapouite, Skoll & Caro Asercion, licensed CC BY 3.0
##        (https://creativecommons.org/licenses/by/3.0/)
##        Judah's lioness: Pinhead Map Icons (Quincy Morgan), CC0
##        Levi's two stones: drawn for this chart
##   Judah  lioness (Pinhead lion)   Reuben   unstable water (waves)
##   Simeon sword                    Levi     Urim & Thummim (two stones)
##   Dan    snake                    Naphtali doe (deer, antlers removed)
##   Asher  plate of food (hot meal) Issachar donkey
##   Zebulun ship                    Benjamin wolf
##   Joseph bull                     Gad      lion
## ============================================================================

## ---- 0. Packages -----------------------------------------------------------
## ggvmap and ltc live on GitHub; everything else is on CRAN.
## NOTE: ggvmap's `interactive = TRUE` runs on ggiraph under the hood, so
## ggiraph must be INSTALLED -- but you never call it yourself below.
# install.packages(c("remotes", "readxl", "dplyr", "ggplot2", "ggiraph", "htmlwidgets", "ggtext"))
# remotes::install_github("loukesio/ggvmap")
# remotes::install_github("loukesio/ltc-color-palettes")   # package name: ltc

suppressMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(ggvmap)
  library(ltc)
})

## ---- 1. Load the spreadsheet -----------------------------------------------
## Folder holding the spreadsheet + icons. Run the script from the
## dave-in-the-cave/ folder, or point DATA_DIR at it.
DATA_DIR <- getwd()
if (!file.exists(file.path(DATA_DIR, "tribe_icons.csv"))) {
  stop("tribe_icons.csv not found in ", DATA_DIR, " -- set DATA_DIR to the folder that holds it.")
}

raw <- read_excel(file.path(DATA_DIR, "Dave in the Cave.xlsx"))   # kept for reference / your own checks

## ---- 2. Reconcile the numbers down to exactly 12 tribes --------------------
## (a) Reuben, Gad and the eastern half of Manasseh are given as ONE combined
##     figure (120,000, 1 Chr 12:37). Split evenly: 120,000 / 3 = 40,000 each.
##     This is an ASSUMPTION -- the text gives no breakdown.
## (b) Ephraim + both halves of Manasseh are merged into "Joseph" so that
##     exactly 12 tribes remain.
## (c) Issachar's 200 counts CHIEFS, not troops (1 Chr 12:32). It is shown
##     as given and flagged in the tooltip, not inflated.
## (d) Levi includes the priests: 4,600 Levites (v.26) + Jehoiada, leader of
##     the Aaronites, and 3,700 with him (v.27) + Zadok and 22 captains of his
##     father's house (v.28). The named leaders are counted as one man each.
## (e) Naphtali = 1,000 captains + 37,000 with them (v.34).

SPLIT <- 120000 / 3

tribes <- tibble::tribble(
  ~tribe,     ~men,
  "Judah",     6800,
  "Simeon",    7100,
  "Levi",      4600 + (1 + 3700) + (1 + 22),   # Levites + Jehoiada & Aaronites + Zadok & captains
  "Benjamin",  3000,
  "Issachar",  200,
  "Zebulun",   50000,
  "Dan",       28600,
  "Naphtali",  1000 + 37000,                   # captains + men with them
  "Asher",     40000,
  "Reuben",    SPLIT,
  "Gad",       SPLIT,
  "Joseph",    20800 + 18000 + SPLIT      # Ephraim + Manasseh (west) + Manasseh (east share)
) |>
  arrange(desc(men))

stopifnot(nrow(tribes) == 12)
cat("Total across all 12 tribes:", format(sum(tribes$men), big.mark = ","), "\n")

## ---- 3. Colours: LTC palette, one colour per cell --------------------------
## Change the name for any other LTC palette (see names(ltc::palettes)).
## casa_natal has 9 colours, so they are interpolated out to 12.
LTC_PALETTE <- "casa_natal"
cell_fill   <- colorRampPalette(ltc::palettes[[LTC_PALETTE]])(nrow(tribes))   # cell i  <->  tribes[i, ]

## Flat-icon colour: a lighter OR darker shade of the SAME cell colour,
## whichever contrasts more with it.
lum <- function(hex) {
  v <- col2rgb(hex)[, 1] / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(v * c(0.2126, 0.7152, 0.0722))
}
mix <- function(hex, with, amt) {
  a <- col2rgb(hex)[, 1]; b <- col2rgb(with)[, 1]
  rgb(t(a * (1 - amt) + b * amt), maxColorValue = 255)
}
contrast <- function(a, b) { la <- lum(a); lb <- lum(b); (max(la, lb) + .05) / (min(la, lb) + .05) }
icon_shade <- function(hex) {
  dark  <- mix(hex, "black", 0.68)
  light <- mix(hex, "white", 0.78)
  if (contrast(hex, dark) >= contrast(hex, light)) dark else light
}
icon_col <- vapply(cell_fill, icon_shade, character(1))

## ---- 4. Tooltips (HTML) -----------------------------------------------------
tooltips <- with(tribes, sprintf(
  "<b style='font-family:Cinzel,serif;letter-spacing:.06em'>%s</b><br/>%s men%s",
  tribe, format(men, big.mark = ","),
  ifelse(tribe == "Issachar",
         "<br/><i>200 chiefs (together with all their kinsmen), not total troops (1 Chr 12:32)</i>",
  ifelse(tribe %in% c("Reuben", "Gad"),
         "<br/><i>even split of the combined 120,000 (1 Chr 12:37)</i>",
  ifelse(tribe == "Levi",
         "<br/><i>4,600 Levites + Jehoiada & 3,700 Aaronites + Zadok & 22 captains (1 Chr 12:26-28)</i>",
  ifelse(tribe == "Naphtali",
         "<br/><i>1,000 captains + 37,000 with them (1 Chr 12:34)</i>",
  ifelse(tribe == "Joseph",
         "<br/><i>Ephraim + Manasseh merged</i>", "")))))
))

## ---- 5. The treemap: ggvmap with interactive = TRUE -------------------------
## Fonts (loaded in step 7 for the browser view)
F_TITLE <- "Cinzel Decorative"
F_LABEL <- "Cinzel"
F_BODY  <- "Cormorant Garamond"

## R must measure text with the SAME fonts the browser draws, or centred /
## right-aligned text drifts. Download the .ttf files once into DATA_DIR/fonts
## (needs internet the first time) and register them for this R session.
options(timeout = max(600, getOption("timeout")))
FONT_DIR <- file.path(DATA_DIR, "fonts")
dir.create(FONT_DIR, showWarnings = FALSE)
font_file <- function(family, weight = 400, italic = FALSE) {
  dest <- file.path(FONT_DIR, sprintf("%s-%d%s.ttf", gsub(" ", "", family), weight, if (italic) "i" else ""))
  if (!file.exists(dest)) {
    css <- readLines(sprintf("https://fonts.googleapis.com/css2?family=%s:ital,wght@%d,%d",
                             gsub(" ", "+", family), as.integer(italic), weight), warn = FALSE)
    src <- regmatches(css, regexpr("https://[^)]+\\.ttf", css))[1]
    download.file(src, dest, mode = "wb", quiet = TRUE)
  }
  dest
}
register_gfont <- function(family) {
  systemfonts::register_font(
    family,
    plain      = font_file(family, 400),
    bold       = font_file(family, 700),
    italic     = font_file(family, 400, italic = family == F_BODY),
    bolditalic = font_file(family, 700, italic = family == F_BODY)
  )
}
for (f in c(F_TITLE, F_LABEL, F_BODY)) {
  known <- c(systemfonts::system_fonts()$family, systemfonts::registry_fonts()$family)
  if (!f %in% known) register_gfont(f)
}

## Light page; every text colour below is a native casa_natal colour from the LTC palette
BG    <- "#FFFFFF"
TEAL  <- "#245E55"    # LTC casa_natal teal  -> title
SLATE <- "#54534F"    # LTC casa_natal slate -> subtitle / counts
INK   <- "#1D1D1B"    # LTC casa_natal black -> names outside the frame
HASHTAG_COL <- TEAL   # #BibleDataStories under the credit (try SLATE for fainter, INK for darker)

## Frame + canvas. Change SEED to shuffle the cell layout (try 22, 30, 71 ...).
RW <- 1; RH <- 0.6; SEED <- 45
W_SVG <- 11; H_SVG <- 8.8                   # canvas size in inches
## XLIM is symmetric about the frame centre (x = 0.5), so the canvas centre -- where the
## title and subtitle are centred -- sits exactly over the middle of the treemap
XLIM  <- c(-0.16, 1.16); YLIM <- c(0.10, 0.90)
## data-units -> millimetres, so text sizes stay sensible at any canvas size
mm_per_unit <- min((W_SVG - 0.4) * 25.4 / diff(XLIM), (H_SVG - 2.3) * 25.4 / diff(YLIM))

p <- ggvmap(
  tribes$men,
  labels      = tribes$tribe,
  palette     = cell_fill,           # explicit LTC colours, cell by cell
  clip        = clip_rectangle(width = RW, height = RH),   # wide frame = biggest viz on the sheet
  show_labels = FALSE,
  legend      = FALSE,
  interactive = TRUE,                # <- ggvmap's own interactive switch
  tooltip     = tooltips,
  border_col  = BG,
  border_size = 2.4,
  max_iter    = 200,
  convergence_ratio = 0.003,
  seed        = SEED                # 45 = a layout with the tiny cells on the edge
)

## ---- 6. Flat icons + labels at each cell centroid ---------------------------
vm  <- attr(p, "vm")
vdf <- vm_as_df(vm)

## "Pole of inaccessibility": the centre and radius of the biggest circle that fits inside a
## (convex) Voronoi cell. Much better than the centroid for placing icons in odd-shaped cells.
pole <- function(P) {
  gx <- seq(min(P$x), max(P$x), length.out = 70); gy <- seq(min(P$y), max(P$y), length.out = 70)
  g  <- expand.grid(x = gx, y = gy)
  x1 <- P$x; y1 <- P$y; x2 <- c(P$x[-1], P$x[1]); y2 <- c(P$y[-1], P$y[1])
  L  <- sqrt((x2 - x1)^2 + (y2 - y1)^2); nx <- -(y2 - y1) / L; ny <- (x2 - x1) / L
  s  <- sign(nx * (mean(P$x) - x1) + ny * (mean(P$y) - y1))          # inward normals
  dm <- sapply(seq_along(x1), function(i) s[i] * (nx[i] * (g$x - x1[i]) + ny[i] * (g$y - y1[i])))
  m  <- apply(dm, 1, min); k <- which.max(m)
  c(px = g$x[k], py = g$y[k], rin = m[k])
}

## Horizontal middle of a (convex) cell at height y -- keeps text centred inside wedge-shaped cells
chord_mid <- function(P, y) {
  x1 <- P$x; y1 <- P$y; x2 <- c(P$x[-1], P$x[1]); y2 <- c(P$y[-1], P$y[1])
  hit <- (y1 - y) * (y2 - y) < 0
  xs  <- x1[hit] + (y - y1[hit]) / (y2[hit] - y1[hit]) * (x2[hit] - x1[hit])
  if (length(xs) < 2) NA_real_ else mean(range(xs))
}
poles <- do.call(rbind, lapply(split(vdf, vdf$cell), function(P) pole(P[, c("x", "y")])))
poles <- data.frame(cell = as.integer(rownames(poles)), poles)

cent <- vm_centroids(vm, inside = TRUE) |>
  left_join(poles, by = "cell") |>
  left_join(tribes, by = c("label" = "tribe")) |>
  mutate(
    fill   = cell_fill[cell],
    ink    = icon_col[cell],
    small  = men < 10000,                      # slivers: name goes OUTSIDE with a leader line
    show_n = !small,
    ## HIERARCHY:  tribe NAME (biggest)  >  headcount  >  symbol
    lab_sz = pmax(5, pmin(10.5, rin * mm_per_unit * 0.27)),   # name size (mm)
    cnt_sz = lab_sz * 0.78,                                    # number size (mm)
    isize  = ifelse(small, 0.60 * rin, pmin(0.20 * rin, 0.040)),   # icon half-size (data units)
    ## stack icon / name / number and centre the block on the cell's pole
    em_n   = lab_sz / mm_per_unit, em_c = cnt_sz / mm_per_unit,
    blk_h  = 2 * isize + ifelse(small, 0, 0.010 + em_n + 0.010 + em_c),
    icon_y = ifelse(small, py, py + blk_h / 2 - isize),
    icon_x = px,
    lab_y  = icon_y - isize - 0.010 - em_n / 2,
    cnt_y  = lab_y - em_n / 2 - 0.010 - em_c / 2
  )
cells <- split(vdf, vdf$cell)
## one shared x per cell for icon + name + count: the mean chord-middle at the name & count heights
cent$icon_x <- ifelse(cent$small, cent$icon_x, mapply(function(c, y1, y2, f) {
  m <- mean(c(chord_mid(cells[[as.character(c)]], y1), chord_mid(cells[[as.character(c)]], y2)))
  ifelse(is.na(m), f, m) }, cent$cell, cent$lab_y, cent$cnt_y, cent$icon_x))

## Outside labels for the small cells: put each one beyond the NEAREST frame edge
x0 <- 0.5 - RW / 2; x1 <- 0.5 + RW / 2; y0 <- 0.5 - RH / 2; y1 <- 0.5 + RH / 2
OUT_N <- 5.6; OUT_C <- 4.8                                   # outside label sizes (mm)
em_n_out <- OUT_N / mm_per_unit; em_c_out <- OUT_C / mm_per_unit
gap <- 0.022
outl <- cent |>
  filter(small) |>
  rowwise() |>
  mutate(
    edge = c("L", "R", "B", "T")[which.min(c(px - x0, x1 - px, py - y0, y1 - py))],
    ex = case_when(edge == "L" ~ x0, edge == "R" ~ x1, TRUE ~ px),     # where the leader meets the frame
    ey = case_when(edge == "B" ~ y0, edge == "T" ~ y1, TRUE ~ py),
    nx = case_when(edge == "L" ~ x0 - gap, edge == "R" ~ x1 + gap, TRUE ~ px),
    ## name always reads first, the count sits next to / under it
    ny = case_when(edge == "T" ~ y1 + gap + em_c_out + em_n_out * 0.5 + 0.008,
                   edge == "B" ~ y0 - gap - em_n_out * 0.5,
                   TRUE ~ py + em_c_out * 0.5 + 0.004),
    cy2 = case_when(edge == "T" ~ y1 + gap + em_c_out * 0.5,
                    edge == "B" ~ y0 - gap - em_n_out - 0.008 - em_c_out * 0.5,
                    TRUE ~ py - em_n_out * 0.5 - 0.004),
    hj = case_when(edge == "L" ~ 1, edge == "R" ~ 0, TRUE ~ 0.5)
  ) |>
  ungroup()

icons <- read.csv(file.path(DATA_DIR, "tribe_icons.csv")) |>
  inner_join(cent[, c("label", "icon_x", "icon_y", "isize", "ink")], by = c("tribe" = "label")) |>
  mutate(X = icon_x + x * isize, Y = icon_y + y * isize)

## Caption wrapped so it never runs off the edge
caption_txt <- paste(strwrap(paste0(
  "Issachar: 200 chiefs (together with all their kinsmen), not a troop count. Reuben & Gad: even split of the combined 120,000 ",
  "Transjordan figure. Joseph = Ephraim + Manasseh. ",
  "Levi includes Jehoiada's 3,700 Aaronites and Zadok with 22 captains. ",
  "Palette: LTC \"", LTC_PALETTE, "\".  Icons: game-icons.net (Lorc, Delapouite, Skoll, Caro Asercion), CC BY 3.0."
), width = 85), collapse = "\n")

cnt <- filter(cent, show_n)
inn <- filter(cent, !small)

p <- p +
  geom_polygon(data = icons, aes(x = X, y = Y, group = tribe, subgroup = sub),
               fill = icons$ink, rule = "winding", colour = NA, inherit.aes = FALSE) +
  geom_text(data = inn, aes(x = icon_x, y = lab_y, label = toupper(label)),
            colour = inn$ink, family = F_LABEL, fontface = "bold", size = inn$lab_sz,
            vjust = 0.5, inherit.aes = FALSE) +
  geom_text(data = cnt, aes(x = icon_x, y = cnt_y, label = format(men, big.mark = ",")),
            colour = cnt$ink, family = F_BODY, fontface = "bold", size = cnt$cnt_sz,
            vjust = 0.5, inherit.aes = FALSE) +
  ## small cells: leader line + name + count just outside the frame
  geom_segment(data = outl, aes(x = px, y = py, xend = ex, yend = ey),
               colour = "#8A857A", linewidth = 0.4, inherit.aes = FALSE) +
  geom_text(data = outl, aes(x = nx, y = ny, label = toupper(label), hjust = hj),
            colour = INK, family = F_LABEL, fontface = "bold", size = OUT_N, inherit.aes = FALSE) +
  geom_text(data = outl, aes(x = nx, y = cy2, hjust = hj,
                             label = paste0(format(men, big.mark = ","),
                                            ifelse(label == "Issachar", " (together with all their kinsmen)", ""))),
            colour = SLATE, family = F_BODY, fontface = "bold", size = OUT_C, inherit.aes = FALSE) +
  expand_limits(x = XLIM, y = YLIM) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  coord_equal(clip = "off") +
  labs(
    title    = "David's army at Hebron",
    subtitle = paste0(format(sum(tribes$men), big.mark = ","),   # total across the 12 tribes
                      " men from all the tribes of Israel who came to David\n",
                      "to make him King of Israel (1 Chr 12)"),
    caption  = caption_txt,
    ## designer credit + hashtag, bottom-right corner (markdown, rendered by ggtext);
    ## "&#35;" = "#" (stops markdown reading it as a heading)
    tag      = paste0("**Designed by Eli Sabblah**<br>",
                      "<span style='font-family:", F_BODY, "; color:", HASHTAG_COL, "; font-size:14pt'>",
                      "***&#35;BibleDataStories***</span>")   # bold italic
  ) +
  theme(
    plot.background  = element_rect(fill = BG, colour = NA),
    panel.background = element_rect(fill = BG, colour = NA),
    ## centre title / subtitle / caption on the whole canvas, not just the panel
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title       = element_text(family = F_TITLE, face = "bold", size = 25, colour = TEAL,
                                    hjust = 0.5, lineheight = 1.05, margin = margin(t = 2, b = 6)),
    plot.subtitle    = element_text(family = F_BODY, face = "italic", size = 15, colour = SLATE,
                                    hjust = 0.5, lineheight = 1.0, margin = margin(b = 4)),
    plot.caption     = element_text(family = F_BODY, size = 13, colour = "#77736A",
                                    hjust = 0.5, lineheight = 1.1, margin = margin(t = 2, b = 0)),
    ## credit: bottom-right, bottom edge level with the caption's last line (both sit 6pt above the edge)
    plot.tag.location = "plot",
    plot.tag.position = c(1 - 10 / (W_SVG * 72), 6 / (H_SVG * 72)),
    plot.tag         = ggtext::element_markdown(family = F_LABEL, size = 13, colour = TEAL,
                                                hjust = 1, vjust = 0, halign = 1, lineheight = 1.25),
    plot.margin      = margin(t = 6, r = 6, b = 6, l = 6)
  )

## ---- 7. Show it / save it ---------------------------------------------------
## vm_girafe() is ggvmap's own wrapper that turns the interactive plot into a widget.
widget <- vm_girafe(
  p,
  width_svg  = W_SVG,
  height_svg = H_SVG,
  hover_css  = "stroke:#1D1D1B;stroke-width:3px;cursor:pointer;",
  bg         = BG                      # device background, so no white bands
)

## (a) Load the decorative Google Fonts in the browser / RStudio Viewer (needs internet).
## (b) Make the icons & labels "click-through" so hovering anywhere on a cell works.
widget <- htmlwidgets::prependContent(
  widget,
  htmltools::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  htmltools::tags$link(
    rel = "stylesheet",
    href = paste0("https://fonts.googleapis.com/css2?",
                  "family=Cinzel+Decorative:wght@400;700&family=Cinzel:wght@400;700",   # same weights R registered
                  "&family=Cormorant+Garamond:ital,wght@0,400;0,700;1,400;1,700&display=swap")),
  htmltools::tags$style(htmltools::HTML(
    "html,body{background:#FFFFFF !important;margin:0}
     svg *:not([data-id]){pointer-events:none}
     svg [data-id]{pointer-events:all}"))
)

## (c) Centre the title & subtitle in the browser. R places each line by its LEFT edge using
##     its own measure of the text width; if the browser's font is a hair wider or narrower the
##     line drifts off-centre. Anchoring each line at its middle makes the centring exact.
centre_lines <- unlist(strsplit(c(p$labels$title, p$labels$subtitle), "\n"))
widget <- htmlwidgets::onRender(widget, sprintf("
function(el) {
  var lines = %s;
  function centre() {
    var svg = null, best = 0;
    el.querySelectorAll('svg').forEach(function(s) {
      var vb = s.viewBox && s.viewBox.baseVal;
      if (vb && vb.width > best) { best = vb.width; svg = s; }
    });
    if (!svg) return;
    var cx = svg.viewBox.baseVal.x + svg.viewBox.baseVal.width / 2;
    svg.querySelectorAll('text').forEach(function(t) {
      if (lines.indexOf(t.textContent.trim()) > -1) {
        t.setAttribute('text-anchor', 'middle');
        t.setAttribute('x', cx);
      }
    });
    rightAlignTag(svg);
  }
  // (d) Credit + hashtag: give the hashtag its full font name (ggtext keeps only 'Cormorant'),
  //     then shift each line so both end exactly at the tag's right edge (10pt from the edge).
  function rightAlignTag(svg) {
    var vb = svg.viewBox.baseVal, right = vb.x + vb.width - 10, rows = {};
    svg.querySelectorAll('text').forEach(function(t) {
      var s = t.textContent.trim();
      if (s === '#BibleDataStories') t.setAttribute('font-family', %s);
      if (s === '#BibleDataStories' || ((t.getAttribute('font-family') || '') === %s && credit.indexOf(s) > -1)) {
        var y = t.getAttribute('y'); (rows[y] = rows[y] || []).push(t);
      }
    });
    Object.keys(rows).forEach(function(y) {
      var edge = Math.max.apply(null, rows[y].map(function(t) { var b = t.getBBox(); return b.x + b.width; }));
      rows[y].forEach(function(t) { t.setAttribute('x', parseFloat(t.getAttribute('x')) + right - edge); });
    });
  }
  var credit = %s;
  centre();
  setTimeout(centre, 300);
  if (document.fonts && document.fonts.ready) document.fonts.ready.then(centre);
}", jsonlite::toJSON(trimws(centre_lines)),
   shQuote(F_BODY, "sh"), shQuote(F_LABEL, "sh"),
   jsonlite::toJSON(c("Designed", "by", "Eli", "Sabblah", "Designed by Eli Sabblah"))))

print(widget)   # explicit print so it shows in the Viewer even when the script is source()d
htmlwidgets::saveWidget(widget, file.path(DATA_DIR, "dave_in_the_cave.html"), selfcontained = TRUE,
                        title = "Dave in the Cave")
cat("Saved dave_in_the_cave.html\n")
