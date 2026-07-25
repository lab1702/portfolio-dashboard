# Colour maths for the palette tests. Test-only: the dashboard never needs any
# of this at runtime, so it stays out of portfolio_core.R.
#
# The palette's invariants — "adjacent slots stay apart, including for a
# colourblind reader" — used to be a sentence in a comment with nothing behind
# it. This is what puts something behind it.

hex_to_rgb01 <- function(hex) as.numeric(grDevices::col2rgb(hex)[, 1]) / 255

srgb_to_linear <- function(c) {
  ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055)^2.4)
}

linear_to_srgb <- function(c) {
  ifelse(c <= 0.0031308, c * 12.92, 1.055 * c^(1 / 2.4) - 0.055)
}

relative_luminance <- function(hex) {
  sum(c(0.2126, 0.7152, 0.0722) * srgb_to_linear(hex_to_rgb01(hex)))
}

contrast_ratio <- function(hex_a, hex_b) {
  la <- relative_luminance(hex_a)
  lb <- relative_luminance(hex_b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

# sRGB D65 primaries.
.RGB_TO_XYZ <- matrix(c(0.4124564, 0.3575761, 0.1804375,
                        0.2126729, 0.7151522, 0.0721750,
                        0.0193339, 0.1191920, 0.9503041),
                      nrow = 3, byrow = TRUE)

.D65 <- c(0.95047, 1.00000, 1.08883)

hex_to_lab <- function(hex) {
  xyz <- as.numeric(.RGB_TO_XYZ %*% srgb_to_linear(hex_to_rgb01(hex))) / .D65
  f <- ifelse(xyz > (6 / 29)^3, xyz^(1 / 3), xyz / (3 * (6 / 29)^2) + 4 / 29)
  c(116 * f[2] - 16, 500 * (f[1] - f[2]), 200 * (f[2] - f[3]))
}

# CIEDE2000, per Sharma, Wu & Dalal (2005). Verified against that paper's
# conformance pairs in test-color-math.R. Written out longhand rather than
# simplified: the hue-difference branches and the RT rotation term are exactly
# where a compressed version goes quietly wrong.
delta_e2000 <- function(lab_a, lab_b) {
  L1 <- lab_a[1]; a1 <- lab_a[2]; b1 <- lab_a[3]
  L2 <- lab_b[1]; a2 <- lab_b[2]; b2 <- lab_b[3]

  C1 <- sqrt(a1^2 + b1^2); C2 <- sqrt(a2^2 + b2^2)
  Cbar <- (C1 + C2) / 2
  G <- 0.5 * (1 - sqrt(Cbar^7 / (Cbar^7 + 25^7)))

  a1p <- (1 + G) * a1; a2p <- (1 + G) * a2
  C1p <- sqrt(a1p^2 + b1^2); C2p <- sqrt(a2p^2 + b2^2)

  h1p <- if (a1p == 0 && b1 == 0) 0 else atan2(b1, a1p) %% (2 * pi)
  h2p <- if (a2p == 0 && b2 == 0) 0 else atan2(b2, a2p) %% (2 * pi)

  dLp <- L2 - L1
  dCp <- C2p - C1p
  dhp <- if (C1p * C2p == 0) 0
         else if (abs(h2p - h1p) <= pi) h2p - h1p
         else if (h2p - h1p > pi) h2p - h1p - 2 * pi
         else h2p - h1p + 2 * pi
  dHp <- 2 * sqrt(C1p * C2p) * sin(dhp / 2)

  Lbar <- (L1 + L2) / 2
  Cbarp <- (C1p + C2p) / 2
  hbarp <- if (C1p * C2p == 0) h1p + h2p
           else if (abs(h1p - h2p) <= pi) (h1p + h2p) / 2
           else if (h1p + h2p < 2 * pi) (h1p + h2p + 2 * pi) / 2
           else (h1p + h2p - 2 * pi) / 2

  T_ <- 1 - 0.17 * cos(hbarp - pi / 6) +
            0.24 * cos(2 * hbarp) +
            0.32 * cos(3 * hbarp + pi / 30) -
            0.20 * cos(4 * hbarp - 63 * pi / 180)

  dTheta <- (30 * pi / 180) * exp(-(((hbarp * 180 / pi) - 275) / 25)^2)
  RC <- 2 * sqrt(Cbarp^7 / (Cbarp^7 + 25^7))
  SL <- 1 + (0.015 * (Lbar - 50)^2) / sqrt(20 + (Lbar - 50)^2)
  SC <- 1 + 0.045 * Cbarp
  SH <- 1 + 0.015 * Cbarp * T_
  RT <- -sin(2 * dTheta) * RC

  sqrt((dLp / SL)^2 + (dCp / SC)^2 + (dHp / SH)^2 +
       RT * (dCp / SC) * (dHp / SH))
}

# Vienot, Brettel & Mollon (1999). The dichromat matrices act in LMS on
# *linear* RGB; applying them to gamma-encoded values is the usual way this
# gets implemented wrong, and it understates how close two hues become.
.RGB_TO_LMS <- matrix(c(17.8824,    43.5161,   4.11935,
                         3.45565,   27.1554,   3.86714,
                         0.0299566,  0.184309, 1.46709),
                      nrow = 3, byrow = TRUE)

.LMS_TO_RGB <- solve(.RGB_TO_LMS)

.CVD_MATRIX <- list(
  protan = matrix(c(0, 2.02344, -2.52581,
                    0, 1,        0,
                    0, 0,        1), nrow = 3, byrow = TRUE),
  deutan = matrix(c(1,        0, 0,
                    0.494207, 0, 1.24827,
                    0,        0, 1), nrow = 3, byrow = TRUE),
  tritan = matrix(c( 1,        0,        0,
                     0,        1,        0,
                    -0.395913, 0.801109, 0), nrow = 3, byrow = TRUE)
)

CVD_TYPES <- names(.CVD_MATRIX)

simulate_cvd <- function(hex, type) {
  stopifnot(type %in% CVD_TYPES)
  lms <- .RGB_TO_LMS %*% srgb_to_linear(hex_to_rgb01(hex))
  out <- as.numeric(.LMS_TO_RGB %*% (.CVD_MATRIX[[type]] %*% lms))
  # Clamp in linear space before encoding: the projection can land outside the
  # sRGB cube, and an out-of-gamut channel is not a colour anyone will see.
  ch <- round(255 * linear_to_srgb(pmin(pmax(out, 0), 1)))
  grDevices::rgb(ch[1], ch[2], ch[3], maxColorValue = 255)
}

lab_under_cvd <- function(hex, type) hex_to_lab(simulate_cvd(hex, type))
