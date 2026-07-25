# The colour maths is only worth having if it agrees with the published
# reference data. These are the CIEDE2000 pairs from Sharma, Wu & Dalal (2005),
# Table 1 — the standard conformance set for the formula.

test_that("contrast_ratio matches the WCAG definition at its endpoints", {
  expect_equal(contrast_ratio("#ffffff", "#000000"), 21, tolerance = 1e-9)
  expect_equal(contrast_ratio("#0f0f0f", "#0f0f0f"), 1, tolerance = 1e-9)
  # order-independent: it is a ratio of the lighter to the darker
  expect_equal(contrast_ratio("#ffffff", "#4f9bf0"),
               contrast_ratio("#4f9bf0", "#ffffff"))
})

test_that("hex_to_lab anchors the achromatic axis", {
  expect_equal(hex_to_lab("#ffffff"), c(100, 0, 0), tolerance = 1e-3)
  expect_equal(hex_to_lab("#000000"), c(0, 0, 0), tolerance = 1e-9)
})

test_that("delta_e2000 reproduces the Sharma reference pairs", {
  cases <- list(
    list(c(50,  2.6772, -79.7751), c(50,  0.0000, -82.7485), 2.0425),
    list(c(50,  3.1571, -77.2803), c(50,  0.0000, -82.7485), 2.8615),
    list(c(50,  2.8361, -74.0200), c(50,  0.0000, -82.7485), 3.4412),
    list(c(50, -1.3802, -84.2814), c(50,  0.0000, -82.7485), 1.0000),
    list(c(50, -1.1848, -84.8006), c(50,  0.0000, -82.7485), 1.0000),
    list(c(50,  0.0000,   0.0000), c(50, -1.0000,   2.0000), 2.3669),
    list(c(50,  2.4900,  -0.0010), c(50, -2.4900,   0.0009), 7.1792),
    list(c(60.2574, -34.0099, 36.2677),
         c(60.4626, -34.1751, 39.4387), 1.2644)
  )
  for (k in cases) {
    expect_equal(delta_e2000(k[[1]], k[[2]]), k[[3]], tolerance = 1e-4,
                 info = sprintf("expected %.4f", k[[3]]))
  }
})

test_that("delta_e2000 is a metric on the cases we rely on", {
  a <- hex_to_lab("#4f9bf0"); b <- hex_to_lab("#ff6b6b")
  expect_equal(delta_e2000(a, a), 0, tolerance = 1e-12)
  expect_equal(delta_e2000(a, b), delta_e2000(b, a), tolerance = 1e-9)
})

test_that("simulate_cvd collapses each dichromacy's own confusion pair", {
  # One pair per type, each chosen because the simulation flattens it: a normal
  # observer separates them easily, the simulated observer barely at all. This
  # is the property the palette test leans on, so a simulation that quietly did
  # nothing has to fail here.
  #
  # The obvious pairs do not work and are a trap. Pure red against pure green
  # under protanopia stays 45.8 apart, because protanopia darkens red and the
  # lightness difference survives; pure blue against pure yellow under
  # tritanopia does not move at all, because that pair differs almost entirely
  # in lightness. Both would be tests that assert nothing. These pairs are
  # matched closely enough in lightness that only the confused axis separates
  # them.
  confusion <- list(
    protan = c("#80c0c0", "#ffc0c0"),   # cyan/pink, the red-green axis
    deutan = c("#80c080", "#ff8080"),   # green/salmon
    tritan = c("#ff0000", "#ff00ff")    # red/magenta, the blue-yellow axis
  )
  for (ty in names(confusion)) {
    pair <- confusion[[ty]]
    normal <- delta_e2000(hex_to_lab(pair[1]), hex_to_lab(pair[2]))
    simulated <- delta_e2000(lab_under_cvd(pair[1], ty),
                             lab_under_cvd(pair[2], ty))
    expect_gt(normal, 40)      # clearly two colours to a normal observer
    expect_lt(simulated, 10)   # barely one to this one
  }
})

test_that("simulate_cvd returns a usable hex and is idempotent in gamut", {
  for (ty in CVD_TYPES) {
    out <- simulate_cvd("#4f9bf0", ty)
    expect_match(out, "^#[0-9a-fA-F]{6}$", info = ty)
    expect_equal(simulate_cvd(out, ty), out, info = ty)
  }
})
