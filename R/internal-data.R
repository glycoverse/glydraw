# Internal SNFG constants: residue-to-shape mappings, fill colors, and polygon
# templates used by cartoon drawing helpers.

# glycan mapping
glycan_color <- c(
  'glyWhite' = '#FFFFFF',
  'glyBlue' = '#0072BC',
  'glyGreen' = '#00A651',
  'glyYellow' = '#FFD400',
  'glyOrange' = '#F47920',
  'glyPink' = '#F69EA1',
  'glyPurple' = '#A54399',
  'glyLightBlue' = '#8FCCE9',
  'glyBrown' = '#A17A4D',
  'glyRed' = '#ED1C24'
)

glycan_dict <- list(
  'Hex' = c('Hex', 'glyWhite'),
  'Glc' = c('Hex', 'glyBlue'),
  'Man' = c('Hex', 'glyGreen'),
  'Gal' = c('Hex', 'glyYellow'),
  'Gul' = c('Hex', 'glyOrange'),
  'Alt' = c('Hex', 'glyPink'),
  'All' = c('Hex', 'glyPurple'),
  'Tal' = c('Hex', 'glyLightBlue'),
  'Ido' = c('Hex', 'glyBrown'),

  'HexNAc' = c('HexNAc', 'glyWhite'),
  'GlcNAc' = c('HexNAc', 'glyBlue'),
  'ManNAc' = c('HexNAc', 'glyGreen'),
  'GalNAc' = c('HexNAc', 'glyYellow'),
  'GulNAc' = c('HexNAc', 'glyOrange'),
  'AltNAc' = c('HexNAc', 'glyPink'),
  'AllNAc' = c('HexNAc', 'glyPurple'),
  'TalNAc' = c('HexNAc', 'glyLightBlue'),
  'IdoNAc' = c('HexNAc', 'glyBrown'),

  'HexN' = c('HexN', 'glyWhite', 'glyWhite'),
  'GlcN' = c('HexN', 'glyBlue', 'glyWhite'),
  'ManN' = c('HexN', 'glyGreen', 'glyWhite'),
  'GalN' = c('HexN', 'glyYellow', 'glyWhite'),
  'GulN' = c('HexN', 'glyOrange', 'glyWhite'),
  'AltN' = c('HexN', 'glyPink', 'glyWhite'),
  'AllN' = c('HexN', 'glyPurple', 'glyWhite'),
  'TalN' = c('HexN', 'glyLightBlue', 'glyWhite'),
  'IdoN' = c('HexN', 'glyBrown', 'glyWhite'),

  'HexA' = c('HexA', 'glyWhite', 'glyWhite'),
  'GlcA' = c('HexA', 'glyBlue', 'glyWhite'),
  'ManA' = c('HexA', 'glyGreen', 'glyWhite'),
  'GalA' = c('HexA', 'glyYellow', 'glyWhite'),
  'GulA' = c('HexA', 'glyOrange', 'glyWhite'),
  'AltA' = c('HexA', 'glyPink', 'glyWhite'),
  'AllA' = c('HexA', 'glyPurple', 'glyWhite'),
  'TalA' = c('HexA', 'glyLightBlue', 'glyWhite'),
  'IdoA' = c('HexA', 'glyBrown', 'glyWhite'),

  'dHex' = c('dHex', 'glyWhite'),
  'Qui' = c('dHex', 'glyBlue'),
  'Rha' = c('dHex', 'glyGreen'),
  '6dGul' = c('dHex', 'glyOrange'),
  '6dAlt' = c('dHex', 'glyPink'),
  '6dTal' = c('dHex', 'glyLightBlue'),
  'Fuc' = c('dHex', 'glyRed'),
  'FucUp' = c('dHexUp', 'glyRed'),
  'FucRight' = c('dHexRight', 'glyRed'),
  'FucLeft' = c('dHexLeft', 'glyRed'),
  'QuiUp' = c('dHexUp', 'glyBlue'),
  'QuiRight' = c('dHexRight', 'glyBlue'),
  'QuiLeft' = c('dHexLeft', 'glyBlue'),
  'RhaUp' = c('dHexUp', 'glyGreen'),
  'RhaRight' = c('dHexRight', 'glyGreen'),
  'RhaLeft' = c('dHexLeft', 'glyGreen'),
  '6dGulUp' = c('dHexUp', 'glyOrange'),
  '6dGulRight' = c('dHexRight', 'glyOrange'),
  '6dGulLeft' = c('dHexLeft', 'glyOrange'),
  '6dAltUp' = c('dHexUp', 'glyPink'),
  '6dAltRight' = c('dHexRight', 'glyPink'),
  '6dAltLeft' = c('dHexLeft', 'glyPink'),
  '6dTalUp' = c('dHexUp', 'glyLightBlue'),
  '6dTalRight' = c('dHexRight', 'glyLightBlue'),
  '6dTalLeft' = c('dHexLeft', 'glyLightBlue'),

  'dHexNAc' = c('dHexNAc', 'glyWhite', 'glyWhite'),
  'QuiNAc' = c('dHexNAc', 'glyBlue', 'glyWhite'),
  'RhaNAc' = c('dHexNAc', 'glyGreen', 'glyWhite'),
  '6dAltNAc' = c('dHexNAc', 'glyPink', 'glyWhite'),
  '6dTalNAc' = c('dHexNAc', 'glyLightBlue', 'glyWhite'),
  'FucNAc' = c('dHexNAc', 'glyRed', 'glyWhite'),
  'QuiNAcUp' = c('dHexNAcUp', 'glyBlue', 'glyWhite'),
  'QuiNAcRight' = c('dHexNAcRight', 'glyBlue', 'glyWhite'),
  'QuiNAcLeft' = c('dHexNAcLeft', 'glyBlue', 'glyWhite'),
  'RhaNAcUp' = c('dHexNAcUp', 'glyGreen', 'glyWhite'),
  'RhaNAcRight' = c('dHexNAcRight', 'glyGreen', 'glyWhite'),
  'RhaNAcLeft' = c('dHexNAcLeft', 'glyGreen', 'glyWhite'),
  '6dAltNAcUp' = c('dHexNAcUp', 'glyPink', 'glyWhite'),
  '6dAltNAcRight' = c('dHexNAcRight', 'glyPink', 'glyWhite'),
  '6dAltNAcLeft' = c('dHexNAcLeft', 'glyPink', 'glyWhite'),
  '6dTalNAcUp' = c('dHexNAcUp', 'glyLightBlue', 'glyWhite'),
  '6dTalNAcRight' = c('dHexNAcRight', 'glyLightBlue', 'glyWhite'),
  '6dTalNAcLeft' = c('dHexNAcLeft', 'glyLightBlue', 'glyWhite'),
  'FucNAcUp' = c('dHexNAcUp', 'glyRed', 'glyWhite'),
  'FucNAcRight' = c('dHexNAcRight', 'glyRed', 'glyWhite'),
  'FucNAcLeft' = c('dHexNAcLeft', 'glyRed', 'glyWhite'),

  'Oli' = c('ddHex', 'glyBlue'),
  'Tyv' = c('ddHex', 'glyGreen'),
  'Abe' = c('ddHex', 'glyOrange'),
  'Par' = c('ddHex', 'glyPink'),
  'Dig' = c('ddHex', 'glyPurple'),
  'Col' = c('ddHex', 'glyLightBlue'),

  'Pen' = c('Pen', 'glyWhite'),
  'Ara' = c('Pen', 'glyGreen'),
  'Lyx' = c('Pne', 'glyYellow'),
  'Xyl' = c('Pen', 'glyOrange'),
  'Rib' = c('Pen', 'glyPink'),

  'dNon' = c('dNon', 'glyWhite'),
  'Kdn' = c('dNon', 'glyGreen'),
  'Neu5Ac' = c('dNon', 'glyPurple'),
  'Neu5Gc' = c('dNon', 'glyLightBlue'),
  'NeuAc' = c('dNon', 'glyWhite'),
  'NeuGc' = c('dNon', 'glyWhite'),
  'Neu' = c('dNon', 'glyBrown'),
  'Sia' = c('dNon', 'glyRed'),

  'ddNon' = c('ddNon', 'glyWhite'),
  'Pse' = c('ddNon', 'glyGreen'),
  'Leg' = c('ddNon', 'glyYellow'),
  'Aci' = c('ddNon', 'glyPink'),
  '4eLeg' = c('ddNon', 'glyLightBlue'),

  'UnKnown' = c('UnKnown', 'glyWhite'),
  'Bac' = c('UnKnown', 'glyBlue'),
  'LDmanHep' = c('UnKnown', 'glyGreen'),
  'Kdo' = c('UnKnown', 'glyYellow'),
  'Dha' = c('UnKnown', 'glyOrange'),
  'DDmanHep' = c('UnKnown', 'glyPink'),
  'MurNAc' = c('UnKnown', 'glyPurple'),
  'MurNGc' = c('UnKnown', 'glyLightBlue'),
  'Mur' = c('UnKnown', 'glyBrown'),

  'Assigned' = c('Assigned', 'glyWhite'),
  'Api' = c('Assigned', 'glyBlue'),
  'Fru' = c('Assigned', 'glyGreen'),
  'Tag' = c('Assigned', 'glyYellow'),
  'Sor' = c('Assigned', 'glyOrange'),
  'Psi' = c('Assigned', 'glyPink')
)

glycan_shape <- list(
  'Hex' = data.frame(
    x = cos(seq(0, 2 * pi, length.out = 50)), # The center is the Core of shape
    y = sin(seq(0, 2 * pi, length.out = 50))
  ),
  'HexNAc' = data.frame(x = c(-1, -1, 1, 1, -1), y = c(-1, 1, 1, -1, -1)),
  'HexN' = data.frame(
    x = c(-1, 1, 1, -1),
    y = c(1, 1, -1, 1),
    xx = c(-1, 1, -1, -1),
    yy = c(1, -1, -1, 1)
  ),
  'HexA' = data.frame(
    x = c(-1, 0, 1, -1),
    y = c(0, 1, 0, 0),
    xx = c(1, 0, -1, 1),
    yy = c(0, -1, 0, 0)
  ),
  'dHex' = data.frame(
    x = c(-1, 0, 1, -1),
    y = c(-1, 1, -1, -1)
  ),
  'dHexUp' = data.frame(
    x = c(-1, 0, 1, -1),
    y = c(1, -1, 1, 1)
  ),
  'dHexRight' = data.frame(
    x = c(-0.33 * sqrt(3), 0.67 * sqrt(3), -0.33 * sqrt(3), -0.33 * sqrt(3)),
    y = c(1, 0, -1, 1)
  ),
  'dHexLeft' = data.frame(
    x = c(0.33 * sqrt(3), -0.67 * sqrt(3), 0.33 * sqrt(3), 0.33 * sqrt(3)),
    y = c(1, 0, -1, 1)
  ),
  'dHexNAc' = data.frame(
    x = c(0, 1, 0, 0),
    y = c(1, -1, -1, 1),
    xx = c(0, -1, 0, 0),
    yy = c(1, -1, -1, 1)
  ),
  'dHexNAcUp' = data.frame(
    x = c(0, 1, 0, 0),
    y = c(-1, 1, 1, -1),
    xx = c(0, -1, 0, 0),
    yy = c(-1, 1, 1, -1)
  ),
  'dHexNAcRight' = data.frame(
    x = c(0.67 * sqrt(3), -0.33 * sqrt(3), -0.33 * sqrt(3), 0.67 * sqrt(3)),
    y = c(0, 1, 0, 0),
    xx = c(0.67 * sqrt(3), -0.33 * sqrt(3), -0.33 * sqrt(3), 0.67 * sqrt(3)),
    yy = c(0, -1, 0, 0)
  ),
  'dHexNAcLeft' = data.frame(
    x = c(-0.67 * sqrt(3), 0.33 * sqrt(3), 0.33 * sqrt(3), -0.67 * sqrt(3)),
    y = c(0, 1, 0, 0),
    xx = c(-0.67 * sqrt(3), 0.33 * sqrt(3), 0.33 * sqrt(3), -0.67 * sqrt(3)),
    yy = c(0, -1, 0, 0)
  ),
  'ddHex' = data.frame(x = c(-1, 1, 1, -1), y = c(0.5, 0.5, -0.5, -0.5)),
  'Pen' = data.frame(
    x = c(
      0,
      0.2345,
      0.9516,
      0.3798,
      0.5872,
      0,
      -0.5872,
      -0.3798,
      -0.9516,
      -0.2345,
      0
    ),
    y = c(
      0.905,
      0.2289,
      0.2132,
      -0.219,
      -0.905,
      -0.4961,
      -0.905,
      -0.219,
      0.2132,
      0.2289,
      0.905
    )
  ),
  'dNon' = data.frame(x = c(0, 1, 0, -1, 0), y = c(1, 0, -1, 0, 1)),
  'ddNon' = data.frame(x = c(0, 1.2, 0, -1.2, 0), y = c(0.8, 0, -0.8, 0, 0.8)),
  'UnKnown' = data.frame(
    x = c(0.6, 1, 0.6, -0.6, -1, -0.6, 0.6),
    y = c(0.6, 0, -0.6, -0.6, 0, 0.6, 0.6)
  ),
  'Assigned' = data.frame(
    x = c(0, 0.9516, 0.5872, -0.5872, -0.9516, 0),
    y = c(0.905, 0.2132, -0.905, -0.905, 0.2132, 0.905)
  )
)

.fucose_like_layout_monosaccharides <- c(
  "Fuc",
  "Qui",
  "Rha",
  "6dGul",
  "6dAlt",
  "6dTal",
  "QuiNAc",
  "RhaNAc",
  "6dAltNAc",
  "6dTalNAc",
  "FucNAc",
  "Oli",
  "Tyv",
  "Abe",
  "Par",
  "Dig",
  "Col",
  "Ara",
  "Lyx",
  "Xyl",
  "Rib"
)

.fucose_like_orient_monosaccharides <- c(
  "Fuc",
  "Qui",
  "Rha",
  "6dGul",
  "6dAlt",
  "6dTal",
  "QuiNAc",
  "RhaNAc",
  "6dAltNAc",
  "6dTalNAc",
  "FucNAc"
)

#' Identify residues that use Fuc-like branch layout
#'
#' @param mono A character vector of monosaccharide names.
#'
#' @returns A logical vector indicating whether each monosaccharide should use
#'   linkage-specific Fuc-like branch offsets.
#' @noRd
.is_fucose_like_layout_monosaccharide <- function(mono) {
  mono %in% .fucose_like_layout_monosaccharides
}

#' Identify residues that use Fuc-like directional shapes
#'
#' @param mono A character vector of monosaccharide names.
#'
#' @returns A logical vector indicating whether each monosaccharide should use
#'   directional glycoform names when `fuc_orient = "flex"`.
#' @noRd
.is_fucose_like_orient_monosaccharide <- function(mono) {
  mono %in% .fucose_like_orient_monosaccharides
}

#' Get internal directional Fuc-like glycoform names
#'
#' @returns A character vector of glycoform names used only for directional
#'   rendering of Fuc-like residues.
#' @noRd
.directional_fucose_like_glycoforms <- function() {
  as.vector(outer(
    .fucose_like_orient_monosaccharides,
    c("Up", "Right", "Left"),
    paste0
  ))
}
