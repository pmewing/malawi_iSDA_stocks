calc_stock = function(carbon, bulk_density, depth) {
  # units
  # carbon: percentage (decimals, ex 1% = 0.01)
  # bulk density: g/cm3
  # depth: m
  # returns stock in Mg/ha
  bulk_density = bulk_density * 100^3 # convert to g/m3
  ha_volume = 10000 * depth # calc volume per 10000 m2 surface (1 ha)
  soil_mass = bulk_density * ha_volume / 1E6
  out = carbon * soil_mass

  return(out)
}
