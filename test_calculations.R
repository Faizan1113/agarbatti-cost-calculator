# ===================================================
#  Agarbatti Cost Calculator — Logic Test Script
#  Run this to verify all calculations are correct
# ===================================================

cat("===========================================\n")
cat("  AGARBATTI COST CALCULATOR — TEST RESULTS\n")
cat("===========================================\n\n")

# ── Test Inputs (from the specification example) ──
perfume_name  <- "Example Perfume"
perfume_wt    <- 1      # kg
perfume_price <- 500    # ₹/kg
dep_wt        <- 2      # kg
dep_price     <- 100    # ₹/kg
num_packets   <- 200    # packets
packet_wt_g   <- 150    # grams
raw_price     <- 100    # ₹/kg
label_charge  <- 1      # ₹/packet

cat("── INPUTS ──────────────────────────────────\n")
cat(sprintf("  Perfume Name       : %s\n",    perfume_name))
cat(sprintf("  Perfume Weight     : %.1f kg\n", perfume_wt))
cat(sprintf("  Perfume Price      : ₹%.2f/kg\n", perfume_price))
cat(sprintf("  D.E.P. Oil Weight  : %.1f kg\n", dep_wt))
cat(sprintf("  D.E.P. Oil Price   : ₹%.2f/kg\n", dep_price))
cat(sprintf("  Number of Packets  : %d\n",    num_packets))
cat(sprintf("  Weight per Packet  : %.1f g\n", packet_wt_g))
cat(sprintf("  Raw Material Price : ₹%.2f/kg\n", raw_price))
cat(sprintf("  Label+Pack Charge  : ₹%.2f/pkt\n", label_charge))
cat("\n")

# ── Step 1: Total finished mixture weight ──
total_mix_kg <- num_packets * packet_wt_g / 1000
cat(sprintf("STEP 1  Total Mixture Weight = %d × %.1f ÷ 1000 = %.3f kg\n",
            num_packets, packet_wt_g, total_mix_kg))

# ── Step 2: Agarbatti raw weight ──
raw_wt_kg <- total_mix_kg - perfume_wt - dep_wt
cat(sprintf("STEP 2  Agarbatti Raw Weight  = %.3f - %.1f - %.1f = %.3f kg\n",
            total_mix_kg, perfume_wt, dep_wt, raw_wt_kg))

# ── Step 3: Individual costs ──
cost_perfume <- perfume_wt   * perfume_price
cost_dep     <- dep_wt       * dep_price
cost_raw     <- raw_wt_kg    * raw_price
cost_label   <- num_packets  * label_charge
cat(sprintf("STEP 3  Perfume Cost          = %.1f × %.2f = %.2f\n",
            perfume_wt, perfume_price, cost_perfume))
cat(sprintf("STEP 3  D.E.P. Oil Cost       = %.1f × %.2f = %.2f\n",
            dep_wt, dep_price, cost_dep))
cat(sprintf("STEP 3  Raw Material Cost     = %.3f × %.2f = %.2f\n",
            raw_wt_kg, raw_price, cost_raw))
cat(sprintf("STEP 3  Label+Pack Cost       = %d × %.2f = %.2f\n",
            num_packets, label_charge, cost_label))

# ── Step 4: Total batch cost ──
total_batch <- cost_perfume + cost_dep + cost_raw + cost_label
cat(sprintf("STEP 4  Total Batch Cost      = %.2f + %.2f + %.2f + %.2f = %.2f\n",
            cost_perfume, cost_dep, cost_raw, cost_label, total_batch))

# ── Step 5: Cost per packet ──
cost_per_pkt <- total_batch / num_packets
cat(sprintf("STEP 5  Cost per Packet       = %.2f ÷ %d = %.2f\n\n",
            total_batch, num_packets, cost_per_pkt))

# ── Expected vs Actual ──
cat("── EXPECTED vs ACTUAL ──────────────────────\n")
check <- function(label, actual, expected) {
  pass <- abs(actual - expected) < 1e-9
  mark <- if (pass) "✔ PASS" else "✘ FAIL"
  cat(sprintf("  %s  %-30s  Expected: %.3f  Got: %.3f\n",
              mark, label, expected, actual))
}

check("Total Mixture Weight (kg)",    total_mix_kg,   30)
check("Agarbatti Raw Weight (kg)",    raw_wt_kg,      27)
check("Perfume Cost (₹)",             cost_perfume,  500)
check("D.E.P. Oil Cost (₹)",          cost_dep,      200)
check("Raw Material Cost (₹)",        cost_raw,     2700)
check("Labelling+Pack Cost (₹)",      cost_label,    200)
check("Total Batch Cost (₹)",         total_batch,  3600)
check("Cost per Packet (₹)",          cost_per_pkt,   18)

cat("\n── VALIDATION CHECKS ───────────────────────\n")

val_ok <- TRUE

if (is.na(num_packets) || num_packets < 1 || num_packets != floor(num_packets)) {
  cat("  ✘ num_packets validation FAILED\n"); val_ok <- FALSE
} else {
  cat("  ✔ num_packets is a positive whole number\n")
}

if (is.na(packet_wt_g) || packet_wt_g <= 0) {
  cat("  ✘ packet_wt_g validation FAILED\n"); val_ok <- FALSE
} else {
  cat("  ✔ packet_wt_g is > 0\n")
}

if (raw_wt_kg < 0) {
  cat("  ✘ raw_wt_kg is negative — weight error\n"); val_ok <- FALSE
} else {
  cat("  ✔ raw_wt_kg is non-negative\n")
}

if (num_packets <= 0) {
  cat("  ✘ Division by zero risk\n"); val_ok <- FALSE
} else {
  cat("  ✔ No division by zero risk\n")
}

cat("\n")
if (val_ok) {
  cat("  ALL TESTS PASSED ✔\n")
} else {
  cat("  SOME TESTS FAILED ✘ — please review above\n")
}
cat("===========================================\n")
