cGENIE.base.config <- function(output_dir, file_name, cookiegen_dir, template = "NONE",
                 ma_flag_ebatmos = ".TRUE.", ma_flag_goldsteinocean = ".TRUE.",
                 ma_flag_goldsteinseaice = ".TRUE.", ma_flag_biogem = ".TRUE.",
                 ma_flag_atchem = ".TRUE.", ma_flag_sedgem = ".FALSE.",
                 ma_flag_rokgem = ".FALSE.", ma_flag_gemlite = ".FALSE.",
                 ma_flag_ecogem = ".FALSE.", ma_flag_ents = ".FALSE.",
                 ma_genie_solar_constant = NULL, go_saln0 = NULL) {

  # Validate inputs
  if (!dir.exists(output_dir)) {
  stop("Output directory does not exist.")
  }

  if (template != "NONE" && template != "BASES") {
  stop("Invalid template specified. Only 'NONE' and 'BASES' are supported.")
  }

  # Create the configuration content for template "NONE"
  config_content <- paste0(
  "# *******************************************************************\n",
  "# CONFIGURATION TEMPLATE -- for a 16 level seasonally forced ocean\n",
  "# *******************************************************************\n",
  "\n",
  "# *******************************************************************\n",
  "# GENIE COMPONENT SELECTION\n",
  "# *******************************************************************\n",
  "# make .TRUE. the cGENIE modules to be included\n",
  "# *******************************************************************\n"
  )

  # Update the configuration content with the flags
  config_content <- paste0(
  config_content,
  "ma_flag_ebatmos=", ma_flag_ebatmos, "\n",
  "ma_flag_goldsteinocean=", ma_flag_goldsteinocean, "\n",
  "ma_flag_goldsteinseaice=", ma_flag_goldsteinseaice, "\n",
  "ma_flag_biogem=", ma_flag_biogem, "\n",
  "ma_flag_atchem=", ma_flag_atchem, "\n",
  "ma_flag_sedgem=", ma_flag_sedgem, "\n",
  "ma_flag_rokgem=", ma_flag_rokgem, "\n",
  "ma_flag_gemlite=", ma_flag_gemlite, "\n",
  "ma_flag_ecogem=", ma_flag_ecogem, "\n",
  "ma_flag_ents=", ma_flag_ents, "\n",
  "# *******************************************************************\n",
  "\n"
  )

  if (template == "NONE") {
  config_content <- paste0(config_content,
    "# *******************************************************************\n",
    "# TRACER CONFIGURATION\n",
    "# *******************************************************************\n",
    "# the total number of tracers includes T and S\n",
    "# T and S do not need to be explicited selected and initialzied\n",
    "# *******************************************************************\n",
    "# Set number of tracers\n",
    "GOLDSTEINNTRACSOPTS='$(DEFINE)GOLDSTEINNTRACS=2'\n",
    "# list selected biogeochemical tracers\n",
    "# <<<                                                             >>>\n",
    "# list biogeochemical tracer initial values\n",
    "# <<<                                                             >>>\n",
    "# *******************************************************************\n",
    "\n"
  )
  } else if (template == "BASES") {
  config_content <- paste0(config_content,
    "# *******************************************************************\n",
    "# TRACER CONFIGURATION\n",
    "# *******************************************************************\n",
    "# the total number of tracers includes T and S\n",
    "# T and S do not need to be explicited selected and initialzied\n",
    "# *******************************************************************\n",
    "# Set number of tracers - these are the tracers from the BASES configs\n",
    "GOLDSTEINNTRACSOPTS='$(DEFINE)GOLDSTEINNTRACS=14'\n",
    "# Set selected tracers\n",
    "gm_atm_select_3=.true. # pCO2\n",
    "gm_atm_select_4=.true. # pCO2_13C\n",
    "gm_atm_select_6=.true. # pO2\n",
    "gm_atm_select_16=.true. # pH2S\n",
    "gm_ocn_select_3=.true. # DIC\n",
    "gm_ocn_select_4=.true. # DIC_13C\n",
    "gm_ocn_select_8=.true. # PO4\n",
    "gm_ocn_select_10=.true. # O2\n",
    "gm_ocn_select_12=.true. # ALK\n",
    "gm_ocn_select_15=.true. # DOM_C\n",
    "gm_ocn_select_16=.true. # DOM_C_13C\n",
    "gm_ocn_select_20=.true. # DOM_P\n",
    "gm_ocn_select_35=.true. # Ca\n",
    "gm_ocn_select_38=.true. # SO4\n",
    "gm_ocn_select_40=.true. # H2S\n",
    "gm_ocn_select_50=.true. # Mg\n",
    "gm_sed_select_3=.true. # POC\n",
    "gm_sed_select_4=.true. # POC_13C\n",
    "gm_sed_select_8=.true. # POP\n",
    "gm_sed_select_14=.true. # CaCO3\n",
    "gm_sed_select_15=.true. # CaCO3_13C\n",
    "gm_sed_select_22=.true. # det\n",
    "gm_sed_select_32=.true. # ash\n",
    "gm_sed_select_33=.true. # POC_frac2\n",
    "gm_sed_select_34=.true. # CaCO3_frac2\n",
    "gm_sed_select_36=.true. # CaCO2_age\n",
    "# *******************************************************************\n",
    "\n"
  )
  }

  config_content <- paste0(config_content,
  "# *******************************************************************\n",
  "# GRID & BOUNDARY CONDITION CONFIGURATION\n",
  "# *******************************************************************\n",
  "# insert the automatically generated muffingen parameter list here\n",
  "# *******************************************************************\n",
  "# <<<                                                             >>>\n",
  "# *******************************************************************\n",
  "\n")

  # Read the first 50 lines from the config file in cookiegen_dir
  config_files <- list.files(cookiegen_dir, pattern = "^config_\\d+", full.names = TRUE)
  if (length(config_files) == 0) {
    stop("No config file found in the specified directory.")
  }
  config_file <- config_files[1]
  config_lines <- readLines(config_file, n = 50)

  # Insert the lines from the config file
  # Replace specific lines if custom values are provided
  if (!is.null(ma_genie_solar_constant)) {
    config_lines[42] <- paste0("# custom solar constant")
    config_lines[43] <- paste0("ma_genie_solar_constant=", ma_genie_solar_constant)
  }
  if (!is.null(go_saln0)) {
    config_lines[44] <- paste0("# custom salinity")
    config_lines[45] <- paste0("go_saln0=", go_saln0)
  }

  config_content <- paste0(config_content, paste(config_lines, collapse = "\n"), "\n")

  config_content <- paste0(config_content,
  "# *******************************************************************\n",
  "# PHYSICAL CLIMATE CALIBRATION\n",
  "# *******************************************************************\n",
  "# based on Cao et al. [2009] with the following exceptions:\n",
  "# (1) warmer (5C) ocean start (could be 10C for a more intense greenhouse world)\n",
  "# (2) scaling of the freshwater re-balancing flux to zero\n",
  "# (3) application of a reduced sea-ice diffusivity and \n",
  "#     + prescribed maximum fractional area for sea-ice advection\n",
  "# (5) no reduced diffusivity over Antarctica\n",
  "# *******************************************************************\n",
  "# rel\n",
  "go_12=0.9000000\n",
  "# scl_tau / SclTau\n",
  "go_13=1.531013488769531300\n",
  "# ocean diffusivites iso (or horiz) / OcnHorizDiff\n",
  "go_14=1494.438354492187500000\n",
  "# ocean diffusivites dia (or vert) / OcnVertDiff\n",
  "go_15=0.000025363247914356\n",
  "# inverse minimum drag in days / InvDrag\n",
  "go_16=2.710164785385131800\n",
  "# scl_tau (should be same as GOLDSTEIN's value) / SclTau\n",
  "ea_11=1.531013488769531300\n",
  "# atm. diff. amp. for T / AtmDiffAmpT\n",
  "ea_12=5204945.000000000000000000\n",
  "# atm. diff. amp. for q / AtmDiffAmpQ\n",
  "ea_13=1173269.250000000000000000\n",
  "# dist'n width / AtmDiffWidthT\n",
  "ea_14=1.410347938537597700\n",
  "# dist'n slope / AtmDiffSlopeT\n",
  "ea_15=0.090003050863742828\n",
  "# atm. advection factor for T_z / AtmAdvectCoeffTm\n",
  "ea_16=0.001037851092405617\n",
  "# atm. advection factor for T_m / AtmAdvectCoeffQm\n",
  "ea_17=0.0000000E+00\n",
  "# atm. advection factor for q_z / AtmAdvectCoeffQz\n",
  "ea_18=0.164652019739151000\n",
  "# atm. advection factor for q_m / AtmAdvectCoeffQz\n",
  "ea_19=0.164652019739151000\n",
  "# temp0 -- start with a warm ocean\n",
  "go_10=5.0\n",
  "# temp1 -- start with a warm ocean\n",
  "go_11=5.0\n",
  "# SclFWF -- scale for zero freshwater re-balancing\n",
  "ea_28=0.0\n",
  "# reduced sea-ice eddy diffusivity\n",
  "gs_11=1000.000\n",
  "# set a fractional sea-ce coverage threshold for preventing advection\n",
  "gs_par_sica_thresh=0.99\n",
  "# set seasonal cycle\n",
  "ea_dosc=.true.\n",
  "go_dosc=.true.\n",
  "gs_dosc=.true.\n",
  "# isoneutral isopycnal/diapycnal mixing scheme\n",
  "# it is recommended that it is turned OFF (=.false.) for 'fake' worlds\n",
  "go_diso=.true.\n",
  "# *******************************************************************\n",
  "\n",
  "# *******************************************************************\n",
  "# USER-APPENDED OPTIONS FOLLOW ...\n",
  "# *******************************************************************\n",
  "# (the following parameter text is appended automatically)\n",
  "# *******************************************************************\n"
  )

  # Write the configuration to a file
  file_path <- file.path(output_dir, paste0(file_name, ".config"))
  writeLines(config_content, file_path)

  message("Configuration file written to: ", file_path)
}
