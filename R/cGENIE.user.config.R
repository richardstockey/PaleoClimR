cGENIE.user.config <- function(output_dir, file_name, template = "PALEO.BIOGEM.PO4",
  bio_new_prod_scheme = if (template == "PALEO.BIOGEM.PO4") "bio_P" else NULL,
  temp_dependent_remin = TRUE, age,
  val_3 = 278.0E-06, val_4 = -6.5, val_6 = 0.2095,
  ac_atm_init_3 = NULL, ac_atm_init_4 = NULL, ac_atm_init_6 = NULL,
  bg_ocn_init_3 = NULL, bg_ocn_init_4 = NULL, bg_ocn_init_8 = NULL,
  bg_ocn_init_10 = NULL, bg_ocn_init_12 = NULL, bg_ocn_init_35 = NULL,
  bg_ocn_init_38 = NULL, bg_ocn_init_50 = NULL,
  bg_par_data_save_level = 7) {

  # Validate inputs
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist.")
  }

  if (!template %in% c("PALEO.BIOGEM.PO4", "ABIOTIC.TRACER")) {
    stop("Invalid template specified.")
  }

  if (template == "PALEO.BIOGEM.PO4" && !bio_new_prod_scheme %in% c("bio_P", "paleo_config", "crichton_2020")) {
    stop("Invalid biological new production scheme.")
  }

  if (missing(age)) {
    stop("Age must be specified.")
  }

  # Determine bg_par_bio_red_POC_CaCO3 based on age
  bg_par_bio_red_POC_CaCO3 <- ifelse(age > 200, "0.0", "0.200")

  # Create the configuration content
  config_content <- if (template == "PALEO.BIOGEM.PO4") {
    paste0(
      "# *******************************************************************\n",
      "# *** USERCONFIG.PALEO.BIOGEM.PO4.SPIN ******************************\n",
      "# *******************************************************************\n",
      "#\n",
      "# *** CLIMATE *******************************************************\n",
      "#\n",
      "# set climate feedback (climate responding to changing pCO2)\n",
      "ea_36=y\n",
      "#\n",
      "# *** BIOLOGICAL NEW PRODUCTION *************************************\n",
      "#\n",
      if (bio_new_prod_scheme == "bio_P") {
   paste0(
     "# *** recommended ***************************************************\n",
     "# biological scheme ID string\n",
     'bg_par_bio_prodopt="bio_P"\n',
     "# biological uptake time-scale\n",
     "bg_par_bio_tau=63.3827\n",
     "# [PO4] M-M half-sat value (mol kg-1)\n",
     "bg_par_bio_c0_PO4=0.10E-06\n"
   )
      } else if (bio_new_prod_scheme == "paleo_config") {
   paste0(
     "# *** published paleo configuration *********************************\n",
     'bg_par_bio_prodopt="1N1T_PO4MM"\n',
     "bg_par_bio_k0_PO4=8.9876e-006\n",
     "bg_par_bio_c0_PO4=8.9369e-007\n"
   )
      } else if (bio_new_prod_scheme == "crichton_2020") {
   paste0(
     "# *** Crichton et al. [2020] calibration ****************************\n",
     'bg_par_bio_prodopt="1N1T_PO4MM_Tdep"\n',
     "bg_par_bio_mu1=10\n",
     "bg_par_bio_c0_PO4=8.9369e-007\n"
   )
      } else {
   ""
      },
      "#\n",
      "# *** ORGANIC MATTER EXPORT RATIOS **********************************\n",
      "#\n",
      "# production fraction of dissolved organic matter\n",
      "bg_par_bio_red_DOMfrac=0.66\n",
      "#\n",
      "# *** INORGANIC MATTER EXPORT RATIOS ********************************\n",
      "#\n",
      "# fixed CaCO3:POC\n",
      "bg_opt_bio_CaCO3toPOCrainratio='FIXED'\n",
      "# underlying export CaCO3 as a proportion of particulate organic matter (i.e., CaCO3/POC)\n",
      'bg_par_bio_red_POC_CaCO3=', bg_par_bio_red_POC_CaCO3, '\n',
      "#\n",
      "# *** REMINERALIZATION **********************************************\n",
      "#\n",
      "# *** recommended ***************************************************\n",
      "# set 'instantaneous' water column remineralziation\n",
      "bg_par_bio_remin_sinkingrate_physical=9.9E9\n",
      "bg_par_bio_remin_sinkingrate_reaction=125.0\n",
      "# *** common settings ***********************************************\n",
      "# DOC lifetime (yrs)\n",
      "bg_par_bio_remin_DOMlifetime=0.5\n",
      "# initial fractional abundance of POC component #2\n",
      "bg_par_bio_remin_POC_frac2=0.0557\n",
      "# depth of remineralization or particulate organic matter\n",
      "bg_par_bio_remin_POC_eL1=589.9451\n",
      "# remineralization length #2 for POC\n",
      "bg_par_bio_remin_POC_eL2=1000000.0\n",
      "# initial fractional abundance of CaCO3 component #2\n",
      "bg_par_bio_remin_CaCO3_frac2=0.45\n",
      "# depth of remineralization or CaCO3\n",
      "bg_par_bio_remin_CaCO3_eL1=1.8905e+003\n",
      "# remineralization length #2 for CaCO3\n",
      "bg_par_bio_remin_CaCO3_eL2=1000000.0\n",
      if (temp_dependent_remin) {
   paste0(
     "# *** Crichton et al. [2020] temperature-dependent remin ************\n",
     "bg_ctrl_bio_remin_POC_fixed=.false.\n",
     "bg_par_bio_remin_POC_K1=9.0E11\n",
     "bg_par_bio_remin_POC_Ea1=54000.0\n",
     "bg_par_bio_remin_POC_K2=1.0E14\n",
     "bg_par_bio_remin_POC_Ea2=80000.0\n",
     "bg_par_bio_remin_POC_frac2=0.008\n"
   )
      } else {
   ""
      },
      "#\n"
    )
  } else if (template == "ABIOTIC.TRACER") {
    paste0(
      "# *******************************************************************\n",
      "# *** USERCONFIG.ABIOTIC.TRACER.SPIN ********************************\n",
      "# *******************************************************************\n",
      "#\n",
      "# *** CLIMATE *******************************************************\n",
      "#\n",
      "# set no climate feedback (climate responding to changing pCO2)\n",
      "ea_36=n\n",
      "# scaling for atmospheric CO2 radiative forcing, calculated as the multiplier of 278 ppm CO2\n",
      "ea_radfor_scl_co2=1.0\n",
      "#\n",
      "# *** BIOLOGICAL NEW PRODUCTION *************************************\n",
      "#\n",
      "# Set no biological production\n",
      "bg_par_bio_prodopt='NONE'\n",
      "#\n"
    )
  }

  # Add tracer initial values if provided
  tracer_section <- paste0(
    "# *******************************************************************\n",
    "# *** TRACERS *******************************************************\n",
    "# *******************************************************************\n"
  )

  if (!is.null(ac_atm_init_3)) {
    tracer_section <- paste0(tracer_section, "ac_atm_init_3=", ac_atm_init_3, "\n")
  }
  if (!is.null(ac_atm_init_4)) {
    tracer_section <- paste0(tracer_section, "ac_atm_init_4=", ac_atm_init_4, "\n")
  }
  if (!is.null(ac_atm_init_6)) {
    tracer_section <- paste0(tracer_section, "ac_atm_init_6=", ac_atm_init_6, "\n")
  }
  if (!is.null(bg_ocn_init_3)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_3=", bg_ocn_init_3, "\n")
  }
  if (!is.null(bg_ocn_init_4)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_4=", bg_ocn_init_4, "\n")
  }
  if (!is.null(bg_ocn_init_8)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_8=", bg_ocn_init_8, "\n")
  }
  if (!is.null(bg_ocn_init_10)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_10=", bg_ocn_init_10, "\n")
  }
  if (!is.null(bg_ocn_init_12)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_12=", bg_ocn_init_12, "\n")
  }
  if (!is.null(bg_ocn_init_35)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_35=", bg_ocn_init_35, "\n")
  }
  if (!is.null(bg_ocn_init_38)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_38=", bg_ocn_init_38, "\n")
  }
  if (!is.null(bg_ocn_init_50)) {
    tracer_section <- paste0(tracer_section, "bg_ocn_init_50=", bg_ocn_init_50, "\n")
  }

  # Append tracer section to config content
  config_content <- paste0(config_content, tracer_section)

  # Append data saving section
  config_content <- paste0(config_content,
    "#\n",
    "# *** DATA SAVING ***************************************************\n",
    "#\n",
    "# BASIC + biology + tracer + proxy diagnostics\n",
    "bg_par_data_save_level=", bg_par_data_save_level, "\n",
    "# *** recommended ***************************************************\n",
    "# force time-slice save at run end only\n",
    "bg_par_infile_slice_name='save_timeslice_NONE.dat'\n",
    "#\n",
    "# *** FORCINGS ******************************************************\n",
    "#\n",
    "# specify forcings -- restoring of atmospheric pCO2, d13C, pO2\n",
    'bg_par_forcing_name="pyyyyz.RpCO2_Rp13CO2.RpO2"\n',
    "bg_par_atm_force_scale_val_3=", val_3, "\n",
    "bg_par_atm_force_scale_val_4=", val_4, "\n",
    "bg_par_atm_force_scale_val_6=", val_6, "\n",
    "#\n",
    "# *** MISC **********************************************************\n",
    "#\n",
    "# *** recommended ***************************************************\n",
    "# maximum time-scale to geochemical reaction completion (days)\n",
    "bg_par_bio_geochem_tau=45.0\n",
    "#\n",
    "# *******************************************************************\n",
    "# *** END ***********************************************************\n",
    "# *******************************************************************\n"
  )

  # Write the configuration to a file
  file_path <- file.path(output_dir, paste0(file_name, ".config"))
  writeLines(config_content, file_path)

  message("Configuration file written to: ", file_path)
}
