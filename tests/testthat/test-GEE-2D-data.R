test_that('vprm_calc_GEE works for two-dimensional (i.e. spatial) driver data', {

    ## get some driver data for VPRM
    data(Park_Falls)
    pfa_dd <- VPRM_driver_data(name_long="Park Falls",
                               name_short = "US-PFa",
                               lat=45.9459,
                               lon=-90.2723,
                               PFT='MF',
                               tower_date=PFa_tower_obs[['date']],
                               NEE_obs=PFa_tower_obs[['FC']],
                               T=PFa_tower_obs[['TA']],
                               PAR=PFa_tower_obs[['PAR']],
                               date_nir = PFa_refl[['date']],
                               rho_nir=PFa_refl[['nir']],
                               date_swir = PFa_refl[['date']],
                               rho_swir = PFa_refl[['swir']],
                               date_EVI = PFa_evi[['date']],
                               EVI=PFa_evi[['evi']],
                               phen=NA)
    ## need some parameters -- the choice is arbitrary, really
    load(file.path(system.file("data",
                               package="VPRMLandSfcModel"),
                   'VPRM_parameters.Rdata'))
    n_obs <- dim(pfa_dd[['data']])[1]
    pfa_dd[['T']] <- array(runif(n_obs * 2 * 2), dim=c(n_obs, 2, 2))
    stop('Does this really work with 2-D temperature data?') p
    NEE <- vprm_calc_NEE(pfa_dd,
                         lambda_param=all_all_VPRM_parameters[['lambda']],
                         PAR_0_param=all_all_VPRM_parameters[['PAR_0']],
                         alpha_param=all_all_VPRM_parameters[['alpha']],
                         beta_param=all_all_VPRM_parameters[['beta']])
    expect_equal(length(NEE), n_obs)

})
