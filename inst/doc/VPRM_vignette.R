## ---- fig.show='hold'----------------------------------------------------
library(VPRMLandSfcModel)
data(Park_Falls)
## determine the phenology phase for each tower observation date
phen_filled <- interp_phenology(PFa_phen, PFa_tower_obs[['date']])
## place the tower observations and MODIS data into a VPRM_driver_data object.
pfa_dd <- VPRM_driver_data(name_long="Park Falls",
                           name_short = "US-PFa",
                           lat=45.9459,
                           lon=-90.2723,
                           PFT='MF',    ## mixed forest
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
                           phen=phen_filled)
## take a look at the result
print(head(as.data.frame(pfa_dd)))

## ---- fig.show='hold'----------------------------------------------------
library(ggplot2)
library(chron)
##
fig_T <- (ggplot(pfa_dd[['data']],
              aes(date, T)) +
        geom_line() +
        scale_x_chron(format="%d %b %Y") +
        labs(title = "US-PFa", y=expression(air~T~(degree*C))) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) +    # center the plot title
		theme(axis.text=element_text(size=14),
			axis.title=element_text(size=14,face="bold")))
print(fig_T)

## ---- fig.show='hold'----------------------------------------------------
data(VPRM_parameters)
attach(all_all_VPRM_parameters)
pfa_dd[['data']][['VPRM_NEE']] <- vprm_calc_NEE(
    pfa_dd, lambda=lambda, PAR_0=PAR_0, alpha=alpha, beta=beta)

fig_NEE_VPRM <- (ggplot(pfa_dd[['data']],
	aes(date, VPRM_NEE)) +
	geom_point() +
	scale_x_chron(format="%d %b %Y") +
	labs(title = "US-PFa", x='date', y=expression(VPRM~NEE~(mu*mol~m^{-2}~s^{-1}))) +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5)) +    # center the plot title
	theme(axis.text=element_text(size=14),
		axis.title=element_text(size=14,face="bold")))
print(fig_NEE_VPRM)

## now plot the eddy covariance-observed NEE
fig_NEE_EC <- (ggplot(pfa_dd[['data']],
	aes(date, NEE_obs)) +
	geom_point() +
	scale_x_chron(format="%d %b %Y") +
	labs(title = "US-PFa", x='date',
		y=expression(eddy~covariance~NEE~(mu*mol~m^{-2}~s^{-1}))) +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5)) +    # center the plot title
	theme(axis.text=element_text(size=14),
		axis.title=element_text(size=14,face="bold")))
print(fig_NEE_EC)

## now plot the difference between covariance-observed NEE and VPRM NEE
fig_dNEE <- (ggplot(pfa_dd[['data']],
	aes(date, NEE_obs-VPRM_NEE)) +
	geom_point() +
	scale_x_chron(format="%d %b %Y") +
	labs(title = "US-PFa", x='date',
		y=expression(Delta*NEE[obs-VPRM]~(mu*mol~m^{-2}~s^{-1}))) +
	theme_classic() +
	theme(plot.title = element_text(hjust = 0.5)) +    # center the plot title
	theme(axis.text=element_text(size=14),
		axis.title=element_text(size=14,face="bold")))
print(fig_dNEE)

## ---- fig.show='hold'----------------------------------------------------
library(VPRMLandSfcModel)
library(DEoptim)

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

## estimate parameter values for all data
par_est_status <- estimate_VPRM_pars(all_data=pfa_dd[['data']],
                                     DE_itermax = 2,
                                     par_set_str='ExampleRun')

## estimate parameter values for monthly windows
par_est_status <-
    estimate_VPRM_pars(all_data=pfa_dd[['data']],
                       DE_itermax = 2,
                       par_set_str='ExampleRun_Monthly',
                       opt_groups=months(pfa_dd[['data']][['date']]))

## ---- fig.show='hold'----------------------------------------------------
attach('ParEst_ExampleRun_Monthly.de.RData')
ls(2)
print(Apr[['optim']][['bestmem']])

