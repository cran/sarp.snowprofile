## -----------------------------------------------------------------------------
library(sarp.snowprofile)

## -----------------------------------------------------------------------------
## Print the structure of a single `snowprofile` object
Profile <- SPpairs$C_day1
str(Profile)

## -----------------------------------------------------------------------------
snowprofile(dropNAs = FALSE)

## -----------------------------------------------------------------------------
## Import a CAAML file
# Filename <- "path/to/file.caaml"
# Profile <- snowprofileCaaml(Filename)

# ## Import all profiles from a directory of CAAML files and create a `snowprofileSet`
# CaamlFiles <- list.files('path/to/caamlprofiles', full.names = T)
# Profiles <- lapply(CaamlFiles, snowprofileCaaml)
# Profiles <- snowprofileSet(Profiles)

## -----------------------------------------------------------------------------
Profile <- SPpairs$A_manual
print(Profile)

## -----------------------------------------------------------------------------
summary(Profile)
summary(SPgroup)

## -----------------------------------------------------------------------------
## Subset all profiles from SPgroup with elevation > 2000 m
Metadata <- summary(SPgroup)
Alpine <- SPgroup[Metadata$elev > 2000]
print(paste(length(Alpine), 'of', length(SPgroup), 'profiles in SPgroup are above 2000 m'))

## -----------------------------------------------------------------------------
## Rbind SPgroup 
TabularProfile <- rbind(SPgroup)
names(TabularProfile)

## Tabulate all grain types
table(TabularProfile$gtype)

## Get elevations of profiles with SH layers > 5 mm
SH_layers <- subset(TabularProfile, gtype == 'SH' & gsize > 5)
sort(SH_layers$elev)

## -----------------------------------------------------------------------------
## Plot a single hardness profile
plot(Profile)
## Plot a timeline
plot(SPtimeline)
## Plot all profiles in the group and sort by height
plot(SPgroup, SortMethod = 'hs')

