import pandas as pd

drivers_raw = pd.read_csv('drivers.csv')
results_raw = pd.read_csv('results.csv')
races_raw_df = pd.read_csv('races.csv')
driver_st_raw = pd.read_csv('driver_standings.csv')
constructors_raw=pd.read_csv('constructor_standings.csv')

def podium_counter(pos):
    if pos in ['1', '2', '3']:
        return True
    else:
        return False

def win_counter(pos):
    if pos == '1':
        return True
    else:
        return False

def pole_counter(grid):
    if grid == 1:
        return True
    else:
        return False

results_copy = results_raw[['raceId', 'driverId','constructorId', 'grid', 'position', 'points', 'laps', 'rank']].copy().rename(
    columns = {'rank':'fastlap'})

# Adding podium, win, pole and fastest lap columns
results_copy['podium'] = results_copy.position.apply(podium_counter)
results_copy['win'] = results_copy.position.apply(win_counter)
results_copy['pole'] = results_copy.grid.apply(pole_counter)
results_copy['fastestLap'] = results_copy.fastlap.apply(win_counter)

# Gouping with respect to drivers
driver_stats_1 = results_copy.drop(columns = ['position', 'grid', 'fastlap']).groupby('driverId')
func_dic = {'raceId':'count', 'points':'sum', 'laps':'sum', 'podium':'sum', 'win':'sum', 'pole':'sum', 'fastestLap':'sum'}
driver_stats_1 = driver_stats_1.aggregate(func_dic).reset_index().rename(columns = {'raceId':'races'})

# Gouping with respect to constuctors
constr_stats_1 = results_copy.drop(columns = ['position', 'grid', 'fastlap']).groupby('constructorId')
constr_stats_1 = constr_stats_1.aggregate(func_dic).reset_index().rename(columns = {'raceId':'races'})

races_raw_df = races_raw_df.sort_values('date')
races_df = races_raw_df[races_raw_df.date <= '2021-08-08']
races_completed = len(races_df)
idx = races_df.groupby(['year'])['date'].transform(max) == races_df['date'] # Finds if the race index is last of the season
season_finale = races_df[idx].rename(columns = {'round' : 'tot_races'})
#season_finale = season_finale[season_finale.year != 2021] # excluding the 2021 season

# Getting driver championship data
driver_st_raw = pd.read_csv('driver_standings.csv')
driver_st_season_end = season_finale[['raceId', 'year', 'tot_races']].merge(driver_st_raw, on = 'raceId')
driver_champ = driver_st_season_end[driver_st_season_end['position'] == 1]
driver_champ_tot = driver_champ[['driverId', 'position']].groupby('driverId').sum().reset_index().merge(
    drivers_raw[['forename', 'surname', 'driverId']]).rename(
    columns={'position':'titles'}).sort_values('titles', ascending = False)

# Merging everthing to a single dataframe
driver_stats = driver_stats_1.merge(driver_champ_tot[['driverId', 'titles']], how = 'left').fillna(0)
driver_stats = drivers_raw[['driverId', 'forename', 'surname', 'nationality']].merge(driver_stats, on = 'driverId')

# Merging everthing to a single dataframe
driver_stats = driver_stats_1.merge(driver_champ_tot[['driverId', 'titles']], how = 'left').fillna(0)
driver_stats = drivers_raw[['driverId', 'forename', 'surname', 'nationality']].merge(driver_stats, on = 'driverId')
driver_stats['driver name']= driver_stats['forename']+ ' '+ driver_stats['surname']
driver_stats= driver_stats[['driverId','driver name','nationality', 'races', 'points',
       'laps', 'podium', 'win', 'pole', 'fastestLap', 'titles']]
driver_stats.sort_values(by=['titles','win','podium'], inplace=True, ascending=False)

#constructors
constructor_st_season_end = season_finale[['raceId', 'year', 'tot_races']].merge(constructors_raw, on = 'raceId')
constructors_name_raw=pd.read_csv('constructors.csv')
constructor_champ = constructor_st_season_end[constructor_st_season_end['position'] == 1]
constructor_champ_tot = constructor_champ[['constructorId', 'position','points','wins']].groupby('constructorId').sum().reset_index().merge(constructors_name_raw[['name', 'nationality','constructorId']], on ='constructorId').rename(
    columns={'position':'titles'}).sort_values('titles', ascending = False)

driver_stats.to_csv('championship.csv')
constructor_champ_tot.to_csv('constructor_championship.csv')
