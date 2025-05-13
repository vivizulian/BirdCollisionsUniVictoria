# Seasonally frugivorous forest birds and window collision fatalities: novel integration of bird counts improves assessment of species vulnerability to collisions 


## Associated publications
Viviane Zulian, Louise K. Blight, Jon Osborne, Adam C. Smith, Andrea R. Norris, Rebecca Golat, Krista L. De Groot. Seasonally frugivorous forest birds and window collision fatalities: novel integration of bird counts improves assessment of species vulnerability to collisions. *In Review*


## Workflow

The workflow for this repository involves data read in as raw data at Level 0 (L0). Level 1 (L1) data represent cleaned L0 data. Level 2 (L2) data are data merged or otherwise derived from two or more L1 data, etc. Data must be organized according to the below Repository structure (i.e., in `Data/`). Scripts are designed to be run in sequence from `1-XXXX.R` -> `2-XXXX.R` -> ... The directory where the data are located (i.e., the parent directory of `Data/`) as well as the run date for each data product need to be specified at the top of associated scripts (in the `set dirs` section) before running.


## Repository structure

* `Scripts/`
  * `1-process-data/`
    * `1a-process-Uvic-point-counts.R` - organize point counts
    * `1b-process-banding-counts.R` - organize banding data
    * `1c-process-collision-data.R` - process collisions dara
    * `1d-process-SE-CP-trial-data.R` - process carcass persistence and searcher efficiency trials
    * `1e-join-datasets-fall.R` - subsetting the data for the fall
    * `1f-join-datasets-winter` - subsetting the data for the winter
  * `2-data-analysis/`
    * `2a-simulation-study.R` - simulation to check the model works
    * `2b-fall-abundance-model.R` - integrated model to estimate abundance and vulnerability for fall
    * `2c-winter-abundance-model.R` - model to estimate abundance and vulnerability for winter
    * `2d-estimate-vulnerability.R` - process results and extract vulnerability
    * `2e-estimating-SE-CP-mortality.R` - use GenEst package to estimate mortality corrected by CP and SE.
    * `2f-extrapolations-season-campus.R` - extrapolate mortality for the whole campus and entire seasons.
  * `3-results-summary-plots/`
    *`3a-results-vulnerability.R` - summarizing and plotting vulnerability results
* `Data/`
  * `L0/`
    * `buildings_data/`
      * `2024 Buildings for campus-wide estimate.csv` - list of buildings on campus for extrapolations.
    * `carcass_persistence_data/`
      * `CarcassPersistenceData.csv` - 
    * `collisions_data/`
      * `collisionsData_Fall.csv` -
      * `collisionsData_Winter.csv` -
    * `count_data/`
      * `BandingData.csv` - banding data processed from L0
      * `FlockingData.csv` - flocking/non-flocking data
      * `RockyPointEffortData.csv` - banding effort data
      * `RPBO_BandingData_2019-2022.csv` - 
      * `UVic_PointCountData.csv` - processed point counts from UVic
    * `searcher_efficiency_data/`
      * `SearcherEfficiencyData.csv` - processed data from L0
      * `SearcherScheduleData.csv` - processed data from L0
  * `Results/` (ignored)

     
## Contact Information

Viviane Zulian - zulian.vi@gmail.com
