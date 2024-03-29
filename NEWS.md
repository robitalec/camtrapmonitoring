# camtrapmonitoring 0.12.1

* added "triplet" case to `grid_design` and `grid_ct` [#22](https://github.com/robitalec/camtrapmonitoring/pull/22)



# camtrapmonitoring 0.12.0 (2023-07-27)

First release with repo status = active, non-development version numbers

* **breaking change**: renamed arguments across all functions for clarity and 
consistency [#17](https://github.com/robitalec/camtrapmonitoring/pull/17)
 [#18](https://github.com/robitalec/camtrapmonitoring/pull/18)
* added `grid_design` function [#16](https://github.com/robitalec/camtrapmonitoring/pull/16)
* improved function linking in documentation [#19](https://github.com/robitalec/camtrapmonitoring/pull/19)



# camtrapmonitoring 0.11.3.9000 (2023-07-25)

* initial pkgdown site [#15](https://github.com/robitalec/camtrapmonitoring/pull/15)



# camtrapmonitoring 0.11.2.9000 (2023-07-25)

* fixed R CMD checks [#14](https://github.com/robitalec/camtrapmonitoring/pull/14)



# camtrapmonitoring 0.11.1.9000 (2023-07-25)

* removed `select_ct` until a more useful alternative structure is developed, see PR [#12](https://github.com/robitalec/camtrapmonitoring/pull/12)
* fixed tests to match large refactoring since v0.9.0.9000 [#13](https://github.com/robitalec/camtrapmonitoring/pull/13)


# camtrapmonitoring 0.11.0.9000 (2023-07-25)

* changed name from {wildcam} to {camtrapmonitoring} [#11](https://github.com/robitalec/camtrapmonitoring/pull/11)
	- to reflect broadened scope of package from planning camera trap surveys to community based monitoring of wildlife
  - to avoid confusion with other projects using camera traps named wildcam or similar



# camtrapmonitoring 0.10.1.9000 (2023-07-13)

* edits to introductory vignette [#9](https://github.com/robitalec/camtrapmonitoring/pull/9)
* replaced leftover `raster` with `terra` [#10](https://github.com/robitalec/camtrapmonitoring/pull/10)



# camtrapmonitoring 0.10.0.9000 (2023-07-13)

* removed data.table methods from `sample_ct`, `grid_ct` and `eval_*` functions [#7](https://github.com/robitalec/camtrapmonitoring/pull/7)
	- adds dependency on `distanceto`
* replaced `raster` with `terra` [#7](https://github.com/robitalec/camtrapmonitoring/pull/7)
* delete old example data [#4](https://github.com/robitalec/camtrapmonitoring/pull/4)
* document new data [#6](https://github.com/robitalec/camtrapmonitoring/pull/6)



# camtrapmonitoring 0.9.1.9000 (2023-07-06)

* add new data for area near Clearwater Lake, Manitoba [#3](https://github.com/robitalec/camtrapmonitoring/pull/3)
	- density
	- elevation (external tif)
	- extent
	- hydro
	- land cover (external tif)
	- roads
	- wetlands
* add scripts to track data origin under data-raw/ [#3](https://github.com/robitalec/camtrapmonitoring/pull/3)
* reorganize source code [#5](https://github.com/robitalec/camtrapmonitoring/pull/5)



# camtrapmonitoring 0.9.0.9000 (2023-07-05)

* move main development from https://gitlab.com/robitalec/camtrapmonitoring to https://github.com/robitalec/camtrapmonitoring
* rough draft intro vignette [#1](https://github.com/robitalec/camtrapmonitoring/pull/1)



# camtrapmonitoring 0.8.2.9000 (2019-06-10)

* changed `select_ct`'s argument 'sub' to accept an expression, directly passed to `data.table`'s i without a join step. 



# camtrapmonitoring 0.8.1.9000 (2019-06-08)

* improved `select_ct` checks and tests



# camtrapmonitoring 0.8.0.9000 (2019-06-01) 

* added function `binary_layer` for making binary raster layers with equi and non-equi functions (e.g. =, >, <=, %in%) [!11](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/11).
* rename `scale_roi` to `scale_layer` [#22](https://gitlab.com/robitalec/camtrapmonitoring/issues/22). 
* rename `strat_sample` to `sample_ct` [#21](https://gitlab.com/robitalec/camtrapmonitoring/issues/21) and [!17](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/17). 



# camtrapmonitoring 0.7.0.9000 (2019-05-30) 

* rename `dist_to` to `eval_dist` and add attributes like other `eval_*` [#19](https://gitlab.com/robitalec/camtrapmonitoring/issues/19) and [!12](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/12). 
* add basic `data.table` data (DT)
* fix preprocessing and passing to methods with `function_` for `eval_pt`, `eval_dist` (skeleton), `eval_buffer` and `grid_ct`  [!15](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/15) 
* rename `make_grid` to `grid_ct` [!14](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/14) 
* return focal identifier with `make_grid`. 
* fix 'camID' returned by sf method incorrectly. 
* fix lack of error when attributes are null [#20](https://gitlab.com/robitalec/camtrapmonitoring/issues/20). 
* return 'camID' with `make_grid` [!10](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/10). 



# camtrapmonitoring 0.6.0.9000 (2019-02-26) 

* Add `select_ct` for selecting camera trap locations from 'x' [!9](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/9). 
* Add 'n' argument to `make_grid` to specify a grid size *instead of* 'case' [!8](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/8). 
* Add 'type' argument to `strat_sample` for regular and random sampling. [!6](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/6). 



# camtrapmonitoring 0.5.0.9000 (2019-02-21) 

* Add `scale_roi` for rescaling rasters in a region of interest  [!5](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/5). 
* Add "dem" example data. 



# camtrapmonitoring 0.4.0.9000 (2019-02-21) 

* Add `eval_pt` and `eval_buffer` for evaluating and characterizing camera trap locations [!4](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/4). 
* Add "wetland" and "lc" example data. 



# camtrapmonitoring 0.3.0.9000 (2019-02-15) 

* Add `make_grid` for making camera trap grids around focal cameras [!3](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/3). 



# camtrapmonitoring 0.2.0.9000 (2019-02-12) 

* Add `dist_to` for distance to calculations [!2](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/2). 
* Add "water" example data. 



# camtrapmonitoring 0.1.0.9000 (2019-02-11) 

* Add `strat_sample` for stratified polygon sampling [!1](https://gitlab.com/robitalec/camtrapmonitoring/merge_requests/1). 
* Add "points" and "densitygrid" example data. 

