# Reproducibility preparation status

Current gate: **FULL 8000-DATASET REPRODUCTION READY**

Completed:
- [x] Real-data reproduction PASS.
- [x] Exact Stage42 manifest PASS.
- [x] Exact Stage46 production seed map PASS.
- [x] 16-dataset production-seed smoke PASS.
- [x] Serial versus 4-worker scientific equivalence PASS.
- [x] Full 8000-dataset safe reproduction wrapper constructed.

Next:
- [ ] Run `source("run_B5G2_full_8000_reproduction.R")`.
- [ ] Require 8000/8000 dataset PASS.
- [ ] Require 40000/40000 method-result rows.
- [ ] Require all 40 cells successful/converged 1000/1000.
- [ ] Require exact Stage46 seed-map match.
- [ ] Require frozen Stage48 performance/weight/seed summaries to match.
- [ ] Complete Stage49 execution-time isolation test.
- [ ] Complete final V01–V20 release audit.

Public GitHub/Zenodo release remains blocked until these gates pass.
