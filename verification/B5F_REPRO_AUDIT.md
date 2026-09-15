# B-5F local Stage56 reproduction audit

## Decision
**PASS**

The frozen Stage56 v1.2 primary real-data application was rerun locally from:

- the frozen Stage55 RDS;
- the exact `200319_DMO_report1_anonymised.csv` input;
- the authoritative frozen Stage56 implementation.

The local run reported:

- Stage55 RDS present: TRUE
- DMO CSV present: TRUE
- primary horizon: 365 days
- methods: M8–M11 only
- M12 real-data fit: NO
- eligible patients: 1962
- eligible eyes: 2612
- post-baseline visits: 18046
- bilateral patients: 650
- all visit models converged
- same-visit leakage absent
- history-rule audit passed
- all M8–M11 outcome models converged
- all 29 Stage56 final-audit checks passed

Reproduced primary day-365 changes:

| Method | Estimate | SE | 95% CI |
|---|---:|---:|---:|
| M8 | 5.012385 | 0.3814003 | 4.264840 to 5.759929 |
| M9 | 5.107116 | 0.3785456 | 4.365166 to 5.849065 |
| M10 | 5.145215 | 0.3810938 | 4.398271 to 5.892159 |
| M11 | 5.136150 | 0.3802364 | 4.390887 to 5.881414 |

The run also regenerated:

- Table 3;
- Figure 3 (PDF and PNG);
- Supplementary Table S5A;
- Supplementary Table S5B;
- Supplementary Table S6.

## Reproduction environment

- R 4.6.1 (2026-06-24 ucrt)
- Windows 11 x64
- geepack 1.3.13

This demonstrates successful reproduction of the frozen real-data application under the current release-machine environment.

The manuscript's original-analysis software statement must remain conceptually separate from the repository reproduction environment. Final release documentation should report both accurately rather than silently replacing one with the other.
