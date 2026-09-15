# B-5G-1 exact Stage42 manifest ingestion

The exact full-precision `stage42_frozen_var_manifest.csv` was ingested and hash-locked.

SHA-256:
`d6248fa4b1572c638d98fef6c05d9fc17a8fdce8dceabf14f7a07afba4ed9a14`

Full-precision total rates:

| Scenario | total_rate |
|---|---:|
| VD_B_N | 10.9740 |
| VD_B_A | 12.0714 |
| VD_SH_N | 10.8900 |
| VD_SH_A | 12.1968 |
| VS_B_N | 10.6200 |
| VS_B_A | 11.8944 |
| VS_SH_N | 10.6920 |
| VS_SH_A | 11.7612 |

These values replace the earlier rounded repository scenario-rate record.
The new 16-dataset smoke runner uses this exact manifest and the first two
final-production seeds per scenario from the Stage46 8000-seed manifest.
