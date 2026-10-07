# Changes

## Remastered source update — 8 October 2026

- Replaced the original dashboard entry point with a modular `app.R` application; previous repository files are preserved by the update procedure.
- Added linked country/currency exploration, currency comparison, amount conversion and detailed data views.
- Added optional 3D plots, fullscreen controls, CSV/PNG exports, saved views and share links.
- Refined responsive settings, themes, typography, chart tooltips, map interactions and form labeling.
- Added local branding assets and self-hosted fonts.
- Hardened shared request backoff, history queue bounds and date validation. Owner-run focused mocked regressions reported 26 passes; this is not a full security audit.
- Fixed hash navigation and share URLs resolving against worker-specific Shiny base paths. Duplicate chart-library downloads disappeared in the subsequent supplied report.
- Reverted deferred chart/map slots after they caused a blank map and overlapping layout. Direct widget loading is retained; DataTables and optional 3D remain on-demand.
- Documented automatic public analytics-tag loading, preserved opt-outs and denied storage consent without explicit allowance.
- Completed dependency-lock entries for R recommended packages needed during deployment.

Remaining work: performance optimization with live interaction verification, complete security/accessibility review, durable archival storage, and an optional independent project page. No Netlify migration is included.
