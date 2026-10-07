# Deployment

## GitHub is source hosting

Updating this repository does not update the live Shiny app. GitHub Pages cannot execute the app's R server. Netlify can host a separate static project page; moving this complete dashboard there requires a different backend/frontend architecture.

## Shinyapps.io

1. Restore dependencies in a staging checkout and verify the app locally.
2. Configure your own provider credentials privately. Never put `.env` in `www` or Git.
3. Review provider redistribution terms, analytics disclosures and music distribution rights.
4. Keep a rollback bundle, then deploy with `rsconnect` to your own account.

Use an explicit deployment file list. A private `.env` may need inclusion in your server deployment bundle, outside `www`, if that is your chosen credential mechanism. That is different from publishing it to GitHub. Deployment configuration records, logs and runtime caches must not become public assets.

After deployment, check map selection/deselection, comparison plots, conversion, exports, mobile settings, fullscreen and Privacy settings. Test both themes. Run Lighthouse on the clean app address and interpret timeout warnings; an offline syntax check is not a deployed functional test.

For continuous history collection, use durable storage and a scheduled collector. Instance-local Shinyapps.io storage is not a durable shared archive.
