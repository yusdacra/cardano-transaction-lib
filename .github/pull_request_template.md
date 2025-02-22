Closes # .

### Pre-review checklist

- [ ] All code has been formatted using our config (`make format` for Purescript, `nixpkgs-fmt` for Nix)
- [ ] All Purescript imports are explicit, including constructors
- [ ] **All** existing examples have been run locally against a fully-synced testnet node
- [ ] The integration and unit tests have been run (`npm run test`) locally
- [ ] The changelog has been updated under the `## Unreleased` header, using the appropriate sub-headings (`### Added`, `### Removed`, `### Fixed`)
