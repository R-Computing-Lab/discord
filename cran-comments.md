
# Description

This is a small fix to ensure compatibility with the transfer of plotPedigree from BGmisc to ggpedigree.

## Pretest Notes
Trattner is not misspelled in the DESCRIPTION file. It is the surname of one of the authors.
As requested, removed duplicate reference to GPL3 license, and added license to rbuild ignore file.
 
# Test Environments

1. Local OS: Windows 11 x64 (build 26120), R 4.5.0 (2025-02-28 ucrt)
4. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/discord/actions/runs/15563627886)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.

        
## R CMD check results

==> devtools::check()

── R CMD check results ─────────── discord 1.2.4.1 ────
Duration: 46.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
