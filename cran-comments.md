
# Description

This update introduces several enhancements to categorical variable handling as well as refining two existing vignettes as well as adding a new one. The new vignette demonstrates the use of the potter dataset from the BGmisc package to create and use other kinship links. 

## Pretest Notes

As requested, removed duplicate reference to GPL3 license, and added license to rbuild ignore file.
 
# Test Environments

1. Local OS: Windows 11 x64 (build 22635), R 4.4.3 (2025-02-28 ucrt)
2. Local OS: Windows 11 x64 (build 26120), R Under development (unstable) (2025-02-17 r87739 ucrt)
3. Local OS: Windows 11 x64 (build 26120), R Under development (unstable) (2025-04-04 r88112 ucrt)
4. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/discord/actions/runs/14284114997)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.
        
## R CMD check results

==> devtools::check()

══ Documenting ════════════════════════════════════════════════════════════════
ℹ Updating discord documentation
ℹ Loading discord

══ Building ═══════════════════════════════════════════════════════════════════
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
── R CMD build ────────────────────────────────────────────────────────────────
✔  checking for file 'E:\Dropbox\Lab\Research\Projects\2023\discord/DESCRIPTION' ...
─  preparing 'discord': (1.6s)
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✔  creating vignettes (24.3s)
─  checking for LF line-endings in source and make files and shell scripts (1.4s)
─  checking for empty or unneeded directories
─  building 'discord_1.2.3.tar.gz'
   
══ Checking ═══════════════════════════════════════════════════════════════════
Setting env vars:
• _R_CHECK_CRAN_INCOMING_REMOTE_               : FALSE
• _R_CHECK_CRAN_INCOMING_                      : FALSE
• _R_CHECK_FORCE_SUGGESTS_                     : FALSE
• _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
• NOT_CRAN                                     : true
── R CMD check ────────────────────────────────────────────────────────────────
─  using log directory 'E:/Dropbox/Lab/Research/Projects/2023/discord.Rcheck'
─  using R Under development (unstable) (2025-04-04 r88112 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 14.2.0
       GNU Fortran (GCC) 14.2.0
─  running under: Windows 11 x64 (build 26120)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran'
✔  checking for file 'discord/DESCRIPTION'
─  checking extension type ... Package
─  this is package 'discord' version '1.2.3'
─  package encoding: UTF-8
✔  checking package namespace information
✔  checking package dependencies ...
✔  checking if this is a source package
✔  checking if there is a namespace
✔  checking for executable files (543ms)
✔  checking for hidden files and directories ...
✔  checking for portable file names
✔  checking whether package 'discord' can be installed (1.9s)
✔  checking installed package size ... 
✔  checking package directory
✔  checking for future file timestamps (17s)
✔  checking 'build' directory ...
✔  checking DESCRIPTION meta-information ... 
✔  checking top-level files
✔  checking for left-over files
✔  checking index information ...
✔  checking package subdirectories (554ms)
✔  checking code files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded ... 
✔  checking whether the package can be loaded with stated dependencies ...
✔  checking whether the package can be unloaded cleanly ... 
✔  checking whether the namespace can be loaded with stated dependencies ...
✔  checking whether the namespace can be unloaded cleanly ... 
✔  checking loading without being on the library search path ... 
✔  checking dependencies in R code (513ms)
✔  checking S3 generic/method consistency ... 
✔  checking replacement functions ...
✔  checking foreign function calls ... 
✔  checking R code for possible problems (3.1s)
✔  checking Rd files ... 
✔  checking Rd metadata ... 
✔  checking Rd line widths ... 
✔  checking Rd cross-references ... 
✔  checking for missing documentation entries ... 
✔  checking for code/documentation mismatches (413ms)
✔  checking Rd \usage sections (410ms)
✔  checking Rd contents ...
✔  checking for unstated dependencies in examples ...
✔  checking contents of 'data' directory ...
✔  checking data for non-ASCII characters ... 
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ...
✔  checking R/sysdata.rda ... 
✔  checking installed files from 'inst/doc' ... 
✔  checking files in 'vignettes' ... 
✔  checking examples (3.4s)
✔  checking for unstated dependencies in 'tests'
─  checking tests ...
    [14s] OKtestthat.R'
   * checking for unstated dependencies in vignettes ... OK
   * checking package vignettes ... OK
   * checking re-building of vignette outputs ... [25s] OK
   * checking for non-standard things in the check directory ... OK
   * checking for detritus in the temp directory ... OK
   * DONE
   
   Status: OK
   
── R CMD check results ───────────────────────────────────── discord 1.2.3 ────
Duration: 1m 15.4s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
