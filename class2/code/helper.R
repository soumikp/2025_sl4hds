loaded_pkgs <- sub("package:", "", search()[grep("^package:", search())])

pkgs_to_keep <- c(getOption("defaultPackages"), "pacman", "base")

pkgs_to_unload <- setdiff(loaded_pkgs, pkgs_to_keep)

if (length(pkgs_to_unload) > 0) {
  p_unload(char = pkgs_to_unload)
  cat("Successfully unloaded the following packages:\n", paste(pkgs_to_unload, collapse = "\n"))
} else {
  cat("No non-base packages were found to unload.\n")
}
