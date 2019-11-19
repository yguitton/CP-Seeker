df = data.frame(installed.packages())

# Ensure that a package is installed
ensure <- function(pkg, pkg_name, pb) {

  setWinProgressBar(pb,
    value = grep(paste0("\\b", pkg_name, "\\b"), names(pkgs)) / (length(pkgs) + 1),
    label = sprintf("Loading - %s...", pkg_name))

  # Get requirements
  installed_version <- df$Version[df$Package == pkg_name]
  breakpoint <- attr(regexpr("[<>=]+", pkg), "match.length")
  inequality <- substr(pkg, 1, breakpoint)
  required_version <- substr(pkg, breakpoint + 1, nchar(pkg))

  # Check if the installed version meets the specs
  if (length(installed_version) == 0) {
    return(FALSE)
  } else {
    return(eval(parse(text =
      paste0("numeric_version('", installed_version, "')",
             inequality,
             "numeric_version('", required_version, "')"))))
  }
}
