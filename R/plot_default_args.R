# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# August 2014, Jurgen Symynck
# Copyright 2014, Jurgen Symynck
#
# For more info, visit http://www.openreliability.org/
#
# For the latest version of this file, check the Subversion repository at
# http://r-forge.r-project.org/projects/abernethy/
#
# Disclaimer:
#    The author is not affiliated with Dr. Abernethy or Wes Fulton - CEO of
#    Fulton Findings(TM) and author of the software package SuperSMITH
#-------------------------------------------------------------------------------
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# +-----------------------------------+
# |  execute this software with R:    |
# |  http://www.r-project.org/        |
# +-----------------------------------+


plot_default_args <- function(){
    paronly <- c("ask","fig", "fin","lheight","mai", "mar", "mex", "mfcol",
        "mfrow", "mfg","new","oma", "omd", "omi","pin", "plt", "ps", "pty",
        "usr","xlog", "ylog","ylbias")
        # parameters that can only be set using par()
        # see $par() for the origin of this list
    parreadonly <- c("xlog", "ylog", "adj", "ann", "ask", 
        "bg", "bty", "cex", "cex.axis", "cex.lab", 
        "cex.main", "cex.sub", "col", "col.axis", "col.lab", 
        "col.main", "col.sub", "crt", "err", "family", 
        "fg", "fig", "fin", "font", "font.axis",
        "font.lab", "font.main", "font.sub", "lab", "las", 
        "lend", "lheight", "ljoin", "lmitre", "lty", 
        "lwd", "mai", "mar", "mex", "mfcol", 
        "mfg", "mfrow", "mgp", "mkh", "new", 
        "oma", "omd", "omi", "pch", "pin", 
        "plt", "ps", "pty", "smo", "srt", 
        "tck", "tcl", "usr", "xaxp", "xaxs", 
        "xaxt", "xpd", "yaxp", "yaxs", "yaxt", 
        "ylbias")
        # par() parameter that can be set
        # par(no.readonly=TRUE)
    parplot <- unique(sort(c(parreadonly[!(parreadonly %in% paronly)],
        "type","xlim","ylim","log","main","sub","xlab","ylab",
          "ann","axes","frame.plot","panel.first","panel.last","asp")))
          # all valid (?) graphical parameters that can be supplied
          # to plot.default
    parplot
}