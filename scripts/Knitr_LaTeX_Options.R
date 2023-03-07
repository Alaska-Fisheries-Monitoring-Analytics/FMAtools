## LATEX AND KNITR OPTIONS
#===========================

## -- LaTeX Options
# render_latex()


## -- knitR options
opts_chunk$set( cache        = FALSE,
                echo         = FALSE,
                eval         = TRUE,
                fig.align    = 'center',
                message      = FALSE,
                out.lines    = 4,
                out.width    = '0.8\\textwidth',
                results      = "asis",
                src.top      = NULL,
                src.bot      = NULL,
                size         = "small",
                tidy         = TRUE,
                warning      = FALSE,
                width.cutoff = 80)

# Hooks
# Optionally allow R Code chunks to be environments so we can refer to them.
# knit_hooks$set(rcode=function(before, options, envir)
# {
#   if (before)
#     sprintf('\\begin{rcode}\\label{%s}\\hfill{}', options$label)
#   else
#     '\\end{rcode}'
# })
#
# # the error messages are written in the Serror environment defined in preamble
# knit_hooks$set(error = function(x, options) {
#   sprintf('\\begin{Serror}\n%s\\end{Serror}\n', x)
# })

