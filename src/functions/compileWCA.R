#' Compile wavelet coherence analysis.

compileWC = function(data){
  set.seed(01042020)
  wc = analyze.coherency(data,
                         my.pair = c("pm25_mean", "new_cases_smooth_detr"),
                         loess.span = 0.33,
                         dt = 1, dj = 1/12,
                         window.type.t = "bar", window.type.s = "bar",
                         window.size.t = 14, window.size.s = 1/4,
                         make.pval = TRUE, method = "white.noise",
                         n.sim = 100,
                         verbose = FALSE)
  return(wc)
}