thin.algorithm <- function(rec.df.orig, thin.par, reps) {
  reduced.rec.dfs <- vector("list", reps)
  DistMat.save <- rdist.earth(x1=rec.df.orig, miles=FALSE) < thin.par
  diag(DistMat.save) <- FALSE
  DistMat.save[is.na(DistMat.save)] <- FALSE
  SumVec.save <- rowSums(DistMat.save)
  df.keep.save <- rep(TRUE, length(SumVec.save))

  for (Rep in seq_len(reps)) {
    DistMat <- DistMat.save
    SumVec <- SumVec.save
    df.keep <- df.keep.save

    while (any(DistMat) && sum(df.keep) > 1) {
      RemoveRec <- which(SumVec == max(SumVec))
      if (length(RemoveRec) > 1) {
        RemoveRec <- sample(RemoveRec, 1)
      }

      SumVec <- SumVec - DistMat[, RemoveRec]
      SumVec[RemoveRec] <- 0L
      DistMat[RemoveRec, ] <- FALSE
      DistMat[, RemoveRec] <- FALSE
      df.keep[RemoveRec] <- FALSE
    }

    rec.df <- rec.df.orig[df.keep, , drop=FALSE]
    colnames(rec.df) <- c("Longitude", "Latitude")
    reduced.rec.dfs[[Rep]] <- rec.df
  }

  return(reduced.rec.dfs)
}
