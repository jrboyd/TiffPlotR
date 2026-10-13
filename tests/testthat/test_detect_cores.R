library(testthat)

.with_env <- function(vars, code) {
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  on.exit({
    for (v in names(old)) if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old[v]))
  })
  for (v in names(vars)) if (is.na(vars[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(vars[v]))
  force(code)
}

test_that(".slurm_mem_mb reads per-node, then per-cpu, else NA", {
  .with_env(c(SLURM_MEM_PER_NODE = "32768", SLURM_MEM_PER_CPU = NA), {
    expect_equal(.slurm_mem_mb(), 32768)
  })
  .with_env(c(SLURM_MEM_PER_NODE = NA, SLURM_MEM_PER_CPU = "4000", SLURM_CPUS_PER_TASK = "4"), {
    expect_equal(.slurm_mem_mb(), 16000)
  })
  .with_env(c(SLURM_MEM_PER_NODE = NA, SLURM_MEM_PER_CPU = NA), {
    expect_true(is.na(.slurm_mem_mb()))
  })
})

test_that(".available_memory_mb is capped by the SLURM allocation", {
  .with_env(c(SLURM_MEM_PER_NODE = "1", SLURM_MEM_PER_CPU = NA), {
    expect_equal(.available_memory_mb(), 1)
  })
})

test_that(".auto_max_full_fetch scales with memory and channels, capped at 100e6", {
  expect_equal(.auto_max_full_fetch(14, mem_mb = 32 * 1024), floor(32 * 1024^3 / (14 * 80)))
  expect_lt(.auto_max_full_fetch(14, mem_mb = 32 * 1024), 100e6)
  expect_equal(.auto_max_full_fetch(5, mem_mb = 150 * 1024), 100e6)
  expect_equal(.auto_max_full_fetch(5, mem_mb = Inf), 100e6)
})
