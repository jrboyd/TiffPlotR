#' Detect a safe number of cores for parallel computing
#'
#' Constrains by (1) CPU availability, honoring a SLURM allocation when present,
#' and (2) free system memory divided by per-core memory need.
#'
#' @param mem_per_core memory required per core in MB
#' @param reserve_user_cores leave at least this many cores for user running interactively
#' @importFrom parallel detectCores
.detect_cores = function(mem_per_core = 2000, reserve_user_cores = 1){
    available_cores = .detect_available_cores()
    available_cores = max(1, available_cores - reserve_user_cores)

    free_mem_mb = .free_system_memory_mb()
    max_cores_by_mem = max(1, floor(free_mem_mb / mem_per_core))

    final_cores = min(available_cores, max_cores_by_mem)
    final_cores
}

#' Number of CPUs available to this process
#'
#' Prefers the SLURM allocation (SLURM_CPUS_PER_TASK, else SLURM_CPUS_ON_NODE)
#' so we don't oversubscribe a shared node. Falls back to the physical core
#' count when not running under SLURM.
#' @importFrom parallel detectCores
.detect_available_cores = function(){
    for (var in c("SLURM_CPUS_PER_TASK", "SLURM_CPUS_ON_NODE")) {
        val = Sys.getenv(var, unset = NA)
        if (!is.na(val) && nzchar(val)) {
            n = suppressWarnings(as.integer(val))
            if (!is.na(n) && n > 0) return(n)
        }
    }
    n = parallel::detectCores()
    if (is.na(n) || n < 1) 1L else n
}

#' Free system memory in MB, cross-platform
#'
#' Returns Inf when it cannot be determined, so memory never spuriously
#' constrains core count on an unsupported platform.
.free_system_memory_mb = function(){
    sysname = Sys.info()[["sysname"]]
    mb = switch(sysname,
                "Linux"   = .free_mem_linux(),
                "Darwin"  = .free_mem_macos(),
                "Windows" = .free_mem_windows(),
                NA_real_
    )
    if (is.null(mb) || is.na(mb) || mb <= 0) Inf else mb
}

#' Linux: MemAvailable from /proc/meminfo (kB -> MB)
.free_mem_linux = function(){
    meminfo_path = "/proc/meminfo"
    if (!file.exists(meminfo_path)) return(NA_real_)
    meminfo = readLines(meminfo_path, warn = FALSE)
    line = grep("^MemAvailable:", meminfo, value = TRUE)
    if (length(line) == 0) {
        # Older kernels (<3.14) lack MemAvailable; fall back to MemFree.
        line = grep("^MemFree:", meminfo, value = TRUE)
    }
    if (length(line) == 0) return(NA_real_)
    kb = as.numeric(gsub("[^0-9]", "", line[1]))
    kb / 1024
}

#' macOS: free + inactive + speculative pages from vm_stat
.free_mem_macos = function(){
    out = tryCatch(system2("vm_stat", stdout = TRUE, stderr = FALSE),
                   error = function(e) character(0))
    if (length(out) == 0) return(NA_real_)

    # Header: "Mach Virtual Memory Statistics: (page size of 16384 bytes)"
    page_size = suppressWarnings(as.numeric(
        sub(".*page size of ([0-9]+) bytes.*", "\\1", out[1])))
    if (is.na(page_size)) page_size = 4096

    pages_of = function(label){
        line = grep(label, out, value = TRUE, fixed = TRUE)
        if (length(line) == 0) return(0)
        as.numeric(gsub("[^0-9]", "", line[1]))
    }
    # Pages reclaimable for a new allocation without paging out.
    free_pages = pages_of("Pages free:") +
        pages_of("Pages inactive:") +
        pages_of("Pages speculative:")
    free_pages * page_size / (1024^2)
}

#' Windows: FreePhysicalMemory from wmic (kB -> MB), PowerShell fallback
.free_mem_windows = function(){
    out = tryCatch(
        system2("wmic", c("OS", "get", "FreePhysicalMemory", "/Value"),
                stdout = TRUE, stderr = FALSE),
        error = function(e) character(0))
    line = grep("FreePhysicalMemory", out, value = TRUE)
    if (length(line) > 0) {
        kb = as.numeric(gsub("[^0-9]", "", line[1]))
        if (!is.na(kb) && kb > 0) return(kb / 1024)
    }
    # wmic is deprecated/absent on newer Windows; try PowerShell (CIM, kB).
    out = tryCatch(
        system2("powershell", c("-NoProfile", "-Command",
                                "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"),
                stdout = TRUE, stderr = FALSE),
        error = function(e) character(0))
    kb = suppressWarnings(as.numeric(gsub("[^0-9]", "", paste(out, collapse = ""))))
    if (!is.na(kb) && kb > 0) kb / 1024 else NA_real_
}
