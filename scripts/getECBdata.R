require(RJSDMX)

getwd()
load_all()


flows <-  getFlows(provider = "OECD")
SDMXWrappers:::sdmxGetAllData(
    provider = "ECB",
    folder = '/Users/jankocizel/Downloads',
    flow = 'SSP'
)

SDMXWrappers:::sdmxGetAllData(
    provider = "OECD",
    folder = '/Users/jankocizel/Downloads',
    flow = 'STAN_IO_TOT_DOM_IMP'
)
