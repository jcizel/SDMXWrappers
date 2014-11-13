getwd()
load_all()


flows <-  getFlows(provider = "ECB")
SDMXWrappers:::sdmxGetAllData(
    provider = "ECB",
    folder = '/Users/jankocizel/Downloads',
    flow = 'SSP'
)
