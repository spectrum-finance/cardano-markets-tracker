let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ txEventsLedgerProducerConfig =
    { producerBrokers = ["127.0.0.1:19092"]
    , producerTimeout = 1000
    },
  txEventsLedgerTopicName = "tx-events-ledger",
  txEventsMempoolProducerConfig =
    { producerBrokers = ["127.0.0.1:19092"]
    , producerTimeout = 1000
    },
  txEventsMempoolTopicName = "tx-events-mempool",
  trackerProgrammConfig =
    { pollTime = 2
    },
  retry =
    { sleepTime = 1000000
    },
  eventSourceConfig =
    { startAt =
        { slot = 9151725
        , hash = "26cb36001cb8ed7a7ab6060d3e2c4471be27722f98181ee8550696d43fc53de2"
        }
    },
  lederHistoryConfig =
    { storePath       = "./data/tracker-v2"
    , createIfMissing = True
    },
  nodeConfigPath = "/home/bromel/projects/cardano-dex-backend/config/preview/config.json",
  ledgerSyncConfig =
    { nodeSocketPath = "/home/bromel/projects/cardano-node/ipc/node.socket"
    , maxInFlight    = 256
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "./logs/tracker.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    , rootLogLevel = LogLevel.Info
    }
}