let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ txEventsProducerConfig =
    { producerBrokers = ["127.0.0.1:19092"]
    , producerTimeout = 1000
    },
  ordersProducerConfig =
    { producerBrokers = ["127.0.0.1:19092"]
    , producerTimeout = 1000
    },
  poolsProducerConfig =
    { producerBrokers = ["127.0.0.1:19092"]
    , producerTimeout = 1000
    },
  txEventsTopicName = "tx-events",
  ordersTopicName = "orders-topic",
  poolsTopicName = "pools-topic-name",
  trackerProgrammConfig =
    { pollTime = 2
    },
  scriptsConfig =
    { swapScriptPath    = "./scripts/swap.uplc"
    , depositScriptPath = "./scripts/deposit.uplc"
    , redeemScriptPath  = "./scripts/redeem.uplc"
    , poolScriptPath    = "./scripts/pool.uplc"
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
    { storePath       = "./data/amm-executor"
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