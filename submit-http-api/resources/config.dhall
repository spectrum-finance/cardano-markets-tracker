let FeePolicy = < Strict | Balance | SplitBetween : List Text >
let CollateralPolicy = < Ignore | Cover >
let Network = < Mainnet | Preview >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ nodeSocketConfig =
    { nodeSocketPath = "/ipc/node.socket"
    , maxInFlight    = 256
    },
  httpConfig =
    { host = "0.0.0.0"
    , port = 8082
    },
  explorerConfig =
    { explorerUri = "https://explorer.spectrum.fi"
    , network = Network.Mainnet
    },
  deafultChangeAddr = "addr1qxdkj7t46gxcj8xpwyarcnfl3q2fpzq20qqpjvmsxlhs08q2aj6rwfkw4gqmftuvg385refyhc2elef9tz2ysgdtzw3snt8cjs",
  secrets =
    { secretFile = "/etc/wallet1TS.json"
    , keyPass    = "your password"
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "./logs/tracker.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    , rootLogLevel = LogLevel.Info
    },
  unsafeEval =
    { unsafeTxFee = +320000
    , exUnits = 165000000
    , exMem = 530000
    },
  httpSubmit =
    { submitUri = "http://localhost:8090/api/submit/tx"
    },
  spfPolicyId = "",
  spfTokenName = "",
  txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr1qxdkj7t46gxcj8xpwyarcnfl3q2fpzq20qqpjvmsxlhs08q2aj6rwfkw4gqmftuvg385refyhc2elef9tz2ysgdtzw3snt8cjs"
    }
}