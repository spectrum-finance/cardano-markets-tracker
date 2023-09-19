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
  secrets =
    { secretFile = "/etc/wallet1TS.json"
    , keyPass    = "your password"
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "./logs/tracker.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    , rootLogLevel = LogLevel.Info
    }
}