package gnode

import (
    "bufio"
    "flag"
    "fmt"
    "github.com/goerlang/etf"
    "github.com/goerlang/node"
    "log"
    "os"
    "github.com/tidwall/gjson"
    "strconv"
    "runtime"
)

type srv struct {
    node.GenServerImpl
    completeChan chan bool
    serverName string
}

var SrvName string
var NodeName string
var LogFile string
var Cookie string
var err error
var EpmdPort int
var EnableRPC bool
var PidFile string

var enode *node.Node
var serverId int

// 根据命令行解析参数
func init() {
    flag.StringVar(&LogFile, "log", "", "log file. if not setted then output on console")
    flag.StringVar(&SrvName, "gen_server", "go_srv", "gonode gen_server name")
    flag.StringVar(&NodeName, "name", "gonode@localhost", "gonode node name")
    flag.StringVar(&Cookie, "cookie", "123", "cookie for gonode for interaction with erlang node")
    flag.IntVar(&EpmdPort, "epmd_port", 5588, "epmd port")
    flag.BoolVar(&EnableRPC, "rpc", false, "enable RPC")
    flag.StringVar(&PidFile, "pid_file", "", "pid file path")

    flag.IntVar(&serverId, "server_id", 10000, "新进程编号")
}

func Start() {
    startNode()
    startGenServer(SrvName)
}

func startNode() {
    // Parse CLI flags
    flag.Parse()

    setup_logging()
    write_pid()

    log.Println("node started")

    // Initialize new node with given name and cookie
    enode = node.NewNode(NodeName, Cookie)

    // Allow node be available on EpmdPort port
    err = enode.Publish(EpmdPort)
    if err != nil {
        log.Fatalf("Cannot publish: %s", err)
    }

    return
}


func startGenServer(serverName string) {
    // Create channel to receive message when main process should be stopped
    completeChan := make(chan bool)

    // Initialize new instance of srv structure which implements Process behaviour
    eSrv := new(srv)

    // Spawn process with one arguments
    enode.Spawn(eSrv, completeChan)

    // 给进程注册一个名称
    eSrv.Node.Register(etf.Atom(serverName), eSrv.Self)
    eSrv.serverName = serverName

    // RPC
    // if EnableRPC {
    //     // Create closure
    //     eClos := func(terms etf.List) (r etf.Term) {
    //         r = etf.Term(etf.Tuple{etf.Atom("gonode"), etf.Atom("reply"), len(terms)})
    //         return
    //     }

    //     // Provide it to call via RPC with `rpc:call(gonode@localhost, go_rpc, call, [as, qwe])`
    //     err = enode.RpcProvide("go_rpc", "call", eClos)
    //     if err != nil {
    //         log.Printf("Cannot provide function to RPC: %s", err)
    //     }
    // }

    // Wait to stop
    <-completeChan

    log.Println("进程结束了=========: %#v", serverName)
    return
}

// call back start ============================================================

// Init
func (gs *srv) Init(args ...interface{}) {
    // log.Printf("Init: %#v", args)

    // Store first argument as channel
    gs.completeChan = args[0].(chan bool)
}

// HandleCast
// Call `gen_server:cast({go_srv, gonode@localhost}, stop)` at Erlang node to stop this Go-node
func (gs *srv) HandleCast(message *etf.Term) {
    log.Printf("HandleCast: %#v", *message)

    // Check type of message
    switch req := (*message).(type) {
    case etf.Tuple:
        if len(req) > 0 {
            switch act := req[0].(type) {
            case etf.Atom:
                if string(act) == "ping" {
                    var self_pid etf.Pid = gs.Self
                    gs.Node.Send(req[1].(etf.Pid), etf.Tuple{etf.Atom("pong"), etf.Pid(self_pid)})
                }
            }
        }
    case etf.Atom:
        // If message is atom 'stop', we should say it to main process
        if string(req) == "stop" {
            if gs.serverName != SrvName {
                // log.Printf("结束进程: %#v", gs.serverName)
                log.Printf("结束进程, server name: %#v", gs.serverName)

                gs.Node.Unregister(etf.Atom(gs.serverName))
                gs.completeChan <- true
            }
        }
    }
}

// HandleCall
// Call `gen_server:call({go_srv, gonode@localhost}, Message)` at Erlang node
func (gs *srv) HandleCall(message *etf.Term, from *etf.Tuple) (reply *etf.Term) {
    // 尝试从message 中提取信息
    switch req := (*message).(type) {
    case etf.Tuple:
        if len(req) > 0 {
            json := byteString(req[1].([]byte))
            reply = call(req[0].(int), json)
        }
    case etf.Atom:
        if string(req) == "start_new_process" {
            // 启动一个新进程, 进程编号依次累加, 以gen_server,简称为前辍
            serverId += 1
            serverName := "gs_" + strconv.Itoa(serverId)

            log.Printf("new goroutine 创建新协程, server name: %#v", serverName)

            go startGenServer(serverName)
            // startGenServer(serverName)

            replyTerm := etf.Term(etf.Tuple{etf.Atom("ok"), etf.Atom(serverName)})
            reply = &replyTerm
        } else if string(req) == "info" {
            // 查看节点上启动了的进程信息
            registered := gs.Node.Registered()
            // log.Printf("node status 注册的进程: %#v, 协程总数量: %#v", registered, runtime.NumGoroutine())

            // 协程总数量
            tupleNumGoroutine := etf.Tuple{etf.Atom("num_goroutine"), runtime.NumGoroutine()}

            // 注册的进程的列表返回
            var listRegisterdGoroutine etf.List
            for i:=0; i<len(registered); i++{
                listRegisterdGoroutine = append(listRegisterdGoroutine, registered[i])
            }
            tupleServerName := etf.Tuple{etf.Atom("registered_goroutine_name"), listRegisterdGoroutine}

            // 返回
            replyTerm := etf.Term(etf.Tuple{etf.Atom("ok"), tupleNumGoroutine, tupleServerName})
            reply = &replyTerm
        }
    }

    return
}

// HandleInfo
func (gs *srv) HandleInfo(message *etf.Term) {
    log.Printf("HandleInfo: %#v", *message)
}

// Terminate
func (gs *srv) Terminate(reason interface{}) {
    log.Printf("Terminate: %#v", reason.(int))
}

// call back end ============================================================================

func setup_logging() {
    // Enable logging only if setted -log option
    if LogFile != "" {
        var f *os.File
        if f, err = os.Create(LogFile); err != nil {
            log.Fatal(err)
        }
        log.SetOutput(f)
    }
}

func write_pid() {
    log.Println("process pid:", os.Getpid())
    if PidFile != "" {
        file, err := os.Create(PidFile)
        if err != nil {
            log.Fatal(err)
        }
        defer file.Close()
        w := bufio.NewWriter(file)
        fmt.Fprintf(w, "%v", os.Getpid())
        w.Flush()
        log.Println("write pid in", PidFile)
    }
}


// private  funs
// 将byte[] 转成string
func byteString(p []byte) string {
    for i := 0; i < len(p); i++ {
        if p[i] == 0 {
            return string(p[0:i])
        }
    }
    return string(p)
}


func call(pid int, json string) (*etf.Term) {

    // log.Printf("元组的第一个变量值解码后是: %#v，　元组的第二个变量值解码后是: %#v", req[0].(int), json)

    if (pid == 10000) {
        name := gjson.Get(json, "name")
        age := gjson.Get(json, "age")
        email := gjson.Get(json, "email")

        // log.Printf("name: %#v, val: %#v", name, name.Str)
        // log.Printf("age: %#v, val: %#v", age, age.Num )
        // log.Printf("email: %#v, val: %#v", email, email.Str)

        // 回复一个元组
        tupleAge := etf.Term(etf.Tuple{etf.Atom("age"), age.Num})
        tupleName := etf.Term(etf.Tuple{etf.Atom("name"), name.Str})
        tupleEmail := etf.Term(etf.Tuple{etf.Atom("email"), email.Str})


        // replyTerm := etf.Term(etf.Tuple{etf.Atom("go_reply"), tupleAge, tupleName, tupleEmail, etf.Pid(gs.Self), gs.serverName})
        replyTerm := etf.Term(etf.Tuple{etf.Atom("go_reply"), tupleAge, tupleName, tupleEmail})

        return &replyTerm
    } else {
        replyTerm := etf.Term(etf.Atom("do_nothing"))
        return &replyTerm
    }

    // return &replyTerm

}


