// Copyright 2012-2013 Metachord Ltd.
// All rights reserved.
// Use of this source code is governed by a MIT license
// that can be found in the LICENSE file.

/*
Package node provides interface for creation and publishing node using
Erlang distribution protocol: http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html

The Publish function allows incoming connection to the current node on 5858 port.
EPMD will reply with this port on name request for this node name

	enode := node.NewNode(name, cookie)
	err := enode.Publish(5858)
	if err != nil {
		log.Printf("Cannot publish: %s", err)
		enode = nil
	}


Function Spawn creates new process from struct which implements Process interface:

	eSrv := new(eclusSrv)
	pid := enode.Spawn(eSrv)

Now you can call Register function to store this pid with arbitrary name:

	enode.Register(etf.Atom("eclus"), pid)

*/
package node

import (
	"encoding/binary"
	"flag"
	"fmt"
	"github.com/goerlang/dist"
	"github.com/goerlang/epmd"
	"github.com/goerlang/etf"
	"log"
	"net"
	"strconv"
	"strings"
)

var nTrace bool

func init() {
	flag.BoolVar(&nTrace, "erlang.node.trace", false, "trace erlang node")
}

func nLog(f string, a ...interface{}) {
	if nTrace {
		log.Printf(f, a...)
	}
}

type regReq struct {
	replyTo  chan etf.Pid
	channels procChannels
}

type regNameReq struct {
	name etf.Atom
	pid  etf.Pid
}

type unregNameReq struct {
	name etf.Atom
}

type registryChan struct {
	storeChan     chan regReq
	regNameChan   chan regNameReq
	unregNameChan chan unregNameReq
}

type nodeConn struct {
	conn  net.Conn
	wchan chan []etf.Term
}

type systemProcs struct {
	netKernel        *netKernel
	globalNameServer *globalNameServer
	rpcRex           *rpcRex
}

type Node struct {
	epmd.NodeInfo
	Cookie     string
	port       int32
	registry   *registryChan
	channels   map[etf.Pid]procChannels
	registered map[etf.Atom]etf.Pid
	neighbors  map[etf.Atom]nodeConn
	sysProcs   systemProcs
}

type procChannels struct {
	in     chan etf.Term
	inFrom chan etf.Tuple
	ctl    chan etf.Term
	init   chan bool
}

// Behaviour interface contains methods you should implement to make own process behaviour
type Behaviour interface {
	ProcessLoop(pcs procChannels, pd Process, args ...interface{}) // method which implements control flow of process
}

// Process interface contains methods which should be implemented in each process
type Process interface {
	Options() (options map[string]interface{}) // method returns process-related options
	setNode(node *Node)                        // method set pointer to Node structure
	setPid(pid etf.Pid)                        // method set pid of started process
}

// NewNode create new node context with specified name and cookie string
func NewNode(name string, cookie string) (node *Node) {
	nLog("Start with name '%s' and cookie '%s'", name, cookie)
	// TODO: add fqdn support
	ns := strings.Split(name, "@")
	nodeInfo := epmd.NodeInfo{
		FullName: name,
		Name:     ns[0],
		Domain:   ns[1],
		Port:     0,
		Type:     77, // or 72 if hidden
		Protocol: 0,
		HighVsn:  5,
		LowVsn:   5,
		Creation: 0,
	}

	registry := &registryChan{
		storeChan:     make(chan regReq),
		regNameChan:   make(chan regNameReq),
		unregNameChan: make(chan unregNameReq),
	}

	node = &Node{
		NodeInfo:   nodeInfo,
		Cookie:     cookie,
		registry:   registry,
		channels:   make(map[etf.Pid]procChannels),
		registered: make(map[etf.Atom]etf.Pid),
		neighbors:  make(map[etf.Atom]nodeConn),
	}
	return node
}

func (n *Node) prepareProcesses() {
	n.sysProcs.netKernel = new(netKernel)
	n.Spawn(n.sysProcs.netKernel)

	n.sysProcs.globalNameServer = new(globalNameServer)
	n.Spawn(n.sysProcs.globalNameServer)

	n.sysProcs.rpcRex = new(rpcRex)
	n.Spawn(n.sysProcs.rpcRex)
}

// Spawn create new process and store its identificator in table at current node
func (n *Node) Spawn(pd Process, args ...interface{}) (pid etf.Pid) {
	options := pd.Options()
	chanSize, ok := options["chan-size"].(int)
	if !ok {
		chanSize = 100
	}
	ctlChanSize, ok := options["chan-size"].(int)
	if !ok {
		chanSize = 100
	}
	in := make(chan etf.Term, chanSize)
	inFrom := make(chan etf.Tuple, chanSize)
	ctl := make(chan etf.Term, ctlChanSize)
	initCh := make(chan bool)
	pcs := procChannels{
		in:     in,
		inFrom: inFrom,
		ctl:    ctl,
		init:   initCh,
	}
	pid = n.storeProcess(pcs)
	pd.setNode(n)
	pd.setPid(pid)
	go pd.(Behaviour).ProcessLoop(pcs, pd, args...)
	<-initCh
	return
}

// Register associates the name with pid
func (n *Node) Register(name etf.Atom, pid etf.Pid) {
	r := regNameReq{name: name, pid: pid}
	n.registry.regNameChan <- r
}

// Unregister removes the registered name
func (n *Node) Unregister(name etf.Atom) {
	r := unregNameReq{name: name}
	n.registry.unregNameChan <- r
}

// Registered returns a list of names which have been registered using Register
func (n *Node) Registered() (pids []etf.Atom) {
	pids = make([]etf.Atom, len(n.registered))
	i := 0
	for p, _ := range n.registered {
		pids[i] = p
		i++
	}
	return
}

func (n *Node) registrator() {
	for {
		select {
		case req := <-n.registry.storeChan:
			// FIXME: make proper allocation, now it just stub
			var id uint32 = 0
			for k, _ := range n.channels {
				if k.Id >= id {
					id = k.Id + 1
				}
			}
			var pid etf.Pid
			pid.Node = etf.Atom(n.FullName)
			pid.Id = id
			pid.Serial = 0 // FIXME
			pid.Creation = byte(n.Creation)

			n.channels[pid] = req.channels
			req.replyTo <- pid
		case req := <-n.registry.regNameChan:
			n.registered[req.name] = req.pid
		case req := <-n.registry.unregNameChan:
			delete(n.registered, req.name)
		}
	}
}

func (n *Node) storeProcess(chs procChannels) (pid etf.Pid) {
	myChan := make(chan etf.Pid)
	n.registry.storeChan <- regReq{replyTo: myChan, channels: chs}
	pid = <-myChan
	return pid
}

// Publish allow node be visible to other Erlang nodes via publishing port in EPMD
func (n *Node) Publish(port int) (err error) {
	nLog("Publish ENode at %d", port)
	l, err := net.Listen("tcp", net.JoinHostPort("", strconv.Itoa(port)))
	if err != nil {
		return
	}
	n.Port = uint16(port)
	aliveResp := make(chan uint16)
	go epmdC(n, aliveResp)
	creation := <-aliveResp
	switch creation {
	case 99:
		return fmt.Errorf("Duplicate name '%s'", n.Name)
	case 100:
		return fmt.Errorf("Cannot connect to EPMD")
	default:
		n.Creation = creation
	}

	go func() {
		for {
			conn, err := l.Accept()
			nLog("Accept new at ENode")
			if err != nil {
				nLog(err.Error())
			} else {
				wchan := make(chan []etf.Term, 10)
				ndchan := make(chan *dist.NodeDesc)
				go n.mLoopReader(conn, wchan, ndchan)
				go n.mLoopWriter(conn, wchan, ndchan)
			}
		}
	}()
	go n.registrator()
	n.prepareProcesses()
	return nil
}

func (currNode *Node) mLoopReader(c net.Conn, wchan chan []etf.Term, ndchan chan *dist.NodeDesc) {

	currNd := dist.NewNodeDesc(currNode.FullName, currNode.Cookie, false)
	ndchan <- currNd
	for {
		terms, err := currNd.ReadMessage(c)

		// log.Println("收到　terms=========: %#v", terms)

		if err != nil {
			nLog("Enode error: %s", err.Error())
			break
		}
		currNode.handleTerms(c, wchan, terms)
	}
	c.Close()
}

func (currNode *Node) mLoopWriter(c net.Conn, wchan chan []etf.Term, ndchan chan *dist.NodeDesc) {

	currNd := <-ndchan

	for {
		terms := <-wchan
		err := currNd.WriteMessage(c, terms)
		if err != nil {
			nLog("Enode error: %s", err.Error())
			break
		}
	}
	c.Close()
}

func (currNode *Node) handleTerms(c net.Conn, wchan chan []etf.Term, terms []etf.Term) {
	nLog("Node terms: %#v", terms)

	if len(terms) == 0 {
		return
	}
	switch t := terms[0].(type) {
	case etf.Tuple:
		if len(t) > 0 {
			switch act := t.Element(1).(type) {
			case int:
				switch act {
				case REG_SEND:
					if len(terms) == 2 {
						currNode.RegSend(t.Element(2), t.Element(4), terms[1])
					} else {
						nLog("*** ERROR: bad REG_SEND: %#v", terms)
					}
				default:
					nLog("Unhandled node message (act %d): %#v", act, t)
				}
			case etf.Atom:
				switch act {
				case etf.Atom("$go_set_node"):
					nLog("SET NODE %#v", t)
					currNode.neighbors[t[1].(etf.Atom)] = nodeConn{conn: c, wchan: wchan}
				}
			default:
				nLog("UNHANDLED ACT: %#v", t.Element(1))
			}
		}
	}
}

// RegSend sends message from one process to registered
func (currNode *Node) RegSend(from, to etf.Term, message etf.Term) {
	nLog("REG_SEND: From: %#v, To: %#v, Message: %#v", from, to, message)

	// log.Println("node:RegSend(): From: %#v", from)
	// log.Println("node:RegSend(): To: %#v", to)
	// log.Println("node:RegSend(): Message: %#v", message)

	var toPid etf.Pid
	switch tp := to.(type) {
	case etf.Pid:
		toPid = tp
	case etf.Atom:
		toPid = currNode.Whereis(tp)
	}
	currNode.SendFrom(from, toPid, message)
}

// Whereis returns pid of registered process
func (currNode *Node) Whereis(who etf.Atom) (pid etf.Pid) {
	pid, _ = currNode.registered[who]
	return
}

// SendFrom sends message from source to destination
func (currNode *Node) SendFrom(from etf.Term, to etf.Pid, message etf.Term) {
	nLog("SendFrom: %#v, %#v, %#v", from, to, message)
	pcs := currNode.channels[to]
	pcs.inFrom <- etf.Tuple{from, message}
}

// Send sends message to destination process withoud source
func (currNode *Node) Send(to etf.Pid, message etf.Term) {
	nLog("Send: %#v, %#v", to, message)
	if string(to.Node) == currNode.FullName {
		nLog("Send to local node")
		pcs := currNode.channels[to]
		pcs.in <- message
	} else {
		nLog("Send to remote node: %#v, %#v", to, currNode.neighbors[to.Node])

		msg := []etf.Term{etf.Tuple{SEND, etf.Atom(""), to}, message}
		currNode.neighbors[to.Node].wchan <- msg
	}
}

func epmdC(n *Node, resp chan uint16) {
	conn, err := net.Dial("tcp", "127.0.0.1:4369")
	if err != nil {
		nLog("Error calling net.Dial : %s", err.Error())
		resp <- 100
		return
	}
	defer conn.Close()

	epmdFROM := make(chan []byte)
	go epmdREADER(conn, epmdFROM)

	epmdTO := make(chan []byte)
	go epmdWRITER(conn, epmdFROM, epmdTO)

	epmdTO <- epmd.Compose_ALIVE2_REQ(&n.NodeInfo)

	for {

		select {
		case reply := <-epmdFROM:
			nLog("From EPMD: %v", reply)

			switch epmd.MessageId(reply[0]) {
			case epmd.ALIVE2_RESP:
				if creation, ok := epmd.Read_ALIVE2_RESP(reply); ok {
					resp <- creation
				} else {
					resp <- 99
				}
			}

		}

	}
}

func epmdREADER(conn net.Conn, in chan []byte) {
	for {
		buf := make([]byte, 1024)
		n, err := conn.Read(buf)
		if err != nil {
			in <- buf[0:n]
			in <- []byte{}
			return
		}
		nLog("Read from EPMD %d: %v", n, buf[:n])
		in <- buf[:n]
	}
}

func epmdWRITER(conn net.Conn, in chan []byte, out chan []byte) {
	for {
		select {
		case data := <-out:
			buf := make([]byte, 2)
			binary.BigEndian.PutUint16(buf[0:2], uint16(len(data)))
			buf = append(buf, data...)
			_, err := conn.Write(buf)
			if err != nil {
				in <- []byte{}
			}
		}
	}
}
