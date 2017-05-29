package main

import (
    "github.com/goerlang/etf"
    // "github.com/tidwall/gjson"
    "log"
    // "io/ioutil"
    // "net/http"
    // "github.com/qiniu/iconv"
    // iconv "github.com/djimenez/iconv-go"
    iconv "github.com/djimenez/iconv-go"
)

// ================================================================
type IconvController struct  {
    // Controller
}

func (this *IconvController) Excute(message etf.Tuple) (*etf.Term) {
    log.Printf("message: %#v", message)

    // url := byteString(message[1].([]byte))
    // log.Printf("url: %#v", url )

    from := string(message[2].(etf.Atom))
    to := string(message[3].(etf.Atom))
    // log.Printf("iconv: %#v", iconv_str(message[1].([]byte), "gb2312", "utf-8") )
    // log.Printf("iconv: %#v", iconv_str(message[1].([]byte), from, to) )

    reply := iconv_str(message[1].([]byte), from, to)

    replyTuple := etf.Tuple{etf.Atom("ok"), reply}

    // replyTerm := etf.Term(etf.Atom("curl"))
    replyTerm := etf.Term(replyTuple)

    return &replyTerm
}


func iconv_str(input []byte, from string, to string) string {
    res := make([]byte, len(input))
    res = res[:]

    // iconv.Convert(input, out, "gb2312", "utf-8")
    iconv.Convert(input, res, from, to)
    // log.Printf("out: %#v", string(out) )
    return string(res)
}


