package main

import (
    "github.com/goerlang/etf"
    // "github.com/tidwall/gjson"
    "log"
    // "github.com/tidwall/gjson"
    "regexp"
)

// ================================================================
type StrController struct  {
    // Controller
}


// http://www.cnblogs.com/golove/p/3270918.html
// 正则表达式使用doc
// 正则表达式demo
// http://www.cnblogs.com/golove/p/3269099.html
// http://xiaorui.cc/2016/03/16/%E5%85%B3%E4%BA%8Egolang-regexp%E6%AD%A3%E5%88%99%E7%9A%84%E4%BD%BF%E7%94%A8%E6%96%B9%E6%B3%95/

//erlang 调用demo:
//gen_server:call(GoMBox, {str, str_replace, StrRes, FindStr, ReplaceTo}).
func (this *StrController) Excute(message etf.Tuple) (*etf.Term) {
    // log.Printf("message str: %#v", message)
    // log.Printf("str =========================: %#v", message[1].(string))

    switch act := message[1].(type) {
    case etf.Atom:
        if string(act) == "str_replace" {
            str := message[2].(string)
            from := message[3].(string)
            to := message[4].(string)

            replyString := str_replace(str, from, to)
            replyTerm := etf.Term(etf.Tuple{etf.Atom("ok"), replyString})
            return &replyTerm
        } else {
            replyTerm := etf.Term(etf.Tuple{etf.Atom("action_undefine")})
            return &replyTerm
        }
    default:
        str := message[1].(string)
        reg := regexp.MustCompile(`<li>(.*)</li>`)
        m := reg.FindAllString(str, -1)
        log.Printf("match =========================: %#v", m)

        var lis etf.List
        for i:=0; i<len(m); i++ {
            lis = append(lis, m[i])
        }

        replyTerm := etf.Term(etf.Tuple{etf.Atom("ok"), lis})
        return &replyTerm
    }

}



// func (this *StrController) Excute(message etf.Tuple) (*etf.Term) {
//     // log.Printf("message str: %#v", message)

//     // log.Printf("str =========================: %#v", message[1].(string))

//     str := message[1].(string)

//     reg := regexp.MustCompile(`<li>(.*)</li>`)
//     m := reg.FindAllString(str, -1)
//     log.Printf("match =========================: %#v", m)

//     var lis etf.List
//     for i:=0; i<len(m); i++ {
//         lis = append(lis, m[i])
//     }

//     replyTerm := etf.Term(etf.Tuple{etf.Atom("ok"), lis})
//     return &replyTerm
// }


