package main

import (
    "github.com/goerlang/etf"
    "github.com/tidwall/gjson"
)



// func router() {

// }


// http://studygolang.com/articles/5207
// http://www.jb51.net/article/80771.htm




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


