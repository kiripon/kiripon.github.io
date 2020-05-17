---
title: Chrome を使って Unity 上で音声認識
date: 2015-08-23
---
# 構成
  * Websocket-sharp
  * Unity

# 使い方
  1. 適当なオブジェクトに以下のスクリプトをあたっちする.
  2. http://localhost:12002 をChrome でひらく
  3. マイクに話しかけるとUnity上のデバッグコンソールに認識したメッセージが表示される
``` C#
using UnityEngine;
using System;
using System.Collections;
using System.Configuration;
using WebSocketSharp;
using WebSocketSharp.Net;
using System.Text;
using WebSocketSharp.Server;
using System.Security.Cryptography.X509Certificates;
public class chromeVoiceRecog : MonoBehaviour {
  private HttpServer httpsv;
  public class MyService : WebSocketBehavior{
    public MyService(){
      base.IgnoreExtensions = true;
    }
    protected override void OnMessage (MessageEventArgs e)
    {
      // Get
      Debug.Log ("Get message:" + e.Data);
    }
    protected override void OnOpen(){
      Debug.Log ("Socket Open");
    }
    protected override void OnError(ErrorEventArgs e){
      Debug.Log ("Error:" + e.Message);
      Debug.Log ("Exception:" + e.Exception);
      Debug.Log ("hoge" + e.Exception.StackTrace);
    }
  }
  void Awake()
  {
    var port = 12002;
    var addr = "localhost";
    var fullUrl = addr + ":" + port;
    httpsv = new HttpServer ("http://" + fullUrl);
    httpsv.RootPath = "./htmlcontents"; // TODO: まともなパスに書き換える
    string[] fs = System.IO.Directory.GetFiles (@httpsv.RootPath, "*" );
    Debug.Log ("current path:" + fs[0]);
    httpsv.Log.Level = LogLevel.Trace;
    httpsv.OnGet += (sender, e) => {
      var req = e.Request;
      var res = e.Response;
      var path = req.RawUrl;
      Debug.Log("http request:"+req);
      if(path == "/") path += "index.html";
      var content = httpsv.GetFile(path);
      if(content == null){
        res.StatusCode = (int)HttpStatusCode.NotFound;
        res.WriteContent(
          System.Text.Encoding.UTF8.GetBytes(
          "File Not Found"));
        return;
      }
      if(path.EndsWith (".html")){
        res.ContentType = "text/html";
        res.ContentEncoding = Encoding.UTF8;
      }
      res.WriteContent(content);
    };
    httpsv.WaitTime = TimeSpan.FromSeconds (2);
    httpsv.AddWebSocketService<MyService> ("/MyService");
    httpsv.Start ();
    Debug.Log ("http server started with " + fullUrl);
  }
  void OnApplicationQuit()
  {
    httpsv.Stop ();
    Debug.Log ("websocket server exitted");
  }
}
```
