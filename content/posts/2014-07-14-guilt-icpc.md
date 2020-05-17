---
title: ICPCの罪状
date: 2014-07-14
tags: ["C++"]
---
今年もicpcに参加しました。
去年よりも解いた問題数は増えたものの、僕が書いたBがバグってて同期にデバッグの手間を取らせたり、Eの解法が思いついたもののコードをバグらせて時間内にACできなかったりと今回も罪人となってしまいました。

Eはバグらせたくなかったので実装が楽なワーシャルフロイドをつかってグラフの直径を求めたのですがしょうもないミスをしてしまいました。

Eのコード
-----

```cpp
#include<iostream>
#include<climits>
using namespace std;
const int maxv = 1000;
int p[maxv];
int d[maxv];
int deg[maxv];
int g[maxv][maxv];

int n;
const int INF = INT_MAX/2;
void mkGraph(){
  fill(*g,g[maxv],INF);
  for(int i = 1;i <= n;i++)g[i][i] = 0; // <- ここを本番で書き忘れてた
  for(int i = 2;i <= n;i++){
    //cout << i << ' ' << p[i] << endl;
    if(deg[i] == 1 or deg[p[i]] == 1)continue;
    int from = i,to = p[i];

    g[from][to] = g[to][from] = d[i];
  }
}

void outgraph(){
  for(int i = 1;i <= n;i++){
    for(int j = 1;j <= n;j++){
      cout << g[i][j] << ' ';
    }
    cout << endl;
  }
}

int radius(){
  mkGraph();
  //outgraph();
  for(int k = 1;k <= n;k++){
    for(int i = 1;i <= n;i++){
      for(int j = 1;j <= n;j++){
	g[i][j] = min(g[i][j], g[i][k] + g[k][j]);
      }
    }
  }
  
  int ret = -1;
  for(int i = 1;i <= n;i++){
    for(int j = 1;j <= n;j++){
      if (g[i][j] == INF)continue;
      ret = max(g[i][j],ret);
    }
  }
  //cout << "rad:" << ret << endl;
  return ret;
}

int weight(){
  int ret = 0;
  for(int i = 2;i <= n;i++){
    ret += d[i];
  }
  //cout << "weight:"<<ret << endl;
  return ret;
}

int main(){
  while(true){
    cin >> n;
    if(n == 0)break;
    
    for(int i = 2;i <= n;i++){
      cin >> p[i];
    }
    for(int i = 2;i <= n;i++){
      cin >> d[i];
    }
    fill(deg,deg+maxv,0);
  
    for(int i = 2;i <= n;i++){
      deg[ p[i] ] += 1;
      deg[ i    ] += 1;
    }
  
    int result = 0;
    for(int i = 1;i <= n;i++){
      if(deg[i] == 1 or deg[p[i]] == 1)continue;
      result += d[i] * 2;
    }
    //cout << "junkai:" << result << endl;
    result -= radius();
    //outgraph();
    result += weight();
    cout << result << endl;
  }
  return 0;
}
```

Eはワーシャルフロイドのグラフの対角成分の初期化を忘れていたために起こっていたバグです。本当にしょうもないバグでした。
