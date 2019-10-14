Rackchain
=========
Trivial implementation of a blockchain POC, just to play a bit with Racket.

## Quickstart

```sh
$ racket rackchain.rkt
Creating new blockchain
Listening on 127.0.0.1:6090
```

Use a simple HTTP client or a browser

```sh
$ curl -X GET http://localhost:6090/blockchain | jq
% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                               Dload  Upload   Total   Spent    Left  Speed
100    88    0    88    0     0   6285      0 --:--:-- --:--:-- --:--:--  6285
{
  "blocks": [
    {
      "data": "Genesis block",
      "timestamp": 1571064218,
      "valid": true,
      "nonce": 511087
    }
  ]
}
```

Add a block

```sh
$ curl -X POST -d "Send 2 BTC to Raiden" http://localhost:6090/blockchain
```
