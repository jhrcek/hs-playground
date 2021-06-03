# reanimate-demo

Generate animation video:
```bash
$ stack run reanimate-demo -- render
# now open the output.mp4 in your favourive video viewer
```

Start built in animation viewer in the browser - with instant reload on .hs file save
```bash
$ stack repl

-- in GHCi run command
:cmd reanimateLiveEntry "anim"
```
