# 🐴orse
Something big is in the making.

🐴orse (also Horse from this point on) is a DSL written to make music
procedurally.

---

## Horse syntax:
```
guitar = use("guitar")
beat   = use("beat.mp3")

bpm = 120

loop 3/4
    play(guitar, C4)
    sleep(2) # 1 beat
    play(guitar, E3)
    sleep(1)
end

# all loops run together
loop 3/4
   play(beat)
end

save("horse.mp3")
```

