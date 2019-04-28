<p align="center"> 
<img src="https://github.com/Godron629/tennis-racket/blob/master/images/tennis_racket.png" alt="tennis-racket logo" width="300"/>
</p>

# About 
A meta-circular evaluator for the Scheme based functional language, Racket.

[Racket Site](https://www.racket-lang.org "Racket's Homepage")

# Example Use

```racket
#lang racket/base

(require "startEval.rkt")

(startEval '(if (equal? 5 5) 99 55))
> 99
```

