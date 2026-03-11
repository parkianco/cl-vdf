# cl-vdf

Standalone Verifiable Delay Functions (VDF) implementation with **zero external dependencies**.

## Features

- **Wesolowski VDF**: Efficient proof construction
- **Pietrzak VDF**: Halving-based proofs
- **RSA groups**: Class group alternative
- **Proof verification**: Fast proof checking
- **Pure Common Lisp**: No CFFI, no external libraries

## Installation

```lisp
(asdf:load-system :cl-vdf)
```

## Quick Start

```lisp
(use-package :cl-vdf)

;; Generate VDF parameters
(let ((params (vdf-setup :bits 2048)))
  ;; Compute VDF (slow - intentional delay)
  (multiple-value-bind (output proof)
      (vdf-eval params *input* :time 1000000)
    ;; Verify (fast)
    (vdf-verify params *input* output proof :time 1000000)))
```

## API Reference

### Setup

- `(vdf-setup &key bits)` - Generate VDF parameters
- `(vdf-setup-from-seed seed &key bits)` - Deterministic setup

### Evaluation

- `(vdf-eval params input &key time)` - Compute VDF (slow)
- `(vdf-eval-with-proof params input &key time)` - Compute with proof

### Verification

- `(vdf-verify params input output proof &key time)` - Verify proof (fast)

### Utilities

- `(vdf-hash-to-group params data)` - Hash to group element
- `(vdf-serialize-proof proof)` - Serialize proof

## Testing

```lisp
(asdf:test-system :cl-vdf)
```

## License

BSD-3-Clause

Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
