---
title: std.crypto
description: Hashes, MACs, key-derivation, stream ciphers, and authenticated encryption — enough to build a login system.
---

import { Aside } from '@astrojs/starlight/components';

`std.crypto.*` collects the primitives needed for authentication, key derivation, and secure
communication. Every module is implemented in pure Sysl with byte-slice inputs and outputs;
no dynamic allocation in the hot paths, no hidden state.

<Aside type="caution" title="Not audited">
  These implementations are correct against the RFC test vectors but have not been reviewed
  by a third-party cryptographer. Production use is your call; the test suite is a good start,
  not a guarantee.
</Aside>

## SHA-256 {#sha256}

`std.crypto.sha256` — SHA-256 hash function as specified in FIPS 180-4. Produces a 256-bit
(32-byte) digest from arbitrary-length input.

```sysl
import std.crypto.sha256.{sha256}

sha256(data: []byte, out: []byte)        // out must be at least 32 bytes
```

```sysl
var digest: [32]byte
sha256("hello world", digest[:])
// digest = b94d27b9 934d3e08 ... (the familiar "hello world" SHA-256)
```

A hardware-accelerated variant `process_block_hw` / `read_hw_state` is available on TRISC
targets with the SHA accelerator enabled. It is a drop-in for the pure-software
`process_block`.

## HMAC-SHA256 {#hmac}

`std.crypto.hmac` — HMAC as specified in RFC 2104, instantiated with SHA-256. Tested against
every RFC 4231 vector.

```sysl
import std.crypto.hmac.{hmac_sha256}

hmac_sha256(key: []byte, msg: []byte, out: []byte)   // out is 32 bytes
```

Use for message authentication, cookie signing, or as a building block for
[PBKDF2](#pbkdf2).

## PBKDF2-HMAC-SHA256 {#pbkdf2}

`std.crypto.pbkdf2` — RFC 2898 key derivation function. Stretches a password with a salt and
an iteration count to produce key material suitable for a login system.

```sysl
import std.crypto.pbkdf2.{pbkdf2_hmac_sha256}

pbkdf2_hmac_sha256(password: []byte, salt: []byte, iterations: int, out: []byte)
```

Typical usage:

```sysl
var derived: [32]byte
pbkdf2_hmac_sha256(
    password.bytes(),
    salt.bytes(),
    100_000,                // OWASP 2023 minimum for PBKDF2-SHA256
    derived[:]
)
```

## ChaCha20 {#chacha20}

`std.crypto.chacha20` — the stream cipher as specified in RFC 8439. The core is a block
function that maps `(key, counter, nonce)` to 64 bytes of pseudorandom output; real
encryption XORs those bytes into the plaintext.

```sysl
import std.crypto.chacha20.{chacha20_block}

chacha20_block(key: *u32, counter: u32, nonce: *u32, out: *byte)
```

The core uses `u32` arithmetic throughout — the wrapping semantics of unsigned arithmetic in
Sysl (`u32 + u32` is `u32`, mod 2³²) map directly to ChaCha20's specification.

## ChaCha20-Poly1305 AEAD {#aead}

`std.crypto.aead` — authenticated encryption with associated data (ChaCha20-Poly1305) as in
RFC 8439. This is what you want for actual confidential communication.

```sysl
import std.crypto.aead.{seal, open}

seal(
    key:   []byte,                  // 32 bytes
    nonce: []byte,                  // 12 bytes (unique per key)
    aad:   []byte,                  // associated data (not encrypted, but authenticated)
    pt:    []byte,                  // plaintext
    ct:    []byte,                  // ciphertext output (same length as pt)
    tag:   []byte                   // 16-byte auth tag output
)

open(
    key:   []byte,
    nonce: []byte,
    aad:   []byte,
    ct:    []byte,                  // ciphertext input
    tag:   []byte,                  // auth tag input
    pt:    []byte                   // plaintext output (same length as ct)
) -> bool                            // false if tag verification fails
```

`open` returns `false` if the tag doesn't verify — **never** look at `pt` in that case, the
plaintext is unauthenticated garbage.

## A complete login example

```sysl
import std.crypto.pbkdf2.{pbkdf2_hmac_sha256}

struct User
    salt:    [16]byte
    derived: [32]byte
    iters:   int

verify(u: *User, password: string) -> bool
    var attempt: [32]byte
    pbkdf2_hmac_sha256(password.bytes(), u.salt[:], u.iters, attempt[:])

    // Constant-time comparison — don't leak byte-by-byte timing.
    var diff: u8 = 0
    for i in 0..<32
        diff = diff | (u.derived[i] ^ attempt[i])

    diff == 0u8
```

## See also

- The [source tree](https://github.com/edadma/trisc/tree/dev/std/crypto) has test vectors
  and detailed commentary in every module.
- [`std.encoding.binary`](/stdlib/overview/#stdencodingbinary) — for the byte-order primitives
  the hash modules build on.
- [`std.rand`](/stdlib/overview/#stdrand) when you need random nonces and salts (use a CSPRNG
  in production — this is a deterministic PRNG today).
